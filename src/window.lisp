(in-package :glide)

;; input-modes 
(defclass glide-window ()
  ((widget
    :reader widget
    :initarg :widget)
   (input-mode
    :reader input-mode
    :initarg :input-mode)
   (input-state
    :reader input-state
    :initform nil)))


(defmethod initialize-instance :after ((window glide-window) &key widget input-mode)
  (declare (ignore widget input-mode))
  (with-slots (input-state input-mode) window
    (setf input-state
          (make-instance 'input-mode-state :input-mode input-mode))))


(defclass input-mode ()
  ((table
    :reader input-table)))


(defclass input-mode-state ()
  ((input-mode
    :initarg :input-mode)
   (accum
    :initform nil)
   (current-value
    :accessor current-value
    :initform '(nil . nil))
   (current-state
    :initform :neutral
    :documentation
    "State of the state-machine - can be one of:
  (:netural :valid :partial)"))
  (:documentation "The input mode state is based on a state-machine"))


(defmethod initialize-instance ((mode input-mode) &key table)
  (with-slots ((result table)) mode
    (setf result (make-input-table table)))
  (call-next-method))


(defun reset (state)
  (with-slots (input-mode accum current-value current-state) state
    (setf accum "")
    (setf current-value nil)1i
    (setf current-state :neutral)))


(defun step-input-state (state key)
  (with-slots (input-mode accum current-value current-state) state
    ;; next = nil OR (maybe char . maybe hash-table) 
    (let ((next (gethash (code-char key)
                         (or (and current-value (cdr current-value))
                             (input-table input-mode)))))

      ;; specific case: we know that this is terminating!
      (if (and next (car next) (not (cdr next)))
          (progn (reset state) (cons :release-valid (car next)))
        (progn
          (setf accum (concatenate 'string accum (string (code-char key))))
          (when next (setf current-value next))
          (ccase current-state
                 (:neutral (when next
                             (progn
                               (if (car next)
                                   (setf current-state :valid)
                                 (setf current-state :partial))
                               (cons :new-seq (car next)))))
                 (:partial (if next
                               (progn
                                 (when (car next) (setf current-state :valid))
                                 (setf current-value next)
                                 (cons :cont-seq (car next)))
                             (let ((ret-str accum))
                               (reset state)
                               (cons :release-invalid ret-str))))
                 (:valid (if next
                             (progn
                               (unless (car next) (setf current-state :partial))
                               (setf current-value next)
                               (cons :cont-seq (car next)))
                           (progn
                             (let ((ret-char (car current-value)))
                               (reset state)
                               (cons :release-valid ret-char)))))))))))


(defun on-keypress (window keyval keycode)
  (declare (ignore keycode))
  (if (< keyval (expt 2 15))
    (let ((result (step-input-state (input-state window) keyval)))
      (if result
          (progn
            (ccase (car result)
              (:new-seq nil)
              (:cont-seq nil)
              (:release-valid
               (text-buffer-insert-at-cursor
                (text-view-buffer (widget window))
                (string (cdr result))
                (utf-8-byte-length (string (cdr result)))))
              (:release-invalid
               (text-buffer-insert-at-cursor
                (text-view-buffer (widget window))
                (cdr result)
                (length (cdr result)))))
            t)))
    nil))


(defun make-input-table (input-table-source)
  (let ((table (make-hash-table)))
    (iter (for (str . output) in input-table-source)
          (for tmp = table)
          (for stack = (map 'list (lambda (x) x) str))
      (iter (while stack)
        (let* ((char (pop stack))
               (result (gethash char tmp)))
          (cond
            ;; case: char → (_ . table) & we want to updated table
            ((and stack result (cdr result))
             (setf tmp (cdr (gethash char tmp))))
            ;; case: char → (_ . nil) & we want to add table
            ((and stack result)
             (setf (gethash char tmp)
                   (cons (car result) (make-hash-table)))
             (setf tmp (cdr (gethash char tmp))))
            ;; case: char → nil & we want to add table
            (stack
             (setf (gethash char tmp) (cons nil (make-hash-table)))
             (setf tmp (cdr (gethash char tmp))))

            ;; case: char → (_ . _) we want to add char
            (result
             (setf (gethash char tmp) (cons output (cdr result))))
            ;; case char → nil and we ant to add char
            (t
             (setf (gethash char tmp) (cons output nil)))))))
    table))


;(defvar +none+ )
(defvar +unicode-input-mode+
  (make-instance
   'input-mode
   :table
   '(;; brackets
        (";<" . #\⟨)
        (";>" . #\⟩)
        (";[" . #\⦗)
        (";]" . #\⦘)
        (";{" . #\⦃)
        (";}" . #\⦄)
        (";<<" . #\⧼)
        (";>>" . #\⧽)

        ;; arrows
        (";to"  . #\→)
        (";fm"  . #\←)
        (";up"  . #\↑)
        (";dn"  . #\↓)
        (";To"  . #\⇒)
        (";Fm"  . #\⇐)
        (";Up"  . #\⇑)
        (";Dn"  . #\⇓)
        (";mto" . #\↦)
        (";mfm" . #\↤)
        (";mup" . #\↥)
        (";mdn" . #\↧)

        ;; logic and sets
        (";fa"   . #\∀)
        (";ex"   . #\∃)
        (";in"   . #\∈)
        (";u"    . #\∪)
        (";n"    . #\∩)
        (";c_"   . #\⊆)
        (";~c_"  . #\⊇)
        (";!c_"  . #\⊈)
        (";!~c_" . #\⊉)
        (";c"    . #\⊂)
        (";~c"   . #\⊃)

        ;; combinators
        (";o" . #\∘)
        (";O" . #\○)
        (";|-" . #\⊢)
        (";-|" . #\⊣)
        (";-o" . #\⊸)
        (";o-" . #\⟜)
        (";|\\" . #\ᚳ)

        ;; numeric & algebraic operations 
        (";:-" . #\÷)
        (";x"  . #\✕)
        (";*"  . #\⋅)
        (";^"  . #\∧)
        (";-^" . #\⊼)
        (";v"  . #\∨)
        (";-v" . #\⊽)
        (";v-" . #\⊻)
        (";-." . #\¬)

        ;; array transfomrations & operations
        (";O/"  . #\∅) ;; nul
        (";O|"  . #\⌽)
        (";O-"  . #\⊖)
        (";O\\" . #\⍉)
        (";.."  . #\¨)
        (";..." . #\…)

        ;; equality & comparisons
        (";^=" . #\≜)
        (";==" . #\≡)
        (";#\=" . ?≟)
        (";o=" . #\≗)
        (";~=" . #\≅)
        (";>=" . #\≥)
        (";!=" . #\≠)
        (";<=" . #\≤)

        ;; Subscripts
        (";_0" . #\₀)
        (";_1" . #\₁)
        (";_2" . #\₂)
        (";_3" . #\₃)
        (";_4" . #\₄)
        (";_5" . #\₅)
        (";_6" . #\₆)
        (";_7" . #\₇)
        (";_8" . #\₈)
        (";_9" . #\₉)
        (";_=" . #\₌)
        (";_-" . #\₋)
        (";_+" . #\₊)
        (";_a" . #\ₐ)
        (";_e" . #\ₑ)
        (";_h" . #\ₕ)
        (";_i" . #\ᵢ)
        (";_j" . #\ⱼ)
        (";_l" . #\ₗ)
        (";_m" . #\ₘ)
        (";_n" . #\ₙ)
        (";_o" . #\ₒ)
        (";_p" . #\ₚ)
        (";_r" . #\ᵣ)
        (";_s" . #\ₛ)
        (";_t" . #\ₜ)
        (";_u" . #\ᵤ)
        (";_v" . #\ᵥ)
        (";_x" . #\ₓ)


        ;; Superscripts
        (";^0" . #\⁰)
        (";^1" . #\1)
        (";^2" . #\²)
        (";^3" . #\³)
        (";^4" . #\⁴)
        (";^5" . #\⁵)
        (";^6" . #\⁶)
        (";^7" . #\⁷)
        (";^8" . #\⁸)
        (";^9" . #\⁹)
        (";^=" . #\⁼)
        (";^-" . #\⁻)
        (";^+" . #\⁺)


        ;; greek
        (";ga" . #\α)
        (";gb" . #\β)
        (";gc" . #\ψ)
        (";gd" . #\δ)
        (";ge" . #\ε)
        (";gf" . #\φ)
        (";gg" . #\γ)
        (";gh" . #\η)
        (";gi" . #\ι)
        (";gj" . #\ξ)
        (";gk" . #\κ)
        (";gl" . #\λ)
        (";gm" . #\μ)
        (";gn" . #\ν)
        (";go" . #\ο)
        (";gp" . #\π)
        (";gr" . #\r)
        (";gs" . #\σ)
        (";gt" . #\τ)
        (";gu" . #\θ)
        (";gv" . #\ω)
        (";gw" . #\ς)
        (";gx" . #\χ)
        (";gy" . #\υ)
        (";gz" . #\ζ)

        (";gA" . #\Α)
        (";gB" . #\Β)
        (";gC" . #\Ψ)
        (";gD" . #\Δ)
        (";gE" . #\Ε)
        (";gF" . #\Φ)
        (";gG" . #\Γ)
        (";gH" . #\Η)
        (";gI" . #\Ι)
        (";gJ" . #\Ξ)
        (";gK" . #\Κ)
        (";gL" . #\Λ)
        (";gM" . #\Μ)
        (";gN" . #\Ν)
        (";gO" . #\Ο)
        (";gP" . #\Π)
        (";gR" . #\R)
        (";gS" . #\Σ)
        (";gT" . #\Τ)
        (";gU" . #\Θ)
        (";gV" . #\Ω)
        (";gW" . #\Σ)
        (";gX" . #\Χ)
        (";gY" . #\Υ)
        (";gZ" . #\Ζ)

        
        ;; double-struck letters
        (";sA" . #\𝔸)
        (";sB" . #\𝔹)
        (";sC" . #\ℂ)
        (";sD" . #\𝔻)
        (";sE" . #\𝔼)
        (";sF" . #\𝔽)
        (";sG" . #\𝔾)
        (";sH" . #\ℍ)
        (";sI" . #\𝕀)
        (";sJ" . #\𝕁)
        (";sK" . #\𝕂)
        (";sL" . #\𝕃)
        (";sM" . #\𝕄)
        (";sN" . #\ℕ)
        (";sO" . #\𝕆)
        (";sP" . #\ℙ)
        (";sQ" . #\ℚ)
        (";sR" . #\ℝ)
        (";sS" . #\𝕊)
        (";sT" . #\𝕋)
        (";sU" . #\𝕌)
        (";sV" . #\𝕍)
        (";sW" . #\𝕎)
        (";sX" . #\𝕏)
        (";sY" . #\𝕐)
        (";sZ" . #\ℤ)

        ;; script/fancy letters
        (";ca" . #\𝒶)
        (";cb" . #\𝒷)
        (";cc" . #\𝒸)
        (";cd" . #\𝒹)
        (";ce" . #\ℯ)
        (";cf" . #\𝒻)
        (";cg" . #\ℊ)
        (";ch" . #\𝒽)
        (";ci" . #\𝒾)
        (";cj" . #\𝒿)
        (";ck" . #\𝓀)
        (";cl" . #\𝓁)
        (";cm" . #\𝓂)
        (";cn" . #\𝓃)
        (";co" . #\ℴ)
        (";cp" . #\𝓅)
        (";cq" . #\𝓆)
        (";cr" . #\𝓇)
        (";cs" . #\𝓈)
        (";ct" . #\𝓉)
        (";cu" . #\𝓊)
        (";cv" . #\𝓋)
        (";cw" . #\𝓌)
        (";cx" . #\𝓍)
        (";cy" . #\𝓎)
        (";cz" . #\𝓏)

        (";cA" . #\𝒜)
        (";cB" . #\ℬ)
        (";cC" . #\𝒞)
        (";cD" . #\𝒟)
        (";cE" . #\𝓔)
        (";cF" . #\𝓕)
        (";cG" . #\𝒢)
        (";cH" . #\ℋ)
        (";cI" . #\ℐ)
        (";cJ" . #\𝒥)
        (";cK" . #\𝒦)
        (";cL" . #\ℒ)
        (";cM" . #\ℳ)
        (";cN" . #\𝒩)
        (";cO" . #\𝒪)
        (";cP" . #\𝒫)
        (";cQ" . #\𝒬)
        (";cR" . #\ℛ)
        (";cS" . #\𝒮)
        (";cT" . #\𝒯)
        (";cU" . #\𝒰)
        (";cV" . #\𝒱)
        (";cW" . #\𝒲)
        (";cX" . #\𝒳)
        (";cY" . #\𝒴)
        (";cZ" . #\𝒵))))
