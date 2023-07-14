

(defclass gtk4-backend (backend) ())

(defgeneric reify ((widget widget) (backend gtk4-backend)))


