# About

Glide is a data editor, which puts it in contrast to programe like VSCode,
Atom, (Neo)Vim and Emacs. While the latter which are intended for editing
text, and feature plugins to offer a richer way of interacting with a particular
type of text (program, markup, config files etc.), Glide is intended to allow
interaction with many different types of data (text, images, databases, etc.),
and to allow interaction with this data from many different perspectives.

However, as developers we interact with many different types of data - programs,
images, databases, colours, version-controlled repositories, 3D objects,
etc. For many of these entities, a text editing area is not necessarily the best
way to interact with them. For example, we may want to use a colour picker
instead of typing in a hex code, see a database schema as a diagram etc. Glide
aims to provide more generic abstractions that will enable plugins to more
elegantly handle these different data-types.

The primary goal is not for Glide to replace heavyweight applications that
already exist for complex file formats (like Blender or Photoshop). Instead, the
goal is two-fold:
+ Provide developers a variety of ways of interacting with files that would
  traditionally use a text-editor. For exaple, we might use a color picker
  widget to edit a colour code in a program source, visualise a function
  as a control-flow diagram, or view and edit class definitions via UML
  diagrams. A key goal is to be able to seamlessly swap between these different
  modes of interaction, depending on what task we want to perform.
+ Provide a more tightly integrated development experience by reducing the need
  to switch to other applications for simple tasks - like viewing an image or 3D
  object (not necessarily editing it).


# Architecture

Glide is build on an architecture related to the MVC framework, except we have
*models*, *views* and *bridges*. 

+ A model is simply a representation of a particular datatype (text, image,
  etc.). Models are arranged in a hierarchy, e.g. 'c++ source code' would be
  below 'text' in a hierarchy of models.
+ A view enables interaction with a particular type of model, like text, source
  code, etc. A view may only enable viewing of the underlying model, or it might
  provide some means of interacting with it. 
+ A bridge enables models to interact with each other. A bridge may relate the
  entirety of two models together, or just part of them. Further a bridge may be
  one-way, or two way. For example, we might have a one-way bridge that converts
  source code into an image (representing, e.g. a dependency graph). We might
  also have a two-way bridge which links a particular section of source code
  (like a colour code) to a colour model, where changes to the model result in
  changes to the source code. 
  
