cl-annot
========

cl-annot is an general annotation library for Common Lisp.

cl-annot is tested under the following implementations:

* Allegro CL v8.2
* SBCL v1.0.45
* CMU CL v20b
* Clozure CL v1.6
* ECL v11.1.1
* GNU CLISP v2.48

Overview
--------

Annotations is a special syntax for annotating and transforming
forms. Annotations look like Python's decorator:

    @annot
    (defun foobar ()
      ...)

Any functions and macros can be annotations which takes one argument
by default. For example, if you define the following function,

    (defun trace (object)
      (print object)
      object)

you can use the function as an annotation like:

    @trace (+ 1 2)

This expression prints `3` and returns `3`. Internally, this
expression will be regarded as `(trace (+ 1 2))`.

Standard annotation `export` exports the symbol of the given
definition. For example,

    @export
    (defun foobar ()
      ...)

defines a function `foobar` and exports the symbol `foobar`. This
equivalents to:

    (progn
      (export 'foobar)
      (defun foobar ()
        ...))

Annotations help you to write the simple and declarative codes.

Usage
-----

Just write the following code at the header of each files:

    (annot:enable-annot-syntax)

After this code, `@...` syntax can be used.

Emacs Configuration
-------------------

If you use Emacs, it is recommended to install `misc/slime-annot.el`
which contains some features of annotations. After locating
`misc/slime-annot.el` into your loadpath, write the following code
into your `.emacs`.

    (require 'slime-annot)

Standard Annotations
--------------------

### Package: `annot.std`

This package contains very basic and useful annotations. You don't
need to `use-package` this package.

#### Annotation: `export`

    @export DEFINITION-FORM

`export` is a macro which adds an `export` form of the definition
form. For example,

    @export (defun f () ...)

is equivalent to

    (progn
      (export 'f)
      (defun f () ...))

#### Annotation: `ignore`

    @ignore VARIABLES

`ignore` is a macro which is equivalent to `(declare (ignore ...))`
form. For example,

    @ignore v

is equivalent to

    (declare (ignore v))

`ignore` can take a list of variables like:

    @ignore (a b c)

#### Annotation: `ignorable`

    @ignorable VARIABLES

Same as `ignore` annotation except that this is equivalent to

    (declare (ignorable v))

#### Annotation: `type`

    @type TYPESPEC NAME

`type` is a macro which is equivalent to `(declare (type ...))`
form. For example,

    @type integer v

is equivalent to

    (declare (type integer v))

#### Annotation: `optimize`

    @optimize QUALITY

`optimize` is a macro which is equivalent to `(declare (optimize
...))` form. For example,

    @optimize (speed 3)

is equivalent to

    (declare (optimize (speed 3)))

#### Annotation: `inline`

    @inline NAME

`inline` is a macro which is equivalent to `(proclaim (inline ...))`
or `(declare (inline ...))` form. If NAME is just a symbol,
declaration will be used. If NAME is a definition form, proclamation
will be used. For example,

    @inline f

is equivalent to

    (declare (inline f))

And

    @inline
    (defun f () ...)

is equivalent to

    (proclam (inline f))
    (defun f () ...)

### Package: `annot.eval-when`

This package contains annotations `eval-when` special form.

#### Annotation: `eval-when-compile`

    @eval-when-compile FORM

`eval-when-compile` is a macro which is equivalent to `(eval-when
(:compile-toplevel) ...)`. For example,

    @eval-when-compile
    (defun macro-util () ...)

is equivalent to

    (eval-when-compile (:compile-toplevel)
      (defun macro-util () ...))

#### Annotation: `eval-when-load`

    @eval-when-load FORM

Same as `eval-when-compile` except that this is equivalent to
`(eval-when (:load-toplevel) ...)`.

#### Annotation: `eval-when-execute`

    @eval-when-execute FORM

Same as `eval-when-compile` except that this is equivalent to
`(eval-when (:execute) ...)`.

#### Annotation: `eval-always`

    @eval-always FORM

`eval-always` is a macro which is equivalent to `(eval-when
(:compile-toplevel :load-toplevel :execute) ...)`.

### Package: `annot.doc`

This package contains documentation annotations.

#### Annotation: `doc`

    @doc DOCSTRING DEFINITION-FORM

`doc` is a macro which inserts documentation string into the
definition form. For example,

    @doc "docstring"
    (defun f () ...)

is equivalent to

    (defun f ()
      "docstring"
      ...)

Mixture of `export` annotation and `doc` annotation is allowed, means

    @export
    @doc "docstring"
    (defun f () ...)

works as you expected.

### Package: `annot.class`

This package contains annotations about classes.

#### Annotation: `metaclass`

    @metaclass METACLASS CLASS-DEFINITION-FORM

`metaclass` embeds `(:metaclsas METACLASS)` into class-options of
`CLASS-DEFINITION-FORM`. For example,

    @metaclass persistent-class
    (defclass foo ()
         ())

is equivalent to

    (defclass foo ()
         ()
      (:metaclass persistent-class))

#### Annotation: `export-slots`

    @export-slots CLASS-DEFINITION-FORM

`export-slots` adds `(export ...)` form for slots of
`CLASS-DEFINITION-FORM`. For example,

    @export-slots
    (defclass foo ()
         (bar baz))

is equivalent to

    (progn
      (export '(bar baz))
      (defclass foo ()
         (bar baz)))

It can also be used with `defstruct` as of the commit
`9043a74815a028a7db664f2fd77a8b009c736df9` (8/31,2013).

#### Annotation: `export-accessors`

    @export-accessors CLASS-DEFINITION-FORM

`export-accessors` adds `(export ...)` form for accessors
(i.e. readers, writers and accessors) of `CLASS-DEFINITION-FORM`. For
example,

    @export-accessors
    (defclass foo ()
         ((bar :reader bar-of)
          (bax :writer bax-of)
          (baz :accessor baz-of)))

is equivalent to

    (progn
      (export '(bar-of bax-of baz-of))
      (defclass foo ()
         ((bar :reader bar-of)
          (bax :writer bax-of)
          (baz :accessor baz-of))))

It can also be used with `defstruct` as of the commit
`9043a74815a028a7db664f2fd77a8b009c736df9` (8/31,2013).

#### Annotation: `export-constructors`

It can be used as of the commit
`9043a74815a028a7db664f2fd77a8b009c736df9` (8/31,2013).

According to the {CLHS: Macro
DEFSTRUCT}[http://www.lispworks.com/documentation/HyperSpec/Body/m_defstr.htm],
`defstruct` can define more than one constructor, for example:

    @export-constructors
    (defstruct (s (:constructor abya a c)
                  (:constructor abya2 a b c))
      a b c)

is equivalent to

    (progn
      (export '(abya abya2))
      (defstruct (s (:constructor abya a c)
                    (:constructor abya2 a b c)) a b c))

and it might have no constructor like this.

    (defstruct (s (:constructor nil)) a b c)

#### Annotation: `export-class` and `export-structure`

`export-class` combines `export`, `export-slots` and
`export-accessors`. `export-structure` also combines
`export-constructors` in addition.

### Package: `annot.slot`

This package contains annotations about slots.

#### Annotation: `optional`

    @optional INITFORM SLOT-SPECIFIER

`optional` embeds `:initarg SLOT-NAME` and `:initform INITFORM` into
`SLOT-SPECIFIER`. For example,

    (defclass c ()
         (@optional nil
          foo))

is equivalent to

    (defclass c ()
         ((foo :initarg :foo
               :initform nil)))

#### Annotation: `required`

    @required SLOT-SPECIFIER

`required` embeds `:initarg SLOT-NAME` and `:initform
(annot.slot:required-argument SLOT-NAME)` into `SLOT-SPECIFIER` so
that `MAKE-INSTANCE` will raise errors when no argument for the slot
given. For example,

    (defclass c ()
         (@required
          foo))

is equivalent to

    (defclass c ()
         ((foo :initarg :foo
               :initform (annot.slot:required-argument :foo))))

Writing Annotations
-------------------

As I mentioned, any functions and macros can be
annotations. Basically, if you have a function or a macro named
`annot`, the following code

    @annot (+ 1 2)

will be expanded like

    (annot (+ 1 2))

### Aliasing

You may use an alias for specifying annotations. This is useful when
you want to use more general names as annotation names. Actually,
`annot.std` uses this technique to overriding the meanings of symbols
in `common-lisp` package. Here is how to alias:

    (setf (annotation-real 'do) 'long-long-name)

Now you can use `do` as meaning `long-long-name` at annotations like:

    @do ...

### Multiple Arguments

By default, annotations can take only one argument. If you want to
write an annotation taking two or more arguments, you need to specify
a number of arguments into the annotation symbol like:

    (use-package :annot.core)
    
    (defun my-annot (x y) (+ x y))
    (setf (annotation-arity 'my-annot) 2)

Now you can use this annotation like:

    @my-annot 2 3
    ;; => 5

### Inlining

In some cases, you want annotations to be expanded at read-time. You can do it by:

    (setf (annotation-inline-p 'annot) t)

Be caseful to use feature.

### Macro: `defannotation`

    defannotation NAME LAMBDA-LIST (:alias ALIAS :arity ARITY :inline INLINE) &body BODY

`defannotation` is an utility macro for creating annotations. Here is an example:

    (defannotation my-annot (x y)
        (:arity 2 :inline t)
      `(+ ,x ,y))

### Annotation: `annotation`

    annotation (:alias ALIAS :arity ARITY :inline INLINE) FUNCTION-DEFINITION-FORM

`annotation` is an annotation for creating annotations in a way of
`defannotation`. Here is an example:

    @annotation (:arity 2 :inline t)
    (defmacro my-annot (x y)
      `(+ ,x ,y))

----

Copyright (C) 2011  Tomohiro Matsuyama <<tomo@cx4a.org>>
