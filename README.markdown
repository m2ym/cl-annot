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

If you use Emacs, you may want to hightlight annotations. To do so, write the following code into your `.emacs`:

    (defvar cl-annot-face 'highlight)
    (font-lock-add-keywords 'lisp-mode `(("\\(?:^\\|[^,]\\)\\(@\\(?:\\sw\\|\\s_\\)+\\)" (1 ,cl-annot-face))))

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

    @type TYPESPEC

`type` is a macro which is equivalent to `(declare (type ...))`
form. For example,

    @type (integer v)

is equivalent to

    (declare (type integer v))

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

    defannotation NAME (:alias ALIAS :arity ARITY :inline INLINE) LAMBDA-LIST &body BODY

`defannotation` macro is a utility for creating annotations. Here is an example:

    (defannotation my-annot (x y)
        (:arity 2 :inline t)
      `(+ ,x ,y))

----

Copyright (C) 2011  Tomohiro Matsuyama <<tomo@cx4a.org>>
