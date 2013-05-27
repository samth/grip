#lang scribble/doc

@begin[(require scribble/manual		
                scribble/base)
       (require (for-label (only-meta-in 0 typed/racket)))]

@title[#:tag "top"]{@bold{System} A Typed Racket set of system routines.}

@table-of-contents[]

by Ray Racine (@tt{ray dot racine at gmail dot com})

This library provides a set of routines associate with a system.  Currently only Linux based systems are supported.

@include-section["filepath.scrbl"]

@include-section["interface.scrbl"]

