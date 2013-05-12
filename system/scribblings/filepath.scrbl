#lang scribble/base

@begin[(require scribble/manual		
		scribble/base)
       (require (for-label (only-meta-in 0 typed/racket)))]

@title{FilePaths}
@section{FilePath}

@defmodule["../filepath.rkt"]{
  @defproc[(make-FilePath [path : String]) FilePath]          
  
   Creates a FilePath instance from a given string path.  
   The actual instance returned is an AbsFilePath or RelFilePath if the given path string is absolute or relative.
}