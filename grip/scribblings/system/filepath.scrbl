#lang scribble/manual

@title{Paths}
@section{FilePath}

@defmodule[grip/system/filepath]{
  @defproc[(make-FilePath [path : String]) FilePath]          
  
   Creates a FilePath instance from a given string path.  The actual
   instance returned is an AbsFilePath or RelFilePath if the given
   path string is absolute or relative.  
}