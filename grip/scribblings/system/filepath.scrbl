#lang scribble/manual

@(require scribble/eval
	  racket/sandbox
	  (for-label grip/system/filepath)
	  (for-label (only-meta-in 0 typed/racket)))

@title{Paths}
@section{FilePath}

Unfortunately I cannot seem to get a useful set of example usages to work.

@defmodule[grip/system/filepath]{

  Constants

  Data structures.

  @defstruct*[Extension ([x String])]
  A file extension.  No validation is performed.

  @defstruct*[FilePath ([p String])]
  A general *nix system path.  May be a relative or absolute path.  A path to a directory or a file resource.

  @defstruct*[(RelFilePath FilePath) ()] 
  A relative @racket[FilePath], a file path that does not start with a '/'.

  @defstruct*[(AbsFilePath FilePath) ()]
  An absolute @racket[FilePath], an absolute file path always starts with a '/'.
 
  Constructing instances of FilePath

  @defproc[(make-FilePath [path String]) FilePath]          
  
  Creates a FilePath instance from a given string path.  The actual
   instance returned is an @racket[AbsFilePath] or
   @racket[RelFilePath] depending on the given path string is absolute
   or relative.

  @defproc[(make-AbsFilePath [path String]) AbsFilePath]

  Creates an @racket[AbsFilePath] from the given path string.  If the
  provided path string is a relative path string, the path string is
  made absolute by prefixing a '/'.

  @defproc[(make-RelFilePath [path String]) RelFilePath]

  Creates an @racket[RelFilePath] from the given path string.  If the
  provided path string is an absolute path string, the path string is
  made relative by dropping the leading '/'. 
 
  File name Extension.

  @defproc[(make-Extension [extension String]) Extension]
  
  Creates an @racket[Extension] from the provided string.  Currently
  no validation is performed.

  @interaction[
  #:eval (begin (parameterize ([sandbox-path-permissions 
                                 (list (list 'read "/usr/local/racket/bin"))]
                               [sandbox-output 'string]
                               [sandbox-error-output 'string])
    (make-evaluator 'typed/racket
      #:requires (list 'grip/system/filepath))))

  (make-Extension "txt")
  ]

}





