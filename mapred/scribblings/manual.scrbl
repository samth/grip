#lang scribble/doc

@begin[(require scribble/manual)
       (require scribble/basic)
       (require (for-label (only-meta-in 0 typed/racket)))]

@title[#:tag "top"]{@bold{Mapred} - A Map/Reduce Framework for AWS}
@declare-exporting[mapred]

by Ray Racine (@tt|{ray.racine@gmail.com}|)

This library provides a simple, yet robust map/reduce framework targeted for running on AWS cloud services.

@table-of-contents[]

@section{Input}

The first major phase of a map/reduce operation is "splitting" the input files in splits or Block where each block is operated over with a map step. 

@subsection{Split Calculation}
@defmodule["input/split.rkt"]

Routines to calculate a set of Block from an arbitrary object as some location of size N given a max block size of M.

@defproc[(n-block [location String] [object-sz Nonnegative-Integer] [block-sz Nonnegative-Integer]) (Listof Block)]{
Given a object of size N at some location split up the object into a list of Blocks of no more than size block-sz.
}

@subsection{S3 Splitting}

@defmodule["input/s3-split.rkt"]

@defproc[(s3-split-bucket-prefix [bucket String] 
				 [prefix String] 
				 [marker String] 
				 [min-splits Natural]
				 [max-split-size Natural])
	 (Values BlockSet String)]{
Split the objects located at provided prefix.  An S3 object, regardless of size is split atomically.  For example, if the first S3 object is several gig in size, the returned BlockSet will split the entire range of the object (data file).

Bucket is the S3 @racket[bucket], @racket[prefix] is the S3 prefix path (the directory path if you will) to the S3 objects, @racket[marker] denotes the S3 object which was last split and therefore where splitting will resume when passed back into this procedure.
Each found S3 object is split into splits of @racket[max-split-size] until at least @racket[min-splits] have been calculated or all S3 objects are split.

FIXME: Consider making @racket[min-splits] @racket[(Option Natural)] where @racket[#f] indicates to split all objects in the S3 bucket prefix.
}

@section{Internals}

@subsection{Messages}
@defmodule["messages.rkt"]

@defstruct*[TaskMsg () #:prefab]{
The parent structure of all master <-> worker messages.
}

@defstruct*[(S3SplitMsgReq TaskMsg) ([bucket String] 
				     [prefix String]
				     [marker String]
				     [min-splits Natural]
				     [max-split-size Natural])
	    #:prefab]{
A map/reduce task message which instructs the reciever to split S3 objects at the designated @racket[bucket] and @racket[prefix] (path) starting at @racket[marker].  The task receiver will stop splitting when at least @racket[min-splits] have been determined or all S3 objects have been split.  

A @racket[marker] value of an empty string indicates to start at the beginning of the S3 listing.  @racket[max-split-size] indicates the max size in bytes of a split.

An S3 object is split atomically, i.e. an S3 object is never partially split.  Therefore very large S3 object (files) may result in an extensive list of splits.  Currently the SWF message size constrains the maximum number of splits from a single task.

By repeatedly scheduling this task message with a new marker a set of S3 objects is plit in stages of S3 objects at a time.
}

@defstruct*[(S3SplitMsgResp TaskMsg) ([blockset BlockSet]
				      [marker String])
	    #:prefab]{
The response to a @racket[S3SplitMsgReq].  The @racket[blockset] contains a the set of split blocks.  @racket[marker] denotes the last S3 object split.  If @racket[marker] is provided in a subsequent @racket[S3SplitMsgReq] splitting resumes from the @racket[marker] value.
}
