#lang scribble/manual
       
@title{Factors}    

@defmodule[pgm/factor]

@section{Data Structure}

@defstruct*[Factor ([vars (Listof Symbol)]
                    [vals (Vectorof Float)])]{
Defines a Factor consisting of a list of variable symbols and their values.
The cardinality of the variables are part of the Model.  
The variable values are cycle through their cardinalities from left to right.

Assume Factor F has variables A,B,C with cardinalities 2, 3, and 2 respectively.
Then the values of the vector, vals, are as follows: @[linebreak]
vals(0) = a0, b0, c0 @[linebreak]
vals(1) = a1, b0, c0 @[linebreak]
vals(2) = a0, b1, c0 @[linebreak]
vals(3) = a1, b1, c0 @[linebreak]
vals(4) = a0, b2, c0 @[linebreak]
}
          
@section{Factor Methods}
                                             
@defproc[(new-factor [model Model] [variables (Listof Symbol)]
                     [values (Vectorof Float)]) Factor]{
Constructs a Factor using the provided model for the variables cardinalities.
It is an error if a variable is not defined in the model.
It is an error if the given vector is not of the appropriate lenght for the variable cardinalities.
}
                                                       
@defproc[(factor-cardinalities [model Model] [factor Factor]) (Listof Integer)]{
The cardinalities of a Factor's variables as specified in the Model.
The order of the cardinalities match the order of the variables in the Factor.
}

@section{Factor Math}

@defproc[(f* [f1 Factor] [f2 Factor]) Factor]{
Factor product of the two given Factors.
}

@defproc[(f+ [f1 Factor] [f2 Factor]) Factor]{
Factor sum of the two given Factors.
}

@section{Log Space}

@defproc[(new-in-log-space [factor Factor]) Factor]{
Constructs a new Factor the same as the given factor except the new factor's values are the log values.
                                                    }

@defproc[(new-from-log-space [factor Factor]) Factor]{
Constructs a new Factor the same as the given factor except the new factor's values are exponentated.                                                      
                                                      }