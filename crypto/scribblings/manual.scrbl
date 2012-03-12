#lang scribble/doc

@begin[(require scribble/manual		
		scribble/base)
       (require (for-label (only-meta-in 0 typed/racket)))]

@title[#:tag "top"]{@bold{Crypto} Typed Racket crypto procedures}

@declare-exporting[(planet rpr/crypto:1)]

@table-of-contents[]

by Ray Racine (@tt{ray dot racine at gmail dot com})

Provides a pure Typed Racket implementation of SHA-256 and typed wrappers for existing functions sucha as Base64 encoding, SHA-1 and MD-5.

@section{Base64}

Base64 encoding and decoding routines.

@defproc[(base64-encode [bytes Bytes]) String]{
Base64 encode a byte array to a string.
}

@defproc[(base64-decode [str String]) Bytes]{
Base64 decode the given string to an array of bytes.			
}

@section{Hash}
Various cryptography hash functions.

@subsection{MD5}
@defmodule[(planet rpr/crypto/hash/md5)]{

@deftogether[[
@defproc[(md5-hex [array (U String Bytes Input-Port)]) Bytes]
@defproc[(md5-bytes [array (U String Bytes Input-Port)]) Bytes]
]]{
Typed wrappers around the MD5 procedures in @tt{file/md5} in untyped Racket.
}

@deftogether[[
@defproc[(md5-file-hex [fname Path]) Bytes]
@defproc[(md5-file-bytes [fname Path]) Bytes]
]]{
Typed wrappers around the MD5 procedures in @tt{file/md5} in untyped Racket except takes the path to a file on the file system
}

}

@subsection{SHA-1}

@defmodule[(planet rpr/crypto/hash/sha1)]{

@defproc[(sha1-bytes [bytes Bytes]) Bytes]{
Typed wrapper around sha1-bytes procedure provided by Racket's @tt{openssl/sha1}.
}

}

@subsection{SHA-256}

Pure Typed Racket implementation of SHA-256.

@defmodule[(planet rpr/crypto/hash/sha256)]{

@defproc[(sha256 [data (U Bytes String)]) Bytes]{
Hash the given data using SHA-256 algorithm.
}

}

@section{HMAC}

@defmodule[(planet rpr/crypto/hmac)]{

@defproc[(hmac-sha256 [secret (U String Bytes)] [message (U Bytes String)]) Bytes]{
Generates the an HMAC signature of the @racket{message} with the @racket{secret}.
}

@defproc[(hmac-sha1 [secret (U String Bytes)] [message (U Bytes String)]) Bytes]{
Similar to @racket{hmac-sha256} except using SHA-1.
}

}


