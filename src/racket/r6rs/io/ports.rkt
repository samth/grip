#lang typed/racket/base
 
(provide 
 Transcoder 
 Codec
 make-transcoder
 string->bytevector)

(require/typed  "private/ports-typed.rkt"
		(opaque Transcoder transcoder?)
		(opaque Codec      codec?)
		(make-transcoder (case-lambda (Codec -> Transcoder)
					 (Codec Symbol -> Transcoder)
					 (Codec Symbol Symbol -> Transcoder)))
		(string->bytevector (String Transcoder -> Bytes)))

