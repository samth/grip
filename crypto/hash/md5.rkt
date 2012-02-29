#lang typed/racket/base

(provide md5-file-hex
	 md5-file-bytes
	 md5-bytes
	 md5-hex)

(require/typed file/md5
	       (md5 ((U String Bytes Input-Port) Boolean -> Bytes)))

(: md5-hex ((U String Bytes Input-Port) -> Bytes))
(define (md5-hex in)
  (md5 in #t))

(: md5-bytes ((U String Bytes Input-Port) -> Bytes))
(define (md5-bytes in)
  (md5 in #f))

;; MD5 as a bytevector
(: md5-file-bytes (Path -> Bytes))
(define (md5-file-bytes fname)
  (let ((inf (open-input-file fname #:mode 'binary)))
    (let ((hash (md5-bytes inf )))
      (close-input-port inf)
      hash)))

;; MD5 as a hex chars
(: md5-file-hex (Path -> Bytes))
(define (md5-file-hex fname)
  (let ((inf (open-input-file fname #:mode 'binary)))
    (let ((hash (md5-hex inf)))
      (close-input-port inf)
      hash)))
