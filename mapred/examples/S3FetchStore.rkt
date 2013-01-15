#| Simple example of fetching and storing a set of files into S3 |#

#lang typed/racket/base

(provide 
 main)

(require 
 racket/pretty
 (only-in httpclient/uri
          parse-uri)
 (only-in "../types.rkt"
          Block Range
          BlockSet)
 (only-in "../s3-fetchstore.rkt"
          fetch-blockset
          store-blockset))

(: main (-> Void))
(define (main)
  
  (define (fetch)
    (fetch-blockset (BlockSet (assert (parse-uri "s3://odbd/itemset/input"))
                              (list (Block "part-00110" (Range 0 99))
                                    (Block "part-00111" (Range 100 299))
                                    (Block "part-00112" (Range 100 1099))))
                    (string->path "/tmp")))
  
  (define (store)
    (store-blockset (BlockSet (assert (parse-uri "file:///tmp"))
                              (list (Block "part-00110" #f)
                                    (Block "part-00111" #f)
                                    (Block "part-00112" #f)))
                    (assert (parse-uri "s3://odbd/itemset/output"))))
  
  (fetch) (store) (void))


