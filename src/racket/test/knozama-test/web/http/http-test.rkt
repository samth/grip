#lang racket 

(provide doit url)

(require knozama/web/uri
	 knozama/web/http/http
	 (planet neil/htmlprag:1:6))

;;	 knozama/xml/ssax/ssax)

(define url (uri "http" (authority #f "www.amazon.com" #f) "/" #f #f))

(define (doit)
  (let-values (((h p) (http-invoke 'GET url (list "Host: www.amazon.com")  #f)))
    (displayln h)
    (html->sxml p)))

    ;; (do ((ch (read-char p) (read-char p)))
    ;; 	((eof-object? ch) (begin (newline) (displayln "ALL DONE")))
    ;;   (display ch))))
