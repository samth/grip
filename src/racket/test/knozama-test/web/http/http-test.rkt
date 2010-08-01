#lang racket 

(provide doit url)

(require knozama/web/uri
	 knozama/web/http/http
	 (planet neil/htmlprag:1:6))

;;	 knozama/xml/ssax/ssax)

(define url (uri "http" (authority #f "www.amazon.com" #f) "/" #f #f))

(define url2 (uri "http" (authority #f "www.publicschoolreview.com" #f)  "/schools-by-distance/33496/10/0/0" #f #f))

(define (doit)
  (let-values (((h p) (http-invoke 'GET url (list "Host: www.amazon.com")  #f)))
    (displayln h)
    (html->sxml p)))

(define (doit2)
  ;;http://www.publicschoolreview.com/schools-by-distance/33496/10/0/0
  (let-values (((h p) (http-invoke 'GET url2 (list "Host: www.publicschoolreview.com")  #f)))
    (displayln h)
    (html->sxml p)))

;; (doit2)

    ;; (do ((ch (read-char p) (read-char p)))
    ;; 	((eof-object? ch) (begin (newline) (displayln "ALL DONE")))
    ;;   (display ch))))
