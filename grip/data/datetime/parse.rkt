#lang typed/racket/base

(provide
 parse-date
 parse-julian-day
 parse-date-string)

(require
 racket/match
 (only-in grip/data/opt
	  opt-map)
 (only-in "convert.rkt"
	  julian-day-number)
 (only-in "types.rkt"
	  Date))

(: ddmmyy-rx Regexp)
(define ddmmyy-rx #px"(\\d{1,2})/(\\d{1,2})/(\\d{4}|\\d{2}$)")

(: parse-date-string (String -> (Option (Listof (Option String)))))
(define (parse-date-string str)
  (let ((ds (regexp-match ddmmyy-rx str)))
    (if (pair? ds)
	(cdr ds)
	#f)))

(: s->n ((Option String) -> (Option Number)))
(define (s->n ostr)
  (opt-map ostr string->number))

(: parse-julian-day (String -> (Option Integer)))
(define (parse-julian-day str)
  (opt-map (parse-date-string str)
	   (λ: ((nums : (Listof (Option String))))	       
	       (match (filter fixnum? (map s->n nums))
		      ((cons m (cons d (cons y _)))
		       (julian-day-number d m y))
		      (_ #f)))))

(: parse-date (String -> (Option Date)))
(define (parse-date str)
  (opt-map (parse-date-string str)
	   (λ: ((nums : (Listof (Option String))))	       
	       (match (filter exact-integer? (map s->n nums))
		      ((cons m (cons d (cons y _)))
		       (Date y m d))
		      (_ #f)))))
