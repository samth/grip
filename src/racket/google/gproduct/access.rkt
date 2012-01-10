#lang typed/racket/base

(provide
 load-key)

(require/typed racket/base
	       ((read read-creds) (Input-Port -> (Listof (Pair Symbol String)))))

(require
 (only-in (planet knozama/common:1/std/opt)
	  opt-apply-orelse))

(: default-access-config Path)
(define default-access-config
  (build-path (find-system-path 'home-dir)
	      ".google-api.sexp"))

(: load-key ((Option Path) -> String))
(define (load-key path)
  
  (define lookup (inst assoc Symbol String))
  (define value  (inst cdr Symbol String))
  
  (: cred-value (Symbol (Listof (Pair Symbol String)) -> String))
  (define (cred-value sym props)
    (opt-apply-orelse  (lookup sym props) value  ""))
  
  (let ((path (if path path default-access-config)))
    (call-with-input-file path
      (lambda: ((ip : Input-Port))
	(let: ((props : (Listof (Pair Symbol String)) (read-creds ip)))
	  (cred-value 'access-key props))))))
