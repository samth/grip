#lang r6rs

(library
 (knozama xml ssax parser-error)
 
 (export parser-error ssax:warn)
 
 (import 
  (rnrs base)
  (rnrs conditions)
  (rnrs exceptions)
  (only (rnrs io ports)
	port?)
  (only (knozama xml sxml env)
	cerr nl))
 
 (define parser-error
   (lambda  args
     (if (port? (car args))
	(cerr nl "Error at position " 
	      ;; (file-position (car args)) nl
	      (cdr args))
	(cerr nl "Error in error handler: its first parameter is not a port" 
	      nl args))
     (cerr nl)
     (raise
      (condition (make-error)
		 (make-message-condition "Error at position ?? ")
		 (make-irritants-condition  (cdr args))))))
 
 (define ssax:warn
   (lambda  args
     (if (port? (car args))
	(cerr nl "Warning at position " 
	      ;; (file-position (car args)) nl
	      (cdr args) nl)
	#f)))
  
)

