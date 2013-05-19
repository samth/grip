#lang racket

;;slightly adapted from code by Jeff Sisskind

(provide  
 ; (rename (d+ +))
 ;         (rename (d- -))
 ;         (rename (d* *))
 ;         (rename (d/ /))
 ;         (rename (dsqrt sqrt))
 ;         (rename (dexp exp))
 ;         (rename (dlog log))
 ;         (rename (dadfloor adfloor))         
 ;         (rename (dexpt expt))
 ;         (rename (dsin sin))
 ;         (rename (dcos cos))
 ;         (rename (datan atan))
 ;         (rename (d= =))
 ;         (rename (d< <))
 ;         (rename (d> >))
 ;         (rename (d<= <=))
 ;         (rename (d>= >=))
 ;         (rename (dzero? zero?))
 ;         (rename (dpositive? positive?))
 ;         (rename (dnegative? negative?))
 ;         (rename (dreal? real?))
 ;         (rename (dlngamma lngamma))
 tapify
 write-real
 forward-mode
 derivative-F
 directional-derivative-list-F
 directional-derivative-vector-F
 gradient-list-F
 gradient-vector-F
 reverse-mode
 derivative-R
 gradient-list-R
 gradient-vector-R
 make-tapifier
 Tape?
 untapify
 xy-gradient-R)


(require
 (only-in math/special-functions log-gamma))

;; (require
;;  (only-in (scheme-tools math gsl-bindings) gsl-lngamma gsl-polygamma))

(define *e* 0)

(define <_e <)

(struct DualNumber (epsilon primal perturbation))

(struct Tape (epsilon
              primal
              factors
              tapes 
              [fanout #:mutable]
              [sensitivity #:mutable]))

(define (new-tape epsilon primal factors tapes)
  (Tape epsilon primal factors tapes 0 0))

(define (tapify x) (new-tape *e* x '() '()))

;;FIXME: circular lists and vectors (e.g. in environments) lead to infinite recursion here.
;;       same bug probably exists in mega-comparator, has been addresses in ais relevant values code.
;;       might want to make a general util for recursively walking a church value, avoiding infinite loop....
(define (untapify x)
  (cond ((Tape? x) (untapify (Tape-primal x)))
        ((and (list? x) (eq? (car x) 'procedure)) x) ;;FIXME: handle circular refernces from recursive proc environments...
        ((list? x) (map untapify x))
        ((pair? x) (cons (untapify (car x)) (untapify (cdr x))))
        ((vector? x) (vector-map untapify x))
        (else x)))
;;  (define (untapify x)
;;     (cond ((Tape? x) (display "tape..")(untapify (Tape-primal x)))
;;           ((list? x) (display "list..")(map untapify x))
;;           ((pair? x) (display "pair..")(cons (untapify (car x)) (untapify (cdr x))))
;;           ((vector? x) (display "vector..")(vector-map untapify x))
;;           (else (display "other.\n") x)))

;;this returns a tapify at a fixed, new epsilon.
;;using this alone never decrements the epsilon, but that should be fine as long as it isn't nested in itself too deeply.
(define (make-tapifier)
  (set! *e* (+ *e* 1))
  (let ((e-this *e*))
    (lambda (x) (new-tape e-this x '() '()))))

(define (lift-real->real f df/dx)
  (letrec ((self (lambda (x)
                   (cond ((DualNumber? x)
                          (DualNumber (DualNumber-epsilon x)
                                      (self (DualNumber-primal x))
                                      (d* (df/dx (DualNumber-primal x))
                                          (DualNumber-perturbation x))))
                         ((Tape? x)
                          (new-tape (Tape-epsilon x)
                                    (self (Tape-primal x))
                                    (list (df/dx (Tape-primal x)))
                                    (list x)))
                         (else (f x))))))
    self))

(define (lift-real*real->real f df/dx1 df/dx2)
  (letrec ((self
            (lambda (x1 x2)
              (cond
                ((DualNumber? x1)
                 (cond
                   ((DualNumber? x2)
                    (cond
                      ((<_e (DualNumber-epsilon x1)
                            (DualNumber-epsilon x2))
                       (DualNumber (DualNumber-epsilon x2)
                                   (self x1 (DualNumber-primal x2))
                                   (d* (df/dx2 x1 (DualNumber-primal x2))
                                       (DualNumber-perturbation x2))))
                      ((<_e (DualNumber-epsilon x2)
                            (DualNumber-epsilon x1))
                       (DualNumber (DualNumber-epsilon x1)
                                   (self (DualNumber-primal x1) x2)
                                   (d* (df/dx1 (DualNumber-primal x1) x2)
                                       (DualNumber-perturbation x1))))
                      (else
                       (DualNumber
                        (DualNumber-epsilon x1)
                        (self (DualNumber-primal x1)
                              (DualNumber-primal x2))
                        (d+ (d* (df/dx1 (DualNumber-primal x1)
                                        (DualNumber-primal x2))
                                (DualNumber-perturbation x1))
                            (d* (df/dx2 (DualNumber-primal x1)
                                        (DualNumber-primal x2))
                                (DualNumber-perturbation x2)))))))
                   ((Tape? x2)
                    (if (<_e (DualNumber-epsilon x1) (Tape-epsilon x2))
                        (new-tape (Tape-epsilon x2)
                                  (self x1 (Tape-primal x2))
                                  (list (df/dx2 x1 (Tape-primal x2)))
                                  (list x2))
                        (DualNumber (DualNumber-epsilon x1)
                                    (self (DualNumber-primal x1) x2)
                                    (d* (df/dx1 (DualNumber-primal x1) x2)
                                        (DualNumber-perturbation x1)))))
                   (else (DualNumber (DualNumber-epsilon x1)
                                     (self (DualNumber-primal x1) x2)
                                     (d* (df/dx1 (DualNumber-primal x1) x2)
                                         (DualNumber-perturbation x1))))))
                ((Tape? x1)
                 (cond
                   ((DualNumber? x2)
                    (if (<_e (Tape-epsilon x1) (DualNumber-epsilon x2))
                        (DualNumber (DualNumber-epsilon x2)
                                    (self x1 (DualNumber-primal x2))
                                    (d* (df/dx2 x1 (DualNumber-primal x2))
                                        (DualNumber-perturbation x2)))
                        (new-tape (Tape-epsilon x1)
                                  (self (Tape-primal x1) x2)
                                  (list (df/dx1 (Tape-primal x1) x2))
                                  (list x1))))
                   ((Tape? x2)
                    (cond
                      ((<_e (Tape-epsilon x1) (Tape-epsilon x2))
                       (new-tape (Tape-epsilon x2)
                                 (self x1 (Tape-primal x2))
                                 (list (df/dx2 x1 (Tape-primal x2)))
                                 (list x2)))
                      ((<_e (Tape-epsilon x2) (Tape-epsilon x1))
                       (new-tape (Tape-epsilon x1)
                                 (self (Tape-primal x1) x2)
                                 (list (df/dx1 (Tape-primal x1) x2))
                                 (list x1)))
                      (else
                       (new-tape (Tape-epsilon x1)
                                 (self (Tape-primal x1) (Tape-primal x2))
                                 (list (df/dx1 (Tape-primal x1) (Tape-primal x2))
                                       (df/dx2 (Tape-primal x1) (Tape-primal x2)))
                                 (list x1 x2)))))
                   (else (new-tape (Tape-epsilon x1)
                                   (self (Tape-primal x1) x2)
                                   (list (df/dx1 (Tape-primal x1) x2))
                                   (list x1)))))
                (else
                 (cond ((DualNumber? x2)
                        (DualNumber (DualNumber-epsilon x2)
                                    (self x1 (DualNumber-primal x2))
                                    (d* (df/dx2 x1 (DualNumber-primal x2))
                                        (DualNumber-perturbation x2))))
                       ((Tape? x2)
                        (new-tape (Tape-epsilon x2)
                                  (self x1 (Tape-primal x2))
                                  (list (df/dx2 x1 (Tape-primal x2)))
                                  (list x2)))
                       (else (f x1 x2))))))))
    self))

(define (fold f l)
  (let loop ((l (cdr l)) (c (car l)))
    (if (null? l) c (loop (cdr l) (f c (car l))))))

(define (lift-real^n->real f df/dx1 df/dx2)
  (lambda xs
    (if (null? xs) (f) (fold (lift-real*real->real f df/dx1 df/dx2) xs))))

(define (lift-real^n+1->real f df/dx df/dx1 df/dx2)
  (lambda xs
    (cond ((null? xs) (f))
          ((null? (cdr xs)) ((lift-real->real f df/dx) (car xs)))
          (else (fold (lift-real*real->real f df/dx1 df/dx2) xs)))))

(define (primal* x)
  (cond ((DualNumber? x) (primal* (DualNumber-primal x)))
        ((Tape? x) (primal* (Tape-primal x)))
        (else x)))

(define (lift-real^n->boolean f) (lambda xs (apply f (map primal* xs))))

(define d+ (lift-real^n->real + (lambda (x1 x2) 1) (lambda (x1 x2) 1)))

(define d- (lift-real^n+1->real
            - (lambda (x) -1) (lambda (x1 x2) 1) (lambda (x1 x2) -1)))

(define d* (lift-real^n->real * (lambda (x1 x2) x2) (lambda (x1 x2) x1)))

(define d/ (lift-real^n+1->real
            /
            (lambda (x) (d- (d/ (d* x x))))
            (lambda (x1 x2) (d/ x2))
            (lambda (x1 x2) (d- (d/ x1 (d* x2 x2))))))

(define dsqrt (lift-real->real sqrt (lambda (x) (d/ (d* 2 (dsqrt x))))))

(define dexp (lift-real->real exp (lambda (x) (dexp x))))

(define dlog (lift-real->real log (lambda (x) (d/ x))))

(define dadfloor (lift-real->real floor (lambda (x) 0)))

(define dexpt
  (lift-real*real->real expt
                        (lambda (x1 x2) (d* x2 (dexpt x1 (d- x2 1))))
                        (lambda (x1 x2) (d* (dlog x1) (dexpt x1 x2)))))

(define dsin (lift-real->real sin (lambda (x) (dcos x))))

(define dcos (lift-real->real cos (lambda (x) (d- (dsin x)))))

(define (datan . xs)
  (cond ((null? xs) (apply atan xs))
        ((null? (cdr xs)) (datan (car xs) 1))
        ((null? (cdr (cdr xs)))
         ((lift-real*real->real
           atan
           (lambda (x1 x2) (d/ x2 (d+ (d* x1 x1) (d* x2 x2))))
           (lambda (x1 x2) (d/ (d- x1) (d+ (d* x1 x1) (d* x2 x2)))))
          (car xs)
          (cadr xs)))
        (else (apply atan xs))))

(define d= (lift-real^n->boolean =))

(define d< (lift-real^n->boolean <))

(define d> (lift-real^n->boolean >))

(define d<= (lift-real^n->boolean <=))

(define d>= (lift-real^n->boolean >=))

(define dzero? (lift-real^n->boolean zero?))

(define dpositive? (lift-real^n->boolean positive?))

(define dnegative? (lift-real^n->boolean negative?))

(define dreal? (lift-real^n->boolean real?))

;(define polygamma0 (lambda (x) (gsl-polygamma 0 x)))
;(define polygamma1 (lambda (x) (gsl-polygamma 1 x)))
;(define polygamma2 (lambda (x) (gsl-polygamma 2 x)))
;(define polygamma3 (lambda (x) (gsl-polygamma 3 x)))
;(define polygamma4 (lambda (x) (gsl-polygamma 4 x)))
;(define polygamma5 (lambda (x) (gsl-polygamma 5 x)))
;
;(define dlngamma (lift-real->real log-gamma (lambda (x) (dpolygamma0 x))))
;(define dpolygamma0 (lift-real->real polygamma0 (lambda (x) (dpolygamma1 x))))
;(define dpolygamma1 (lift-real->real polygamma1 (lambda (x) (dpolygamma2 x))))
;(define dpolygamma2 (lift-real->real polygamma2 (lambda (x) (dpolygamma3 x))))
;(define dpolygamma3 (lift-real->real polygamma3 (lambda (x) (dpolygamma4 x))))
;(define dpolygamma4 (lift-real->real polygamma4 (lambda (x) (dpolygamma5 x))))
;(define dpolygamma5 (lift-real->real polygamma5 (lambda (x) (error "higher (>5) order derivatives not bound for lngamma"))))

(define (write-real x)
  (cond ((DualNumber? x) (write-real (DualNumber-primal x)) x)
        ((Tape? x) (write-real (Tape-primal x)) x)
        (else (write x) (newline) x)))

(define (forward-mode map-independent map-dependent f x x-perturbation)
  ;; needs work: We don't support what the AD community calls tangent vector
  ;;             mode.
  (set! *e* (+ *e* 1))
  (let ((y-forward
         (f (map-independent (lambda (x x-perturbation)
                               (DualNumber *e* x x-perturbation))
                             x
                             x-perturbation))))
    (set! *e* (- *e* 1))
    (list (map-dependent (lambda (y-forward)
                           (if (or (not (DualNumber? y-forward))
                                   (<_e (DualNumber-epsilon y-forward) *e*))
                               y-forward
                               (DualNumber-primal y-forward)))
                         y-forward)
          (map-dependent (lambda (y-forward)
                           (if (or (not (DualNumber? y-forward))
                                   (<_e (DualNumber-epsilon y-forward) *e*))
                               0
                               (DualNumber-perturbation y-forward)))
                         y-forward))))

(define (derivative-F f)
  (lambda (x)
    (cadr (forward-mode (lambda (f x x-perturbation) (f x x-perturbation))
                        (lambda (f y-forward) (f y-forward))
                        f
                        x
                        1))))

(define (directional-derivative-list-F f)
  (lambda (x x-perturbation)
    (cadr (forward-mode (lambda (f x x-perturbation) (map f x x-perturbation))
                        (lambda (f y-forward) (map f y-forward))
                        f
                        x
                        x-perturbation))))

(define (map-vector f v . vs)
  (let ((u (make-vector (vector-length v))))
    (for-each-n
     (lambda (i)
       (vector-set!
        u i (apply f (vector-ref v i) (map (lambda (v) (vector-ref v i)) vs))))
     (vector-length v))
    u))

(define (directional-derivative-vector-F f)
  (lambda (x x-perturbation)
    (cadr
     (forward-mode (lambda (f x x-perturbation) (map-vector f x x-perturbation))
                   (lambda (f y-forward) (map-vector f y-forward))
                   f
                   x
                   x-perturbation))))

(define (replace-ith x i xi)
  (if (zero? i)
      (cons xi (cdr x))
      (cons (car x) (replace-ith (cdr x) (- i 1) xi))))

(define (map-n f n)
  (let loop ((result '()) (i 0))
    (if (= i n) (reverse result) (loop (cons (f i) result) (+ i 1)))))

(define (gradient-list-F f)
  (lambda (x)
    (map-n
     (lambda (i)
       ((derivative-F (lambda (xi) (f (replace-ith x i xi)))) (list-ref x i)))
     (length x))))

(define (map-n-vector f n)
  (let ((v (make-vector n)))
    (let loop ((i 0))
      (when (< i n)
        (vector-set! v i (f i))
        (loop (+ i 1))))
    v))

(define (replace-ith-vector x i xi)
  (map-n-vector
   (lambda (j) (if (= j i) xi (vector-ref x j))) (vector-length x)))

(define (gradient-vector-F f)
  (lambda (x)
    (map-n-vector (lambda (i)
                    ((derivative-F (lambda (xi) (f (replace-ith-vector x i xi))))
                     (vector-ref x i)))
                  (vector-length x))))

(define (determine-fanout! tape)
  (set-Tape-fanout! tape (+ (Tape-fanout tape) 1))
  (when (= (Tape-fanout tape) 1)
    (for-each determine-fanout! (Tape-tapes tape))))

(define (initialize-sensitivity! tape)
  (set-Tape-sensitivity! tape 0)
  (set-Tape-fanout! tape (- (Tape-fanout tape) 1))
  (when (zero? (Tape-fanout tape))
    (for-each initialize-sensitivity! (Tape-tapes tape))))

(define (reverse-phase! sensitivity tape)
  (set-Tape-sensitivity! tape (d+ (Tape-sensitivity tape) sensitivity))
  (set-Tape-fanout! tape (- (Tape-fanout tape) 1))
  (when (zero? (Tape-fanout tape))
    (let ((sensitivity (Tape-sensitivity tape)))
      (for-each
       (lambda (factor tape) (reverse-phase! (d* sensitivity factor) tape))
       (Tape-factors tape)
       (Tape-tapes tape)))))

(define (reverse-mode map-independent
                      map-dependent
                      for-each-dependent1!
                      for-each-dependent2!
                      f
                      x
                      y-sensitivities)
  ;; needs work: We don't support providing the y-sensitivies (potentially
  ;;             incrementally) after computing the primal in the forward
  ;;             phase.
  (set! *e* (+ *e* 1))
  (let* ((x-reverse (map-independent tapify x))
         (y-reverse (f x-reverse))
         (x-sensitivities
          (map (lambda (y-sensitivity)
                 (for-each-dependent1!
                  (lambda (y-reverse)
                    (when (and (Tape? y-reverse)
                               (not (<_e (Tape-epsilon y-reverse) *e*)))
                      (determine-fanout! y-reverse)
                      (initialize-sensitivity! y-reverse)))
                  y-reverse)
                 (for-each-dependent2!
                  (lambda (y-reverse y-sensitivity)
                    (when (and (Tape? y-reverse)
                               (not (<_e (Tape-epsilon y-reverse) *e*)))
                      (determine-fanout! y-reverse)
                      (reverse-phase! y-sensitivity y-reverse)))
                  y-reverse
                  y-sensitivity)
                 (map-independent Tape-sensitivity x-reverse))
               y-sensitivities)))
    (set! *e* (- *e* 1))
    (list (map-dependent
           (lambda (y-reverse)
             (if (or (not (Tape? y-reverse)) (<_e (Tape-epsilon y-reverse) *e*))
                 y-reverse
                 (Tape-primal y-reverse)))
           y-reverse)
          x-sensitivities)))

(define (derivative-R f)
  (lambda (x)
    (car (cadr (reverse-mode
                (lambda (f x) (f x))
                (lambda (f y-reverse) (f y-reverse))
                (lambda (f y-reverse) (f y-reverse))
                (lambda (f y-reverse y-sensitivity) (f y-reverse y-sensitivity))
                f
                x
                '(1))))))

(define (gradient-list-R f)
  (lambda (x)
    (car (cadr (reverse-mode
                (lambda (f x) (map f x))
                (lambda (f y-reverse) (f y-reverse))
                (lambda (f y-reverse) (f y-reverse))
                (lambda (f y-reverse y-sensitivity) (f y-reverse y-sensitivity))
                f
                x
                '(1))))))

(define (for-each-n f n)
  (let loop ((i 0)) (when (< i n) (f i) (loop (+ i 1)))))

(define (gradient-vector-R f)
  (lambda (x)
    (car (cadr (reverse-mode
                (lambda (f x) (map-vector f x))
                (lambda (f y-reverse) (f y-reverse))
                (lambda (f y-reverse) (f y-reverse))
                (lambda (f y-reverse y-sensitivity) (f y-reverse y-sensitivity))
                f
                x
                '(1))))))


;;this is to compute gradient when the function application and tapifying are being done externally.
;;assumes that each independent variables has been tapified with the tapify argument.
(define (xy-reverse-mode map-independent
                         map-dependent
                         for-each-dependent1!
                         for-each-dependent2!
                         x-reverse
                         y-reverse
                         tapify
                         y-sensitivities)
  (let* ((this-e (Tape-epsilon (tapify 0.0)))
         (x-sensitivities
          (map (lambda (y-sensitivity)
                 (for-each-dependent1!
                  (lambda (y-reverse)
                    (when (and (Tape? y-reverse)
                               (not (<_e (Tape-epsilon y-reverse) this-e)))
                      (determine-fanout! y-reverse)
                      (initialize-sensitivity! y-reverse)))
                  y-reverse)
                 (for-each-dependent2!
                  (lambda (y-reverse y-sensitivity)
                    (when (and (Tape? y-reverse)
                               (not (<_e (Tape-epsilon y-reverse) this-e)))
                      (determine-fanout! y-reverse)
                      (reverse-phase! y-sensitivity y-reverse)))
                  y-reverse
                  y-sensitivity)
                 (map-independent Tape-sensitivity x-reverse))
               y-sensitivities)))
    (list (map-dependent
           (lambda (y-reverse)
             (if (or (not (Tape? y-reverse)) (<_e (Tape-epsilon y-reverse) this-e))
                 y-reverse
                 (Tape-primal y-reverse)))
           y-reverse)
          x-sensitivities)))

;;take gradient for pre-computed x and y. assume y is a single (tapified) real value.
(define (xy-gradient-R map-independent
                       x-reverse
                       y-reverse
                       tapify)
  (xy-reverse-mode map-independent
                   (lambda (f y-reverse) (f y-reverse))
                   (lambda (f y-reverse) (f y-reverse))
                   (lambda (f y-reverse y-sensitivity) (f y-reverse y-sensitivity))
                   x-reverse
                   y-reverse
                   tapify
                   '(1)))
