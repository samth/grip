#lang typed/racket/base

(provide 
 (struct-out Tracker)
 completed?
 todo-count inprogress-count complete-count
 make-tracker
 record-inprogress record-complete reschedule restart)

(require
 racket/set)

(struct: (A) Tracker ([todo : (Setof A)]
                      [inprogress : (Setof A)]
                      [complete : (Setof A)]))

(: completed? (All (A) (Tracker A) -> Boolean))
(define (completed? tt)
  (and (set-empty? (Tracker-todo tt))
       (set-empty? (Tracker-inprogress tt))))

(: todo-count (All (A) (Tracker A) -> Natural))
(define (todo-count tt)
  (set-count (Tracker-todo tt)))

(: inprogress-count (All (A) (Tracker A) -> Natural))
(define (inprogress-count tt)
  (set-count (Tracker-inprogress tt)))

(: complete-count (All (A) (Tracker A) -> Natural))
(define (complete-count tt)
  (set-count (Tracker-complete tt)))

(: make-tracker (All (A) (Setof A) -> (Tracker A)))
(define (make-tracker ts)
  (Tracker ts ((inst set A)) ((inst set A))))

(: record-inprogress (All (A) (Tracker A) A -> (Tracker A)))
(define (record-inprogress tt a)
  (let ((todos (Tracker-todo tt)))
    (if (set-member? todos a)
        (Tracker (set-remove todos a)
                 (set-add (Tracker-inprogress tt) a)
                 (Tracker-complete tt))
        tt)))

(: record-complete (All (A) (Tracker A) A -> (Tracker A)))
(define (record-complete tt a)
  (let ((inflight (Tracker-inprogress tt)))
    (if (set-member? inflight a)
        (Tracker (Tracker-todo tt)
                 (set-remove inflight a)
                 (set-add (Tracker-complete tt) a))
        tt)))

(: reschedule (All (A) (Tracker A) A -> (Tracker A)))
(define (reschedule tt a)
  (let ((inflight (Tracker-inprogress tt)))
    (if (set-member? inflight a)
        (Tracker (set-add (Tracker-complete tt) a)
                 (set-remove inflight a)
                 (Tracker-complete tt))
        tt)))

(: restart (All (A) (Tracker A) A -> (Tracker A)))
(define (restart tt a)
  (let ((inflight (Tracker-inprogress tt)))
    (if (set-member? inflight a)        
        (Tracker (Tracker-todo tt)
                 (set-add (set-remove inflight a) a)
                 (Tracker-complete tt))
        tt)))


