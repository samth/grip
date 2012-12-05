#lang typed/racket/base

(provide 
 completed?
 todo-count inprogress-count complete-count
 track-tasks
 record-inprogress record-complete reschedule restart)
 
(require
 racket/set)

(struct: (A) Track ([todo : (Setof A)]
                    [inprogress : (Setof A)]
                    [complete : (Setof A)]))

(: completed? (All (A) (Track A) -> Boolean))
(define (completed? tt)
  (and (set-empty? (Track-todo tt))
       (set-empty? (Track-inprogress tt))))

(: todo-count (All (A) (Track A) -> Natural))
(define (todo-count tt)
  (set-count (Track-todo tt)))

(: inprogress-count (All (A) (Track A) -> Natural))
(define (inprogress-count tt)
  (set-count (Track-inprogress tt)))

(: complete-count (All (A) (Track A) -> Natural))
(define (complete-count tt)
  (set-count (Track-complete tt)))

(: track-tasks (All (A) (Setof A) -> (Track A)))
(define (track-tasks ts)
  (Track ts ((inst set A)) ((inst set A))))

(: record-inprogress (All (A) (Track A) A -> (Track A)))
(define (record-inprogress tt a)
  (let ((todos (Track-todo tt)))
    (if (set-member? todos a)
      (Track (set-remove todos a)
             (set-add (Track-inprogress tt) a)
             (Track-complete tt))
      tt)))

(: record-complete (All (A) (Track A) A -> (Track A)))
(define (record-complete tt a)
  (let ((inflight (Track-inprogress tt)))
    (if (set-member? inflight a)
        (Track (Track-todo tt)
               (set-remove inflight a)
               (set-add (Track-complete tt) a))
        tt)))

(: reschedule (All (A) (Track A) A -> (Track A)))
(define (reschedule tt a)
  (let ((inflight (Track-inprogress tt)))
    (if (set-member? inflight a)
        (Track (set-add (Track-complete tt) a)
               (set-remove inflight a)
               (Track-complete tt))
        tt)))

(: restart (All (A) (Track A) A -> (Track A)))
(define (restart tt a)
  (let ((inflight (Track-inprogress tt)))
    (if (set-member? inflight a)        
        (Track (Track-todo tt)
               (set-add (set-remove inflight a) a)
               (Track-complete tt))
        tt)))


