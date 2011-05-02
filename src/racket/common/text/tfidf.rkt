#lang racket/base

(require 
 racket/set)


(define non-words-regex #rx"(?i:[^a-z0-9' ])")

;; http://www.textfixer.com/resources/common-english-words.txt
(define stop-words 
  (set "a" "able" "about" "across" "after" "all" "almost" "also" "am" "among"
       "an" "and" "any" "are" "as" "at" "be" "because" "been" "but" "by" "can" "cannot" 
       "could" "dear" "did" "do" "does" "either" "else" "ever" "every" "for" "from" "get" 
       "got" "had" "has" "have" "he" "her""hers" "him" "his" "how" "however" "i" 
       "if" "in" "into" "is" "it" "its" "just" "least" "let" "like" "likely" "may" 
       "me" "might" "most" "must" "my" "neither" "no" "nor" "not" "of" "off" "often" "on"
       "only" "or" "other" "our" "own" "rather" "said" "say" "says" "she" "should" "since" 
       "so" "some" "than" "that" "the" "their" "them" "then" "there" "these" "they" "this" 
       "tis" "to" "too" "twas" "us" "wants" "was" "we" "were" "what" "when" "where" "which" 
       "while" "who" "whom" "why" "will" "with" "would" "yet" "you" "your"))

;; Select a word if its not a stop word and has length > 1
(define (word-select word)
  (and (not (set-member? stop-words word))
     (> (string-length word) 1)))

;; lower case the text and then replace all non simple word chars with spaces.
;; then split it and trim. leading / trailing whitespace will be emtpy strings ""
;; return listof string?
(define (cleanse text)
  (filter word-select
	  (regexp-split #rx" +" (regexp-replace* non-words-regex text " "))))
    
;; Parse the given text string into simple words and count each token into a hash
;; returns hashOf word count
(define (tf text)
  (let ((word-freq (make-hash)))
    (for-each (lambda (word)
		(hash-update! word-freq word add1 0))
	      (cleanse text))
    (let ((word-count (hash-count word-freq))) ;; FIXME use root mean square of the count
      (hash-for-each word-freq
		     (lambda (k v)
		       (hash-update! word-freq k (lambda (v) (/ v word-count)))))
      word-freq)))


;; given a list of document tfs determine each terms idf 
;; returns a HashOf Word -> IDF
(define (idf tfs)
  (let ((idf (make-hash)) 
      (doc-cnt 0))
    (for-each (lambda (tf)
		(set! doc-cnt (add1 doc-cnt))
	        (hash-for-each tf
			       (lambda (k v)
				 (hash-update! idf k add1 0))))
	      tfs)
    (hash-for-each idf
		   (lambda (k v)
		     (hash-update! idf k (lambda (cnt) (log (/ doc-cnt cnt))))))
    idf))
    
;; listof hashof word -> cnt
;; idf hashof word -> value
(define (tf-idf tfs idf)
  (let ((tfidfs (make-hash)))
    (map (lambda (tf)
	   (let ((tfidf (make-hash)))
	     (hash-for-each tf
			    (lambda (word freq)
			      (hash-set! tfidf word (* freq (hash-ref idf word)))))
	     tfidf))
	 tfs)))


;; (define tfs (map tf '("The quick brown fox jumped over the lazy dog."
;; 		 "The brown dog bit the lazy cat."
;; 		 "The Racines, Ray Eve and Cory")))

;; tfs 
;; (displayln "------------------")
;; (idf tfs)
;; (displayln "------------------")
;; (tf-idf tfs (idf tfs))
	  
					   
			       
      
  
