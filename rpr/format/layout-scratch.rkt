#lang typed/racket

(provide 
 orderheader
 orderline)

(require 
 ;;(for-syntax "layout-types.rkt")
 (only-in "layout-types.rkt"
          Field 
          Layout) 
 (only-in "tabbed/reader.rkt"
	  string-field)
 (only-in "tabbed/layout.rkt"
	  define-tabbed-layout)
 (only-in "tabbed/parser.rkt"
	  define-tabbed-parser-for-layout)
 (only-in "fixed/layout.rkt"                   
          define-fixed-layout)
 (only-in "fixed/parser.rkt"
          define-static-parser-for-layout))

(define-tabbed-layout Sales-layout
  (date C)
  (dow C)
  (call F)
  (web F))

(define-tabbed-parser-for-layout (Sales-parser Sales-layout)
  (date web))

(define-fixed-layout orderline
  (order C 11)
  (order-sub C 5)
  (line C 7)
  (account C 10)
  (address-seq C 7)
  (create-date D 10)
  (order-type S 1)
  (location C 4)
  (dept C 2)
  (class C 3)
  (sku C 20)
  (vendor C 2)
  (entered-sku C 20)
  (source C 2)
  (list-price N 9)
  (sku-price N 10)
  (cost N 10)
  (uom C 2)
  (pack-qty I 7)
  (order-qty I 7)
  (ship-qty I 7)
  (misc-charge S 1)
  (sub-flag S 1)
  (contract-pp C 9)
  (contract-pp-seq C 5)
  (commission N 9)
  (contract-addendum C 1))

(define-fixed-layout orderheader-layout
  (order C 10)
  (order-sub C 5))

(define-static-parser-for-layout (oh-parser orderheader-layout)
  (order order-sub))
