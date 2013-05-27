#lang typed/racket/base

(provide
 FileName Extension CompoundExtension FilePath RelFilePath AbsFilePath)

(provide:
 ;; Types construction
 [make-FileName     (FilePath -> FileName)]
 [make-FilePath     (String -> FilePath)]
 [make-Extension    (String -> Extension)]
 [make-RelFilePath  (String -> RelFilePath)]
 [make-AbsFilePath  (String -> AbsFilePath)]
 [FileName->FilePath (FileName -> FilePath)]
 [FilePath->string  (FilePath -> String)]
 [as-directory-path (FilePath -> FilePath)]
 [split-search-path (String -> (Listof FilePath))]
 ;; Extensions
 [split-extension   (FilePath -> (Pair FilePath Extension))]
 [path-extension    (FilePath -> Extension)]
 [drop-extension    (FilePath -> FilePath)]
 [add-extension     (FilePath Extension -> FilePath)]
 [has-extension?    (FilePath -> Boolean)]
 [split-extensions  (FilePath -> (Pair FilePath CompoundExtension))]
 [drop-extensions   (FilePath -> FilePath)]
 [path-extensions   (FilePath -> CompoundExtension)]
 ;; Path and Files
 [split-filename    (FilePath -> (Pair FilePath FileName))]
 [path-filename     (FilePath -> FileName)]
 [drop-filename     (FilePath -> FilePath)]
 [base-name         (FilePath -> FileName)]
 [file-base-name    (FileName -> FileName)]
 [replace-base-name (FilePath FileName -> FilePath)]
 [up-from-directory (FilePath -> FilePath)]
 [replace-directory-path (FilePath FilePath -> FilePath)]
 [append-extension  (FileName Extension -> FileName)]
 [build             (FilePath RelFilePath -> FilePath)]
 [build*            (FilePath (Listof RelFilePath) -> FilePath)]
 [join              ((Pair FilePath (Listof RelFilePath)) -> FilePath)]
 [split             (FilePath -> (Pair FilePath (Listof RelFilePath)))]
 [segments (FilePath -> (Listof String))]
 ;; Trailing path separator
 [trailing-path-separator?       (FilePath -> Boolean)]
 [append-trailing-path-separator (FilePath -> FilePath)]
 [drop-trailing-separator        (FilePath -> FilePath)]
 ;; Normalize paths and comparison
 [normalize (FilePath -> FilePath)]
 [identically-structured? (FilePath FilePath -> Boolean)]
 [normalize (FilePath -> FilePath)]
 [make-relative (FilePath FilePath -> RelFilePath)]
 [relative? (FilePath RelFilePath -> Boolean)])

(require
 racket/match
 (only-in "../data/string.rkt"
          starts-with?
          starts-with-char?
          ends-with-char?
          string-first-char-occurence
          string-relative-to-prefix))

(define-type PathSegment String)
(struct: FileName ([fn : String]) #:transparent)
(struct: Extension ([x : String]) #:transparent)
(struct: CompoundExtension Extension () #:transparent)
(struct: FilePath ([p : String])  #:transparent)
(struct: RelFilePath FilePath ()  #:transparent)
(struct: AbsFilePath FilePath ()  #:transparent)

;; Separators and Predicates

(: relative-dir-ch Char)
(define relative-dir-ch #\.)

(: path-separator String)
(define path-separator "/")

(: path-sep-ch Char)
(define path-sep-ch (string-ref path-separator 0))

(: path-separator? ((U Char String) -> Boolean))
(define (path-separator? s)
  (cond    
    ((string? s)
     (string=? s path-separator))
    ((char? s)
     (char=? s path-sep-ch))))

(: search-path-separator String)
(define search-path-separator ":")

(: search-path-ch Char)
(define search-path-ch (string-ref search-path-separator 0))

(: search-path-separator? ((U Char String) -> Boolean))
(define (search-path-separator? s)
  (cond
    ((string? s)
     (string=? s search-path-separator))
    (else (char=? s search-path-ch))))

(: extension-separator String)
(define extension-separator ".")

(: ext-sep-ch Char)
(define ext-sep-ch (string-ref extension-separator 0))

(: extension-separator? ((U String Char) -> Boolean))
(define (extension-separator? s)
  (cond
    ((string? s)
     (string=? s extension-separator))
    (else (char=? s ext-sep-ch))))

;; Path Methods

(: make-FilePath (String -> FilePath))
(define (make-FilePath s)
  (if (zero? (string-length s))
      (make-RelFilePath s)      
      (if (starts-with-char? s path-sep-ch)
          (AbsFilePath s)
          (if (starts-with-char? s relative-dir-ch)
              (RelFilePath s)
              (RelFilePath (string-append "./" s))))))

(: FilePath->string (FilePath -> String))
(define (FilePath->string fp)
  (FilePath-p fp))

(: as-directory-path (FilePath -> FilePath))
(define (as-directory-path fp)
  (let ((sfp (FilePath-p fp)))
    (if (ends-with-char? sfp path-sep-ch)
        fp
        (make-FilePath (string-append sfp path-separator)))))

(: make-Extension (String -> Extension))
(define (make-Extension s)
  (if (string=? s "")
      (Extension "")
      (if (extension-separator? (string-ref s 0))
          (Extension (substring s 1 (string-length s)))
          (Extension s))))

(: starts-with-path-separator? (String -> Boolean))
(define (starts-with-path-separator? s)
  (starts-with-char? s path-sep-ch))

(: make-RelFilePath (String -> RelFilePath))
(define (make-RelFilePath s)
  (if (string=? s "")
      (RelFilePath ".")
      (if (starts-with-path-separator? s)
          (make-RelFilePath (substring s 1 (string-length s)))
          (if (starts-with-char? s relative-dir-ch)              
              (RelFilePath s)              
              (RelFilePath (string-append (string relative-dir-ch path-sep-ch) s))))))

(: make-AbsFilePath (String -> AbsFilePath))
(define (make-AbsFilePath s)
  (if (zero? (string-length s))
      (AbsFilePath "/")
      (if (starts-with-char? s path-sep-ch)
          (AbsFilePath s)
          (AbsFilePath (string-append path-separator s)))))

(: FileName->FilePath (FileName -> FilePath))
(define (FileName->FilePath fn)
  (make-RelFilePath (FileName-fn fn)))

(: split-search-path (String -> (Listof FilePath)))
(define (split-search-path s)
  (map FilePath (regexp-split (regexp search-path-separator) s)))

;(: get-search-path (-> (Listof FilePath)))
;(define (get-search-path)
;  (environment-variables-ref (current-environment-variables) "PATH"))

;; Extension Methods
(: split-extension (FilePath -> (Pair FilePath Extension)))
(define (split-extension fp)
  (define: sfp : String (FilePath-p fp))  
  (define: len : Natural (string-length sfp))
  (let loop ((i (sub1 (string-length sfp))))
    (if (zero? i)
        (cons fp (Extension ""))
        (let ((ch (string-ref sfp i)))
          (if (extension-separator? ch)
              (let ((s (add1 i)))               
                (if (< s len)
                    (let ((ext (substring sfp s len)))
                      (if (not (string-first-char-occurence ext path-sep-ch))
                          (cons (make-FilePath (substring sfp 0 i))
                                (Extension ext))
                          (cons fp (Extension ""))))                      
                    (cons fp (Extension ""))))
              (if (search-path-separator? ch)
                  (cons (make-FilePath sfp) (Extension ""))
                  (loop (sub1 i))))))))

(: path-extension (FilePath -> Extension))
(define (path-extension fp)
  (cdr (split-extension fp)))

(: drop-extension (FilePath -> FilePath))
(define (drop-extension fp)
  (car (split-extension fp)))

(: prefix-extension-separator (String -> String))
(define (prefix-extension-separator ext-str)
  (string-append extension-separator ext-str))

(: add-extension (FilePath Extension -> FilePath))
(define (add-extension fp ext)
  (FilePath (string-append (FilePath-p fp) (prefix-extension-separator (Extension-x ext)))))

(: has-extension? (FilePath -> Boolean))
(define (has-extension? fp)
  (not (string=? (Extension-x (path-extension fp)) "")))

(: split-extensions (FilePath -> (Pair FilePath CompoundExtension)))
(define (split-extensions fp)
  
  (: split-one-extension (FilePath String -> (Pair FilePath CompoundExtension)))
  (define (split-one-extension fp exts)
    (match (split-extension fp)
      ((cons fp1 ext1)
       (let ((ext1-str (Extension-x ext1)))
         (if (string=? "" ext1-str)
             (cons fp1 (CompoundExtension exts))
             (split-one-extension fp1 (string-append (prefix-extension-separator ext1-str) exts)))))))
  
  (split-one-extension fp ""))

(: drop-extensions (FilePath -> FilePath))
(define (drop-extensions fp)
  (car (split-extensions fp)))

(: path-extensions (FilePath -> CompoundExtension))
(define (path-extensions fp)
  (cdr (split-extensions fp)))

(: split-filename (FilePath -> (Pair FilePath FileName)))
(define (split-filename rfp)
  (define rfp-str (FilePath-p rfp))
  (define len (string-length (FilePath-p rfp)))
  (if (positive? len)
      (let loop ((i (sub1 len)))
        (if (zero? i)
            (cons (FilePath "./") (FileName rfp-str))
            (if (path-separator? (string-ref rfp-str i))
                (cons (make-FilePath (substring rfp-str 0 (min (add1 i) len)))
                      (FileName (substring rfp-str (min (add1 i) len) len)))
                (loop (sub1 i)))))
      (cons (FilePath "") (FileName ""))))

(: path-filename (FilePath -> FileName))
(define (path-filename fp)
  (cdr (split-filename fp)))

(: drop-filename (FilePath -> FilePath))
(define (drop-filename fp)
  (car (split-filename fp)))

(: base-name (FilePath -> FileName))
(define (base-name fp)
  (file-base-name (cdr (split-filename fp))))

(: file-base-name (FileName -> FileName))
(define (file-base-name fn)
  (make-FileName (drop-extension (make-FilePath (FileName-fn fn)))))

(: replace-base-name (FilePath FileName -> FilePath))
(define (replace-base-name fp fn)
  (match (split-filename fp)
    ((cons path file)     
     (let ((ext (path-extension (make-FilePath (FileName-fn file)))))       
       (build path (RelFilePath (FileName-fn (append-extension fn ext))))))))

(: up-from-directory (FilePath -> FilePath))
(define (up-from-directory fp)
  (let* ((path (FilePath-p (drop-filename fp)))
         (len (string-length path)))
    (if (zero? len)
        fp
        (let loop ((i (sub1 len)))
          (if (zero? i)
              (RelFilePath (string relative-dir-ch))
              (if (path-separator? (string-ref path i))
                  (make-FilePath (substring path 0 i))
                  (loop (sub1 i))))))))

(: replace-directory-path (FilePath FilePath -> FilePath))
(define (replace-directory-path fp1 fp2)
  (let ((fp1 (split-filename fp1)))        
    (build (as-directory-path fp2)
           (make-RelFilePath (FileName-fn (cdr fp1))))))              

(: clean-local-relative-path (RelFilePath -> String))
(define (clean-local-relative-path rfp)
  (let* ((srfp (FilePath-p rfp))
         (len (string-length srfp)))    
    (if (> len 2)
        (if (starts-with? srfp (string relative-dir-ch path-sep-ch))
            (substring srfp 2 len)
            srfp)
        srfp)))

(: make-FileName (FilePath -> FileName))
(define (make-FileName fp)
  (FileName (clean-local-relative-path (make-RelFilePath (FileName-fn (path-filename fp))))))

(: append-extension (FileName Extension -> FileName))
(define (append-extension fn ext)
  (make-FileName (make-FilePath (string-append (FileName-fn fn) extension-separator (Extension-x ext)))))

(: build (FilePath RelFilePath -> FilePath))
(define (build fp1 fp2)    
  (: ends-with-path-sep (String -> Boolean))
  (define (ends-with-path-sep s)
    (path-separator? (string-ref s (sub1 (string-length s)))))
  (define: sfp1 : String (FilePath-p fp1))
  (define: sfp2 : String (clean-local-relative-path fp2))
  (define: maybe-inter-sep : String (if (ends-with-path-sep sfp1) "" path-separator))
  (make-FilePath (string-append sfp1 maybe-inter-sep sfp2)))

(: build* (FilePath (Listof RelFilePath) -> FilePath))
(define (build* file-path rel-paths)
  (let loop ((fp file-path) (rel-paths rel-paths))
    (if (null? rel-paths)
        fp
        (loop (build fp (car rel-paths)) (cdr rel-paths)))))

(: join ((Pair FilePath (Listof RelFilePath)) -> FilePath))
(define (join fps)
  (build* (car fps) (cdr fps)))

(: split (FilePath -> (Pair FilePath (Listof RelFilePath))))
(define (split fp)
  (let ((sfp (FilePath-p fp)))
    (let: ((segs : (Listof String)
                 (filter (λ: ((seg : String))
                           (not (string=? seg "")))
                         (regexp-split (regexp path-separator) sfp))))
      (if (starts-with-char? sfp path-sep-ch)
          (cons (AbsFilePath "/") (map make-RelFilePath segs))
          (if (pair? segs)
              (map make-RelFilePath segs)
              (list (RelFilePath ".")))))))

(: segments (FilePath -> (Listof String)))
(define (segments fp) 
  (filter (λ: ((seg : String))
            (not (or (string=? (string relative-dir-ch) seg)
                     (string=? seg ""))))
          (regexp-split (regexp path-separator) (FilePath-p fp))))

(: trailing-path-separator? (FilePath -> Boolean))
(define (trailing-path-separator? fp)
  (ends-with-char? (FilePath-p fp) path-sep-ch))

(: append-trailing-path-separator (FilePath -> FilePath))
(define (append-trailing-path-separator fp)
  (if (trailing-path-separator? fp)
      fp
      (make-FilePath (string-append (FilePath-p fp) path-separator))))

(: drop-trailing-separator (FilePath -> FilePath))
(define (drop-trailing-separator fp)
  (if (trailing-path-separator? fp)
      (let ((sfp (FilePath-p fp)))            
        (make-FilePath (substring sfp 0 (sub1 (string-length sfp)))))
      fp))

(: identically-structured? (FilePath FilePath -> Boolean))
(define (identically-structured? fp1 fp2)
  (string=? (FilePath-p fp1) (FilePath-p fp2)))

(: normalize (FilePath -> FilePath))
(define (normalize fp)
  (let ((fps (split fp)))
    (if (pair? fps)
        (join (cons (car fps) 
                    (filter (λ: ((fp : RelFilePath))
                              (not (string=? (FilePath-p fp) (string relative-dir-ch))))
                            (cdr fps))))
        (join (split fp)))))

(: make-relative (FilePath FilePath -> RelFilePath))
(define (make-relative fp-base fp)
  (make-RelFilePath (string-relative-to-prefix (FilePath-p fp-base) (FilePath-p fp))))

(: relative? (FilePath RelFilePath -> Boolean))
(define (relative? fp-base fp)
 (error 'filepath-relative? "Not implemented"))


