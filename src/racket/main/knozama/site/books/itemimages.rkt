#lang racket 

(provide
 extract-item-imageset)

(require
 (only-in knozama/aws/configuration
	  a2s-nss)
 (only-in (planet lizorkin/sxml:2:1/sxml)
	  sxpath sxml:text)
 (only-in knozama/site/books/utils
	  select-single-node-text))

(define sx-item-image-set
  (sxpath "/a2s:ImageSets/a2s:ImageSet" a2s-nss))

(define swatch-url
  (select-single-node-text "/a2s:SwatchImage/a2s:URL" a2s-nss))

(define small-url
  (select-single-node-text "/a2s:SmallImage/a2s:URL" a2s-nss))

(define thumbnail-url
  (select-single-node-text "/a2s:ThumbnailImage/a2s:URL" a2s-nss))

(define tiny-url
  (select-single-node-text "/a2s:TinyImage/a2s:URL" a2s-nss))

(define medium-url
  (select-single-node-text "/a2s:MediumImage/a2s:URL" a2s-nss))

(define large-url
  (select-single-node-text "/a2s:LargeImage/a2s:URL" a2s-nss))

(define extract-item-imageset
  (lambda (item-sxml)
    (let ((imageset-sxml (let ((ii (sx-item-image-set item-sxml)))
			 (if (pair? ii)
			    (car ii)
			    '()))))
      (let ((swatch-url    (cons 'swatch    (swatch-url imageset-sxml)))
	  (small-url     (cons 'small     (small-url imageset-sxml)))
	  (thumbnail-url (cons 'thumbnail (thumbnail-url imageset-sxml)))
	  (tiny-url      (cons 'tiny      (tiny-url imageset-sxml)))
	  (medium-url    (cons 'medium    (medium-url imageset-sxml)))
	  (large-url     (cons 'large     (large-url imageset-sxml))))
	(let ((ii (list swatch-url
		      small-url
		      thumbnail-url
		      tiny-url
		      medium-url
		      large-url)))
	  ii)))))
