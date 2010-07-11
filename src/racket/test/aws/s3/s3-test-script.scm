(module s3-test-script mzscheme
  
  (require
   (planet "ssax.ss" ("lizorkin" "ssax.plt" 1 3)) )

  (time 
   (let ((doc 
          (ssax:xml->sxml (open-input-file "/code/editio/bravais/trunk/src/scheme/w3c/xml/ssax/SyncPurchaseOrder.xml") (list (cons 'br "http://bravais.org"))))) #f))
)