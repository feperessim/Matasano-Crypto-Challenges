(define-module (c9)
  #:export (pkcs7-padding))

(use-modules (rnrs bytevectors))

(define (pkcs7-padding bv blocksize)
  (let* ((bv-length (bytevector-length bv))
	 (pad (- blocksize
		 (remainder bv-length blocksize)))
	 (bv-padded
	  (make-bytevector (+ bv-length pad) pad)))
    (bytevector-copy! bv 0 bv-padded 0 bv-length)
    bv-padded))


;; (define key (u8-list->bytevector
;; 	     (map char->integer
;; 		  (string->list "YELLOW SUBMARINE"))))
				    		    
;; (define blocksize 20)

;; (display (pkcs7-padding key blocksize))
  
