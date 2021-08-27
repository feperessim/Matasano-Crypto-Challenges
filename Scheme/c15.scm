(define-module (c15)
  #:export (pkcs7-unpadding))


(use-modules (rnrs bytevectors))


(define (pkcs7-unpadding bv blocksize)
  (let* ((bv-length (bytevector-length bv))
	 (pad (bytevector-u8-ref bv (- bv-length 1)))
	 (new-length (- bv-length pad))
	 (bv1 (make-bytevector pad pad))
	 (bv2 (make-bytevector pad)))
    (cond ((or (< new-length 0)
	       (not (= (remainder bv-length blocksize) 0)))
	   (error "Bad padding"))
	   (else
	    (bytevector-copy! bv new-length bv2 0 pad)
	    (if (bytevector=? bv1 bv2)
		(let ((new-bv (make-bytevector new-length)))
		  (bytevector-copy! bv 0 new-bv 0 new-length)
		  new-bv)
		(error "Bad padding"))))))

;; tests
(define blocksize 16)
(define text-bv (u8-list->bytevector
		       (map char->integer
			    (string->list "ICE ICE BABY\x04\x04\x04\x04"))))
(newline)
(display (pkcs7-unpadding text-bv blocksize))

(define text-bv (u8-list->bytevector
		       (map char->integer
			    (string->list "ICE ICE BABY\x05\x05\x05\x05"))))
(newline)
(display (pkcs7-unpadding text-bv blocksize))

(define text-bv (u8-list->bytevector
		       (map char->integer
			    (string->list "ICE ICE BABY\x05\x05\x05\x4D"))))
(newline)
(display (pkcs7-unpadding text-bv blocksize))

(define text-bv (u8-list->bytevector
		       (map char->integer
			    (string->list "ICE ICE BABY"))))
(newline)
(display (pkcs7-unpadding text-bv blocksize))
