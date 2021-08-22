(define-module (c12)
  #:export (find-blocksize))

(add-to-load-path "./")

(use-modules (rnrs bytevectors)
	     ((c6) #:select (decode-base64))
	     ((c7) #:select (AES-ecb-encrypt))
	     ((c9) #:select (pkcs7-padding))
	     ((c10) #:select (pkcs7-unpadding))
	     ((c11) #:select (gen-rand-aes-key)))


(define (encryption-oracle plain-text-bv key)
  (let* ((secret (decode-base64
		  (string-append
		   "Um9sbGluJyBpbiBteSA1LjAKV2l0aCBteSByYWctdG9wIGRvd24gc28gbXkg"
		   "aGFpciBjYW4gYmxvdwpUaGUgZ2lybGllcyBvbiBzdGFuZGJ5IHdhdmluZyBq"
		   "dXN0IHRvIHNheSBoaQpEaWQgeW91IHN0b3A/IE5vLCBJIGp1c3QgZHJvdmUg"
		   "YnkK")))
	 (plain-text-bv (u8-list->bytevector
			 (append (bytevector->u8-list plain-text-bv)
				 (bytevector->u8-list secret)))))
    (AES-ecb-encrypt (pkcs7-padding plain-text-bv 16) key)))
					      

(define (find-blocksize)
  (define key (gen-rand-aes-key))
  (define previous-length
    (bytevector-length (encryption-oracle (make-bytevector 0) key)))
  (define (iter length)
    (let ((later-length (bytevector-length
			 (encryption-oracle (make-bytevector length) key))))
      (if (or (not (= later-length previous-length))
	      (>= length 65))
	  (- later-length previous-length)
	  (iter (+ length 1)))))
  (iter 1))
 

(define (byte-at-a-time-ecb-simple)
  (let* ((blocksize (find-blocksize))
	 (key (gen-rand-aes-key))
	 (length-output
	  (bytevector-length
	   (encryption-oracle (make-bytevector 0) key)))
	 (n-blocks (quotient length-output blocksize))
	 (recovered (make-list 0)))
    (do ((i 0 (+ i 1)))
	((>= i n-blocks)
	 (pkcs7-unpadding (u8-list->bytevector recovered) blocksize))
      (let* ((start (* i blocksize)))
	(do ((k 1 (+ k 1)))
	    ((>= k (+ blocksize 1)))
	  (let* ((input-block (make-bytevector (- blocksize k)
					       (char->integer #\A)))
		 (encrypted (make-bytevector blocksize))
		 (composed-input (u8-list->bytevector
				  (append (bytevector->u8-list input-block)
					  recovered
					  (list 0))))
		 (length-composed-input (bytevector-length composed-input)))
	    (bytevector-copy! (encryption-oracle input-block key) start
			      encrypted 0 blocksize)
	    (do ((j 0 (+ j 1)))
		((>= j 256) )
	      (let ((encrypted-composed (make-bytevector blocksize)))
		(bytevector-u8-set! composed-input (- length-composed-input 1) j)
		(bytevector-copy! (encryption-oracle composed-input key) start
				  encrypted-composed 0 blocksize)
		(cond ((bytevector=? encrypted-composed encrypted)
		       (set! recovered (append! recovered (list j)))))))))))))


;; (define result (byte-at-a-time-ecb-simple))

;; (display (list->string (map integer->char (bytevector->u8-list result))))

