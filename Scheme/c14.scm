(add-to-load-path "./")

(use-modules (rnrs bytevectors)
	     (srfi srfi-1)
	     ((c6) #:select (decode-base64))
	     ((c7) #:select (AES-ecb-encrypt))
	     ((c9) #:select (pkcs7-padding))
	     ((c10) #:select (pkcs7-unpadding))
	     ((c11) #:select (gen-rand-aes-key
			      gen-random-sequence))
	     ((c12) #:select (find-blocksize)))


(set! *random-state* (random-state-from-platform))

(define random-prefix-bv (gen-random-sequence (+ 1 (random 16)) 256))

(define (encryption-oracle plain-text-bv key)
  (let* ((secret (decode-base64
		  (string-append
		   "Um9sbGluJyBpbiBteSA1LjAKV2l0aCBteSByYWctdG9wIGRvd24gc28gbXkg"
		   "aGFpciBjYW4gYmxvdwpUaGUgZ2lybGllcyBvbiBzdGFuZGJ5IHdhdmluZyBq"
		   "dXN0IHRvIHNheSBoaQpEaWQgeW91IHN0b3A/IE5vLCBJIGp1c3QgZHJvdmUg"
		   "YnkK")))
	 (plain-text-bv (u8-list->bytevector
			 (append (bytevector->u8-list random-prefix-bv)
				 (bytevector->u8-list plain-text-bv)
				 (bytevector->u8-list secret)))))
    (AES-ecb-encrypt (pkcs7-padding plain-text-bv 16) key)))


(define (find-prefix-size blocksize)
  (define (loop previous-enc i count key)
    (let* ((plain-text (make-bytevector i (char->integer #\A)))
	   (encrypted (make-bytevector blocksize)))
      (bytevector-copy! (encryption-oracle plain-text key) 0
			encrypted 0 blocksize)
      (if (or (= i 17) (bytevector=? encrypted previous-enc))
	  (- blocksize count)
	  (loop encrypted (+ i 1) (+ count 1) key))))
  (let* ((key (gen-rand-aes-key))
	 (previous-enc (make-bytevector blocksize))
	 (count 0))
    (bytevector-copy! (encryption-oracle (make-bytevector 0) key) 0
		      previous-enc 0 blocksize)
    
    (loop previous-enc 1 count key)))


(define (byte-at-a-time-ecb-simple)
  (let* ((blocksize (find-blocksize))
	 (key (gen-rand-aes-key))
	 (length-output
	  (bytevector-length
	   (encryption-oracle (make-bytevector 0) key)))
	 (n-blocks (quotient length-output blocksize))
	 (recovered (make-list 0))
	 (prefix-size (find-prefix-size blocksize)))
    (do ((i 0 (+ i 1)))
    	((>= i n-blocks)
    	 (pkcs7-unpadding (u8-list->bytevector recovered) blocksize))
      (let* ((start (+ (* i blocksize) blocksize)))
    	(do ((k 1 (+ k 1)))
    	    ((>= k (+ blocksize 1)) )
    	  (let* ((input-block (make-bytevector (+ (- blocksize prefix-size)
    						  (- blocksize k))
    					       (char->integer #\A)))
    		 (encrypted (make-bytevector blocksize))
    		 (composed-input (u8-list->bytevector
    				  (append (bytevector->u8-list input-block)
    					  recovered
    					  (list 0))))
    		 (length-composed-input (bytevector-length composed-input)))
	    (cond ((< start length-composed-input)
		   (bytevector-copy! (encryption-oracle input-block key) start
				     encrypted 0 blocksize)
		   (do ((j 0 (+ j 1)))
		       ((>= j 256) )
		     (let ((encrypted-composed (make-bytevector blocksize)))
		       (bytevector-u8-set! composed-input (- length-composed-input 1) j)
		       (bytevector-copy! (encryption-oracle composed-input key) start
					 encrypted-composed 0 blocksize)
		       (cond ((bytevector=? encrypted-composed encrypted)
			      (set! recovered (append! recovered (list j)))))))))))))))


(define result (byte-at-a-time-ecb-simple))

(display (list->string (map integer->char (bytevector->u8-list result))))

					      
