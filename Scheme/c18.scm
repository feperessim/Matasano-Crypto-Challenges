(define-module (c18)
  #:export (AES-ctr-encrypt))

(add-to-load-path "./")

(use-modules (rnrs bytevectors)
	     ((c6) #:select (decode-base64))
	     ((c7) #:select (AES-ecb-encrypt)))


(define (to-bytes-little-endian uinteger length)
  (let ((bytes-bv (make-bytevector length 0)))
    (do ((i 0 (+ i 1)))
	((>= i length) bytes-bv)
      (bytevector-u8-set! bytes-bv i (logand uinteger #xff))
      (set! uinteger (ash uinteger -8)))))
    
	      
(define (byte-vector-logxor-min-len bv1 bv2)
  (let* ((bv1-length (bytevector-length bv1))
	 (bv2-length (bytevector-length bv2))
	 (min-length (min bv1-length bv2-length))
	 (bv1-xor-bv2 (make-bytevector min-length)))
    (do ((i 0 (+ 1 i)))
	((>= i min-length) bv1-xor-bv2)
      (bytevector-u8-set! bv1-xor-bv2 i
			  (logxor (bytevector-u8-ref bv1 i)
				  (bytevector-u8-ref bv2 i))))))


(define (break-into-blocks bv blocksize)
  (let ((bv-length (bytevector-length bv))
	(blocks (list )))
    (do ((i 0 (+ i blocksize)))
	((>= i bv-length) blocks)
      (cond ((> (+ i blocksize) bv-length)
	     (let* ((diff (- bv-length i))
		   (block (make-bytevector diff)))
	       (bytevector-copy! bv i block 0 diff)
	       (set! blocks (append blocks (list block)))))
	    (else (let ((block (make-bytevector blocksize)))
		    (bytevector-copy! bv i block 0 blocksize)
		    (set! blocks (append blocks (list block)))))))))


(define (AES-ctr-encrypt plain-text key nonce)
  (let* ((blocksize 16)
	 (counter 0)
	 (plain-text-length (bytevector-length plain-text))
	 (encrypted (make-bytevector plain-text-length))
	 (plain-text-blocks-list (break-into-blocks plain-text blocksize))
	 (plain-text-blocks-list-length (length plain-text-blocks-list))
	 (block (make-bytevector blocksize)))
    (bytevector-copy! (to-bytes-little-endian nonce 8) 0 block 0 8)
    (do ((i 0 (+ i 1)))
	((>= i plain-text-blocks-list-length) encrypted)
      (bytevector-copy! (to-bytes-little-endian counter 8) 0 block 8 8)
      (let* ((plain-text-block (list-ref plain-text-blocks-list i))
	     (plain-text-block-length (bytevector-length plain-text-block))
	     (encrypted-block (AES-ecb-encrypt block key)))
	(bytevector-copy! (byte-vector-logxor-min-len encrypted-block plain-text-block) 0
			  encrypted (* i blocksize) plain-text-block-length))     
      (set! counter (+ counter 1)))))


(define (AES-ctr-decrypt encrypted-text key nonce)
  (AES-ctr-encrypt encrypted-text key nonce))


;; (define string "L77na/nrFsKvynd6HzOoG7GHTLXsTVu9qvY/2syLXzhPweyyMTJULu/6/kXX0KSvoOLSFQ==")
;; (define key (string->utf8 "YELLOW SUBMARINE"))
;; (define nonce 0)
;; (define encrypted (decode-base64 string))
;; (define decrypted (AES-ctr-decrypt encrypted key nonce))
;; (display (utf8->string decrypted))
