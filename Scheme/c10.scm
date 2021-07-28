(define-module (c10)
  #:export (AES-cbc-encrypt))

(add-to-load-path "./")

(use-modules (rnrs bytevectors)
	     (srfi srfi-1)
	     ((ice-9 textual-ports) #:select (get-string-all))
	     ((c9) #:select (pkcs7-padding))
	     ((c7) #:select (AES-ecb-encrypt
			     AES-ecb-decrypt))
	     ((c6) #:select (decode-base64
			     bytevector-logxor)))


(define (AES-cbc-encrypt plain-text-bv key iv)
  (let* ((blocksize 16)
	 (iv-copy (bytevector-copy iv))
	 (padded-plain-text-bv (pkcs7-padding plain-text-bv blocksize))
	 (text-length (bytevector-length padded-plain-text-bv))
	 (encrypted  (make-bytevector text-length))
	 (block (make-bytevector blocksize)))
    (do ((i 0 (+ i blocksize)))
	((>= i text-length) encrypted)
      (bytevector-copy! padded-plain-text-bv i block 0 blocksize)
      (bytevector-copy! (AES-ecb-encrypt (bytevector-logxor block iv-copy) key) 0
			encrypted i blocksize)
      (bytevector-copy! encrypted i iv-copy 0 blocksize))))


(define (AES-cbc-decrypt encrypted-text-bv key iv)
  (let* ((blocksize 16)
	 (iv-copy (bytevector-copy iv))
	 (text-length (bytevector-length encrypted-text-bv))
	 (decrypted (make-bytevector text-length))
	 (block (make-bytevector blocksize)))
    (do ((i 0 (+ i blocksize)))
	((>= i text-length) (pkcs7-unpadding decrypted blocksize))
      (bytevector-copy! encrypted-text-bv i block 0 blocksize)
      (bytevector-copy! (bytevector-logxor (AES-ecb-decrypt block key) iv-copy) 0
			decrypted i blocksize)
      (bytevector-copy! block 0 iv-copy 0 blocksize))))
  

(define (pkcs7-unpadding bv blocksize)
  (let* ((bv-length (bytevector-length bv))
	 (pad (bytevector-u8-ref bv (- bv-length 1)))
	 (new-length (- bv-length pad))
	 (bv1 (make-bytevector pad pad))
	 (bv2 (make-bytevector pad)))
    (cond ((< new-length 0) bv)
	  (else
	   (bytevector-copy! bv new-length bv2 0 pad)
	   (if (bytevector=? bv1 bv2)
	       (let ((new-bv (make-bytevector new-length)))
		 (bytevector-copy! bv 0 new-bv 0 new-length)
		 new-bv)
	       bv)))))

;; Initial tests
;; (define plain-text-bv (u8-list->bytevector
;; 		    (map char->integer
;; 			 (string->list "Hello World"))))

;; (define key (u8-list->bytevector
;; 	     (map char->integer
;; 		  (string->list "YELLOW SUBMARINE"))))

;; (define iv (u8-list->bytevector
;; 	    (map random (make-list 16 256))))


;; (define encrypted-text-bv (AES-cbc-encrypt plain-text-bv key iv))

;; (display encrypted-text-bv)
;; (newline)

;; (define decrypted-text-bv (AES-cbc-decrypt encrypted-text-bv key iv))

;; (display (list->string (map integer->char (bytevector->u8-list decrypted-text-bv))))
;; (newline)

(define key (u8-list->bytevector
	     (map char->integer
		  (string->list "YELLOW SUBMARINE"))))

(define iv (u8-list->bytevector (make-list 16 0)))

(define encrypted-text
  (string-filter
   (call-with-input-file "../text_files/10.txt" get-string-all)
   (lambda (ch) (not (char=? ch #\newline)))))

(define decoded-encrypted-text
  (decode-base64 encrypted-text))

(define decrypted-text-bv (AES-cbc-decrypt decoded-encrypted-text key iv))

(display "Decrypted text:")
(newline)
(newline)
(display (list->string (map integer->char (bytevector->u8-list decrypted-text-bv))))

