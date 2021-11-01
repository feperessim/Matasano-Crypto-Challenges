(add-to-load-path "./")

(use-modules (rnrs bytevectors)
	     (srfi srfi-1)
	     (system repl error-handling)
	     ((c6) #:select (decode-base64
			     bytevector-logxor))
	     ((c10) #:select (AES-cbc-encrypt
			      AES-cbc-decrypt))
	     ((c11) #:select (gen-rand-aes-key))
	     ((c15) #:select (pkcs7-unpadding)))


(set! *random-state* (random-state-from-platform))


(define strings
  '("MDAwMDAwTm93IHRoYXQgdGhlIHBhcnR5IGlzIGp1bXBpbmc="
    "MDAwMDAxV2l0aCB0aGUgYmFzcyBraWNrZWQgaW4gYW5kIHRoZSBWZWdhJ3MgYXJlIHB1bXBpbic="
    "MDAwMDAyUXVpY2sgdG8gdGhlIHBvaW50LCB0byB0aGUgcG9pbnQsIG5vIGZha2luZw=="
    "MDAwMDAzQ29va2luZyBNQydzIGxpa2UgYSBwb3VuZCBvZiBiYWNvbg=="
    "MDAwMDA0QnVybmluZyAnZW0sIGlmIHlvdSBhaW4ndCBxdWljayBhbmQgbmltYmxl"
    "MDAwMDA1SSBnbyBjcmF6eSB3aGVuIEkgaGVhciBhIGN5bWJhbA=="
    "MDAwMDA2QW5kIGEgaGlnaCBoYXQgd2l0aCBhIHNvdXBlZCB1cCB0ZW1wbw=="
    "MDAwMDA3SSdtIG9uIGEgcm9sbCwgaXQncyB0aW1lIHRvIGdvIHNvbG8="
    "MDAwMDA4b2xsaW4nIGluIG15IGZpdmUgcG9pbnQgb2g="
    "MDAwMDA5aXRoIG15IHJhZy10b3AgZG93biBzbyBteSBoYWlyIGNhbiBibG93"))


(define key (gen-rand-aes-key))


(define blocksize 16)


(define (cbc-padding-oracle-encrypt)
  (let* ((random-index (random (length strings)))
	 (iv (gen-rand-aes-key))
	 (plain-text (u8-list->bytevector
		      (map char->integer
			   (string->list (list-ref strings random-index))))))
    (list (AES-cbc-encrypt plain-text key iv) iv)))


(define (cbc-padding-oracle-decrypt encrypted iv)
  (let ((decrypted (AES-cbc-decrypt encrypted key iv)))
    (false-if-exception (pkcs7-unpadding decrypted blocksize))))



(define (cbc-padding-attack-single-block block iv)
  (let ((padding #x01)
	(i-state (make-bytevector blocksize)))
    (do ((i (- blocksize 1) (- i 1)))
	((< i 0) (bytevector-logxor i-state iv))
      (do ((byte 0 (+ byte 1)))
	  ((>= byte 256) )
	(bytevector-u8-set! block i byte)
	(cond ((cbc-padding-oracle-decrypt block iv)
	       (bytevector-u8-set! i-state i (logxor byte padding))
	       (set! padding (+ padding #x01))
	       (set! byte 256))))
      (do ((k 1 (+ k 1)))
	  ((>= k padding) )
	(bytevector-u8-set! block (- blocksize k)
			    (logxor (bytevector-u8-ref i-state (- blocksize k))
				    padding))))))


(define (cbc-padding-attack)
  (let* ((encrypted-and-iv-list (cbc-padding-oracle-encrypt))
	 (encrypted (car encrypted-and-iv-list))
	 (iv (cadr encrypted-and-iv-list))
	 (encrypted-length (bytevector-length encrypted))
	 (block (make-bytevector (* 2 blocksize)))
	 (plain-text (make-bytevector encrypted-length)))
    (do ((block-index 0 (+ block-index blocksize)))
	((>= block-index encrypted-length) (pkcs7-unpadding plain-text blocksize))
      (bytevector-copy! encrypted block-index
			block blocksize blocksize)
      (bytevector-copy! (cbc-padding-attack-single-block block iv) 0
			plain-text block-index blocksize)
      (bytevector-copy! encrypted block-index
			iv 0 blocksize))))


;; (display (utf8->string (decode-base64 (utf8->string (cbc-padding-attack)))))
