(add-to-load-path "./")

(use-modules (rnrs bytevectors)
	     ((c9) #:select (pkcs7-padding))
	     ((c10) #:select (AES-cbc-encrypt
			      AES-cbc-decrypt))
	     ((c11) #:select (gen-rand-aes-key))
	     ((c15) #:select (pkcs7-unpadding)))


(define (encrypt plain-text key iv)
  (let* ((prefix "comment1=cooking%20MCs;userdata=")
	(suffix ";comment2=%20like%20a%20pound%20of%20bacon")
	(plain-text (string-delete (string-delete plain-text #\;) #\=))
	(to-encrypt (u8-list->bytevector
		     (map char->integer
			  (string->list (string-append prefix
						       plain-text
						       suffix)))))
	(blocksize 16))    
    (AES-cbc-encrypt (pkcs7-padding to-encrypt blocksize)
		     key
		     iv)))
			 

(define (decrypt encrypted key iv)
  (let* ((blocksize 16)
	 (decrypted-bv (pkcs7-unpadding
			(AES-cbc-decrypt encrypted
					 key
					 iv)
		       blocksize))
	 (decrypted-str (list->string (map integer->char
					   (bytevector->u8-list decrypted-bv)))))
    (string-contains decrypted-str ";admin=true;")))


(define (bit-flip-attack)
  (let* ((key (gen-rand-aes-key))
	(iv (gen-rand-aes-key))
	(plain-text "AadminAtrue")
	(encrypted (encrypt plain-text key iv)))
    (bytevector-u8-set! encrypted 16 (logxor (bytevector-u8-ref encrypted 16)
					     (char->integer #\A)
					     (char->integer #\;)))
    (bytevector-u8-set! encrypted 22 (logxor (bytevector-u8-ref encrypted 22)
					     (char->integer #\A)
					     (char->integer #\=)))
    (decrypt encrypted key iv)))
