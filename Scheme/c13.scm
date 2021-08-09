(add-to-load-path "./")

(use-modules (rnrs bytevectors)
	     ((c7) #:select (AES-ecb-encrypt
			     AES-ecb-decrypt))
	     ((c9) #:select (pkcs7-padding))
	     ((c10) #:select (pkcs7-unpadding))
	     ((c11) #:select (gen-rand-aes-key)))


(define (string-replace str exclude)
  (list->string
   (filter (lambda (char) (not (eq? char exclude)))
	   (string->list str))))


(define (kv-parser cookie)
  (map (lambda (str) (string-split str #\=))
       (string-split cookie #\&)))


(define (profile-for email)
  (string-concatenate
   (list "email="
	 (string-replace (string-replace email #\&) #\=)
	 "&uid=10&role=user")))


(define (encrypt-user-profile user-profile key)
  (AES-ecb-encrypt (pkcs7-padding (u8-list->bytevector
				   (map char->integer
					(string->list user-profile)))
				  16)
		   key))


(define (decrypt-user-profile encrypted key)
  (kv-parser
   (list->string
    (map integer->char
	 (bytevector->u8-list
	  (pkcs7-unpadding (AES-ecb-decrypt encrypted key) 16))))))


(define (profile-for-oracle email key)
  (encrypt-user-profile (profile-for email) key))


(define (ecb-cut-and-paste-attack)
  (let* ((email "oo@bar.comaaa")
	 (email-to-trick (string-concatenate (list "oo@bar.com"
						   "admin"
						   (make-string 11 #\x0b))))
	 (key (gen-rand-aes-key))
	 (encrypted (profile-for-oracle email key))
	 (encrypted-admin (profile-for-oracle email-to-trick key))
	 (encrypted-tricky (make-bytevector 48)))
    (bytevector-copy! encrypted 0 encrypted-tricky 0 32)
    (bytevector-copy! encrypted-admin 16 encrypted-tricky 32 16)
    (decrypt-user-profile encrypted-tricky key)))


;; tests
;; (define cookie "foo=bar&baz=qux&zap=zazzle")
;; (kv-parser cookie)
;; (define email "foo@bar.com")
;; (define encoded (profile-for email))
;; (define key (gen-rand-aes-key))
;; (define encrypted (encrypt-user-profile encoded key))
;; (define encrypted-parsed (decrypt-user-profile encrypted key))
;; (assoc-ref (kv-parser cookie) "baz")
