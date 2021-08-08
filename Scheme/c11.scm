(define-module (c11)
  #:export (gen-rand-aes-key))

(add-to-load-path "./")

(use-modules (rnrs bytevectors)
	     (srfi srfi-1)
	     ((c6) #:select (break-into-chunks))
	     ((c7) #:select (AES-ecb-encrypt))
	     ((c9) #:select (pkcs7-padding))
	     ((c10) #:select (AES-cbc-encrypt)))

(set! *random-state* (random-state-from-platform))


(define (gen-random-sequence length upper-bound)
  (u8-list->bytevector
   (map (lambda (x) (random x)) (make-list length upper-bound))))

  
(define (gen-rand-aes-key)
  (gen-random-sequence 16 256))


(define (encryption-oracle plain-text-bv)
  (let* ((key (gen-rand-aes-key))
	(n (+ (random 6)  5))
	(m (+ (random 6)  5))
	(left-bytes (gen-random-sequence n 256))
	(right-bytes (gen-random-sequence m 256))
	(plain-text-bv (u8-list->bytevector
			(append (bytevector->u8-list left-bytes)
				(bytevector->u8-list plain-text-bv)
				(bytevector->u8-list right-bytes)))))
    (if (= (random 2) 1)
	(cons (AES-ecb-encrypt (pkcs7-padding plain-text-bv 16) key) 1)
	(let ((iv (gen-rand-aes-key)))
	  (cons (AES-cbc-encrypt plain-text-bv key iv) 0)))))


(define (detect-block-cipher-mode encrypted-bv)
  (let ((chunks (map bytevector->u8-list (break-into-chunks encrypted-bv 16))))
    (if (> (- (length chunks) (length (delete-duplicates chunks))) 0)
	1
	0)))
    
    
	
