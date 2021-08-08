(define-module (c5)
  #:export (convert-str-dec
	    repeating-xor))

(add-to-load-path "./")

(use-modules (rnrs bytevectors)
	     (ice-9 format)
	     ((c1) #:select (encode-hex))
	     ((rnrs base) #:select (assert)))


(define (convert-str-dec str)
  (u8-list->bytevector
   (map char->integer (string->list str))))
  

(define (repeating-xor message key)
  (let* ((message-length (bytevector-length message))
	(key-length (bytevector-length key))
	(xored-message (make-bytevector message-length)))
    (do ((i 0 (+ i 1))
	 (j 0 (remainder (+ j 1) key-length)))
	((>= i message-length) xored-message)
      (bytevector-u8-set! xored-message i
			  (logxor
			   (bytevector-u8-ref message i)
			   (bytevector-u8-ref key j))))))
	 

(define (decode-formated-hex bv)
  (string-concatenate
   (map
    (lambda (x) (format #f "~2,'0x" x))
    (bytevector->u8-list bv))))


;; (define message "Burning 'em, if you ain't quick and nimble\nI go crazy when I hear a cymbal")
;; (define key "ICE")
;; (define encrypted "0b3637272a2b2e63622c2e69692a23693a2a3c6324202d623d63343c2a26226324272765272a282b2f20430a652e2c652a3124333a653e2b2027630c692b20283165286326302e27282f")


;; (assert (string=?
;; 	 (decode-formated-hex
;; 	  (repeating-xor
;; 	   (convert-str-dec message)
;; 	   (convert-str-dec key)))
;; 	 encrypted))

