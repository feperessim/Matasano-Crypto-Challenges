(define-module (c2)
  #:export (byte-vector-logxor
	    decode-hex))

(add-to-load-path "./")

(use-modules (rnrs bytevectors)
             ((c1) #:select (encode-hex))
             ((rnrs base) #:select (assert)))


(define (byte-vector-logxor bv1 bv2)
  (let ((bv1-length (bytevector-length bv1))
	(bv2-length (bytevector-length bv2))
	(bv1-xor-bv2 (make-bytevector (bytevector-length bv1))))
    (if (not (= bv1-length bv2-length))
	(error "not equal-length buffers")
	(do ((i 0 (+ 1 i)))
	    ((>= i bv1-length) bv1-xor-bv2)
	  (bytevector-u8-set! bv1-xor-bv2 i
			      (logxor (bytevector-u8-ref bv1 i)
				      (bytevector-u8-ref bv2 i)))))))

(define (decode-hex bv)
  (with-output-to-string
    (lambda ()
      (array-for-each (lambda (b) (display (number->string b 16)))
                      bv))))

(assert (string=?
         (decode-hex
          (byte-vector-logxor
           (encode-hex "1c0111001f010100061a024b53535009181c")
            (encode-hex "686974207468652062756c6c277320657965")))
          "746865206b696420646f6e277420706c6179"))

