(define-module (c1)
  #:export (encode-hex))

(use-modules (rnrs bytevectors)
	     ((rnrs base)
              #:select (assert)))
	     

(define (encode-hex hex-string)
  (let* ((hex-str-length (ash (string-length hex-string) -1))
	(byte-vector (make-bytevector hex-str-length)))
    (do ((i 0 (+ i 2))
	 (j 0 (+ j 1)))
	((>= j hex-str-length) byte-vector)
      (bytevector-u8-set! byte-vector j
			  (string->number
			   (substring hex-string i (+ i 2))
			   16)))))


(define (encode-base64 hex-byte-vector)
  (let* ((hex-bv-length (bytevector-length hex-byte-vector))
	 (result-str-length (* 4 (ceiling (/ hex-bv-length 3))))
	 (result-b64-str (make-string result-str-length))
	 (pad (remainder (* 8 hex-bv-length) 3)))
    (do ((i 0 (+ i 3))
	 (j 0 (+ j 4)))
	((>= i (- hex-bv-length (* (ceiling (/ pad 3)) 3)))
	 (pad-base64-str hex-byte-vector result-b64-str i j pad))      
      (enc-base-64-set hex-byte-vector result-b64-str i j))))
      

(define (pad-base64-str hex-byte-vector result-b64-str i j pad)
  (let ((byte-vector (make-bytevector 3)))
    (cond ((= pad 0) result-b64-str)
	  ((= pad 1)
	   (bytevector-u8-set! byte-vector 0
			       (bytevector-u8-ref hex-byte-vector i))
	   (bytevector-u8-set! byte-vector 1
			       (bytevector-u8-ref hex-byte-vector (+ i 1)))
	   (bytevector-u8-set! byte-vector 2 0)
	   (enc-base-64-set byte-vector result-b64-str 0 j)
	   (string-set! result-b64-str (+ j 3) #\=)
	   result-b64-str)
	  (else
	   (bytevector-u8-set! byte-vector 0
			       (bytevector-u8-ref hex-byte-vector i))
	   (enc-base-64-set byte-vector result-b64-str 0 j)
	   (string-set! result-b64-str (+ j 2) #\=)
	   (string-set! result-b64-str (+ j 3) #\=)
	   result-b64-str))))
	      
  

(define (enc-base-64-set hex-byte-vector result-b64-str i j)
  (let ((base64-map "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/")
	(index-0 (ash (logand (bytevector-u8-ref hex-byte-vector i) #xFC) -2))
	(index-1 (logior (ash (logand (bytevector-u8-ref hex-byte-vector i) #x03) 4)
			 (ash (logand (bytevector-u8-ref hex-byte-vector (+ i 1)) #xF0) -4)))
	(index-2 (logior (ash (logand (bytevector-u8-ref hex-byte-vector (+ i 1)) #xF) 2)
			 (ash (logand (bytevector-u8-ref hex-byte-vector (+ i 2)) #xC0) -6)))
	(index-3 (logand (bytevector-u8-ref hex-byte-vector (+ i 2)) #x3F)))
    (string-set! result-b64-str j
		 (string-ref base64-map
			     index-0))
    (string-set! result-b64-str (+ j 1)
		 (string-ref base64-map
			     index-1))    
    (string-set! result-b64-str (+ j 2)
		 (string-ref base64-map
			     index-2))
    (string-set! result-b64-str (+ j 3)
		 (string-ref base64-map
			     index-3))))


;;;;;;;;;;;;
;; tests ;;
;;;;;;;;;;;

;; test 1
;; (define hex-string "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d")
;; (define base-64-string "SSdtIGtpbGxpbmcgeW91ciBicmFpbiBsaWtlIGEgcG9pc29ub3VzIG11c2hyb29t")
;; (assert (string=
;; 	 (encode-base64
;; 	  (encode-hex hex-string))
;; 	 base-64-string))

;; test 2
;; (define hex-string "616e79206361726e616c20706c656173")
;; (define base-64-string "YW55IGNhcm5hbCBwbGVhcw==")
;; (assert (string=
;; 	 (encode-base64
;; 	  (encode-hex hex-string))
;; 	 base-64-string))


;; test 3
;; (define hex-string "616e79206361726e616c20706c65617375")
;; (define base-64-string "YW55IGNhcm5hbCBwbGVhc3U=")
;; (assert (string=
;; 	 (encode-base64
;; 	  (encode-hex hex-string))
;; 	 base-64-string))


;; test 4
;; (define hex-string "616e79206361726e616c20706c6561737572")
;; (define base-64-string "YW55IGNhcm5hbCBwbGVhc3Vy")
;; (assert (string=
;; 	 (encode-base64
;; 	  (encode-hex hex-string))
;; 	 base-64-string))

;; test 5
;; y(define hex-string "616e79206361726e616c20706c656173757265")
;; (define base-64-string "YW55IGNhcm5hbCBwbGVhc3VyZQ==")
;; (assert (string=
;; 	 (encode-base64
;; 	  (encode-hex hex-string))
;; 	 base-64-string))


;; (display "Done - Passed all tests")
;; (newline)
