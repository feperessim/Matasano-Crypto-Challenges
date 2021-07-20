(define-module (c6)
  #:export (decode-base64
	    hamming-distance
	    break-into-chunks
	    bytevector-logxor))
  
(add-to-load-path "./")

(use-modules (rnrs bytevectors)
	     ((rnrs base) #:select (assert))
	     ((ice-9 textual-ports) #:select (get-string-all))
	     (srfi srfi-60) ;; bit-count
	     (srfi srfi-1) ;; list-index
	     ((c3) #:select (brute-force-xor
			     letter-freq-map
			     keys))
	     ((c5) #:select (convert-str-dec
			     repeating-xor)))


(define (bytevector-logxor bv1 bv2)
  (let* ((bv1-length (bytevector-length bv1))
	(bv2-length (bytevector-length bv2))
	(bv1-xor-bv2 (make-bytevector bv1-length)))
    (if (not (= bv1-length bv2-length))
	(error "Error! Input vectors must have same length")
	(do ((i 0 (+ 1 i)))
	    ((>= i bv1-length) bv1-xor-bv2)
	  (bytevector-u8-set! bv1-xor-bv2 i
			      (logxor (bytevector-u8-ref bv1 i)
				      (bytevector-u8-ref bv2 i)))))))

   
(define (hamming-distance bv1 bv2)
  (let ((bv1-length (bytevector-length bv1))
	(bv2-length (bytevector-length bv2)))
    (if (not (= bv1-length bv2-length))
	(error "Error! Input vectors must have same length")
	(let ((bit-count-list (make-list bv1-length))
	      (bv1-xor-bv2 (bytevector-logxor bv1 bv2)))
	  (do ((i 0 (+ 1 i)))
	      ((>= i bv1-length) (apply + bit-count-list))
	    (list-set! bit-count-list i
		       (bit-count (bytevector-u8-ref bv1-xor-bv2 i))))))))


(define (decode-base64 b64-string)
  (let* ((base64-map "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/")
	 (list-base64-map (string->list base64-map))
	 (pad (string-count b64-string #\=))
	 (encoded-bv (u8-list->bytevector
		      (map
		       (lambda (ch) (if (char=? ch #\=) 0 (list-index (lambda (x) (char=? x ch))
									      list-base64-map)))
		       (string->list b64-string))))
	 (encoded-bv-length (bytevector-length encoded-bv))
	 (decoded-bv-length (* 3 (floor (/ encoded-bv-length 4))))
	 (decoded-bv (make-bytevector decoded-bv-length)))
    (do ((i 0 (+ i 4))
	 (j 0 (+ j 3)))
	((>= i encoded-bv-length) (remove-pad-bv decoded-bv pad))
      (dec-base-64-set encoded-bv decoded-bv i j))))
      
	
(define (dec-base-64-set encoded-bv decoded-bv i j)
  (let ((dec-0 (logior (ash (logand (bytevector-u8-ref encoded-bv i) #x3F) 2)
		       (ash (logand (bytevector-u8-ref encoded-bv (+ i 1)) #x30) -4)))
	(dec-1 (logior (ash (logand (bytevector-u8-ref encoded-bv (+ i 1))  #xF) 4)
		       (ash (logand (bytevector-u8-ref encoded-bv (+ i 2))  #x3C) -2)))
	(dec-2 (logior (ash (logand (bytevector-u8-ref encoded-bv (+ i 2))  #x03) 6)
		       (bytevector-u8-ref encoded-bv (+ i 3)))))
    (bytevector-u8-set! decoded-bv j dec-0)
    (bytevector-u8-set! decoded-bv (+ j 1) dec-1)
    (bytevector-u8-set! decoded-bv (+ j 2) dec-2)))
    

(define (remove-pad-bv decoded-bv pad)
  (if (= pad 0)
      decoded-bv
      (let* ((decoded-bv-length (bytevector-length decoded-bv))
	     (decoded-bv-copy (make-bytevector (- decoded-bv-length pad))))
	(bytevector-copy! decoded-bv
			  0
			  decoded-bv-copy
			  0
			  (- decoded-bv-length pad))
	decoded-bv-copy)))


(define (break-into-chunks bv keysize)
  (let* ((bv-length (bytevector-length bv))
	 (rem (remainder bv-length keysize))
	 (chunks-length (quotient bv-length keysize))
	 (chunks (make-list chunks-length)))
    (do ((i 0 (+ i keysize))
	 (j 0 (+ j 1)))
	((>= i (- bv-length rem)) chunks)
      (let ((chunk (make-bytevector keysize)))
	(do ((k i (+ k 1))
	     (l 0 (+ l 1)))
	    ((>= l keysize) (list-set! chunks j chunk))
	  (bytevector-u8-set! chunk l
			      (bytevector-u8-ref bv k)))))))


(define (avg-norm-hamming-distance chunks keysize)
  (let* ((chunk-length (length chunks))
	 (rem (remainder chunk-length 2))
	 (distances-list-length (quotient chunk-length 2))
	 (distances-list (make-list distances-list-length 0)))
    (do ((i 0 (+ i 2))
	 (j 0 (+ j 1)))
	((>= i (- chunk-length rem)) (/ (apply + distances-list)
					distances-list-length))
      (list-set! distances-list j
		 (/ (hamming-distance (list-ref chunks i)
				      (list-ref chunks (+ i 1)))
		    keysize)))))
  
   
(define (find-keysize-min-score bv)
  (let ((scores (make-list 39)))
    (do ((i 0 (+ i 1))
	 (keysize 2 (+ keysize 1)))
	((>= keysize 41) (let ((pair (min-key-score scores)))
			   (list (+ (car pair) 2)
				 (cadr pair))))
      (list-set! scores i
		 (avg-norm-hamming-distance (break-into-chunks bv keysize) keysize)))))
	
     
(define (min-key-score scores)
  (let ((list-max-len (length scores))
	(best-key-score (make-list 2 +inf.0)))
    (do ((i 0 (+ 1 i)))
	((>= i list-max-len) best-key-score)
      (cond ((<= (list-ref scores i)
		 (list-ref best-key-score 1))
	     (list-set! best-key-score 0 i)
	     (list-set! best-key-score 1 (list-ref scores i)))))))


(define (transpose blocks)
  (let* ((m-rows (length blocks))
	(n-cols (bytevector-length (first blocks)))
	(blocks-transposed (make-list n-cols)))
    (do ((col 0 (+ col 1)))
	((>= col n-cols) blocks-transposed)
      (let ((new-block (make-bytevector m-rows)))
	(do ((row 0 (+ row 1)))
	    ((>= row m-rows) (list-set! blocks-transposed col new-block))
	  (bytevector-u8-set! new-block row
			      (bytevector-u8-ref (list-ref blocks row) col)))))))
						 

(define (brute-force-xor-blocks decoded-encrypted-text best-keysize)
  (let ((blocks-transposed
	 (transpose
	  (break-into-chunks decoded-encrypted-text best-keysize))))
    (map (lambda (block) (first (brute-force-xor block letter-freq-map keys)))
	 blocks-transposed)))

 

(define encrypted-text
  (string-filter
   (call-with-input-file "../text_files/6.txt" get-string-all)
   (lambda (ch) (not (char=? ch #\newline)))))


(define decoded-encrypted-text
  (decode-base64 encrypted-text))


(define keysize-min-score (find-keysize-min-score decoded-encrypted-text))
(define best-keysize (first keysize-min-score))
(define min-score (second keysize-min-score))
(define key (brute-force-xor-blocks decoded-encrypted-text best-keysize))
(define decrypted-text (repeating-xor decoded-encrypted-text (u8-list->bytevector key)))


(display "Best keysize: ")
(display best-keysize)
(newline)
(display "Minimum Score: ")
(display (exact->inexact min-score))
(newline)
(display "Key to decrypt the text: ")
(display (list->string (map integer->char key)))
(newline)
(newline)
(display "Decrypted text:")
(newline)
(newline)
(display (list->string (map integer->char (bytevector->u8-list decrypted-text))))


;;;;;;;;;;;;
;; tests ;;
;;;;;;;;;;;

;; test 1
;; (define str1 "this is a test")
;; (define str2 "wokka wokka!!!")
;; (assert (= (hamming-distance (convert-str-dec str1)
;; 			     (convert-str-dec str2))
;; 	   37))


;; test 2
;; (define base-64-string "SSdtIGtpbGxpbmcgeW91ciBicmFpbiBsaWtlIGEgcG9pc29ub3VzIG11c2hyb29t")
;; (assert (string=
;; 	 (encode-base64
;; 	  (decode-base64 base-64-string))
;; 	 base-64-string))


;; test3
;; (define base-64-string "YW55IGNhcm5hbCBwbGVhcw==")
;; (assert (string=
;; 	 (encode-base64
;; 	  (decode-base64 base-64-string))
;; 	 base-64-string))

;; test 4
;; (define base-64-string "YW55IGNhcm5hbCBwbGVhc3U=")
;; (assert (string=
;; 	 (encode-base64
;; 	  (decode-base64 base-64-string))
;; 	 base-64-string))



