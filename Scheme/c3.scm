(define-module (c3)
  #:export (brute-force-xor
	    byte-vector-logxor
	    max-key-score
	    letter-freq-map
	    keys))

(add-to-load-path "./")

(use-modules (rnrs bytevectors)
	     ((c1)  #:select (encode-hex))
	     (srfi srfi-1)
             (srfi srfi-13)
	     (ice-9 hash-table)
             ((rnrs base) #:select (assert)))

;; Procedures necessary for hash table
(define (my-hash str size)
  (remainder (string-hash-ci str) size))


(define (my-assoc str alist)
  (find (lambda (pair) (string-ci=? str (car pair))) alist))


(define (hash-set! table key val)
  (hashx-set! my-hash my-assoc table key val))


(define (hash-ref table key)
  (hashx-ref my-hash my-assoc table key))


(define (byte-vector-logxor bv1 key)
  (let ((bv1-length (bytevector-length bv1))
	(bv1-xor-key (make-bytevector (bytevector-length bv1))))
    (do ((i 0 (+ 1 i)))
	((>= i bv1-length) bv1-xor-key)
      (bytevector-u8-set! bv1-xor-key i
			  (logxor (bytevector-u8-ref bv1 i) key)))))

;; helper procedure
(define (bytevector-map proc bv)
  (let* ((bv-length (bytevector-length bv))
	 (result (make-list bv-length)))
    (do ((i 0 (+ 1 i)))
	((>= i bv-length) result)
      (list-set! result i (proc (bytevector-u8-ref bv i))))))


(define (populate-letter-freq-map letter-freq-map)
  (define english-alphabet
  (list "a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m"
	"n" "o" "p" "q" "r" "s" "t" "u" "v" "x" "w" "y" "z" " "))
  ;; The frequency of the letters of the alphabet in English
  (define freq-alphabet
    (list 0.08167 0.01492 0.02782 0.04253 0.12702 0.02228 0.02015
	  0.06094 0.06966 0.00153 0.00772 0.04025 0.02406 0.06749
	  0.07507 0.01929 0.00095 0.05987 0.06327 0.09056 0.02758
	  0.00978 0.02360 0.00150 0.01974 0.00074 0.13591))
    (map (lambda (key value)
	 (hashx-set! my-hash my-assoc letter-freq-map key value))
       english-alphabet
       freq-alphabet)
    #t)


(define (max-key-score scores)
  (let ((list-max-len (length scores))
	(best-key-score (make-list 2 0)))
    (do ((i 0 (+ 1 i)))
	((>= i list-max-len) best-key-score)
      (cond ((>= (list-ref scores i)
		 (list-ref best-key-score 1))
	     (list-set! best-key-score 0 i)
	     (list-set! best-key-score 1 (list-ref scores i)))))))


(define (brute-force-xor bv freq-map keys)
  ;; converts bytevector contents to a list of frequencies
  (define (bv-hex-to-freq-vec bv)
    (map
     (lambda (ch)
       (let ((freq (hashx-ref my-hash my-assoc freq-map (string (char-downcase ch)))))
	 (if  (not freq)
	      0
	      freq))) ;;(string->list (utf8->string bv))))
     (map integer->char (bytevector->u8-list bv))))
    ;; input bv xored against all 256 possible 8 bit integers
  (let* ((list-bvs-logxored (bytevector-map
			     (lambda (key) (byte-vector-logxor bv key))  keys))
	 (hex-enc-bv-freq-vec (bv-hex-to-freq-vec bv)) ;; main vec
	 ;; list of all frequency vecs
	 (list-freq-vecs (map bv-hex-to-freq-vec list-bvs-logxored))
	 (scores (map (lambda (vec) (apply + vec)) list-freq-vecs)))
    (max-key-score scores)))



;; (define hex-enc-bv
;;   (encode-hex "1b37373331363f78151b7f2b783431333d78397828372d363c78373e783a393b3736"))
;; ;;(define ascii-scores-map (make-hash-table))
;; (define letter-freq-map (make-hash-table)) ;; English unigram frequency map
;; (define keys (u8-list->bytevector (iota 256 0)))   ;; ascii decimal map
;; (populate-letter-freq-map letter-freq-map) ;; Populate letter-freq-map

;; (define best-key-score
;;   (brute-force-xor hex-enc-bv letter-freq-map keys))


;; (display "Best key: ")
;; (display (first best-key-score))
;; (newline)
;; (display "Best Score: ")
;; (display (second best-key-score))
;; (newline)
;; (display "Message: ")
;; (display (utf8->string (byte-vector-logxor hex-enc-bv (first best-key-score))))
;; (newline)

;; (string=? "Cooking MC's like a pound of bacon" (utf8->string (byte-vector-logxor hex-enc-bv (first best-key-score))))

