(add-to-load-path "./")

(use-modules (rnrs bytevectors))


(define (MT19937 seed sixty-four-bits)
  (define (get-lowest-bits number n-bits )
    (logand number (- (ash 1 n-bits) 1)))
  
  (define (generate-prng-proc w n m r a u d s b t c l f)
    (define (twist MT lower-mask upper-mask)
      (do ((i 0 (+ i 1)))
	  ((>= i n) )
	(let* ((x (+ (logand (list-ref MT i) upper-mask)
		    (logand (list-ref MT (remainder (+ i 1) n)) lower-mask)))
	       (xA (ash x (- 1))))
	  (cond ((not (eq? (remainder x 2) 0))
		 (set! xA (logxor xA a))))
	  (list-set! MT i (logxor (list-ref MT (remainder (+ i m) n))
				  xA)))))

    (define (extract-number MT)
      (let* ((index (+ n 1))
	    (lower-mask (- (ash 1 r) 1))
	    (upper-mask (- (- (ash 1 w) 1) lower-mask)))
	(lambda ()
	  (cond ((>= index n) (twist MT lower-mask upper-mask) (set! index 0)))
	  (let ((y (list-ref MT index)))
	    (set! y (logxor y (logand (ash y (- u)) d)))
	    (set! y (logxor y (logand (ash y s) b)))
	    (set! y (logxor y (logand (ash y t) c)))
	    (set! y (logxor y (ash y (- l))))
	    (set! index (+ index 1))
	    (get-lowest-bits y w)))))

    (let ((MT (make-list n 0)))
      (list-set! MT 0 seed)
      (do ((i 1 (+ i 1)))
	  ((>= i n) (extract-number MT))
	(list-set! MT i
		   (get-lowest-bits
		    (+ (* f (logxor (list-ref MT (- i 1))
				    (ash (list-ref MT (- i 1))
					 (- (- w 2))))) i)
		    w)))))
  
  (cond ((not sixty-four-bits)
	 (let ((w 32)
	       (n 624)
	       (m 397)
	       (r 31)
	       (a #x9908B0DF)
	       (u 11)
	       (d #xFFFFFFFF)
	       (s 7)
	       (b #x9D2C5680)
	       (t 15)
	       (c #xEFC60000)
	       (l 18)
	       (f 1812433253))
	   (generate-prng-proc w n m r a u d s b t c l f)))
	(else
	 (let ((w 64)
	       (n 312)
	       (m 156)
	       (r 31)
	       (a #xB5026F5AA96619E9)
	       (u 29)
	       (d #x5555555555555555)
	       (s 17)
	       (b #x71D67FFFEDA60000)
	       (t 37)
	       (c #xFFF7EEE000000000)
	       (l 43)
	       (f 6364136223846793005))
	   (generate-prng-proc w n m r a u d s b t c l f)))))



(define random (MT19937 47 #f)) ;; 32-bit
(for-each (lambda (x) (display x) (newline))
	  (list (random)
		(random)
		(random)
		(random)
		(random)
		(random)
		(random)
		(random)
		(random)
		(random)))

(newline)
(define random (MT19937 47 #t)) ;; 64-bit
(for-each (lambda (x) (display x) (newline))
	  (list (random)
		(random)
		(random)
		(random)
		(random)
		(random)
		(random)
		(random)
		(random)
		(random)))