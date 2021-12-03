(add-to-load-path "./")

(use-modules ((c21) #:select (MT19937)))

(set! *random-state* (random-state-from-platform))


(define (rnd-MT19937-unix-timestamp-seed)
  (let ((seconds (+ 40 (random 1001))))
    (sleep seconds)
    (let* ((seed (current-time))
	   (random-proc (MT19937 seed #f))
	   (seconds (+ 40 (random 1001))))
      (sleep seconds)
      (cons (random-proc) seed))))


(define (crack-MT19937-seed)
  (define (crack-it now random-number)
    (cond ((< now 0) (display "Could not recover the seed\n"))
	  ((= random-number ((MT19937 now #f)))
	   (display "Recovered seed ")
	   (display now)
	   (newline))
	  (else (crack-it (- now 1) random-number)))) 
  (let ((rnd-num-and-seed-list (rnd-MT19937-unix-timestamp-seed)))
    (let ((random-number (car rnd-num-and-seed-list))
	  (real-seed (cdr rnd-num-and-seed-list))
	  (now (current-time)))
      (display "Generated random number ")
      (display random-number)
      (newline)
      (display "Real seed ")
      (display real-seed)
      (newline)
      (crack-it now random-number))))
	     

	 
	 
	 
    
	
		 
  
