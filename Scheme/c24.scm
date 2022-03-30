(add-to-load-path "./")

(use-modules ((c21) #:select (MT19937)))


(define MAX-SEED 65535)


(define (keystream-generator seed length)
  (let ((prng (MT19937 seed #f)))
    (map (lambda (val) (prng)) (make-list length))))


(define (stream-cipher-mt19937-enc keystream plain-text)
  (map (lambda (x y) (logxor x y))  keystream  plain-text))


(define (stream-cipher-mt19937-dec keystream encrypted-text)
  (stream-cipher-mt19937-enc keystream encrypted-text))
				  				  

(define (brute-force-stream-cipher-mt19937 plain-text encrypted-text)
  (define (decrypt seed length)
    (stream-cipher-mt19937-dec (keystream-generator seed length) encrypted-text))
  (define (crack-seed seed length)
    (cond ((> seed MAX-SEED) '())
	  ((equal? (decrypt seed length) plain-text) seed)
	  (else (crack-seed (+ seed 1) length))))
  (crack-seed 0 (length encrypted-text)))


(define keystream (keystream-generator 47 24))
(define plain-text (append (map (lambda (x) (random 256)) (make-list 10)) (make-list 14 (char->integer #\A))))
(define encrypted-text (stream-cipher-mt19937-enc keystream plain-text))
(define found-seed (brute-force-stream-cipher-mt19937 plain-text encrypted-text))
(display "Seed found: ")
(display found-seed)
(newline)
