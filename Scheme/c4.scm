(define-module (c4)
  #:export (read-strings-from-file))

(add-to-load-path "./")

(use-modules (rnrs bytevectors)
	     ((c1) #:select (encode-hex))
	     ((c3) #:select (brute-force-xor
			     byte-vector-logxor
			     max-key-score
			     letter-freq-map
			     keys))
	     (srfi srfi-1)
	     (ice-9 textual-ports)
             ((rnrs base) #:select (assert)))


(define (read-strings-from-file filename)
  (define (read-strings file)
    (let ((line (get-line file)))
      (if (eof-object? line)
	  '()
	  (cons line (read-strings file)))))    
  (let ((file (open-file filename "r")))
    (read-strings file)))


(define (argmax scores)
  (first (max-key-score scores)))
  

(define filename "../text_files/4.txt")
(define file (open-file filename "r"))
(define list-of-strings (read-strings-from-file filename))
(close-port file) 
  
(define list-of-key-score-str
  (map (lambda (string) (append
			 (brute-force-xor (encode-hex string) letter-freq-map keys)
			 (list string)))
       list-of-strings))

(define index-best-score
  (argmax (map cadr list-of-key-score-str)))

(define best-key
  (first (list-ref list-of-key-score-str index-best-score)))

(define best-score
  (second (list-ref list-of-key-score-str index-best-score)))

(define str
  (third (list-ref list-of-key-score-str index-best-score)))

(define message
  (list->string (map integer->char
		     (bytevector->u8-list
		    (byte-vector-logxor (encode-hex str) best-key)))))


(display "Best key: ")
(display best-key)
(newline)
(display "Best Score: ")
(display best-score)
(newline)
(display "String: ")
(display str)
(newline)
(display "Message: ")
(display message)
(newline)


