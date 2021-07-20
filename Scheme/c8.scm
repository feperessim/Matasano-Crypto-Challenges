(add-to-load-path "./")

(use-modules (rnrs bytevectors)
	     (srfi srfi-1)
	     ((c4) #:select (read-strings-from-file))
	     ((c6) #:select (break-into-chunks
			     decode-base64)))


(define (arg-min values)
  (define (loop values index-min-val min-val index)
    (if (null? values)
	index-min-val
	(if (< (car values) min-val)
	    (loop (cdr values) index (car values) (+ index 1))
	    (loop (cdr values) index-min-val min-val (+ index 1)))))
  (loop values 0 (car values) 0))
	 

(define strings (read-strings-from-file "../text_files/8.txt"))


(define distances
  (map (lambda (string)
	 (length
	  (delete-duplicates
	   (map bytevector->u8-list
		(break-into-chunks
		 (decode-base64 string) 16)))))
       strings))


(define index (arg-min distances))

(display "String encrypted with AES in ECB mode")
(newline)
(newline)
(display (list-ref strings index))
(newline)
(newline)



	 
;; (define (break-str-into-chunks string)
;;   (map (lambda (start) (substring string start (+ start  16)))
;;        (filter (lambda (x) (= (remainder x 16) 0))
;; 	       (iota (string-length string)))))
