(add-to-load-path "./")

(use-modules (rnrs bytevectors)
	     ((c3) #:select (brute-force-xor
			     letter-freq-map
			     keys
			     populate-letter-freq-map))
	     ((c4) #:select (read-strings-from-file))
	     ((c6) #:select (decode-base64
			     transpose
			     bytevector-logxor))
	     ((c11) #:select (gen-rand-aes-key))
	     ((c18) #:select (AES-ctr-encrypt)))


(define (truncate-min-length list-of-bv)
  (let ((min-length (apply min (map bytevector-length list-of-bv))))
    (map (lambda (bv) (let ((truncated-bv (make-bytevector min-length)))
			(bytevector-copy! bv 0 truncated-bv 0 min-length)
			truncated-bv))
	 list-of-bv)))


(define (brute-force-xor-lines encrypted-lines)
  (let* ((t-lines (truncate-min-length encrypted-lines))
	 (transposed-lines (transpose t-lines))
	 (key-length (length transposed-lines))
	 (keystream (make-bytevector key-length)))
    (do ((i 0 (+ i 1)))
	((>= i key-length) keystream)
      (let ((found-key (car (brute-force-xor (list-ref transposed-lines i)
					     letter-freq-map keys))))
	(bytevector-u8-set! keystream i found-key)))))
      

(define letter-freq-map (make-hash-table)) ;; English unigram frequency map
(define keys (u8-list->bytevector (iota 256)))   ;; ascii decimal map
(populate-letter-freq-map letter-freq-map)  ;; Populate letter-freq-map

(define key (gen-rand-aes-key))
(define nonce 0)
(define lines (map decode-base64
		   (read-strings-from-file "../text_files/20.txt")))

(define encrypted-lines (map (lambda (line) (AES-ctr-encrypt line key nonce))
			     lines))

(define keystream (brute-force-xor-lines encrypted-lines))

(define decrypted-lines (map (lambda (bv) (bytevector-logxor bv keystream))
			     (truncate-min-length encrypted-lines)))

(for-each (lambda (line) (display line) (newline))
	  (map utf8->string decrypted-lines))
