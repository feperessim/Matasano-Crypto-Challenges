(define-module (c7)
  #:export (AES-ecb-encrypt
	   AES-ecb-decrypt))

(add-to-load-path "./")

(use-modules (rnrs bytevectors)
	     ((ice-9 textual-ports) #:select (get-string-all))
	     ((c6) #:select (decode-base64)))

(define s-box
  (u8-list->bytevector
   (list
    #x63 #x7c #x77 #x7b #xf2 #x6b #x6f #xc5 #x30 #x1 #x67 #x2b #xfe #xd7 #xab #x76
    #xca #x82 #xc9 #x7d #xfa #x59 #x47 #xf0 #xad #xd4 #xa2 #xaf #x9c #xa4 #x72 #xc0
    #xb7 #xfd #x93 #x26 #x36 #x3f #xf7 #xcc #x34 #xa5 #xe5 #xf1 #x71 #xd8 #x31 #x15 
    #x4 #xc7 #x23 #xc3 #x18 #x96 #x5 #x9a #x7 #x12 #x80 #xe2 #xeb #x27 #xb2 #x75 
    #x9 #x83 #x2c #x1a #x1b #x6e #x5a #xa0 #x52 #x3b #xd6 #xb3 #x29 #xe3 #x2f #x84 
    #x53 #xd1 #x0 #xed #x20 #xfc #xb1 #x5b #x6a #xcb #xbe #x39 #x4a #x4c #x58 #xcf 
    #xd0 #xef #xaa #xfb #x43 #x4d #x33 #x85 #x45 #xf9 #x2 #x7f #x50 #x3c #x9f #xa8 
    #x51 #xa3 #x40 #x8f #x92 #x9d #x38 #xf5 #xbc #xb6 #xda #x21 #x10 #xff #xf3 #xd2 
    #xcd #xc #x13 #xec #x5f #x97 #x44 #x17 #xc4 #xa7 #x7e #x3d #x64 #x5d #x19 #x73 
    #x60 #x81 #x4f #xdc #x22 #x2a #x90 #x88 #x46 #xee #xb8 #x14 #xde #x5e #xb #xdb 
    #xe0 #x32 #x3a #xa #x49 #x6 #x24 #x5c #xc2 #xd3 #xac #x62 #x91 #x95 #xe4 #x79 
    #xe7 #xc8 #x37 #x6d #x8d #xd5 #x4e #xa9 #x6c #x56 #xf4 #xea #x65 #x7a #xae #x8 
    #xba #x78 #x25 #x2e #x1c #xa6 #xb4 #xc6 #xe8 #xdd #x74 #x1f #x4b #xbd #x8b #x8a 
    #x70 #x3e #xb5 #x66 #x48 #x3 #xf6 #xe #x61 #x35 #x57 #xb9 #x86 #xc1 #x1d #x9e 
    #xe1 #xf8 #x98 #x11 #x69 #xd9 #x8e #x94 #x9b #x1e #x87 #xe9 #xce #x55 #x28 #xdf 
    #x8c #xa1 #x89 #xd #xbf #xe6 #x42 #x68 #x41 #x99 #x2d #xf #xb0 #x54 #xbb #x16)))


(define s-box-inv
  (u8-list->bytevector
   (list
    #x52 #x9 #x6a #xd5 #x30 #x36 #xa5 #x38 #xbf #x40 #xa3 #x9e #x81 #xf3 #xd7 #xfb 
    #x7c #xe3 #x39 #x82 #x9b #x2f #xff #x87 #x34 #x8e #x43 #x44 #xc4 #xde #xe9 #xcb 
    #x54 #x7b #x94 #x32 #xa6 #xc2 #x23 #x3d #xee #x4c #x95 #xb #x42 #xfa #xc3 #x4e 
    #x8 #x2e #xa1 #x66 #x28 #xd9 #x24 #xb2 #x76 #x5b #xa2 #x49 #x6d #x8b #xd1 #x25 
    #x72 #xf8 #xf6 #x64 #x86 #x68 #x98 #x16 #xd4 #xa4 #x5c #xcc #x5d #x65 #xb6 #x92 
    #x6c #x70 #x48 #x50 #xfd #xed #xb9 #xda #x5e #x15 #x46 #x57 #xa7 #x8d #x9d #x84 
    #x90 #xd8 #xab #x0 #x8c #xbc #xd3 #xa #xf7 #xe4 #x58 #x5 #xb8 #xb3 #x45 #x6 
    #xd0 #x2c #x1e #x8f #xca #x3f #xf #x2 #xc1 #xaf #xbd #x3 #x1 #x13 #x8a #x6b 
    #x3a #x91 #x11 #x41 #x4f #x67 #xdc #xea #x97 #xf2 #xcf #xce #xf0 #xb4 #xe6 #x73 
    #x96 #xac #x74 #x22 #xe7 #xad #x35 #x85 #xe2 #xf9 #x37 #xe8 #x1c #x75 #xdf #x6e 
    #x47 #xf1 #x1a #x71 #x1d #x29 #xc5 #x89 #x6f #xb7 #x62 #xe #xaa #x18 #xbe #x1b 
    #xfc #x56 #x3e #x4b #xc6 #xd2 #x79 #x20 #x9a #xdb #xc0 #xfe #x78 #xcd #x5a #xf4 
    #x1f #xdd #xa8 #x33 #x88 #x7 #xc7 #x31 #xb1 #x12 #x10 #x59 #x27 #x80 #xec #x5f 
    #x60 #x51 #x7f #xa9 #x19 #xb5 #x4a #xd #x2d #xe5 #x7a #x9f #x93 #xc9 #x9c #xef 
    #xa0 #xe0 #x3b #x4d #xae #x2a #xf5 #xb0 #xc8 #xeb #xbb #x3c #x83 #x53 #x99 #x61 
    #x17 #x2b #x4 #x7e #xba #x77 #xd6 #x26 #xe1 #x69 #x14 #x63 #x55 #x21 #xc #x7d)))
	 

(define rcon
  (u8-list->bytevector
   (list
    #x00 #x01 #x02 #x04 #x08 #x10 #x20 #x40 #x80 #x1B #x36)))


(define nb 4) ;; # nb - The number of columns comprising a state in AES. This is a constant
(define nk 4) ;; nk - The number of columns of the Cipher Key is equal to the key length * 8 divided by 32.
(define nr 10) ;; nr - The number of rounds is nk + 6 e.g 128 bit - 10; 196 - 12 e 256 - 14


(define (map-bytevector p bv)
  (u8-list->bytevector (map p (bytevector->u8-list bv))))


(define (rot-bytes word)
  (let ((tmp (bytevector->u8-list word)))
    (u8-list->bytevector (append (cdr tmp) (list (car tmp))))))
  

(define (sub-bytes state)
  (map-bytevector
   (lambda (x) (bytevector-u8-ref s-box x)) state))
  

(define (sub-bytes-inv state)
  (map-bytevector
   (lambda (x) (bytevector-u8-ref s-box-inv x)) state))


(define (key-expansion key)
  (let ((expanded-key (make-bytevector (* nb (+ nr 1) nb)))
	(tmp (make-bytevector nb))
	(expanded-length (* nb (+ nr 1))))
    (bytevector-copy! key 0 expanded-key 0 (bytevector-length key))
    (do ((i nk (+ i 1)))
	((>= i expanded-length) expanded-key)
      (bytevector-copy! expanded-key (* nb (- i 1)) tmp 0 nb)
      (cond ((= (remainder i nk) 0)
	     (bytevector-copy! (sub-bytes (rot-bytes tmp)) 0 tmp 0 nb)
	     (bytevector-u8-set! tmp 0 (logxor (bytevector-u8-ref tmp 0)
					       (bytevector-u8-ref rcon (quotient i nk)))))
	    ((= (remainder i nk) 4) (bytevector-copy! (sub-bytes tmp) 0 tmp 0 nb)))
      (let ((j (* i nb))
	    (k (* (- i nk) nb)))
	(bytevector-u8-set! expanded-key (+ j 0)
			    (logxor (bytevector-u8-ref expanded-key (+ k 0))
				    (bytevector-u8-ref tmp 0)))
	(bytevector-u8-set! expanded-key (+ j 1)
			    (logxor (bytevector-u8-ref expanded-key (+ k 1))
				    (bytevector-u8-ref tmp 1)))
	(bytevector-u8-set! expanded-key (+ j 2)
			    (logxor (bytevector-u8-ref expanded-key (+ k 2))
				    (bytevector-u8-ref tmp 2)))
	(bytevector-u8-set! expanded-key (+ j 3)
			    (logxor (bytevector-u8-ref expanded-key (+ k 3))
				    (bytevector-u8-ref tmp 3)))))))


(define (add-round-key state word)
  (u8-list->bytevector
   (map (lambda (x y) (logxor x y))
	(bytevector->u8-list state)
	(bytevector->u8-list word))))


(define (add-round-key-inv state word)
  (add-round-key state word))

		   
(define (shift-rows state)
  (define (shift state i)
    (do ((j i (+ j 1)))
	((>= j nb) state)
      (let ((tmp (bytevector-u8-ref state (+ (* nb 0) j))))
	(bytevector-u8-set! state (+ (* nb 0) j)
			    (bytevector-u8-ref state (+ (* nb 1) j)))
	(bytevector-u8-set! state (+ (* nb 1) j)
			    (bytevector-u8-ref state (+ (* nb 2) j)))
	(bytevector-u8-set! state (+ (* nb 2) j)
			    (bytevector-u8-ref state (+ (* nb 3) j)))
	(bytevector-u8-set! state (+ (* nb 3) j)
			    tmp))))
  (do ((i 1 (+ i 1)))
      ((>= i nb) state)
    (shift state i)))


(define (shift-rows-inv state)
  (define (shift state i)
    (do ((j i (+ j 1)))
	((>= j nb) state)
      (let ((tmp (bytevector-u8-ref state (+ (* nb 3) j))))
	(bytevector-u8-set! state (+ (* nb 3) j)
			    (bytevector-u8-ref state (+ (* nb 2) j)))
	(bytevector-u8-set! state (+ (* nb 2) j)
			    (bytevector-u8-ref state (+ (* nb 1) j)))
	(bytevector-u8-set! state (+ (* nb 1) j)
			    (bytevector-u8-ref state (+ (* nb 0) j)))
	(bytevector-u8-set! state (+ (* nb 0) j)
			    tmp))))
  (do ((i 1 (+ i 1)))
      ((>= i nb) state)
    (shift state i)))


(define (mix-columns state)
  (do ((i 0 (+ i 1)))
      ((>= i nb) state)
    (let ((a (bytevector-u8-ref state (+ (* i nb) 0)))
	  (b (bytevector-u8-ref state (+ (* i nb) 1)))
	  (c (bytevector-u8-ref state (+ (* i nb) 2)))
	  (d (bytevector-u8-ref state (+ (* i nb) 3))))
      (bytevector-u8-set! state (+ (* i nb) 0)
			  (logxor (gf2-mul #x02 a) (gf2-mul #x03 b) c d))
      (bytevector-u8-set! state (+ (* i nb) 1)
			  (logxor a (gf2-mul #x02 b) (gf2-mul #x03 c) d))
      (bytevector-u8-set! state (+ (* i nb) 2)
			  (logxor a b (gf2-mul #x02 c) (gf2-mul #x03 d)))
      (bytevector-u8-set! state (+ (* i nb) 3)
			  (logxor (gf2-mul #x03 a) b c (gf2-mul #x02 d))))))


(define (mix-columns-inv state)
  (do ((i 0 (+ i 1)))
      ((>= i nb) state)
    (let ((a (bytevector-u8-ref state (+ (* i nb) 0)))
	  (b (bytevector-u8-ref state (+ (* i nb) 1)))
	  (c (bytevector-u8-ref state (+ (* i nb) 2)))
	  (d (bytevector-u8-ref state (+ (* i nb) 3))))
      (bytevector-u8-set! state (+ (* i nb) 0)
			  (logxor (gf2-mul #x0E a)
				  (gf2-mul #x0B b)
				  (gf2-mul #x0D c)
				  (gf2-mul #x09 d)))
      (bytevector-u8-set! state (+ (* i nb) 1)
			  (logxor (gf2-mul #x09 a)
				  (gf2-mul #x0E b)
				  (gf2-mul #x0B c)
				  (gf2-mul #x0D d)))
      (bytevector-u8-set! state (+ (* i nb) 2)
			  (logxor (gf2-mul #x0D a)
				  (gf2-mul #x09 b)
				  (gf2-mul #x0E c)
				  (gf2-mul #x0B d)))
      (bytevector-u8-set! state (+ (* i nb) 3)
			  (logxor (gf2-mul #x0B a)
				  (gf2-mul #x0D b)
				  (gf2-mul #x09 c)
				  (gf2-mul #x0E d))))))


(define (gf2-mul a b)
  (define irreductible #x11B)
  (define mask #xFF)
  (define (update-p p a b)
    (if (= (logand b #x01) 1)
	(logxor p a)
	p))
  (define (update-a a)
    (if (not (= (logand a #x80) #x00))
	(logand mask (logxor (ash a #x01) irreductible))
	(logand mask (ash a #x01))))
  (define (loop a b p)
    (if (or (= a 0) (= b 0))
	p
	(loop (update-a a) (ash b -1) (update-p p a b))))
  (loop a b 0))
	    

(define (AES-Encrypt plain-text-bv key)
  (let ((state (add-round-key plain-text-bv key))
	(expanded-key (key-expansion key))
	(sub-key (make-bytevector (* nb nb)))
	(nb-sq (* nb nb)))
    (do ((i 0 (+ i 1)))
	((>= i (- nr 1))
	 (bytevector-copy! expanded-key (* nr nb-sq) sub-key 0 nb-sq)
	 (add-round-key (shift-rows (sub-bytes state)) sub-key))
      (bytevector-copy! expanded-key (* (+ i 1) nb-sq) sub-key 0 nb-sq)
      (let ((new-state (mix-columns
			(shift-rows
			 (sub-bytes state)))))
	(bytevector-copy! (add-round-key new-state sub-key) 0 state 0 nb-sq)))))
	

(define (AES-Decrypt encrypted-text key)
  (let ((state encrypted-text)
	(expanded-key (key-expansion key))
	(sub-key (make-bytevector (* nb nb)))
	(nb-sq (* nb nb)))
    (bytevector-copy! expanded-key (* nr nb-sq) sub-key 0 nb-sq)
    (let ((state (sub-bytes-inv
		  (shift-rows-inv
		   (add-round-key-inv state sub-key)))))
      (do ((i (- nr 1) (- i 1)))
	  ((<= i 0)(add-round-key-inv state key))
	(bytevector-copy! expanded-key (* i nb-sq) sub-key 0 nb-sq)
	(let ((new-state (sub-bytes-inv
			  (shift-rows-inv
			   (mix-columns-inv
			    (add-round-key-inv state sub-key))))))
	  (bytevector-copy! new-state 0 state 0 nb-sq))))))

 
(define (AES-ecb-encrypt plain-text-bv key)
  (let* ((nb-sq (* nb nb))
	 (text-length (bytevector-length plain-text-bv))
	 (encrypted (make-bytevector text-length))
	 (block (make-bytevector nb-sq)))
    (do ((i 0 (+ i nb-sq)))
	((>= i text-length) encrypted)
      (bytevector-copy! plain-text-bv i block 0 nb-sq)
      (bytevector-copy! (AES-Encrypt block key) 0 encrypted i nb-sq))))
    

(define (AES-ecb-decrypt encrypted-text key)
  (let* ((nb-sq (* nb nb))
	 (text-length (bytevector-length encrypted-text))
	 (decrypted (make-bytevector text-length))
	 (block (make-bytevector nb-sq)))
    (do ((i 0 (+ i nb-sq)))
	((>= i text-length) decrypted)
      (bytevector-copy! encrypted-text i block 0 nb-sq)
      (bytevector-copy! (AES-Decrypt block key) 0 decrypted i nb-sq))))


(define encrypted-text
  (string-filter
   (call-with-input-file "../text_files/7.txt" get-string-all)
   (lambda (ch) (not (char=? ch #\newline)))))

(define decoded-encrypted-text
  (decode-base64 encrypted-text))

(define key (u8-list->bytevector
	     (map char->integer
		  (string->list "YELLOW SUBMARINE"))))

(define decrypted-text (AES-ecb-decrypt decoded-encrypted-text key))

(display "Decrypted text:")
(newline)
(newline)
(display (list->string (map integer->char (bytevector->u8-list decrypted-text))))
