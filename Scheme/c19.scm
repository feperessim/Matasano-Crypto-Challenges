;; Trying to guess the correct keystream by hand is tough. To say
;; that I haven't tried anything, I did a small program in python
;; (see the c19.py file) where the user input his guess and the program
;; performs a xor agains a single char per time and shows the result
;; on the screen. There is something similar in [1] but very fashion and with
;; a rich ui which does something similar but performs the xor operation
;; in all those given strings.
;; [1] https://fattybeagle.com/2017/01/03/cryptopals-challenge-19/
