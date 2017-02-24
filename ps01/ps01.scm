(load "~/Dropbox (MIT)/Classes/6.945/ps01/regexp.scm")

;;; Problem 1.1

(define (r:* expr)
  (r:repeat 0 #f expr))

(define (r:+ expr)
  (r:repeat 1 #f expr))

;;; Tests for Problem 1.1 (test-1.1.txt is attached)

(r:grep 
 (r:seq
  (r:bol)
  (r:* (r:quote "abc"))
  (r:eol))
 "test-1.1.txt")
;Value 25: ("abcabcabc" "abc" "")

(r:grep
 (r:seq
  (r:bol)
  (r:+ (r:quote "abc"))
  (r:eol))
 "test-1.1.txt")
;Value 26: ("abcabcabc" "abc")

;;; Problem 1.2

; (a) This results in an infinite recursion.
; (b) The sizes of the generated regular expressions will be smaller, as (r:repeat 3 7 <x>) would compile to (xxx(x)?(x)?(x)?(x)?) instead of (xxx|xxxx|xxxxx|xxxxxx|xxxxxxx). The code would also be easier to implement since Bonnie is proposing a one-line replacement, whereas Alyssa's program would probably have to contain a loop to create all those alternate possibilities.
; (c) Ben's proposal only uses properties of Basic Regular Expressions, not Extended Regular Expressions. This may make the implementation more robust / portable. The size of the compiled expressions will be smaller. The compiled expressions will have a closer mapping to the Scheme functions they came from, making the compiled expressions easier to read. The code to implement this should also be shorter and easier to understand.
; (d)

(define (r:repeat min max expr)
  (apply r:seq
	 (list
	  expr
	  "\\\{"
	  (number->string min)
	  ","
	  (if max
	      (number->string max)
	      "")
	  "\\\}")))

; Tests


(r:grep
 (r:seq
  (r:bol)
  (r:repeat 1 2 (r:quote "abc"))
  (r:eol))
 "test-1.1.txt")
;Value 38: ("abc")

(r:grep
 (r:seq
  (r:bol)
  (r:repeat 0 2 (r:quote "abc"))
  (r:eol))
 "test-1.1.txt")
;Value 39: ("abc" "")

(r:grep
 (r:seq
  (r:bol)
  (r:repeat 1 #f (r:quote "abc"))
  (r:eol))
 "test-1.1.txt")
;Value 40: ("abcabcabc" "abc")


