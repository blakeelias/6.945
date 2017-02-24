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


; 1.3 Optimization 

(define (r:quote string)
   (list->string
    (append-map (lambda (char)
                  (if (memv char chars-needing-quoting)
                      (list #\\ char)
                      (list char)))
                (string->list string))))

; test
(display (r:seq (r:quote "a") (r:dot) (r:quote "c")))
; displays:  \(a.c\)
;Unspecified return value

(define parenthesize r:seq)

(define (parenthesized? expr)
  (and (string=? "\\(" (string-head expr 2))
       (string=? "\\)" (string-tail expr (- (string-length expr) 2)))))

(define (bracketed? expr)
  (and (string=? "\[" (string-head expr 1))
       (string=? "\]" (string-tail expr (- (string-length expr) 1)))))

(define (parenthesize-if-needed expr)
  (if (or
       (= (string-length expr) 1)
       (parenthesized? expr)
       (bracketed? expr))
      expr
      (parenthesize expr)))

; test

(parenthesize-if-needed "abc")
;Value 74: "\\(abc\\)"

(parenthesize-if-needed "\\(abc\\)")
;Value 75: "\\(abc\\)"

(parenthesize-if-needed "a")
;Value 99: "a"

(define (r:repeat min max expr)
  (string-append
   (parenthesize-if-needed expr)
   "\\\{"
   (number->string min)
   ","
   (if max
       (number->string max)
       "")
   "\\\}"))

; tests

(r:seq (r:quote "a") (r:dot) (r:quote "c") 
       (r:seq (r:* (r:quote "x"))
	      (r:repeat 1 5 (r:char-from "def"))))
;Value 113: "\\(a.c\\(x\\{0,\\}[def]\\{1,5\\}\\)\\)"


(r:seq (r:quote "a") (r:dot) (r:quote "c") 
       (r:seq (r:* (r:quote "xy"))
	      (r:repeat 1 5 (r:char-from "def"))))
;Value 114: "\\(a.c\\(\\(xy\\)\\{0,\\}[def]\\{1,5\\}\\)\\)"

#|
 Considerations / subtle cases for this:
  1) There should always be a way for the user to add parens
     when they'd like. This is accomplished through r:seq
     always adding parens.

  2) Quoted things (made by r:quote) don't need to be parenthesized
     at first; not until and unless we're doing something with them later
     (see below, (3)).

  3) r:repeat (as well as r:+ and r:*) don't need to add parens
     around that thing which they are repeating if that thing
     is either a single character, or already has parens or brackets
     around it. Otherwise, they do need to add parens around it.

|#

; Problem 1.4: Back-references

(define (r:back-ref n)
  (string-append "\\" (number->string n)))


#|
(define (r:seq . exprs)
  (cons 'r:seq exprs))

(define (r:alt . exprs)
  (cons 'r:alt exprs))

(define (r:repeat min max expr)
  (list 'r:repeat min max expr))

(define (r:quote string)
  (list 'r:quote string))

(define (r:dot) 'r:dot)
(define (r:bol) 'r:bol)
(define (r:eol) 'r:eol)

(define (r:char-from string)
  (case (string-length string)
    ((0) (r:seq))
    ((1) (r:quote string))
    (else
     (list 'r:char-from string))))

(define (r:char-not-from string)
  (list 'r:char-not-from string))
|#

