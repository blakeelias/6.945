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


