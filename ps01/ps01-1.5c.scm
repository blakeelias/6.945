(load "~/Dropbox (MIT)/Classes/6.945/ps01/regexp.scm")

; Problem 1.5 (c)

(define (r:dot standard) ".")
(define (r:bol standard) "^")
(define (r:eol standard) "$")

; Quotes a character if needed, according to the standard (BRE or ERE)
(define (quote-char standard char)
  (case char
    ((#\. #\[ #\] #\* #\^) (list #\\ char))
    ((#\$) (list #\[ #\$ #\]))
    ((#\( #\) #\? #\+ #\{ #\})
     (if (eq? standard 'bre)
	 char
	 (list #\\ char)))
    (else (list char))))

(define (r:quote standard string)
   (list->string
       (append-map (lambda (char)
                     (quote-char standard char))
                (string->list string))))

(define (r:char-from standard string)
  (case (string-length string)
    ((0) (r:seq standard))
    ((1) (r:quote standard string))
    (else
     (bracket string
              (lambda (members)
                (if (lset= eqv? '(#\- #\^) members)
                    '(#\- #\^)
                    (quote-bracketed-contents members)))))))

(define (r:char-not-from standard string)
  (bracket string
           (lambda (members)
             (cons #\^ (quote-bracketed-contents members)))))

(define (bracket string procedure)
  (list->string
   (append '(#\[)
           (procedure (string->list string))
           '(#\]))))

(define (quote-bracketed-contents members)
  (let ((optional
         (lambda (char) (if (memv char members) (list char) '()))))
    (append (optional #\])
            (remove (lambda (c)
		      (memv c chars-needing-quoting-in-brackets))
		    members)
            (optional #\^)
            (optional #\-))))


;;; Means of combination for patterns

(define (make-special standard char)
	(list->string
		(case char
		((#\( #\) #\{ #\} #\|) (case standard
								('bre (list #\\ char))
								('ere (list char))))
		(else (list char)))))

(define (r:seq . (standard . exprs))
  (string-append 
   (make-special standard #\()
   (apply string-append exprs)
   (make-special standard #\))))

(define parenthesize r:seq)

;;; An extension to POSIX basic regular expressions.
;;; Supported by GNU grep and possibly others.
(define (r:alt . (standard . exprs))
  (if (pair? exprs)
      (apply r:seq
             (cons (car exprs)
                   (append-map (lambda (expr)
                                 (list (make-special standard #\|)
									   expr))
                               (cdr exprs))))
      (r:seq)))

(define (parenthesized? standard expr)
  (case standard
    ('ere
     (and (string=? "(" (string-head expr 1))
	      (string=? ")" (string-tail expr (- (string-length expr) 1)))))
    ('bre
     (and (string=? "\\(" (string-head expr 2))
	      (string=? "\\)" (string-tail expr (- (string-length expr) 2)))))))

(define (bracketed? standard expr)
	(and (string=? "[" (string-head expr 1))
	      (string=? "]" (string-tail expr (- (string-length expr) 1)))))

(define (parenthesize-if-needed standard expr)
  (if (or
       (= (string-length expr) 1)
       (parenthesized? standard expr)
       (bracketed? standard expr))
      expr
      (parenthesize standard expr)))

(define (r:repeat standard min max expr)
	(string-append
	 (parenthesize-if-needed standard expr)
	 (make-special standard #\{)
	 (number->string min)
	 ","
	 (if max
	     (number->string max)
	     "")
	 (make-special standard #\})))

(define (r:* standard expr)
   (r:repeat standard 0 #f expr))

(define (r:+ standard expr)
   (r:repeat standard 1 #f expr))

(define (r:back-ref standard n)
  (string-append "\\" (number->string n)))

; Test
(define bre 'bre)
(define ere 'ere)
(define std bre)

(r:seq std
       (r:quote std "a") (r:dot std) (r:quote std "c") 
       (r:seq std (r:* std (r:quote std "xy")))
       (r:back-ref std 1))
	   
; this appears to return correct regular expressions. Just annoying that you have to keep passing in `std'

(define (add-args code std)
	(cond
		((eq? code '()) '())
		((list? code)
			(if (memv (car code) '(r:+ r:* r:repeat r:char-not-from r:char-from r:quote r:seq r:alt r:back-ref r:dot r:bol r:eol))
			  	(append (list (car code) std)
				      	(map (lambda (expr) (add-args expr std))
						   	 (cdr code)))
				(append (cons (add-args (car code) std)
		 				      (map (lambda (expr) (add-args expr std))
		 						   (cdr code))))))
	    (else code)))

(define code
	'(r:seq 
	          (r:quote  "a") (r:dot ) (r:quote  "c") 
	          (r:seq  (r:*  (r:quote  "xy")))
	          (r:back-ref  1)))


(eval (add-args code bre) (the-environment))
;Value 22: "\\(a.c\\(\\(xy\\)\\{0,\\}\\)\\1\\)"

(eval (add-args code ere) (the-environment))
;Value 23: "(a.c((xy){0,})\\1)"

