;;;; File:  load.scm -- Loader for pattern matching system

;;; Pattern matcher:

(load "ghelper")
(load "matcher")


;;; Term rewriting / pattern-directed invocation system:

;;; A stub put in place in case you want to
;;; play with memoization in the term
;;; rewriter
(define (memoize-simplifier f) f) 

(load "utils")
(load "rule-implementation")
(load "rules")

(load "pattern-operator")
