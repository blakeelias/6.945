;;; Load up extended Scheme interpreter

;;; The next three expressions build an environment structure for safely
;;;  experimenting with generic operations.
(set! user-initial-environment (make-top-level-environment))
(environment-define user-initial-environment 
                    'generic-evaluation-environment
                    (extend-top-level-environment user-initial-environment))
(define generic-evaluation-environment 
  (access generic-evaluation-environment user-initial-environment))

(load "utils" user-initial-environment)
(load "ghelper" user-initial-environment)
(load "syntax" user-initial-environment)
(load "rtdata" user-initial-environment)

(load "interp" generic-evaluation-environment)
(load "repl" generic-evaluation-environment)

;;; This allows nonstrict definitions.
;;; (load "general-procedures" generic-evaluation-environment)
;;; (load "kons" generic-evaluation-environment)

(ge generic-evaluation-environment)

