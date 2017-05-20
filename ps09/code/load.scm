(set! (access user-initial-environment system-global-environment)
      (extend-top-level-environment system-global-environment))

(environment-define system-global-environment
		    'generic-evaluation-environment
		    (extend-top-level-environment user-initial-environment))

(load "/Users/blake/Dropbox (MIT)/Classes/6.945/ps09/code/utils" user-initial-environment)
(load "/Users/blake/Dropbox (MIT)/Classes/6.945/ps09/code/time-share" user-initial-environment)
(load "/Users/blake/Dropbox (MIT)/Classes/6.945/ps09/code/schedule" user-initial-environment)

(load "/Users/blake/Dropbox (MIT)/Classes/6.945/ps09/code/ghelper" user-initial-environment)
(load "/Users/blake/Dropbox (MIT)/Classes/6.945/ps09/code/syntax" user-initial-environment)
(load "/Users/blake/Dropbox (MIT)/Classes/6.945/ps09/code/rtdata" user-initial-environment)

(load "/Users/blake/Dropbox (MIT)/Classes/6.945/ps09/code/interp-actor" generic-evaluation-environment)
(load "/Users/blake/Dropbox (MIT)/Classes/6.945/ps09/code/repl" generic-evaluation-environment)

;; 

(ge generic-evaluation-environment)