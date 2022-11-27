;;; This is a procedure that uses Duck's and Mres's tournament
;;; code to run a tournament.

(define *games-played*)

(define (print-tournament-results name/strategy-list)
  (for-each (lambda (result)
	      (newline)
	      (princ (first (list-ref name/strategy-list (first result) )))
	      (princ ": ")
	      (princ (second result)))
	    *FINAL-STANDINGS*))

(define (print-entries)
  (define (enumerate-from-to low high)
    (if (> low high)
	'()
	(cons low (enumerate-from-to (1+ low) high))))
  (for-each (lambda (proc num)
	      (print (list num proc)))
	    *PROCEDURE-ENTRIES*
	    (enumerate-from-to 0 (-1+ (length *PROCEDURE-ENTRIES*)))))

(define (run-tournament name/strategy-list)
  (set! *TESTED-ENTRIES* '())
  (set! *UNTESTED-ENTRIES* '())
  (set! *SCORES-VECTOR* '())
  (set! *FINAL-STANDINGS* '())
  (set! *TOURNAMENT-SCHEDULE* '())
  (set! *GAMES-PLAYED-SO-FAR* '())
  (set! *REMAINING-GAMES* '())
  (for-each test-and-add-new-entry (mapcar second name/strategy-list))
  (set-up-complete-tournament)
  (set! *PROCEDURE-ENTRIES* *TESTED-ENTRIES*)
  (set! *PROCEDURE-VECTOR* '())
  (initialize-tournament-variables)
  (place-entries-in-vector)
  (print-entries)
  (play-tournament-games 0)
  (set! *games-played* *games-played-so-far*)
  (initialize-post-mortem (length name/strategy-list))
  (calculate-scores-2)
  (get-total-scores)
  (newline)
  (newline)

  (print-tournament-results (reverse name/strategy-list))
  )