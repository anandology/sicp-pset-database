;;; Copyright (c) 1990 Massachusetts Institute of Technology
;;; 
;;; This material was developed by the Scheme project at the Massachusetts
;;; Institute of Technology, Department of Electrical Engineering and
;;; Computer Science.  Permission to copy this material, to redistribute
;;; it, and to use it for any non-commercial purpose is granted, subject
;;; to the following restrictions and understandings.
;;; 
;;; 1. Any copy made of this material must include this copyright notice
;;; in full.
;;; 
;;; 2. Users of this material agree to make their best efforts (a) to
;;; return to the MIT Scheme project any improvements or extensions that
;;; they make, so that these may be included in future releases; and (b)
;;; to inform MIT of noteworthy uses of this material.
;;; 
;;; 3. All materials developed as a consequence of the use of this
;;; material shall duly acknowledge such use, in accordance with the usual
;;; standards of acknowledging credit in academic research.
;;; 
;;; 4. MIT has made no warrantee or representation that this material
;;; (including the operation of software contained therein) will be
;;; error-free, and MIT is under no obligation to provide any services, by
;;; way of maintenance, update, or otherwise.
;;; 
;;; 5. In conjunction with products arising from the use of this material,
;;; there shall be no use of the name of the Massachusetts Institute of
;;; Technology nor of any adaptation thereof in any advertising,
;;; promotional, or sales literature without prior written consent from
;;; MIT in each case. 

;;;;  6.001 Register Machine Simulator

;;; Syntactic sugar for DEFINE-MACHINE
;;;   Magic syntax hack... DO NOT expect to understand this. 
;;;   Hal doesn't and he wrote it!
 (enable-language-features)
 (define-macro (define-machine name registers controller)
    `(define ,name
       (build-model ',(cdr registers) ',(cdr controller))))
 (disable-language-features)
 
;;;model building

(define (build-model registers controller)
  (let ((machine (make-new-machine)))
    (set-up-registers machine registers)
    (set-up-controller machine controller)
    machine))

(define (set-up-registers machine registers)
  (mapc (lambda (register-name)
          (make-machine-register machine register-name))
        registers))

(define (mapc proc l)
  (if (null? l)
      'done
      (sequence (proc (car l))
                (mapc proc (cdr l)))))

(define (set-up-controller machine controller)
  (build-instruction-list machine (cons '*start* controller)))

(define (build-instruction-list machine op-list)
  (if (null? op-list)
      '()
      (let ((rest-of-instructions
             (build-instruction-list machine (cdr op-list))))
        (if (label? (car op-list))
            (sequence
             (declare-label machine
                             (car op-list)
                             rest-of-instructions)
             rest-of-instructions)
            (cons (make-machine-instruction machine
                                            (car op-list))
                  rest-of-instructions)))))

(define (label? expression)
  (symbol? expression))

(define (make-machine-register machine name)
  (remote-define machine name (make-register name)))

;;;register model

(define (make-register name)
  (define contents nil)
  (define (get) contents)
  (define (set value)
    (set! contents value))
  (define (dispatch message)
    (cond ((eq? message 'get) (get))
          ((eq? message 'set) set)
          (else (error "Unknown request -- REGISTER"
                       name
                       message))))
  dispatch)

(define (get-contents register)
  (register 'get))

(define (set-contents register value)
  ((register 'set) value))

(define (declare-label machine label labeled-entry)
  (let ((defined-labels (remote-get machine '*labels*)))
    (if (memq label defined-labels)
        (error "Multiply-defined label" label)
        (sequence
         (remote-define machine label labeled-entry)
         (remote-set machine
                      '*labels*
                      (cons label defined-labels))))))


;;; stack model -- monitored stack

(define (make-stack)
  (define s '())
  (define number-pushes 0)
  (define max-depth 0)
  (define (push x)
    (set! s (cons x s))
    (set! number-pushes (1+ number-pushes))
    (set! max-depth (max (length s) max-depth)))
  (define (pop)
    (if (null? s)
        (error "Empty stack -- POP")
        (let ((top (car s)))
          (set! s (cdr s))
          top)))
  (define (initialize)
    (set! s '())
    (set! number-pushes 0)
    (set! max-depth 0))
  (define (print-statistics)
    (print (list 'total-pushes: number-pushes
                 'maximum-depth: max-depth)))
  (define (dispatch message)
    (cond ((eq? message 'push) push)
          ((eq? message 'pop) (pop))
          ((eq? message 'initialize) (initialize))
          ((eq? message 'print-statistics) (print-statistics))
          (else (error "Unknown request -- STACK" message))))
  dispatch)


(define (pop stack)
  (stack 'pop))

(define (push stack value)
  ((stack 'push) value))

;;;name-value association

(define (remote-get machine variable)
  (eval variable machine))

(define (remote-set machine variable value)
  (eval (list 'set! variable (list 'quote value))
        machine))

(define (remote-define machine variable value)
  (eval (list 'define variable (list 'quote value))
        machine))

(define (make-machine-instruction machine op)
  (eval (list 'lambda '() op) machine))


;;; Monitored stack machine maker.

(define (make-new-machine)
  (make-environment
   (define *labels* '())
   (define *the-stack* (make-stack))
   (define (initialize-stack)
     (*the-stack* 'print-statistics)
     (*the-stack* 'initialize))
   (define fetch get-contents)

   (define *program-counter* '())
   (define (execute sequence)
     (set! *program-counter* sequence)
     (if (null? *program-counter*)
	 'done
	 ((car *program-counter*))))
   (define (normal-next-instruction)
     (execute (cdr *program-counter*)))

   (define (assign register value)
     (set-contents register value)
     (normal-next-instruction))

   (define (save reg)
     (push *the-stack* (get-contents reg))
     (normal-next-instruction))

   (define (restore reg)
     (set-contents reg (pop *the-stack*))
     (normal-next-instruction))

   (define (goto new-sequence)
     (execute new-sequence))

   (define (branch predicate alternate-next)
     (if predicate
	 (goto alternate-next)
	 (normal-next-instruction)))

   (define (perform operation)
     (normal-next-instruction))

   ;; end of make-new-machine
   ))


;;;rest of simulator interface

(define (remote-fetch machine register-name)
  (get-contents (remote-get machine register-name)))

(define (remote-assign machine register-name value)
  (set-contents (remote-get machine register-name) value)
  'done)

(define (start machine)
  (eval '(goto *start*) machine))
