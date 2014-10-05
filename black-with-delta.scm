(define scheme-apply apply)
;
; Eval functions
;
(define (base-eval exp env cont)
  (cond ((number? exp)		 (meta-apply cont exp))
	((boolean? exp)		 (meta-apply cont exp))
	((string? exp)		 (meta-apply cont exp))
	((symbol? exp)		 (meta-apply 'eval-var exp env cont))
	((eq? (car exp) 'quote)  (meta-apply 'eval-quote exp env cont))
	((eq? (car exp) 'if)	 (meta-apply 'eval-if exp env cont))
	((eq? (car exp) 'cond)	 (meta-apply 'eval-cond (cdr exp) env cont))
	((eq? (car exp) 'define) (meta-apply 'eval-define exp env cont))
	((eq? (car exp) 'set!)	 (meta-apply 'eval-set! exp env cont))
	((eq? (car exp) 'lambda) (meta-apply 'eval-lambda exp env cont))
	((eq? (car exp) 'delta)  (meta-apply 'eval-delta exp env cont))
	((eq? (car exp) 'begin)  (meta-apply 'eval-begin (cdr exp) env cont))
	((eq? (car exp) 'let)	 (meta-apply 'eval-let
				   (car (cdr exp)) (cdr (cdr exp)) env cont))
	((eq? (car exp) 'let*)	 (meta-apply 'eval-let*
				   (car (cdr exp)) (cdr (cdr exp)) env cont))
	((eq? (car exp) 'letrec) (meta-apply 'eval-letrec
				   (car (cdr exp)) (cdr (cdr exp)) env cont))
	((eq? (car exp) 'EM)	 (meta-apply 'eval-EM exp env cont))
	((eq? (car exp) 'exec-at-metalevel)
				 (meta-apply 'eval-EM exp env cont))
	((eq? (car exp) 'exit)	 (meta-apply 'eval-exit exp env cont))
	((eq? (car exp) 'load)	 (meta-apply 'eval-load exp env cont))
	((eq? (car exp) 'and)	 (meta-apply 'eval-and (cdr exp) env cont))
	((eq? (car exp) 'or)	 (meta-apply 'eval-or (cdr exp) env cont))
	(else (meta-apply 'eval-application exp env cont))))
(define (eval-application exp env cont)
  (meta-apply 'base-eval (car exp) env
	      (lambda (operator)
		 (if (and (pair? operator) (eq? (car operator) delta-tag))
		     (meta-apply 'apply-delta operator (cdr exp) env cont)
		     (meta-apply 'eval-list (cdr exp) env
				 (lambda (operand)
				   (meta-apply 'base-apply
					       operator operand env cont)))))))
(define (eval-var exp env cont)
  (let ((pair (get exp env)))
    (if (pair? pair)
	(meta-apply cont (cdr pair))
	(meta-apply 'my-error
		    (list 'eval-var: 'unbound 'variable: exp) env cont))))
(define (eval-quote exp env cont) (meta-apply cont (car (cdr exp))))
(define (eval-if exp env cont)
  (let ((pred-part (car (cdr exp)))
	(then-part (car (cdr (cdr exp))))
	(else-part (cdr (cdr (cdr exp)))))
     (meta-apply 'base-eval pred-part env (lambda (p)
		 (cond (p (meta-apply 'base-eval then-part env cont))
		       ((null? else-part) (meta-apply cont #f))
		       (else
			(meta-apply 'base-eval (car else-part) env cont)))))))
(define (eval-cond clauses env cont)
  (cond ((null? clauses) (meta-apply cont '()))
	((eq? (car (car clauses)) 'else)
	 (meta-apply 'eval-begin (cdr (car clauses)) env cont))
	(else
	 (meta-apply 'base-eval
		     (car (car clauses))
		     env
		     (lambda (pred)
			(if pred
			    (meta-apply 'eval-begin (cdr (car clauses))
					env cont)
			    (meta-apply 'eval-cond (cdr clauses)
					env cont)))))))
(define (eval-define exp env cont)
  (if (pair? (car (cdr exp)))
      (let ((var (car (car (cdr exp))))
	    (body (cons 'lambda
			(cons (cdr (car (cdr exp)))
			      (cdr (cdr exp))))))
	 (meta-apply 'base-eval body env
		     (lambda (data)
			(define-value var data env)
			(meta-apply cont var))))
      (let ((var (car (cdr exp)))
	    (body (car (cdr (cdr exp)))))
	 (meta-apply 'base-eval body env
		     (lambda (data)
			(define-value var data env)
			(meta-apply cont var))))))
(define (eval-set! exp env cont)
  (let ((var (car (cdr exp)))
	(body (car (cdr (cdr exp)))))
    (meta-apply 'base-eval body env
		(lambda (data)
		   (let ((pair (get var env)))
		      (if (pair? pair)
			  (begin (set-value! var data env)
				 (meta-apply cont var))
			  (meta-apply 'my-error
				      (list 'eval-set!: 'unbound 'variable var)
				      env cont)))))))
(define lambda-tag (cons 'lambda 'tag))
(define (eval-lambda exp env cont)
  (let ((lambda-body (cdr (cdr exp)))
	(lambda-params (car (cdr exp))))
     (meta-apply cont (list lambda-tag lambda-params lambda-body env))))
(define delta-tag (cons 'delta 'tag))
(define (eval-delta exp env cont)
  (let ((delta-body (cdr (cdr exp)))
	(delta-params (car (cdr exp))))
     (meta-apply cont (list delta-tag delta-params delta-body))))
(define (eval-begin body env cont)
  (define (eval-begin-local body)
     (if (null? (cdr body))
	 (meta-apply 'base-eval (car body) env cont)
	 (meta-apply 'base-eval (car body) env
		     (lambda (x) (eval-begin-local (cdr body))))))
  (if (null? body)
      (meta-apply 'my-error '(eval-begin: null body) env cont)
      (eval-begin-local body)))
(define (eval-let pairs body env cont)
  (let ((params (map car pairs))
	(args (map (lambda (x) (car (cdr x))) pairs)))
     (meta-apply 'eval-list args env
		 (lambda (operand)
		    (meta-apply 'eval-begin body
				(extend env params operand)
				cont)))))
(define (eval-let* pairs body env cont)
  (if (null? pairs)
      (meta-apply 'eval-begin body env cont)
      (meta-apply 'base-eval (car (cdr (car pairs))) env (lambda (arg)
		  (meta-apply 'eval-let* (cdr pairs) body
			      (extend env (list (car (car pairs))) (list arg))
			      cont)))))
(define (eval-letrec pairs body env cont)
  (define (set-value-list! params operand env)
     (if (null? params)
	 #f
	 (begin (set-value! (car params) (car operand) env)
		(set-value-list! (cdr params) (cdr operand) env))))
  (let ((params (map car pairs))
	(args (map (lambda (x) (car (cdr x))) pairs)))
   (let ((letrec-env (extend env params params)))
     (meta-apply 'eval-list args letrec-env
		 (lambda (operand)
		    (set-value-list! params operand letrec-env)
		    (meta-apply 'eval-begin body letrec-env cont))))))
(define (eval-EM exp env cont)
  (lambda (Mcont)
     (let ((meta-env (car (head Mcont)))
	   (meta-cont (car (cdr (head Mcont))))
	   (meta-Mcont (tail Mcont)))
	((meta-apply 'base-eval
		     (car (cdr exp))
		     meta-env
		     (lambda (ans) (lambda (Mcont2)
			((meta-apply cont ans)
			 (cons-stream (head Mcont) Mcont2)))))
	 meta-Mcont))))
(define (eval-exit exp env cont)
  (meta-apply 'base-eval (car (cdr exp)) env
	     (lambda (x) (meta-apply 'my-error x env cont))))
(define (eval-list exp env cont)
  (if (null? exp)
      (meta-apply cont '())
      (meta-apply 'base-eval (car exp) env
		  (lambda (val1)
		     (meta-apply 'eval-list (cdr exp) env
				 (lambda (val2)
				    (meta-apply cont (cons val1 val2))))))))
(define (base-apply operator operand env cont)
  (cond ((procedure? operator)
	 (cond ((eq? operator map)
		(meta-apply 'eval-map
			    (car operand) (car (cdr operand)) env cont))
	       ((eq? operator scheme-apply)
		(meta-apply 'base-apply
			    (car operand) (car (cdr operand)) env cont))
	       ((pair? (member operator primitive-procedures))
		(meta-apply cont (scheme-apply operator operand)))
	       (else ; called when going down a level.
		(lambda (Mcont)
		   ((scheme-apply operator operand)
		    (cons-stream (list (get-global-env env) cont) Mcont))))))
	((and (pair? operator)
	      (eq? (car operator) lambda-tag))
	 (let ((lambda-params        (car (cdr operator)))
	       (lambda-body     (car (cdr (cdr operator))))
	       (lambda-env (car (cdr (cdr (cdr operator))))))
	    (if (can-receive? lambda-params operand)
		(meta-apply 'eval-begin
			    lambda-body
			    (extend lambda-env lambda-params operand)
			    cont)
		(meta-apply 'my-error
			    (list 'base-apply: 'Wrong 'number 'of 'arguments:
				  operand 'to: lambda-params)
			    env cont))))
	(else
	 (meta-apply 'my-error (list 'Not 'a 'function: operator) env cont))))
(define (apply-delta operator operand env cont)
  (lambda (Mcont)
     (let ((meta-env (car (head Mcont)))
	   (meta-cont (car (cdr (head Mcont))))
	   (meta-Mcont (tail Mcont)))
	(let ((delta-params    (car (cdr operator)))
	      (delta-body (car (cdr (cdr operator)))))
	   ((meta-apply 'eval-begin
			delta-body
			(extend meta-env delta-params (list operand env cont))
			meta-cont)
	    meta-Mcont)))))
(define old-env 0)
(define old-cont 0)
(define (my-error exp env cont)
  (lambda (Mcont)
     (let ((meta-env (car (head Mcont)))
	   (meta-cont (car (cdr (head Mcont))))
	   (meta-Mcont (tail Mcont)))
	(set-value! 'old-env env meta-env)
	(set-value! 'old-cont cont meta-env)
	((meta-apply meta-cont exp) meta-Mcont))))
(define (eval-load exp env cont)
  (define port (open-input-file (car (cdr exp))))
  (define (load-local)
     (let ((input (read port)))
	(if (eof-object? input)
	    (begin (close-input-port port)
		   (meta-apply cont 'done))
	    (meta-apply 'base-eval input env
			(lambda (value) (load-local))))))
  (load-local))
(define (eval-and body env cont)
  (cond ((null? body) (meta-apply cont #t))
	((null? (cdr body))
	 (meta-apply 'base-eval (car body) env cont))
	(else
	 (meta-apply 'base-eval (car body) env
		    (lambda (result)
			(if result
			    (meta-apply 'eval-and (cdr body) env cont)
			    (meta-apply cont result)))))))
(define (eval-or body env cont)
  (if (null? body)
      (meta-apply cont #f)
      (meta-apply 'base-eval (car body) env
		  (lambda (result)
		     (if result
			 (meta-apply cont result)
			 (meta-apply 'eval-or (cdr body) env cont))))))
;
; Primitives
;
(define (eval-map fun lst env cont)
  (if (null? lst)
      (meta-apply cont '())
      (meta-apply 'base-apply fun (list (car lst)) env
		  (lambda (x) (meta-apply 'eval-map fun (cdr lst) env
		  (lambda (y) (meta-apply cont (cons x y))))))))
(define (primitive-procedure? . operand)
  (let ((arg (car operand)))
     (or (procedure? arg)
	 (and (pair? arg)
	      (eq? (car arg) lambda-tag)))))
(define (primitive-output write args)
  (let ((answer (car args)))
     (if (and (pair? answer)
	      (eq? (car answer) lambda-tag))
	 (let ((lambda-params        (car (cdr answer)))
	       (lambda-body     (car (cdr (cdr answer))))
	       (lambda-env (car (cdr (cdr (cdr answer))))))
	    (write (cons 'lambda (cons lambda-params lambda-body))))
	 (write answer))))
(define (primitive-display . args)
  (primitive-output display args))
(define (primitive-write . args)
  (primitive-output write args))
(define (primitive-pp . args)
  (primitive-output pp args))
(define (primitive-print lst depth length)
  (define (print-sub lst d l top?)
     (cond ((pair? lst)
	    (cond ((= l 0)
		   (if top? (display "(...)") (display " ...)")))
		  ((= d 0) (display "#"))
		  (else
		   (if top? (display "(") (display " "))
		   (print-sub (car lst) (- d 1) length #t)
		   (cond ((pair? (cdr lst))
			  (print-sub (cdr lst) d (- l 1) #f))
			 ((null? (cdr lst))
			  (display ")"))
			 (else
			  (begin (display " . ")
				 (print-sub (cdr lst) d (- l 1) #f)
				 (display ")")))))))
	   (else (display lst))))
  (print-sub lst depth length #t))
;
; Meta-apply
;
(define (meta-apply proc-name-or-cont . operand)
  (lambda (Mcont)
   (let ((meta-env (car (head Mcont)))
	 (meta-cont (car (cdr (head Mcont))))
	 (meta-Mcont (tail Mcont)))
    (let ((operator (if (symbol? proc-name-or-cont)
			(cdr (get proc-name-or-cont meta-env))
			proc-name-or-cont)))
  (cond ((procedure? operator)
	 (cond ((pair? (member operator primitive-procedures))
		((meta-apply 'base-apply operator operand meta-env meta-cont)
		 meta-Mcont))
	       (else ; evaluator functions
		((scheme-apply operator operand) Mcont))))
	(else
	 ((meta-apply 'base-apply operator operand meta-env meta-cont)
	  meta-Mcont)))))))
;
; Initial Continuation
;
(define (init-cont env level turn cont)
  (meta-apply cont
     (lambda (answer)
	(write level)(write '-)(write turn)(display ": ")
	(primitive-write answer)
	(newline)
	(write level)(write '-)(write (+ turn 1))(display "> ")
	(meta-apply 'base-eval (read) env
		    (lambda (ans)
		      (meta-apply 'init-cont env level (+ turn 1)
				  (lambda (cont) (meta-apply cont ans))))))))

(define (run env level answer)
  (meta-apply 'init-cont env level 0
	     (lambda (cont) (meta-apply cont answer))))
;
; Environment
;
;(load "env.scm")
;
; Primitive Procedures
;
(define primitive-procedures
  (list car cdr cons list pair? null? eq? eqv? equal? not set-car! set-cdr!
	append
	primitive-write primitive-pp primitive-display newline read
	primitive-print primitive-procedure?
	+ - * / = < > quotient remainder number?
	boolean? string? symbol? assq member length
	open-input-file close-input-port eof-object?
	map scheme-apply
	make-pairs extend can-receive? get set-value! define-value
	search copy
))
;
; Initial Environment
;
(define init-env (list (list
  (cons 'car			car)
  (cons 'cdr			cdr)
  (cons 'cons			cons)
  (cons 'list			list)
  (cons 'pair?			pair?)
  (cons 'null?			null?)
  (cons 'eq?			eq?)
  (cons 'eqv?			eqv?)
  (cons 'equal?			equal?)
  (cons 'not			not)
  (cons 'set-car!		set-car!)
  (cons 'set-cdr!		set-cdr!)
  (cons 'append			append)
  (cons 'write			primitive-write)
  (cons 'pp			primitive-pp)
  (cons 'display		primitive-display)
  (cons 'print			primitive-print)
  (cons 'newline		newline)
  (cons 'read			read)
  (cons '+			+)
  (cons '-			-)
  (cons '*			*)
  (cons '/			/)
  (cons '=			=)
  (cons '>			>)
  (cons '<			<)
  (cons 'quotient		quotient)
  (cons 'remainder		remainder)
  (cons 'number?		number?)
  (cons 'boolean?		boolean?)
  (cons 'string?		string?)
  (cons 'symbol?		symbol?)
  (cons 'procedure?		primitive-procedure?)
  (cons 'assq			assq)
  (cons 'member			member)
  (cons 'length			length)
  (cons 'open-input-file	open-input-file)
  (cons 'close-input-port	close-input-port)
  (cons 'eof-object?		eof-object?)

  (cons 'map			map)
  (cons 'scheme-apply		scheme-apply)

  (cons 'empty-env		empty-env)
  (cons 'make-pairs		make-pairs)
  (cons 'extend			extend)
  (cons 'can-receive?		can-receive?)
  (cons 'get			get)
  (cons 'set-value!		set-value!)
  (cons 'define-value		define-value)
  (cons 'search			search)
  (cons 'copy			copy)

  (cons 'base-eval		base-eval)
  (cons 'eval-application	eval-application)
  (cons 'eval-var		eval-var)
  (cons 'eval-quote		eval-quote)
  (cons 'eval-if		eval-if)
  (cons 'eval-cond		eval-cond)
  (cons 'eval-define		eval-define)
  (cons 'eval-set!		eval-set!)
  (cons 'lambda-tag		lambda-tag)
  (cons 'eval-lambda		eval-lambda)
  (cons 'delta-tag		delta-tag)
  (cons 'eval-delta		eval-delta)
  (cons 'eval-begin		eval-begin)
  (cons 'eval-let		eval-let)
  (cons 'eval-let*		eval-let*)
  (cons 'eval-letrec		eval-letrec)
  (cons 'eval-EM		eval-EM)
  (cons 'eval-exit		eval-exit)
  (cons 'eval-list		eval-list)
  (cons 'base-apply		base-apply)
  (cons 'apply-delta		apply-delta)
  (cons 'my-error		my-error)
  (cons 'eval-load		eval-load)
  (cons 'eval-and		eval-and)
  (cons 'eval-or		eval-or)
  (cons 'eval-map		eval-map)

  (cons 'init-env		0)	; to be filled when making a new level
  (cons 'init-cont		init-cont)
  (cons 'run			run)
  (cons 'primitive-procedures	primitive-procedures)
  (cons 'old-env		old-env)
  (cons 'old-cont		old-cont)
)))
;
; Meta-level Initial Continuation
;
;(define (return result)
;  (lambda (Mcont)
;     (let ((meta-env (car (head Mcont)))
;	   (meta-cont (car (cdr (head Mcont))))
;	   (meta-Mcont (tail Mcont)))
;	((meta-apply meta-cont result)
;	 meta-Mcont))))
(define (meta-init-cont env level supplied-env)
  (define-value 'init-env supplied-env env) ; share-env
  (display "New level loaded.")(newline)
  (lambda (result)
    (meta-apply 'run env level result)))
;
; Initial Meta-Continuation
;
(define (init-Mcont level supplied-env)
  (let ((env (copy init-env)))
     (cons-stream (list env (meta-init-cont env level supplied-env))
		  (init-Mcont (+ level 1) env))))
;
; Start up
;
(define (black)
  (let* ((base-Mcont (init-Mcont 0 (copy init-env)))
	 (env (car (head base-Mcont)))
	 (cont (car (cdr (head base-Mcont))))
	 (Mcont (tail base-Mcont)))
     ((cont 'start) Mcont)))

;(newline)
;(black)
