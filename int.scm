;
; Eval functions
;
(define (base-eval exp env cont)
  (cond ((number? exp)		 (cont exp))
	((boolean? exp)		 (cont exp))
	((string? exp)		 (cont exp))
	((symbol? exp)		 (eval-var exp env cont))
	((eq? (car exp) 'quote)  (eval-quote exp env cont))
	((eq? (car exp) 'if)	 (eval-if exp env cont))
	((eq? (car exp) 'cond)	 (eval-cond (cdr exp) env cont))
	((eq? (car exp) 'define) (eval-define exp env cont))
	((eq? (car exp) 'set!)	 (eval-set! exp env cont))
	((eq? (car exp) 'lambda) (eval-lambda exp env cont))
	((eq? (car exp) 'begin)  (eval-begin (cdr exp) env cont))
	((eq? (car exp) 'let)	 (eval-let
				   (car (cdr exp)) (cdr (cdr exp)) env cont))
	((eq? (car exp) 'let*)	 (eval-let*
				   (car (cdr exp)) (cdr (cdr exp)) env cont))
	((eq? (car exp) 'letrec) (eval-letrec
				   (car (cdr exp)) (cdr (cdr exp)) env cont))
	((eq? (car exp) 'EM)	 (eval-EM exp env cont))
	((eq? (car exp) 'exec-at-metalevel)
				 (eval-EM exp env cont))
	((eq? (car exp) 'primitive-EM)
				 (eval-primitive-EM exp env cont))
	((eq? (car exp) 'exit)	 (eval-exit exp env cont))
	((eq? (car exp) 'load)	 (eval-load exp env cont))
	((eq? (car exp) 'and)	 (eval-and (cdr exp) env cont))
	((eq? (car exp) 'or)	 (eval-or (cdr exp) env cont))
	((eq? (car exp) 'delay)	 (eval-delay exp env cont))
	((eq? (car exp) 'cons-stream)
				 (eval-cons-stream exp env cont))
	(else (eval-application exp env cont))))
(define (eval-application exp env cont)
  (eval-list exp env
	      (lambda (l) (base-apply (car l) (cdr l) env cont))))
(define (eval-var exp env cont)
  (let ((pair (get exp env)))
    (if (pair? pair)
	(cont (cdr pair))
	(my-error
		    (list 'eval-var: 'unbound 'variable: exp) env cont))))
(define (eval-quote exp env cont) (cont (car (cdr exp))))
(define (eval-if exp env cont)
  (let ((pred-part (car (cdr exp)))
	(then-part (car (cdr (cdr exp))))
	(else-part (cdr (cdr (cdr exp)))))
    (base-eval pred-part env (lambda (p)
		(cond (p (base-eval then-part env cont))
		      ((null? else-part) (cont #f))
		      (else
		       (base-eval (car else-part) env cont)))))))
(define (eval-cond clauses env cont)
  (cond ((null? clauses) (cont '()))
	((eq? (car (car clauses)) 'else)
	 (eval-begin (cdr (car clauses)) env cont))
	(else
	 (base-eval
		     (car (car clauses))
		     env
		     (lambda (pred)
		       (if pred
			   (eval-begin (cdr (car clauses))
				       env cont)
			   (eval-cond (cdr clauses)
				       env cont)))))))
(define (eval-define exp env cont)
  (if (pair? (car (cdr exp)))
      (let ((var (car (car (cdr exp))))
	    (body (cons 'lambda
			(cons (cdr (car (cdr exp)))
			      (cdr (cdr exp))))))
	(base-eval body env
		    (lambda (data)
		      (define-value var data env)
		      (cont var))))
      (let ((var (car (cdr exp)))
	    (body (car (cdr (cdr exp)))))
	(base-eval body env
		    (lambda (data)
		      (define-value var data env)
		      (cont var))))))
(define (eval-set! exp env cont)
  (let ((var (car (cdr exp)))
	(body (car (cdr (cdr exp)))))
    (base-eval body env
		(lambda (data)
		  (let ((pair (get var env)))
		    (if (pair? pair)
			(begin (set-value! var data env)
			       (cont var))
			(my-error
				    (list 'eval-set!: 'unbound 'variable var)
				    env cont)))))))
(define lambda-tag (cons 'lambda 'tag))
(define (eval-lambda exp env cont)
  (let ((lambda-body (cdr (cdr exp)))
	(lambda-params (car (cdr exp))))
    (cont (list lambda-tag lambda-params lambda-body env))))
(define (eval-begin body env cont)
  (define (eval-begin-local body)
    (if (null? (cdr body))
	(base-eval (car body) env cont)
	(base-eval (car body) env
		    (lambda (x) (eval-begin-local (cdr body))))))
  (if (null? body)
      (my-error '(eval-begin: null body) env cont)
      (eval-begin-local body)))
(define (eval-let pairs body env cont)
  (let ((params (map car pairs))
	(args (map (lambda (x) (car (cdr x))) pairs)))
    (eval-list args env
		(lambda (operand)
		  (eval-begin body
			      (extend env params operand)
			      cont)))))
(define (eval-let* pairs body env cont)
  (if (null? pairs)
      (eval-begin body env cont)
      (base-eval (car (cdr (car pairs))) env (lambda (arg)
		  (eval-let* (cdr pairs) body
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
      (eval-list args letrec-env
		  (lambda (operand)
		    (set-value-list! params operand letrec-env)
		    (eval-begin body letrec-env cont))))))
(define (eval-EM exp env cont)
  (cont (primitive-EM (car (cdr exp)))))
(define (eval-primitive-EM exp env cont)
  (base-eval (car (cdr exp)) env
	      (lambda (body) (cont (primitive-EM body)))))
(define (eval-exit exp env cont)
  (base-eval (car (cdr exp)) env
	      (lambda (x) (my-error x env cont))))
(define (eval-list exp env cont)
  (if (null? exp)
      (cont '())
      (base-eval (car exp) env
		  (lambda (val1)
		    (eval-list (cdr exp) env
				(lambda (val2)
				  (cont (cons val1 val2))))))))
(define (base-apply operator operand env cont)
  (cond ((procedure? operator)
	 (cond ((eq? operator map)
		(eval-map
			    (car operand) (car (cdr operand)) env cont))
	       ((eq? operator scheme-apply)
		(base-apply
			    (car operand) (car (cdr operand)) env cont))
	       ((eq? operator force)
		(let ((arg (car operand)))
		  (if (and (pair? arg)
			   (eq? (car arg) delay-tag))
		      (let ((promise-body (car (cdr arg)))
			    (promise-env (car (cdr (cdr arg))))
			    (pair (cdr (cdr arg))))
			(if (pair? (cdr pair))
			    (cont (car (cdr pair)))
			    (base-eval promise-body promise-env
					(lambda (ans)
					  (set-cdr! pair (list ans))
					  (cont ans)))))
		      (cont arg))))
	       ((pair? (member operator primitive-procedures))
		(cont (scheme-apply operator operand)))
	       (else ; called when going down a level.
		(cont (scheme-apply operator operand)))))
	((and (pair? operator)
	      (eq? (car operator) lambda-tag))
	 (let ((lambda-params        (car (cdr operator)))
	       (lambda-body     (car (cdr (cdr operator))))
	       (lambda-env (car (cdr (cdr (cdr operator))))))
	   (if (can-receive? lambda-params operand)
	       (eval-begin
			   lambda-body
			   (extend lambda-env lambda-params operand)
			   cont)
	       (my-error
			   (list 'base-apply: 'Wrong 'number 'of 'arguments:
				 operand 'to: lambda-params)
			   env cont))))
	(else
	 (my-error (list 'Not 'a 'function: operator) env cont))))
(define old-env 0)
(define old-cont 0)
(define (my-error exp env cont)
  (set! old-env env)
  (set! old-cont cont)
  exp)
(define (eval-load exp env cont)
  (define port (open-input-file (car (cdr exp))))
  (define (load-local)
    (let ((input (read port)))
      (if (eof-object? input)
	  (begin (close-input-port port)
		 (cont 'done))
	  (base-eval input env
		      (lambda (value) (load-local))))))
  (load-local))
(define (eval-and body env cont)
  (cond ((null? body) (cont #t))
	((null? (cdr body))
	 (base-eval (car body) env cont))
	(else
	 (base-eval (car body) env
		     (lambda (result)
		       (if result
			   (eval-and (cdr body) env cont)
			   (cont result)))))))
(define (eval-or body env cont)
  (if (null? body)
      (cont #f)
      (base-eval (car body) env
		  (lambda (result)
		    (if result
			(cont result)
			(eval-or (cdr body) env cont))))))
(define delay-tag (cons 'delay 'tag))
(define (eval-delay exp env cont)
  (let ((delay-body (car (cdr exp))))
    (cont (list delay-tag delay-body env))))
(define (eval-cons-stream exp env cont)
  (let ((car-part (car (cdr exp)))
	(cdr-part (car (cdr (cdr exp)))))
    (base-eval (list 'cons car-part (list 'delay cdr-part))
			   env cont)))

;
; Primitives
;
(define (eval-map fun lst env cont)
  (if (null? lst)
      (cont '())
      (base-apply fun (list (car lst)) env
		  (lambda (x) (eval-map fun (cdr lst) env
		  (lambda (y) (cont (cons x y))))))))
(define (primitive-procedure? . operand)
  (let ((arg (car operand)))
    (or (procedure? arg)
	(and (pair? arg)
	     (eq? (car arg) lambda-tag)))))
(define (filter arg)
  (if (pair? arg)
      (cond ((eq? (car arg) lambda-tag)
	     (let ((lambda-params        (car (cdr arg)))
		   (lambda-body     (car (cdr (cdr arg)))))
	       (cons 'lambda (cons lambda-params lambda-body))))
	    ((eq? (car arg) delay-tag)
	     (let ((delay-body (car (cdr arg))))
	       (list '<delay> delay-body)))
	    (else
	     (cons (filter (car arg))
		   (filter (cdr arg)))))
      arg))
(define (primitive-display . args)
  (display (filter (car args))))
(define (primitive-write . args)
  (write (filter (car args))))
(define (primitive-pp . args)
  (pp (filter (car args))))
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
; Initial Continuation
;
(define (init-cont env level turn cont)
  (cont
    (lambda (answer)
      (write level) (write '-) (write turn) (display ": ")
      (primitive-write answer)
      (newline)
      (write level) (write '-) (write (+ turn 1)) (display "> ")
      (base-eval (read) env
		  (lambda (ans)
		    (init-cont env level (+ turn 1)
				(lambda (cont) (cont ans))))))))

(define (run env level answer)
  (init-cont env level 0
	      (lambda (cont) (cont answer))))
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
	boolean? string? symbol? assq member length force
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
  (cons 'force			force)
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
  (cons 'get-global-env		get-global-env)

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
  (cons 'eval-begin		eval-begin)
  (cons 'eval-let		eval-let)
  (cons 'eval-let*		eval-let*)
  (cons 'eval-letrec		eval-letrec)
  (cons 'eval-EM		eval-EM)
  (cons 'eval-primitive-EM	eval-primitive-EM)
  (cons 'eval-exit		eval-exit)
  (cons 'eval-list		eval-list)
  (cons 'base-apply		base-apply)
  (cons 'my-error		my-error)
  (cons 'eval-load		eval-load)
  (cons 'eval-and		eval-and)
  (cons 'eval-or		eval-or)
  (cons 'delay-tag		delay-tag)
  (cons 'eval-delay		eval-delay)
  (cons 'eval-cons-stream	eval-cons-stream)
  (cons 'eval-map		eval-map)

  (cons 'init-env		0)	; to be filled when making a new level
  (cons 'init-cont		init-cont)
  (cons 'run			run)
  (cons 'primitive-procedures	primitive-procedures)
  (cons 'old-env		old-env)
  (cons 'old-cont		old-cont)
)))
(define-value 'init-env init-env init-env)
