(define (init-Mcont)
  (cons-stream (list (copy init-env) init-cont)
	       (init-Mcont)))

(define (base-eval exp env cont)
  (cond ((number? exp) (meta-apply cont exp))
	((boolean? exp) (meta-apply cont exp))
	((string? exp) (meta-apply cont exp))
	((symbol? exp) (meta-apply ’eval-var exp env cont))
	((eq? (car exp) ’quote) (meta-apply ’eval-quote exp env cont))
	((eq? (car exp) ’if) (meta-apply ’eval-if exp env cont))
	((eq? (car exp) ’set!) (meta-apply ’eval-set! exp env cont))
	((eq? (car exp) ’lambda) (meta-apply ’eval-lambda exp env cont))
	((eq? (car exp) ’begin) (meta-apply ’eval-begin exp env cont))
	((eq? (car exp) ’exec-at-metalevel)
	 (meta-apply ’eval-EM exp env cont))
	((eq? (car exp) ’exit) (meta-apply ’eval-exit exp env cont))
	(else (meta-apply ’eval-application exp env cont))))

(define (eval-var exp env cont)
  (let ((pair (get exp env)))
    (if (pair? pair)
	(meta-apply cont (cdr pair))
	(meta-apply ’my-error
		     (list ’eval-var: ’unbound ’variable: exp) env cont))))
(define (eval-quote exp env cont) (meta-apply cont (car (cdr exp))))
(define (eval-if exp env cont)
  (meta-apply ’base-eval (car (cdr exp)) env
	       (lambda (pred)
		 (if pred (meta-apply ’base-eval (car (cdr (cdr exp)))
				       env cont)
		     (meta-apply ’base-eval (car (cdr (cdr (cdr exp))))
				  env cont)))))
(define (eval-set! exp env cont)
  (let ((var (car (cdr exp)))
	(body (car (cdr (cdr exp)))))
    (meta-apply ’base-eval body env
		 (lambda (data)
		   (let ((pair (get var env)))
		     (if (pair? pair)
			 (begin (set-value! var data env)
				(meta-apply cont var))
			 (meta-apply ’my-error
				      (list ’eval-set!: ’unbound ’variable var)
				      env cont)))))))

(define lambda-tag (cons ’lambda ’tag))

(define (eval-lambda exp env cont)
  (let ((lambda-body (cdr (cdr exp)))
	(lambda-params (car (cdr exp))))
    (meta-apply cont (list lambda-tag lambda-params lambda-body env))))

(define (eval-EM exp env cont)
  (lambda (Mcont)
    (let ((meta-env (car (head Mcont)))
	  (meta-cont (car (cdr (head Mcont))))
	  (meta-Mcont (tail Mcont)))
      ((meta-apply ’base-eval (car (cdr exp))
		    meta-env
		    (lambda (ans)
		      (lambda (Mcont2)
			((meta-apply cont ans)
			 (cons-stream (head Mcont) Mcont2)))))
       meta-Mcont))))

(define (base-apply operator operand env cont)
  (cond ((procedure? operator)
	 (cond ((eq? operator map)
		(meta-apply ’eval-map
			     (car operand) (car (cdr operand)) env cont))
	       ((eq? operator scheme-apply)
		(meta-apply ’base-apply
			     (car operand) (car (cdr operand)) env cont))
	       ((pair? (memq operator primitive-procedures))
		(meta-apply cont (scheme-apply operator operand)))
	       (else ; evaluator functions <=========================== (A)
		(lambda (Mcont)
		  ((scheme-apply operator operand)
		   (cons-stream (list (get-global-env env) cont)
				Mcont))))))
	((and (pair? operator)
	      (eq? (car operator) lambda-tag))
	 (let ((lambda-params (car (cdr operator)))
	       (lambda-body (car (cdr (cdr operator))))
	       (lambda-env (car (cdr (cdr (cdr operator))))))
	   (meta-apply ’eval-begin-body
			lambda-body
			(extend lambda-env lambda-params operand)
			cont)))
	(else (meta-apply ’my-error
			   (list ’Not ’a ’function: operator) env cont))))

(define (meta-apply proc-name-or-cont . operand)
  (lambda (Mcont)
    (let* ((meta-env (car (head Mcont)))
	   (meta-cont (car (cdr (head Mcont))))
	   (meta-Mcont (tail Mcont))
	   (operator (if (symbol? proc-name-or-cont)
			 (cdr (get proc-name-or-cont meta-env))
			 proc-name-or-cont)))
      (cond ((procedure? operator)
	     (if (pair? (memq operator primitive-procedures))
		 ((meta-apply ’base-apply operator operand
			       meta-env meta-cont)
		  meta-Mcont)
		 ((scheme-apply operator operand) ; evaluator functions
		  Mcont)))
	    (else
	     ((meta-apply ’base-apply operator operand meta-env meta-cont)
	      meta-Mcont))))))

(define old-env 0)
(define old-cont 0)

(define (my-error exp env cont)
  (lambda (Mcont)
    (let ((meta-env (car (head Mcont)))
	  (meta-cont (car (cdr (head Mcont))))
	  (meta-Mcont (tail Mcont)))
      (set-value! ’old-env env meta-env)
      (set-value! ’old-cont cont meta-env)
      ((meta-apply meta-cont exp) meta-Mcont))))

(define (black)
  (let* ((base-Mcont (init-Mcont 0 (copy init-env)))
	 (env (car (head base-Mcont)))
	 (cont (car (cdr (head base-Mcont))))
	 (Mcont (tail base-Mcont)))
    ((cont ’start) Mcont)))

(define (init-Mcont level env-below)
  (let ((env (copy init-env)))
    (cons-stream (list env (meta-init-cont env level env-below))
		 (init-Mcont (+ level 1) env))))

(define (meta-init-cont env level env-below)
  (define-value ’init-env env-below env) ; share-env
  (display "New level loaded.") (newline)
  (lambda (result) (meta-apply ’run env level result)))


;; aux functions

(define (primitive-procedure? . operand)
  (let ((arg (car operand)))
    (or (procedure? arg)
	(and (pair? arg)
	     (eq? (car arg) lambda-tag)))))

(define (primitive-write arg)
  (if (and (pair? arg)
	   (eq? (car arg) lambda-tag))
      (let ((lambda-params (car (cdr arg)))
	    (lambda-body (car (cdr (cdr arg))))
	    (lambda-env (car (cdr (cdr (cdr arg))))))
	(write (cons ’lambda (cons lambda-params lambda-body))))
      (write arg)))

(define primitive-procedures
  (list car cdr cons list pair? null? not eq? eqv? equal? set-car! set-cdr!
	append newline read primitive-write + - * / = > < quotient remainder
	number? symbol? boolean? string? memq length assq primitive-procedure?
	map scheme-apply
	make-pairs extend get set-value! define-value copy get-global-env))

