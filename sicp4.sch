; chapter 4

; ex 4.1
(define (list-of-values exps env)
	(if (no-operands? exps)
		'()
		(cons
			(eval (first-operand exps) env)
			(list-of-values (rest-operands exps) env)
		)
	)
)

; forces to evaluate first operand before going into tail recursion (evaluation from left to right)
; (let ( (<var1> <exp1>) (<var2> <exp2>)) <body>) <--> ( (lambda (<var1> <var2>) <body>) <exp1> <exp2>)
(define (list-of-values exps env)
	(if (no-operands? exps)
		'()
		(let ((first-val (eval (first-operand exps) env)))
			(cons first-val
				(list-of-values (rest-operands exps) env)
			)
		)
	)
)

; same trick, expression inside let binding is evaluated first (evaluation of exps from right to left)
(define (list-of-values exps env)
	(if (no-operands? exps)
		'()
		(let ((last-val (list-of-values (rest-operands exps) env)))
			(cons (eval (first-operand exps) env)
				last-val
			)
		)
	)
)

; ex 4.2
; (a) (define x 3) -> evaluator decides it to be a procedure with operator `define`, which is neither primitive nor compound procedure, that's why this expression causes error in evaluator
; (b) procedure syntax is (call proc . arguments)
; (1) move application branch higher than assignment
(define (eval exp env)
	(cond ((self-evaluating? exp) exp)
		((variable? exp) (lookup-variable-value exp env))
		((quoted? exp) (text-of-quotation exp))
		((application? exp)
			(apply (eval (operator exp) env)
			(list-of-values (operands exp) env)))
		((assignment? exp) (eval-assignment exp env))
		((definition? exp) (eval-definition exp env))
		((if? exp) (eval-if exp env))
		((lambda? exp)
			(make-procedure (lambda-parameters exp)
			(lambda-body exp) env)
		)
		((begin? exp) 
			(eval-sequence (begin-actions exp) env))
		((cond? exp) (eval (cond->if exp) env))
		(else (error "Unknown expression type -- EVAL" exp))
	)
)
; (2) redefine selectors
(define (application? exp)
	(and (pair? exp) (eq? (car exp) 'call))
)
(define (operator exp) (cadr exp))
(define (operands exp) (cddr exp))
; selectors below don't change
(define (no-operands? ops) (null? ops))
(define (first-operand ops) (car ops))
(define (rest-operands ops) (cdr ops))

(define (apply procedure arguments)
	(cond 
		((primitive-procedure? procedure)
		...)
		((compound-procedure? procedure)
		...)
		(else
			(error "Unknown type of procedure to apply.")
		)
	)
)

; ex 4.3
; use make-table from ex 3.24
; lengthy attempt..
(define (make-table same-key?)
	(let ((local-table (list '*table*)) (same? same-key?))
		(define (assoc key records)
			(cond
				((null? records) false)
				((same? key (caar records)) (car records))
				(else (assoc key (cdr records)))
			)
		)
		(define (lookup key-1)
			(let ((record (assoc key-1 (cdr local-table))))
				(if record (cdr record) false)
			)
		)
		(define (insert! key-1 value)
			(let ((record (assoc key-2 (cdr subtable))))
				(if record
					(set-cdr! record value)
					(set-cdr! local-table (cons (list key value) (cdr local-table)))
				)
			)
			'ok
		)    
		(define (dispatch m)
			(cond
				((eq? m 'lookup) lookup)
				((eq? m 'insert!) insert!)
				(else (error "Unknown operation -- TABLE" m))
			)
		)
		dispatch
	)
)

(define eval-table (make-table eq?))
(define (get-eval-op key) ((eval-table 'lookup)) key)
(define (put-eval-op key val) ((eval-table 'insert!)) key val)

(put-eval-op 'set! (lambda (exp env) (eval-assignment exp env)))
(put-eval-op 'define (lambda (exp env) (eval-definition exp env)))
(put-eval-op 'if (lambda (exp env) (eval-if exp env)))
(put-eval-op 'lambda (lambda (exp env) (make-procedure (lambda-parameters exp) (lambda-body exp) env))
(put-eval-op 'begin (lambda (exp env) (eval-sequence (begin-actions exp) env)))
(put-eval-op 'cond (lambda (exp env) (eval (cond->if exp) env)))
;(put-eval-op 'apply (lambda (exp env) (apply (eval (operator exp) env) (list-of-values (operands exp) env))))

(define (type-of exp) (car exp))

(define (tagged-list? exp tag)
	(if (pair? exp)
		(eq? (car exp) tag)
		false
	)
)

(define (eval exp env)
	(cond ((self-evaluating? exp) exp)
		((variable? exp) (lookup-variable-value exp env))
		((quoted? exp) (text-of-quotation exp))
		(else
			(let ( (compound (get-eval-op (type-of exp))) )
				(cond
					( compound (apply compound exp env) )
					( (pair? exp) (apply (eval (operator exp) env) (list-of-values (operands exp) env)))
					(else (error "Unknown expression type -- EVAL" exp))
				)
			)
		)
	)
)


; ex 4.4

(define (eval exp env)
	(cond ((self-evaluating? exp) exp)
		((variable? exp) (lookup-variable-value exp env))
		((quoted? exp) (text-of-quotation exp))
		((assignment? exp) (eval-assignment exp env))
		((definition? exp) (eval-definition exp env))
		((if? exp) (eval-if exp env))
		((and? exp) (eval-and exp env))
		((or? exp) (eval-or exp env))
		((lambda? exp)
			(make-procedure (lambda-parameters exp)
			(lambda-body exp) env)
		)
		((begin? exp) 
			(eval-sequence (begin-actions exp) env))
		((cond? exp) (eval (cond->if exp) env))
		((application? exp)
			(apply (eval (operator exp) env)
			(list-of-values (operands exp) env)))
		(else (error "Unknown expression type -- EVAL" exp))
	)
)

(define (and? exp) (tagged-list? exp 'and))
(define (or? exp) (tagged-list? exp 'or))

(define (eval-and exp env)
	(define (iter exps)
		(cond
			((null? exps) true)
			((null? (cdr exps)) (eval (car exps) env))
			((not (eval (car exps) env)) false)
			(else (iter (cdr exps)))
		)
	)
	(iter exp)
)

(define (eval-or exp env)
	(define (iter exps)
		(cond
			((null? exps) false)
			((null? (cdr exps)) (eval (car exps) env))
			((eval (car exps) env) true)
			(else (iter (cdr exps)))
		)
	)
	(iter exp)
)

; ex 4.5

; cond expression from the book

(define (cond? exp) (tagged-list? exp 'cond))
(define (cond-clauses exp) (cdr exp))
(define (cond-else-clause? clause)
 	(eq? (cond-predicate clause) 'else))
(define (cond-predicate clause) (car clause))
(define (cond-actions clause) (cdr clause))
(define (cond->if exp)
 	(expand-clauses (cond-clauses exp)))

(define (expand-clauses clauses)
	(if (null? clauses)
		'false                          ; no else clause
		(let ((first (car clauses))
			  (rest (cdr clauses)))
			(if (cond-else-clause? first)
				(if (null? rest)
					(sequence->exp (cond-actions first))
					(error "ELSE clause isn't last -- COND->IF" clauses)
				)
				(make-if (cond-predicate first)
					(sequence->exp (cond-actions first))
					(expand-clauses rest)
				)
			)
		)
	)
)

; (<test> => <recipient>) 
; <test> evaluates to true -> return (<recipient> <test>)

(define (cond-test-clause? clause) (eq? (cadr clause) '=>) )
(define (cond-test-proc clause) (caddr clause))
(define (cond-test-arg clause) (cons-predicate clause))

(define (expand-clauses clauses)
	(if (null? clauses)
		'false                          ; no else clause
		(let ((first (car clauses))
			(rest (cdr clauses)))
			(cond
				((cond-else-clause? first)
					(if (null? rest)
						(sequence->exp (cond-actions first))
						(error "ELSE clause isn't last -- COND->IF" clauses)
					)
				)
				((cond-test-clause? first)
					(make-if (cond-predicate first)
						(list (cond-test-proc first) (cond-test-arg clause))
						(expand-clauses rest)
					)
				)
				(else
					(make-if (cond-predicate first)
						(sequence->exp (cond-actions first))
						(expand-clauses rest)
					)
				)
			)
		)
	)
)

; ex 4.6

(define (eval exp env)
	(cond ((self-evaluating? exp) exp)
		((variable? exp) (lookup-variable-value exp env))
		((quoted? exp) (text-of-quotation exp))
		((assignment? exp) (eval-assignment exp env))
		((definition? exp) (eval-definition exp env))
		((if? exp) (eval-if exp env))
		((and? exp) (eval-and exp env))
		((or? exp) (eval-or exp env))
		((lambda? exp)
			(make-procedure (lambda-parameters exp)
			(lambda-body exp) env)
		)
		((begin? exp) 
			(eval-sequence (begin-actions exp) env))
		((cond? exp) (eval (cond->if exp) env))
		((let? exp) 
			(eval (let->combination exp) env)
		)
		((application? exp)
			(apply (eval (operator exp) env)
			(list-of-values (operands exp) env)))
		(else (error "Unknown expression type -- EVAL" exp))
	)
)

(define (let? exp) (tagged-list? exp 'let))
(define (named-let? exp) (and (let? exp) (not (list? (cadr exp)))))
(define (named-let-name exp) (cadr exp))
(define (let-keyval exp) (if (named-let? exp) (caddr exp) (cadr exp)))
(define (let-body exp) (if (named-let? exp) (cadddr exp) (caddr exp)))
(define (let->combination exp)
	(list (make-lambda (map car (let-keyval exp)) (let-body exp)) (map cadr (let-keyval exp)) )
)

; ex 4.7

(define (let*? exp) (tagged-list? exp 'let*))

(define (let*->nested-lets exp)
	(if (null? (cdr (let-keyval exp)))
		(let-combination (list (car (let-keyval exp))))
		(let-combination (let*->nested-lets (list (cdr (let-keyval exp)))) )
	)
)

(define (eval exp env)
	(cond ((self-evaluating? exp) exp)
		((variable? exp) (lookup-variable-value exp env))
		((quoted? exp) (text-of-quotation exp))
		((assignment? exp) (eval-assignment exp env))
		((definition? exp) (eval-definition exp env))
		((if? exp) (eval-if exp env))
		((and? exp) (eval-and exp env))
		((or? exp) (eval-or exp env))
		((lambda? exp)
			(make-procedure (lambda-parameters exp)
			(lambda-body exp) env)
		)
		((begin? exp) 
			(eval-sequence (begin-actions exp) env))
		((cond? exp) (eval (cond->if exp) env))
		((let? exp) 
			(eval (let->combination exp) env)
		)
		((let*? exp) 
			(eval (let*->nested-lets exp) env)
		)
		((application? exp)
			(apply (eval (operator exp) env)
			(list-of-values (operands exp) env)))
		(else (error "Unknown expression type -- EVAL" exp))
	)
)

; ex 4.8

(define (append elem lst)
	(if (null? lst)
		(cons elem '())
		(cons (car lst) (append elem (cdr lst)))
	)
)

; (let <var> <bindings> <body>)
; (let (<bindings> (<var (lambda (<bindings>) <body))) <body>)

(define (named-let->let exp)
	(let->combination
		(list 'let (append (let-keyval exp) (list (named-let-name exp) (make-lambda (map car (let-keyval exp)) (let-body exp)))) (let-body exp))
	)
)

; ex 4.9, 4.10 - skip

; ex 4.11

; environment hierarchy is represented as list
(define (first-frame env) (car env))
(define (enclosing-environment env) (cdr env))
(define the-empty-environment '())

; each frame is a list of key-value pairs
(define (make-frame variables values)
	(define (iter vars vals frame)
		(if (null? vars)
			frame
			(iter (cdr vars) (cdr vals) (cons (list (car vars) (car vals)) frame))
		)
	)
	(if (= (length variables) (length values))
		(iter variables values the-empty-environment)
		(if (> (length variables) (length values))
			(error "There are more variables than values")
			(error "There are more values than variables")
		)
	)
)

(define (extend-environment vars vals base-env)
	(if (= (length vars) (length vals))
		(cons (make-frame vars vals) base-env)
		(if (> (length vars) (length vals))
			(error "More variables than values")
			(error "More values than variables")
		)
	)
)
(define (lookup-variable-value var env)
	(define (scan pairs env)
		(display var) (newline) (display pairs) (newline)
		(cond
			((null? pairs) (lookup-variable-value var (enclosing-environment env)))
			((eq? var (caar pairs)) (cdar pairs))
			(else (scan (cdr pairs) env))
		)
	)
	(if (eq? env the-empty-environment)
		(error "Unbound variable LOOKUP")
		(scan (first-frame env) env)
	)
)
(define (define-variable! var val env)
	(define (scan pairs env)
		(cond
			((eq? var (caar pairs)) (set-cdr! (car pairs) val))
			((null? (cdr pairs)) (set-cdr! pairs (cons (cons var val) '())))
			(else (scan (cdr pairs) env))
		)
	)
	(scan (first-frame env) env)
)
(define (set-variable-value! var val env)
	(define (scan pairs env)
		(display var) (newline) (display pairs) (newline)
		(cond
			((eq? var (caar pairs)) (set-cdr! (car pairs) val))
			((null? (cdr pairs)) (set-variable-value! var val (enclosing-environment env)))
			(else (scan (cdr pairs) env))
		)
	)
	(if (eq? env the-empty-environment)
		(error "Unbound variable SET-VARIABLE")
		(scan (first-frame env) env)
	)
)

; ex 4.12 - skip, there are a structure that can be implemented in abstract structures, but it is not evident quickly to me how to do it

; ex 4.13 - use filter to remove element from list

(define (make-unbound! var env)
	(define (scan pairs env)
		(begin
			(filter (lambda (v) (not (eq? var (caar v)))) pairs)
			(make-unbound! var (enclosing-environment env))
		)
	)
	(if (not (eq? env the-empty-environment))
		(scan (first-frame env) env)
	)
)

; ex 4.20
(define (f x)
	(letrec ((even? (lambda (n) (if (= n 0) true (odd? (- n 1)))))
		  (odd?  (lambda (n) (if (= n 0) false (even? (- n 1))))))
		(even? x)
	)
)
(define (f x)
	(define even? (lambda (n) (if (= n 0) true (odd? (- n 1)))))
	(define odd?  (lambda (n) (if (= n 0) false (even? (- n 1)))))
	(even? x)
)
(define (f x)
	(let ((even? (lambda (n) (if (= n 0) true (odd? (- n 1)))))
		  (odd?  (lambda (n) (if (= n 0) false (even? (- n 1))))))
		(even? x)
	)
)

; ex 4.21 (a)
(
	(lambda (n)
		(
			(lambda (fact) (fact fact n))
			(lambda (ft k)
				(if (= k 1)
					1
					(* k (ft ft (- k 1)))
				)
			)
		)
	)
	10
)

(
	(lambda (n)
		(
			(lambda (fib) (fib fib 0 1 n))
			(lambda (fb a b k)
				(cond 
					((= k 0) a)
					((= k 1) b)
					(else (fb fb b (+ a b) (- k 1)))
				)
			)
		)
	)
	10
)

; ex 4.21 (b)
(define (f x)
	((lambda (even? odd?) (even? even? odd? x))
		(lambda (ev? od? n) (if (= n 0) true (od? ev? od? (- n 1))))
		(lambda (ev? od? n) (if (= n 0) false (ev? ev? od? (- n 1))))
	)
)

