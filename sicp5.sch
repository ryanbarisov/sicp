
; ex 5.3
; Design a machine to compute square roots using Newton's method
; Begin by assuming that good-enough? and improve operations are available as primitives.
(define (sqrt x)
	(define (good-enough? guess)
		(< (abs (- (square guess) x)) 0.0001)
	)
	(define (average . args)
		(/ (apply + args) (length args))
	)
	(define (improve guess)
		(average guess (/ x guess))
	)
	(define (iter guess)
		(if (good-enough? guess)
			guess
			(iter (improve guess))
		)
	)
	(iter 1.0)
)

(controller
		(assign g (const 1.0))
	test-good
		;;(test (op good-enough?) (reg g))
		(assign t1 (op square) (reg g))
		(assign t2 (op -) (reg t1) (reg x))
		(assign t1 (op abs) (reg t2))
		(test (op <) (reg t1) (const 0.0001))
		;;
		(branch (label sqrt-done))
		;;(assign t (op improve) (reg g))
		(assign t1 (op /) (reg x) (reg g))
		(assign t2 (op average) (reg g) (reg t1))
		;;
		(assign g (reg t2))
		(goto (label test-good))
	sqrt-done
)

(define (average . args) (if (null? args) 0 (/ (apply + args) (length args))))
(define sqrt-machine
	(make-machine
		'(t1 t2 g x)
		(list (list 'square square) (list '- -) (list 'abs abs) (list '< <) (list 'average average) (list '/ /))
		'(
				(assign g (const 1.0))
			test-good
				(assign t1 (op square) (reg g))
				(assign t2 (op -) (reg t1) (reg x))
				(assign t1 (op abs) (reg t2))
				(test (op <) (reg t1) (const 0.0001))
				(branch (label sqrt-done))
				(assign t1 (op /) (reg x) (reg g))
				(assign t2 (op average) (reg g) (reg t1))
				(assign g (reg t2))
				(goto (label test-good))
			sqrt-done
		)
	)
)

;;; 

(define (factorial n)
	(if (= n 1)
		1
		(* n (factorial (- n 1)))
	)
)

(controller
		(assign continue (label fact-done))
	start
		(test (op =) (reg n) (const 1))
		(branch (label trivial))
		(save n)
		(assign n (op -) (reg n) (const 1))
		(save continue)
		(assign continue (label mult))
		(goto (label start))
	mult
		(restore n)
		(assign val (op *) (reg n) (reg val))
		(restore continue)
		(goto (reg continue))
	trivial
		(assign val (const 1))
		(goto (reg continue))
	fact-done
)

(define fact-machine
	(make-machine
		'(n val continue)
		(list (list '= =) (list '- -) (list '* *))
		'(
			init
				(assign continue (label fact-done))
			start
				(test (op =) (reg n) (const 1))
				(branch (label trivial))
				(save n)
				(assign n (op -) (reg n) (const 1))
				(save continue)
				(assign continue (label mult))
				(goto (label start))
			mult
				(restore continue)
				(restore n)
				(assign val (op *) (reg n) (reg val))
				(goto (reg continue))
			trivial
				(assign val (const 1))
				(goto (reg continue))
			fact-done
		)
	)
)

;;; ex 5.6

(define (fib n)
	(if (< n 2) n
		(+ (fib (- n 1)) (fib (- n 2)))
	)
)

(controller
		(assign continue (label fib-done))
	loop
		(test (op <) (reg n) (const 2))
		(branch (label trivial))
		(save n)
		(assign n (op -) (reg n) (const 1))	; n-1
		(save continue)				; 
		(assign continue (label n1))		; request to compute fib(n-1)
		(goto (label loop))			; in loop
	n1
		(save val)				; save fib(n-1)
		(assign n (op -) (reg n) (const 1))	; n-2
		(assign continue (label n2))		; request to compute fib(n-2)
		(goto (label loop))			; in loop
	n2
		(assign t (reg val))			; fib(n-2)
		(restore val)				; fib(n-1)
		(assign val (op +) (reg t) (reg val))	; fib(n-1)+fib(n-2)
		(restore continue)			; return to
		(restore n)				; fib(n)
		(goto (reg continue))
	trivial
		(assign val (reg n))
		(goto (reg continue))
	fib-done
)

(define fib-machine 
	(make-machine
		'(n val continue t)
		(list (list '+ +) (list '< <) (list '- -))
		'(
				(assign continue (label fib-done))
			loop
				(test (op <) (reg n) (const 2))
				(branch (label trivial))
				(save n)
				(assign n (op -) (reg n) (const 1))
				(save continue)
				(assign continue (label n1))
				(goto (label loop))
			n1
				(save val)
				(assign n (op -) (reg n) (const 1))
				(assign continue (label n2))
				(goto (label loop))
			n2
				(assign t (reg val))
				(restore val)
				(assign val (op +) (reg t) (reg val))
				(restore continue)
				(restore n)
				(goto (reg continue))
			trivial
				(assign val (reg n))
				(goto (reg continue))
			fib-done
		)
	)
)
(set-register-contents! fib-machine 'n 10)
(start fib-machine)
(get-register-contents fib-machine 'val)


;; ex 5.4
(define (expt b n)
	(if (= n 0)
		1
		(* b (expt b (- n 1)))
	)
)

(controller
		(assign continue (label expt-done))
	loop
		(test (op =) (reg n) (const 0))
		(branch (label trivial))
		(assign n (op -) (reg n) (const 1))
		(save continue)
		(assign continue (label mult))
		(goto (label loop))
	mult
		(assign val (op *) (reg b) (reg val))
		(restore continue)
		(goto (reg continue))
	trivial
		(assign val (const 1))
		(goto (reg continue))
	expt-done
)

(define expt-rec-machine
	(make-machine
		'(n val continue b)
		(list (list '* *) (list '- -) (list '= =))
		'(
				(assign continue (label expt-done))
			loop
				(test (op =) (reg n) (const 0))
				(branch (label trivial))
				(assign n (op -) (reg n) (const 1))
				(save continue)
				(assign continue (label mult))
				(goto (label loop))
			mult
				(assign val (op *) (reg b) (reg val))
				(restore continue)
				(goto (reg continue))
			trivial
				(assign val (const 1))
				(goto (reg continue))
			expt-done
		)
	)
)
(set-register-contents! expt-rec-machine 'n 3)
(set-register-contents! expt-rec-machine 'b 4)
(start expt-rec-machine)
(get-register-contents expt-rec-machine 'val)

(define (expt b n)
	(define (iter counter product)
		(if (= counter 0)
			product
			(iter (- counter 1) (* b product))
		)
	)
	(iter n 1)
)

(controller
		(assign c (reg n))
		(assign p (const 1))
	loop
		(test (op =) (reg c) (const 0))
		(branch (label expt-done))
		(assign c (op -) (reg c) (const 1))
		(assign p (op *) (reg p) (reg b))
		(goto (label loop))
	expt-done
		(assign val p)
)

(define expt-iter-machine
	(make-machine
		'(c p n b val)
		(list (list '* *) (list '- -) (list '= =))
		'(
				(assign c (reg n))
				(assign p (const 1))
			loop
				(test (op =) (reg c) (const 0))
				(branch (label expt-done))
				(assign c (op -) (reg c) (const 1))
				(assign p (op *) (reg p) (reg b))
				(goto (label loop))
			expt-done
				(assign val (reg p))
		)
	)
)
(set-register-contents! expt-iter-machine 'n 3)
(set-register-contents! expt-iter-machine 'b 4)
(start expt-iter-machine)
(get-register-contents expt-iter-machine 'val)

; ex 5.8
(define ex58-machine
	(make-machine '(a) (list (list '+ +))    
		'(
			start
				(goto (label here))
			here
				(assign a (const 3))
				(goto (label there))
			here
				(assign a (const 4))
				(goto (label there))
			there
		)
	)
)
; Multiply defined label -- ASSEMBLE (here ((assign a (const 4))) ((goto (label there))))

; ex 5.9
(define ex59-machine
	(make-machine '(a) (list (list '+ +))    
		'(
			start
				(goto (label here1))
			here1
				(assign a (op +) (const 3) (label here1))
				(goto (label there))
			there
		)
	)
)
;Operations can only be used with registers and constants -- ASSEMBLE


