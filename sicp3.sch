; Chapter 3

; ex. 3.1
; An accumulator is a procedure that is called repeatedly with a single numeric argument and accumulates its arguments into a sum.
; Each time it is called, it returns the currently accumulated sum.
; Write a procedure make-accumulator that generates accumulators, each maintaining an independent sum.
; The input to make-accumulator should specify the initial value of the sum.
(define (make-accumulator initial)
	(lambda (amount) (begin (set! initial (+ amount initial)) initial)))

(define A (make-accumulator 5))
(A 10) ; 15
(A 10) ; 25


; ex. 3.2
; Write a procedure make-monitored that takes as input a procedure, f, that itself takes one input.
; The result returned by make-monitored is a third procedure, say mf, that keeps track of the number of times it has been called by maintaining an internal counter. 
; If the input to mf is the special symbol how-many-calls?, then mf returns the value of the counter.
; If the input is the special symbol reset-count, then mf resets the counter to zero.
; For any other input, mf returns the result of calling f on that input and increments the counter.
(define (make-monitored f)
	(let ( (amount 0) )
		(define (how-many-calls?) amount)
		(define (reset-count) (set! amount 0))
		(define (evaluate x) (begin (set! amount (+ amount 1)) (f x)))
		(define (dispatch m)
			(cond ( (eq? m 'how-many-calls?) (how-many-calls?))
			      ( (eq? m 'reset-count) (reset-count))
			      ( else (evaluate m) )
			)
		)
	dispatch
	)
)

(define s (make-monitored sqrt))
(s 100)
(s 4)
(s 'how-many-calls?) ; 2
(s 'reset-count)
(s 16)
(s 'how-many-calls?) ; 1


(define (make-account balance)
	(define (withdraw amount)
		(if (>= balance amount)
			(begin (set! balance (- balance amount)) balance)
		"Insufficient funds")
	)
	(define (deposit amount)
		(set! balance (+ balance amount))
		balance
	)
	(define (dispatch m)
		(cond ((eq? m 'withdraw) withdraw)
			((eq? m 'deposit) deposit)
			(else (error "Unknown request -- MAKE-ACCOUNT" m))
		)
	)
	dispatch
)

; ex 3.3
; Modify the make-account procedure so that it creates password-protected accounts.
; That is, make-account should take a symbol as an additional argument as in:
;  (define acc (make-account 100 'secret-password))
; The resulting account object should process a request only if it is accompanied by the password with which the account was created, and should otherwise return a complaint:
;  ((acc 'secret-password 'withdraw) 40) ; 60
;  ((acc 'some-other-password 'deposit) 50) ; "Incorrect password"

(define (make-account initial password)
	(let (
			(balance initial)
			(passwd password)
		)
		(define (withdraw amount) (begin (set! balance (- balance amount)) balance))
		(define (deposit amount) (set! balance (+ amount balance)))
		(define (dispatch proc)
			(cond 
				((eq? proc 'withdraw) withdraw)
				((eq? proc 'deposit) deposit)
				( else "Unknown operation!")
			)
		)
		(define (checked-dispatch pwd proc)
			(if (eq? pwd passwd)
				(dispatch proc)
				(lambda (x) "Incorrect password!")
			)
		)
	checked-dispatch
	)
)

(define acc (make-account 100 'secret-password))
((acc 'secret-password 'withdraw) 40) ; 60
((acc 'incorrect-pwd 'withdraw) 30) ; "Incorrect password"


; ex 3.4
; Modify the make-account procedure of exercise 3.3 by adding another local state variable so that,
; if an account is accessed more than seven consecutive times with an incorrect password, it invokes the procedure call-the-cops.
(define (make-account initial password)
	(let (
			(balance initial)
			(passwd password)
			(failed-attempts 0)
		)
		(define (withdraw amount) (begin (set! balance (- balance amount)) balance))
		(define (deposit amount) (set! balance (+ amount balance)))
		(define (dispatch proc)
			(cond 
				((eq? proc 'withdraw) withdraw)
				((eq? proc 'deposit) deposit)
				( else "Unknown operation!")
			)
		)
		(define (checked-dispatch pwd proc)
			(if (eq? pwd passwd)
				(begin (set! failed-attempts 0)
					   (dispatch proc)
				)
				(lambda (x)
					(begin
						(set! failed-attempts (+ 1 failed-attempts))
						(if (> failed-attempts 7)
							"We call to the police!"
							"Incorrect password"
						)
					)
				)
			)
		)
	checked-dispatch
	)
)

(define acc (make-account 100 'secret-password))
((acc 'secret-password 'withdraw) 40) ; 60
((acc 'incorrect-pwd 'withdraw) 30) ; "Incorrect password"
; ... x6 times 
((acc 'incorrect-pwd 'withdraw) 30) ; "We call to the police!"


; ex 3.5
; Implement Monte Carlo integration as a procedure estimate-integral
; that takes as arguments a predicate P, upper and lower bounds x1, x2, y1, and y2 for the rectangle,
; and the number of trials to perform in order to produce the estimate.
; Your procedure should use the same monte-carlo procedure that was used above to estimate .
; Use your estimate-integral to produce an estimate of by measuring the area of a unit circle.
(define (monte-carlo trials experiment)
	(define (iter success total)
		(cond	( (= 0 total) (/ success trials)) 
				( (experiment) (iter (+ success 1) (- total 1) ) )
				( else (iter success (- total 1)) )
		)
	)
	(iter 0 trials)
)
(define (random-in-range low high)
	(+ low (random (- high low)))
)
(define (estimate-integral pred x1 x2 y1 y2 trials)
	(let	(	(rect-area (* (- x2 x1) (- y2 y1)) )
			(experiment (lambda () (pred (random-in-range x1 x2) (random-in-range y1 y2))) )
		)
		(* (monte-carlo trials experiment) rect-area)
	)
)
(define (unit-circle? x y)
	(< (+ (* x x) (* y y)) 1) )
(estimate-integral unit-circle? -1.0 1.0 -1.0 1.0 100000)


; ex 3.6
; It is useful to be able to reset a random-number generator to produce a sequence starting from a given value.
; Design a new rand procedure that is called with an argument that is either the symbol generate or the symbol reset and behaves as follows:
;   (rand 'generate) produces a new random number;
;   ((rand 'reset) <new-value>) resets the internal state variable to the designated <new-value>.
; Thus, by resetting the state, one can generate repeatable sequences.
; These are very handy to have when testing and debugging programs that use random numbers. 
(define rand
	(let ((val 0))
		(define (rand-update arg)
			(define maxval 10000)
			(remainder (+ arg (random maxval)) maxval)
		)
		(define (generate)
			(begin (set! val (rand-update val)) val)
		)
		(define (reset newval)
			(begin (set! val newval) newval val)
		)
		(define (dispatch m)
			(display val)
			(cond	( (eq? m 'generate) (generate) )
					( (eq? m 'reset) reset )
					;( else m)
					( else (error "Unsupported operation."))
			)
		)
		dispatch
	)
)

(rand 'generate)
( (rand 'reset) 10)


; ex 3.7
; Consider the bank account objects created by make-account, with the password modification described in exercise 3.3.
; Suppose that our banking system requires the ability to make joint accounts. Define a procedure make-joint that accomplishes this. 
; Make-joint should take three arguments.
;   The first is a password-protected account.
;   The second argument must match the password with which the account was defined in order for the make-joint operation to proceed.
;   The third argument is a new password.
; Make-joint is to create an additional access to the original account using the new password. For example, if peter-acc is a bank account with password open-sesame, then
;   (define paul-acc (make-joint peter-acc 'open-sesame 'rosebud))
; will allow one to make transactions on peter-acc using the name paul-acc and the password rosebud. You may wish to modify your solution to exercise 3.3 to accommodate this new feature. 

(define (make-account initial password)
	(let (
			(balance initial)
			(passwd password)
		)
		(define (withdraw amount) (begin (set! balance (- balance amount)) balance))
		(define (deposit amount) (set! balance (+ amount balance)))
		(define (dispatch proc)
			(cond 
				((eq? proc 'withdraw) withdraw)
				((eq? proc 'deposit) deposit)
				( else "Unknown operation!")
			)
		)
		(define (checked-dispatch pwd proc)
			(if (eq? pwd passwd)
				(dispatch proc)
				(lambda (x) "Incorrect password!")
			)
		)
	checked-dispatch
	)
)
(define (make-joint acc accpwd newpwd)
	(let ((passwd newpwd))
		(define (checked-dispatch pwd proc)
			(if (eq? pwd passwd)
				(acc accpwd proc)
				(lambda (x) "Incorrect password!")
			)
		)
		checked-dispatch
	)
)

(define acc (make-account 100 'secret-password))
((acc 'secret-password 'withdraw) 40) ; 60
((acc 'incorrect-pwd 'withdraw) 30) ; "Incorrect password"
(define acc2 (make-joint acc 'secret-password 'rosebud))
((acc2 'rosebud 'withdraw) 20) ; 40
((acc 'secret-password 'deposit) 30) ; 40
((acc2 'rosebud 'withdraw) 20) ; 50
((acc2 'secret-password 'withdraw) 20) ; "Incorrect password"


; ex 3.8 - order in which the subexpressions should be evaluated?
; Define a simple procedure f such that
; evaluating (+ (f 0) (f 1)) will return 0 if the arguments to + are evaluated from left to right
; but will return 1 if the arguments are evaluated from right to left.
(define (f x)
	(let ((flag #f))
		(if (eq? flag #t)
			0
			(begin (set! flag #t) x)
		)
	)
)
(+ (f 0) (f 1))

;; 3.3 Modeling with mutable data

; ex 3.16
; Ben Bitdiddle decides to write a procedure to count the number of pairs in any list structure.
; ``It's easy,'' he reasons. 
; ``The number of pairs in any structure is the number in the car plus the number in the cdr plus one more to count the current pair.''
; So Ben writes the following procedure:
(define (count-pairs x)
	(if (not (pair? x))
		0
		(+ (count-pairs (car x)) (count-pairs (cdr x)) 1)
	)
)
; Show that this procedure is not correct. 
; In particular, draw box-and-pointer diagrams representing list structures made up of exactly three pairs for which Ben's procedure would 
; return 3; return 4; return 7; never return at all.

(define z1 (list 'a 'b 'c)) ; (a b c)
(count-pairs z1) ; 3

(define z2 (cons z1 z1)) ; ((a b c) a b c)
(count-pairs z2) ; 7

(define z3 (list 'b 'c)) ; (b c)
(define z3 (cons z3 (cdr z3))) ; ((b c) c)
(count-pairs z3) ; 4

(define (last-pair x)
	(if (null? (cdr x))
		x
		(last-pair (cdr x))
	)
)
(define (make-cycle x)
	(set-cdr! (last-pair x) x)
	x
)
(define z4 (make-cycle (list 'a 'b 'c)))
(count-pairs z4) ; recursion depth exceeded

; ex 3.17
; Devise a correct version of the count-pairs procedure of exercise 3.16 that returns the number of distinct pairs in any structure.
; (Hint: Traverse the structure, maintaining an auxiliary data structure that is used to keep track of which pairs have already been counted.)

; this naive version almost surely can be simplified
(define (count-pairs var)
	(let ((uniq '()) (amount 0))
		; this might be replaced with fold* version
		(define (find-pair p)
			(define (iter-find p where)
				(cond	((null? where) #f)
						((eq? p (car where)) #t)
						(else (iter-find p (cdr where)))
				)
			)
			(iter-find p uniq)
		)
		; this might be rewritten without following procedural style?
		(define (traverse-pair p)
			(if (pair? p)
				(begin
					(if (not (find-pair p))
						(begin
							(set! amount (+ amount 1))
							(set! uniq (cons p uniq))
						)
					)
					(traverse-pair (car p))
					(traverse-pair (cdr p))
				)
				(if (not (find-pair p))
					(set! uniq (cons p uniq))
				)
			)
		)
		(begin
			(traverse-pair var)
			amount
		)
	)
)

; ex 3.18
; Write a procedure that examines a list and determines whether it contains a cycle, 
; that is, whether a program that tried to find the end of the list by taking successive cdrs would go into an infinite loop. 
; Exercise 3.13 constructed such lists.
(define (contains-cycle? var)
	(let ((uniq '()))
		(define (iter-find p where)
			(cond	((null? where) #f)
					((eq? p (car where)) #t)
					(else (iter-find p (cdr where)))
			)
		)
		(define (find-pair p)
			(iter-find p uniq)
		)
		(define (find-cycle p)
			(if (null? (cdr p))
				#f
				(iter-find (cdr p) uniq)
			)
		)
		(define (traverse-pair p)
			(if (pair? p)
				(if (not (find-pair p))
					(begin
						(set! uniq (cons p uniq))
						(if (find-cycle p)
							#t
							(traverse-pair (cdr p))
						)
					)
				)
				#f
			)
		)
		(traverse-pair var)
	)
)

; ex 3.19
; Redo exercise 3.18 using an algorithm that takes only a constant amount of space. (This requires a very clever idea.) 
; Idea from code job interview: iteratively check for the equality of the two pointers, which start at first/second list element;
; first makes one cdr each iteration, second - two cdr
(define (contains-cycle? lst)
	(let ((ptr1 '()) (ptr2 '()))
		(define (iter ptr1 ptr2)
			(cond
				((or (null? ptr1) (null? ptr2) (null? (cdr ptr2))) #f)
				((eq? ptr1 ptr2) #t)
				(else (iter (cdr ptr1) (cddr ptr2)))
			)
		)
		(if (null? lst)
			#f
			(iter lst (cdr lst))
		)
	)
)

;; Representing queues
; ex 3.21

(define (print-queue q)
	(front-ptr q)
)

; ex 3.22
; Instead of representing a queue as a pair of pointers, we can build a queue as a procedure with local state.
; The local state will consist of pointers to the beginning and the end of an ordinary list.
; Thus, the make-queue procedure will have the form
;  (define (make-queue)
;   (let ((front-ptr ...)
;         (rear-ptr ...))
;     <definitions of internal procedures>
;     (define (dispatch m) ...)
;     dispatch))
; Complete the definition of make-queue and provide implementations of the queue operations using this representation.
(define (make-queue)
	(let ((front-ptr '()) (rear-ptr '()))
    	(define (empty-queue?)
    		(null? front-ptr)
    	)
    	(define (front-queue)
    		(if (empty-queue?)
    			(error "Fail to get element from empty queue")
    			(car front-ptr)
    		)
    	)
    	(define (insert-queue! val)
    		(let (( p (cons val '()) ))
    			(if (and (null? front-ptr) (null? rear-ptr))
    				(begin
    					(set! front-ptr p)
	    				(set! rear-ptr p)
	    				front-ptr
    				)
    				(begin
    					(set-cdr! rear-ptr p)
    					(set! rear-ptr p)
    					front-ptr
    				)
	    		)
    		)
    	)
    	(define (delete-queue!)
    		(if (empty-queue?)
    			(error "Cannot delete from empty queue")
    			(begin 
    				(set! front-ptr (cdr front-ptr))
    				front-ptr
    			)
    		)
    	)
		(define (dispatch m)
			(cond
				((eq? m 'empty-queue?) (empty-queue?))
				((eq? m 'front-queue) (front-queue))
				((eq? m 'insert-queue!) insert-queue!)
				((eq? m 'delete-queue!) (delete-queue!))
			)
		)
		dispatch
    )
)

(define (empty-queue? q) (q 'empty-queue?))
(define (front-queue q) (q 'front-queue))
(define (insert-queue! q val) ((q 'insert-queue!) val))
(define (delete-queue! q) (q 'delete-queue!))

; ex 3.23
; A deque (``double-ended queue'') is a sequence in which items can be inserted and deleted at either the front or the rear.
; Operations on deques are 
;   the constructor make-deque
;   the predicate empty-deque?
;   selectors front-deque and rear-deque, 
;   and mutators front-insert-deque!, rear-insert-deque!, front-delete-deque!, and rear-delete-deque!. 
; Show how to represent deques using pairs, and give implementations of the operations.
; All operations should be accomplished in (1) steps. 
(define (make-deque)
	(cons '() '())
)
(define (front-ptr dq) (car dq))
(define (rear-ptr dq) (cdr dq))
(define (empty-deque? dq) (or (null? (front-ptr dq)) (null? (rear-ptr dq))))
(define (front-deque dq)
	(if (empty-deque? dq)
		(error "FRONT called with empty deque")
		(car (front-ptr dq))
	)
)
(define (rear-deque dq)
	(if (empty-deque? dq)
		(error "REAR called with empty deque")
		(car (rear-ptr dq))
	)
)
(define (front-insert-deque! dq val)
	(let ( (p (cons val (cons '() '()))) )
		(if (empty-deque? dq)
			(begin
				(set-car! dq p)
				(set-cdr! dq p)
				dq
			)
			(begin
				(set-car! (cdr (front-ptr dq)) p)
				(set-cdr! (cdr p) (front-ptr dq))
				(set-car! dq p)
				dq
			)
		)
	)
)
(define (rear-insert-deque! dq val)
	(let ( (p (cons val (cons '() '()))) )
		(if (empty-deque? dq)
			(begin
				(set-car! dq p)
				(set-cdr! dq p)
				dq
			)
			(begin
				(set-cdr! (cdr (rear-ptr dq)) p)
				(set-car! (cdr p) (rear-ptr dq))
				(set-cdr! dq p)
				dq
			)
		)
	)
)
(define (front-delete-deque! dq)
	(cond
		((empty-deque? dq) (error "Cannot delete from empty deque"))
		((eq? (front-ptr dq) (rear-ptr dq))
			(begin
				(set-car! dq '())
				(set-cdr! dq '())
				dq
			)
		)
		(else
			(begin
				(set-car! dq (cddr (front-ptr dq)))
				(set-car! (cdr (front-ptr dq)) '())
				dq
			)
		)
	)
)
(define (rear-delete-deque! dq)
	(cond
		((empty-deque? dq) (error "Cannot delete from empty deque"))
		((eq? (front-ptr dq) (rear-ptr dq))
			(begin
				(set-car! dq '())
				(set-cdr! dq '())
				dq
			)
		)
		(else
			(begin
				(set-cdr! dq (cadr (rear-ptr dq)))
				(set-cdr! (cdr (rear-ptr dq)) '())
				dq
			)
		)
	)
)

;; Representing tables

; ex 3.24
(define (make-table same-key?)
	(let ((local-table (list '*table*)) (same? same-key?))
		(define (assoc key records)
			(cond
				((null? records) false)
				((same? key (caar records)) (car records))
				(else (assoc key (cdr records)))
			)
		)
		(define (lookup key-1 key-2)
			(let ((subtable (assoc key-1 (cdr local-table))))
				(if subtable
					(let ((record (assoc key-2 (cdr subtable))))
						(if record (cdr record) false)
					)
					false
				)
			)
		)
		(define (insert! key-1 key-2 value)
			(let ((subtable (assoc key-1 (cdr local-table))))
				(if subtable
					(let ((record (assoc key-2 (cdr subtable))))
						(if record
							(set-cdr! record value)
							(set-cdr! subtable (cons (cons key-2 value) (cdr subtable)))
						)
					)
					(set-cdr! local-table
						(cons (list key-1 (cons key-2 value)) (cdr local-table))
					)
				)
			)
			'ok
		)    
		(define (dispatch m)
			(cond
				((eq? m 'lookup-proc) lookup)
				((eq? m 'insert-proc!) insert!)
				(else (error "Unknown operation -- TABLE" m))
			)
		)
		dispatch
	)
)

; ex 3.25
; Generalizing one- and two-dimensional tables, show how to implement a table in which values are stored under an arbitrary number of keys and different values may be stored under different numbers of keys. 
; The lookup and insert! procedures should take as input a list of keys used to access the table.

; incorrect behaviour:
(define tab (make-table))
((tab 'insert!) 1 'a') ; (*table* (a . 1))
((tab 'insert!) 2 'a 'b) ; (*table* (a (b . 2) . 1))
((tab 'lookup) 'a) ; ((b . 2) . 1) , but expected 1

(define (make-table)
	(let ((local-table (list '*table*)))
		(define (assoc key records)
			(cond
				((null? records) #f)
				((not (pair? records)) #f)
				((equal? key (caar records)) (car records))
				(else (assoc key (cdr records)))
			)
		)
		(define (lookup . key)
			(define (iter ks tab)
				(if (null? ks)
					#f
					(let ((rec (assoc (car ks) (cdr tab))))
						(if rec
							(if (null? (cdr ks))
								(cdr rec)
								(iter (cdr ks) rec)
							)
							#f
						)
					)
				)
			)
			(iter key local-table)
		)
		(define (insert! val . key)
			(define (iter ks tab)
				(display ks) (display tab)
				(let ((rec (assoc (car ks) (cdr tab))))
					(if (null? (cdr ks))
						(begin
							;(display tab) (display rec) (display (car ks))
							(if rec
								(set-cdr! rec val)
								(set-cdr! tab (cons (cons (car ks) val) (cdr tab)))
							)
							(display tab)
							tab
						)
						(begin
							(if rec
								(let ((tail (cdr rec)) (p (iter (cdr ks) rec)) )
									(set! rec (cons p tail))
									tab
								)
								(begin
									(set-cdr! tab (cons (cons (car ks) '()) (cdr tab)))
									(iter (cdr ks) (cadr tab))
								)
							)
						)
					)
				)
			)
			(if (null? key)
				(error "No keys for insertion in table")
				(iter key local-table)
			)
		)
		(define (dispatch m)
			(cond
				((eq? m 'insert!) insert!)
				((eq? m 'lookup) lookup)
				(else (error "Unsupported operation"))
			)
		)
		dispatch
	)
)
(define (lookup key table) ((table 'lookup) key))
(define (insert! key val table) ((table 'insert!) val key))

; ex 3.26
; To search a table as implemented above, one needs to scan through the list of records. 
; This is basically the unordered list representation of section 2.3.3. 
; For large tables, it may be more efficient to structure the table in a different manner. 
; Describe a table implementation where the (key, value) records are organized using a binary tree, assuming that keys can be ordered in some way (e.g., numerically or alphabetically). 
; (Compare exercise 2.66 of chapter 2.) 

; done only for the table (stored using binary tree) with single key
; todo: table with list of keys 

(define (make-entry key val left right) (cons (cons key val) (cons left right)))
(define (make-node-entry key val) (make-entry key val '() '()))
(define (left-branch e) (cadr e))
(define (right-branch e) (cddr e))
(define (key-of e) (caar e))
(define (value-of e) (cdar e))
(define (make-table same-key? less-key?)
	(let ((local-table (list '*table*)) (same? same-key?) (less? less-key?))
		(define (assoc key records)
			(display records) (newline)
			(cond
				((null? records) (cons #f '()))
				((not (pair? records)) (cons #f records))
				((same? key (key-of records)) (cons #t records))
				((less? key (key-of records))
					(if (null? (left-branch records))
						(cons #f records)
						(assoc key (left-branch records))
					)
				)
				(else
					(display (right-branch records))
					(if (null? (right-branch records))
						(cons #f records)
						(assoc key (right-branch records))
					)
				)
			)
		)
		(define (lookup . key)
			(define (iter ks tab)
				(let ((rec (assoc (car ks) (cdr tab))))
					(newline) (newline) (display rec) (newline) (newline)
					(if (null? (cdr ks))
						(if (car rec)
							(value-of (cdr rec))
							#f
						)
						(iter (cdr ks) rec)
					)
				)
			)
			(if (null? key)
				(error "Lookup with empty key")
				(iter key local-table)
			)
		)
		(define (insert! val . key)
			(define (iter ks tab)
				(let ((rec (assoc (car key) (cdr tab))))
					(if (null? (cdr ks))
						(begin
							(newline) (display tab) (display rec)
							(if (car rec)
								(set-cdr! rec val)
								(cond
									((null? (cdr rec)) (set-cdr! tab (make-node-entry (car key) val)))
									((less? (car key) (key-of (cdr rec)))
										(set-car! (cddr rec) (make-node-entry (car key) val))
									)
									(else
										(set-cdr! (cddr rec) (make-node-entry (car key) val))
									)
								)
							)
							tab
						)
						(begin
							(if (car rec)
								(begin
									(iter (cdr ks) rec)
								)
								(cond
									((null? (cdr rec)) 
										(let ((p (make-node-entry (car key) val)) )
											(set-cdr! tab p)
											(set-cdr! (car p) (iter (cdr ks) p))
											tab
										)
									)
									((less? (car key) (key-of (cdr rec)))
										(iter (cdr ks) (left-branch (cdr rec)))
										;(set-car! (cddr rec) (make-node-entry (car key) val))
									)
									(else
										(set-cdr! (cddr rec) (make-node-entry (car key) val))
									)
								)
							)
						)
					)
				)
			)
			(if (null? key)
				(error "No keys found to insert value")
				(iter key local-table)
			)
		)
		(define (dispatch m)
			(cond
				((eq? m 'lookup) lookup)
				((eq? m 'insert!) insert!)
				(else (error "Unsupported operation"))
			)
		)
		dispatch
	)
)

(define tab (make-table (lambda (x y) (< (abs (- x y)) 0.00001)) <))
((tab 'insert!) 'd 4)
((tab 'insert!) 'b 2)
((tab 'insert!) 'f 6)
((tab 'insert!) 'a 1)
((tab 'insert!) 'c 3)
((tab 'insert!) 'e 5)
((tab 'insert!) 'g 7)
((tab 'insert!) 'a 1 2)
((tab 'lookup) 2)
((tab 'lookup) 3)
((tab 'lookup) 1 2)


;; A simulator for digital circuits

; ex 3.28
; Define an or-gate as a primitive function box. Your or-gate constructor should be similar to and-gate.

(define (or-gate a1 a2 output)
	(define (or-action-procedure)
		(let ((new-value (logical-or (get-signal a1) (get-signal a2))))
			(after-delay or-gate-delay (
				(lambda () (set-signal! output new-value))
			))
		)
	)
	(add-action! a1 or-action-procedure)
	(add-action! a2 or-action-procedure)
	'ok
)

; ex 3.29
; Another way to construct an or-gate is as a compound digital logic device, built from and-gates and inverters.
; Define a procedure or-gate that accomplishes this. What is the delay time of the or-gate in terms of and-gate-delay and inverter-delay?

(define (or-gate a1 a2 output)
	(let (w1 (make-wire)) (w2 (make-wire)) (w3 (make-wire)) (not-a1 (inverter a1 w1)) (not-a2 (inverter a2 w2))
		(inverter 
			(and-gate 
				not-a1 not-a2 w3
			)
		output)
	)
)

; ex 3.30
; Figure 3.27 shows a ripple-carry adder formed by stringing together n full-adders. 
; This is the simplest form of parallel adder for adding two n-bit binary numbers. 
; The inputs A1, A2, A3, ..., An and B1, B2, B3, ..., Bn are the two binary numbers to be added (each Ak and Bk is a 0 or a 1). 
; The circuit generates S1, S2, S3, ..., Sn, the n bits of the sum, and C, the carry from the addition. 
; Write a procedure ripple-carry-adder that generates this circuit. 
; The procedure should take as arguments three lists of n wires each -- the Ak, the Bk, and the Sk -- and also another wire C. 
; The major drawback of the ripple-carry adder is the need to wait for the carry signals to propagate. What is the delay needed to obtain the complete output from an n-bit ripple-carry adder, expressed in terms of the delays for and-gates, or-gates, and inverters?

; (define (full-adder a b c-in sum c-out) ...)

(define (ripple-carry-adder ak bk sk c)
	(let ((c0 (make-wire)))
		(if (null? (cdr ak))
			(full-adder (car ak) (car bk) c0 (car sk) c)
			(begin
				(ripple-carry-adder (cdr ak) (cdr bk) (cdr sk) c0)
				(full-adder (car ak) (car bk) c0 (car sk) c)
			)
		)
	)
)

;; Propagation of constraints
; ex 3.33
; Using primitive multiplier, adder, and constant constraints, define a procedure averager that takes three connectors a, b, and c as inputs and establishes the constraint that the value of c is the average of the values of a and b. 
(define (averager a b c)
	(let
		( (d (make-connector))
		  (e (make-connector))
		)
		(adder a b d)
		(constant 0.5 e)
		(multiplier e d c)
		'ok
	)
)

; ex 3.47
; A semaphore (of size n) is a generalization of a mutex. 
; Like a mutex, a semaphore supports acquire and release operations, but it is more general in that up to n processes can acquire it concurrently. 
; Additional processes that attempt to acquire the semaphore must wait for release operations. 
; Give implementations of semaphores
;   a. in terms of mutexes
;   b. in terms of atomic test-and-set! operations.

; 3.47(b)
(define (make-semaphore n)
	(define (make-cell-list k)
		(if (= k 1)
			(cons (list false) '())
			(cons (list false) (make-mutex-list (- k 1)))
		)
	)
	(define (last-pair x)
		(if (null? (cdr x))
			x
			(last-pair (cdr x))
		)
	)
	(define (make-cycle x)
		(set-cdr! (last-pair x) x)
	)
	(let ((mut (make-cycle (make-mutex-list n))))
		(define (acquire mut)
			(if (test-and-set! (car mut))
				(acquire (cdr mut))
				false
			)
		)
		(define (release mut)
			(if (test-and-set! (car mut))
				(clear! (car mut))
				(release (cdr mut))
			)
		)
		(define (dispatch m)
			(cond
				((eq? m 'acquire) (acquire mut))
				((eq? m 'release) (release mut))
				(else (error "Unknown operation"))
			)
		)
		dispatch
	)
)

; 3.47(a)
(define (make-semaphore n)
	(define (make-node prev) (list (cons (make-mutex) prev)) )
	(define (make-double-link-list n)
		(define (iter prev k)
			(if (= k 0)
				prev
				(let ((p (make-node prev)))
					(set-cdr! p (iter p (- k 1)))
					p
				)
			)
		)
		(iter (make-node) (- n 1))
	)
	(let (mut (make-double-link-list n))
		(define (acquire)
			(let ((next (cdr mut)))
				(if (null? next)
					(acquire)
					(begin
						(set! mut (cdr mut))
						((caar mut) 'acquire)
					)
				)
			)
		)
		(define (release)
			(let ((prev (cdar mut)))
				(if (not (null? prev))
					(begin
						(set! mut prev)
					)
				)
			)
		)
		(define (dispatch m)
			(cond
				((eq? m 'acquire) (acquire mut))
				((eq? m 'release) (release mut))
				(else (error "Unknown operation"))
			)
		)
		dispatch
	)
)

; ex 3.48
; Explain in detail why the deadlock-avoidance method described above, (i.e., the accounts are numbered, and each process attempts to acquire the smaller-numbered account first) 
; avoids deadlock in the exchange problem. 
; Rewrite serialized-exchange to incorporate this idea. 
; (You will also need to modify make-account so that each account is created with a number, which can be accessed by sending an appropriate message.) 

(define gid 0)

(define (make-account-and-serializer balance)
	(define (withdraw amount)
		(if (>= balance amount)
			(begin (set! balance (- balance amount)) balance)
			"Insufficient funds"
		)
	)
	(define (deposit amount)
		(set! balance (+ balance amount))
		balance
	)
	(let ((balance-serializer (make-serializer)) (id gid))
		(define (dispatch m)
			(cond ((eq? m 'withdraw) withdraw)
				((eq? m 'deposit) deposit)
				((eq? m 'balance) balance)
				((eq? m 'id) id)
				((eq? m 'serializer) balance-serializer)
				(else (error "Unknown request -- MAKE-ACCOUNT" m))
			)
		)
		(+ gid 1)
		dispatch
	)
)
(define (serialized-exchange account1 account2)
	(let
		((serializer1 (account1 'serializer)) 
		 (serializer2 (account2 'serializer))
		 (id1 (account1 'id))
		 (id2 (account2 'id))
		 )
		(if (< id1 id2)
			((serializer2 (serializer1 exchange)) account1 account2)
			((serializer1 (serializer2 exchange)) account1 account2)
		)
	)
)

;; Streams
; ex 3.50
; Complete the following definition, which generalizes stream-map to allow procedures that take multiple arguments, analogous to map in section 2.2.3, footnote 12.

;(define (stream-map proc . argstreams)
;  (if (<??> (car argstreams))
;      the-empty-stream
;      (<??>
;       (apply proc (map <??> argstreams))
;       (apply stream-map
;              (cons proc (map <??> argstreams))))))

(define (stream-map proc . argstreams)
	(if (null? (car argstreams))
		the-empty-stream
		(cons-stream
			(apply proc (map stream-car argstreams))
			(apply stream-map
				(cons proc (map stream-cdr argstreams))
			)
		)
	)
)

; ex 3.51
; In order to take a closer look at delayed evaluation, we will use the following procedure, which simply returns its argument after printing it:

(define (show x) (display-line x) x)

; What does the interpreter print in response to evaluating each expression in the following sequence?

(define x (stream-map show (stream-enumerate-interval 0 10))) ; prints 0, returns x
(stream-ref x 5) ; prints 1 2 3 4 5, then returns 5
(stream-ref x 7) ; prints 6 7, then returns 7


; ex 3.52
; Consider the sequence of expressions
; What is the value of sum after each of the above expressions is evaluated? 
; What is the printed response to evaluating the stream-ref and display-stream expressions? 
; Would these responses differ if we had implemented (delay <exp>) simply as (lambda () <exp>) without using the optimization provided by memo-proc ?

(define sum 0)
(define (accum x)
	(set! sum (+ sum x))
	sum
)
(define seq (stream-map accum (stream-enumerate-interval 1 20)))
; seq = {1 ...}
(define y (stream-filter even? seq))
; y = {6 ...}, seq = {1 3 6 ...}
(define z (stream-filter (lambda (x) (= (remainder x 5) 0)) seq))
; z = {10 ...}, seq = {1 3 6 10 ...}
(stream-ref y 7)
; value: 136, y = {6 10 28 36 66 78 120 136}, seq = {1 3 6 10 15 21 28 36 45 55 66 78 91 105 120 136 ...}
(display-stream z)
; z = {10 15 45 55 105 120 190 210}
; 


;; Infinite streams
;;   Defining streams implicitly

(define (divisible? x y) (= (remainder x y) 0))
(define (sieve stream)
	(cons-stream 
		(stream-car stream)
		(sieve
			(stream-filter (lambda (x) (not (divisible? x (stream-car stream))))
				(stream-cdr stream)
			)
		)
	)
)

; ex 3.54
; Define a procedure mul-streams, analogous to add-streams, that produces the elementwise product of its two input streams. 
; Use this together with the stream of integers to complete the following definition of the stream whose nth element (counting from 0) is n + 1 factorial:
;  (define factorials (cons-stream 1 (mul-streams <??> <??>)))

(define (integers-starting-from n) (cons-stream n (integers-starting-from (+ n 1))))
(define (mul-streams s1 s2) (stream-map * s1 s2)))
(define factorials (cons-stream 1 (mul-streams (integers-starting-from 1) factorials)))

; ex 3.55
; Define a procedure partial-sums that takes as argument a stream S and returns the stream whose elements are S0, S0 + S1, S0 + S1 + S2, .... 
; For example, (partial-sums integers) should be the stream 1, 3, 6, 10, 15, .... 

(define (add-streams s1 s2) (stream-map + s1 s2))
(define (partial-sums stream)
	(cons-stream (stream-car stream) (add-streams (partial-sums stream) (stream-cdr stream)))
)

; ex 3.56
; Define a procedure merge that combines two ordered streams into one ordered result stream, eliminating repetitions
(define (merge s1 s2)
	(cond
		((stream-null? s1) s2)
		((stream-null? s2) s1)
		(else (let
			((s1car (stream-car s1)) (s2car (stream-car s2s)))
			(
				(cond
					((< s1car s2car) (cons-stream s1car (merge (stream-cdr s1) s2)))
					((> s1car s2car) (cons-stream s2car (merge s1 (stream-cdr s2))))
					(else (cons-stream s1car (merge (stream-cdr s1) (stream-cdr s2))))
				)
			)
		))
	)
)

(define S (cons-stream 1 (merge (scale-stream S 2) (merge (scale-stream S 3) (scale-stream S 5)))))

; ex 3.58
; Give an interpretation of the stream computed by the following procedure:

; representation of num/den in using radix amount of digits
(define (expand num den radix)
	(cons-stream
		(quotient (* num radix) den)
		(expand (remainder (* num radix) den) den radix)
	)
)
; (Quotient is a primitive that returns the integer quotient of two integers.) 
; What are the successive elements produced by (expand 1 7 10) ? 
;   digits of 1/7 in base 10 = ( 1 4 2 8 5 7 1 4 2 8 5 7 .. )
; What is produced by (expand 3 8 10) ? 
;   digits of 3/8 in base 10 = ( 3 7 5 0 0 0 .. )

; ex 3.59
; We will represent the series a0 + a1 x + a2 x2 + a3 x3 + ··· as the stream whose elements are the coefficients a0, a1, a2, a3, ....
; (a) Define a procedure integrate-series that takes as input a stream a0, a1, a2, ... representing a power series 
;     and returns the stream a0, (1/2)a1, (1/3)a2, ... of coefficients of the non-constant terms of the integral of the series. 
;     (Since the result has no constant term, it doesn't represent a power series; when we use integrate-series, we will cons on the appropriate constant.)

(define (integrate-series s)
	(stream-map / s (integers-starting-from 1))
)

; (b)  Show how to generate the series for sine and cosine, starting from the facts that the derivative of sine is cosine and the derivative of cosine is the negative of sine:
;      (define cosine-series
;        (cons-stream 1 <??>))
;      (define sine-series
;        (cons-stream 0 <??>))

(define exp-series (cons-stream 1 (integrate-series exp-series)))
(define (scale-stream s n) (cons-stream (* n (stream-car s)) (scale-stream (stream-cdr s) n)))
(define cosine-series (cons-stream 1 (integrate-series (scale-stream sine-series -1))))
(define sine-series (cons-stream 0 (integrate-series cosine-series)))

; ex 3.60
; Complete the definition of the following procedure for multiplying series:
;  (define (mul-series s1 s2)
;     (cons-stream <??> (add-streams <??> <??>)))

(define (mul-series s1 s2)
	(cons-stream
		(* (stream-car s1) (stream-car s2))
		(add-streams 
			(mul-series (stream-cdr s1) s2)
			(scale-stream (stream-cdr s2) (stream-car s1))
		)
	)
)
; (7+2x+3x^2)(4+5x+6x^2) = 28 + 43x + 64x^2 + 27x^3 + 18x^4
(define (test-mul-series)
	(define s1 (cons-stream 7 (cons-stream 2 (cons-stream 3 (cons-stream 0 (cons-stream 0 the-empty-stream)))))))
	(define s2 (cons-stream 4 (cons-stream 5 (cons-stream 6 (cons-stream 0 (cons-stream 0 the-empty-stream)))))))
	(define s1xs2 (mul-series s1 s2))
	(and
		(= 28 (stream-ref s1xs2 0))
		(= 43 (stream-ref s1xs2 1))
		(= 64 (stream-ref s1xs2 2))
		(= 27 (stream-ref s1xs2 3))
		(= 18 (stream-ref s1xs2 4))
	)
)
(test-mul-series)

; ex 3.61
; Let S be a power series (exercise 3.59) whose constant term is 1.
; Suppose we want to find the power series 1/S, that is, the series X such that S*X = 1. 
; Write S = 1 + SR where SR is the part of S after the constant term. Then we can solve for X as follows:
;        S*X = 1
;        (1+SR)*X = 1
;        X + SR*X = 1
;        X = 1 - SR*X
; In other words, X is the power series whose constant term is 1 and whose higher-order terms are given by the negative of SR times X. 
; Use this idea to write a procedure invert-unit-series that computes 1/S for a power series S with constant term 1. 
; You will need to use mul-series from exercise 3.60. 

(define (invert-unit-series s)
	(cons-stream
		1 
		(scale-stream (mul-series (invert-unit-series s) (stream-cdr s)) -1)
	)
)

; ex 3.62
; Use the results of exercises 3.60 and 3.61 to define a procedure div-series that divides two power series. 
; Div-series should work for any two series, provided that the denominator series begins with a nonzero constant term.
; (If the denominator has a zero constant term, then div-series should signal an error.)
; Show how to use div-series together with the result of exercise 3.59 to generate the power series for tangent.
(define (div-series num den)
	(if (= 0 (stream-car den))
		(error "Zero free term in denominator series")
		(mul-series num (invert-unit-series (scale-stream den (/ 1 (stream-car den)))))
	)
)
(define tan-series (div-series sine-series cosine-series))


;; Formulating iterations as streams processing

; ex 3.64
; Write a procedure stream-limit that takes as arguments a stream and a number (the tolerance).
; It should examine the stream until it finds two successive elements that differ in absolute value by less than the tolerance, and return the second of the two elements.
(define (stream-limit s tol)
	(let ((val1 (stream-car s)) (rest (stream-cdr s)))
		(if (< (abs (- val1 (stream-car rest))) tol)
			(stream-car rest)
			(stream-limit (stream-cdr s) tol)
		)
	)
)

; ex 3.65
; Use the series ln 2 = 1 - 1/2 + 1/3 - 1/4 + .. to compute three sequences of approximations to the natural logarithm of 2
(define (ln2-summands n)
	(cons-stream (/ 1.0 n)
		(scale-stream (ln2-summands (+ n 1)) -1)
	)
)
(define ln2-series 
	(partial-sums (ln2-summands 1))
)
(define (euler-transform s)
	(let
		((s0 (stream-ref s 0))
		(s1 (stream-ref s 1))
		(s2 (stream-ref s 2))
		)
		(cons-stream
			(- s2 (/ (square (- s2 s1)) (+ s0 s2 (* -2 s1))) )
			(euler-transform (stream-cdr s))
		)
	)
)
(define (make-tableau transform s)
	(cons-stream s
		(make-tableau transform (transform s))
	)
)


;; Infinite streams os pairs


(define (interleave s1 s2)
	(cons-stream
		(stream-car s1)
		(interleave s2 (stream-cdr s1))
	)
)
(define (pairs s t)
	(cons-stream 
		(list (stream-car s) (stream-car t))
		(interleave
			(stream-map (lambda (x) (list (stream-car s) x )) (stream-cdr s))
			(pairs (stream-cdr s) (stream-cdr t))
		)
	)
)
(define integer-pairs (pairs integers integers))

; ex 3.67
; Modify the pairs procedure so that (pairs integers integers) will produce the stream of all pairs of integers (i,j) (without the condition i < j). 
; Hint: You will need to mix in an additional stream. 
(define (all-pairs s t)
	(cons-stream
		(list (stream-car s) (stream-car t))
		(interleave
			(interleave
				(stream-map (lambda (x) (list (stream-car s) x )) (stream-cdr s))
				(stream-map (lambda (x) (list x (stream-car t) )) (stream-cdr t))
			)
			(all-pairs (stream-cdr s) (stream-cdr t))
		)
	)
)


; ex 3.68
; Louis Reasoner thinks that building a stream of pairs from three parts is unnecessarily complicated. 
; Instead of separating the pair (S0,T0) from the rest of the pairs in the first row, he proposes to work with the whole first row, as follows:
(define (pairs2 s t)
	(interleave
		(stream-map (lambda (x) (list (stream-car s) x)) (stream-cdr t))
		(pairs2 (stream-cdr s) (stream-cdr t))
	)
)
; Does this work? Consider what happens if we evaluate (pairs2 integers integers) using Louis's definition of pairs. 

; Infinite recursion, since interleave must evaluate all arguments before procedure call


; ex 3.69
; Write a procedure triples that takes three infinite streams, S, T, and U, and produces the stream of triples (Si,Tj,Uk) such that i < j < k. 
; Use triples to generate the stream of all Pythagorean triples of positive integers, i.e., the triples (i,j,k) such that i < j and i^2 + j^2 = k^2. 

; interleave lesser number with pair of greater number using pairs procedure
(define (triples s t u)
	(let
		((p (pairs t u)))
		(cons-stream
			(cons (stream-car s) (stream-car p))
			(interleave
				(stream-map (lambda (x) (cons (stream-car s) x)) (stream-cdr p))
				(triples (stream-cdr s) (stream-cdr t) (stream-cdr u))
			)
		)
	)
)
(define (integers-starting-from n)
	(cons-stream n (integers-starting-from (+ n 1)))
)
(define integers (integers-starting-from 1))
(define integer-triples (triples integers integers integers))
(define pythag-triples 
	(stream-filter
		(lambda (x) 
			(= 
				(+ (square (car x)) (square (cadr x)))
				(square (caddr x))
			)
		)
		integer-triples
	)
)

; ex 3.70
; Write a procedure merge-weighted that is like merge, except that merge-weighted takes an additional argument weight, 
; which is a procedure that computes the weight of a pair, and is used to determine the order in which elements should appear in the resulting merged stream.
(define (merge-weighted s1 s2 weight)
	(cond
		((stream-null? s1) s2)
		((stream-null? s2) s1)
		(else (let
			( (s1car (stream-car s1))
			  (s2car (stream-car s2)) )
			(cond
				((< (weight s1car) (weight s2car)) (cons-stream s1car (merge-weighted (stream-cdr s1) s2 weight)))
				((> (weight s1car) (weight s2car)) (cons-stream s2car (merge-weighted s1 (stream-cdr s2) weight)))
				(else (cons-stream s1car (cons-stream s2car (merge-weighted (stream-cdr s1) (stream-cdr s2) weight)) ))
			)
		))
	)
)

; Using this, generalize pairs to a procedure weighted-pairs that takes two streams, 
; together with a procedure that computes a weighting function, and generates the stream of pairs, ordered according to weight.
(define (weighted-pairs s t weight)
	(cons-stream
		(list (stream-car s) (stream-car t))
		(merge-weighted
			(stream-map (lambda (x) (list (stream-car s) x)) (stream-cdr t))
			(weighted-pairs (stream-cdr s) (stream-cdr t) weight)
			weight
		)
	)
)

; Use your procedure to generate
; (a) the stream of all pairs of positive integers (i,j) with i < j ordered according to the sum i + j
; (b) the stream of all pairs of positive integers (i,j) with i < j, where neither i nor j is divisible by 2, 3, or 5, and the pairs are ordered according to the sum 2 i + 3 j + 5 i j. 

; (a)
(define s1 (weighted-pairs integers integers (lambda (x) (+ (car x) (cadr x)))))

; (b)
(define (not-divisible x) (not (or (= (remainder x 2) 0) (= (remainder x 3) 0) (= (remainder x 5) 0)) ))
(define s2 (stream-filter (lambda (x) (not-divisible x)) integers))
(define (weight-proc x y) (+ (* 2 x) (* 3 y) (* 5 x y)))
(define s3 (weighted-pairs s2 s2 (lambda (x) (weight-proc (car x) (cadr x) ))))



; ex 3.71
; Numbers that can be expressed as the sum of two cubes in more than one way are sometimes called Ramanujan numbers, in honor of the mathematician Srinivasa Ramanujan.
; Ordered streams of pairs provide an elegant solution to the problem of computing these numbers. 
; To find a number that can be written as the sum of two cubes in two different ways, we need only generate the stream of pairs of integers (i,j) weighted according to the sum i^3 + j^3 (see exercise 3.70), 
; then search the stream for two consecutive pairs with the same weight. 
; Write a procedure to generate the Ramanujan numbers. The first such number is 1,729. What are the next five?
(define (weight-taxicab x) (+ (cube (car x)) (cube (cadr x))))
(define s-cubes (weighted-pairs integers integers weight-taxicab))
(define (traverse-streams-2 s1 s2 weight)
	(cond
		((empty-stream? s1) the-empty-stream)
		((empty-stream? s2) the-empty-stream)
		(else
			(let ( (s1car (stream-car s1)) (s2car (stream-car s2)) )
				(if (= (weight s1car) (weight s2car)) 
					(cons-stream s1car (traverse-streams-2 (stream-cdr s1) (stream-cdr s2) weight))
					(traverse-streams-2 (stream-cdr s1) (stream-cdr s2) weight)
				)
			)
		)
	)
)
(define s-taxicab (traverse-streams-2 s-cubes (stream-cdr s-cubes) weight-taxicab))
(define w-taxicab (stream-map (lambda (x) (weight-taxicab x)) s-taxicab))

; ex 3.72
; In a similar way to exercise 3.71 generate a stream of all numbers that can be written as the sum of two squares in three different ways (showing how they can be so written).
(define (weight-square x) (+ (square (car x)) (square (cadr x))))
(define s-squares (weighted-pairs integers integers weight-square))
(define (traverse-streams-3 s1 s2 s3)
	(cond
		((empty-stream? s1) the-empty-stream)
		((empty-stream? s2) the-empty-stream)
		((empty-stream? s3) the-empty-stream)
		(else
			(let ( (s1car (stream-car s1)) (s2car (stream-car s2)) (s3car (stream-car s3)) )
				(display s1car) (display (weight s1car)) (newline)
				(display s2car) (display (weight s2car)) (newline)
				(display s3car) (display (weight s3car)) (newline) (newline)
				(if (= (weight-square s1car) (weight-square s2car) (weight-square s3car))
					(cons-stream s1car (traverse-streams-3 (stream-cdr s1) (stream-cdr s2) (stream-cdr s3)))
					(traverse-streams-3 (stream-cdr s1) (stream-cdr s2) (stream-cdr s3))
				)
			)
		)
	)
)
(define two-squares-three-ways (traverse-streams-3 s-squares (stream-cdr s-squares) (stream-cdr (stream-cdr s-squares))))
(define w-squares (stream-map (lambda (x) (weight-square x)) two-squares-three-ways))



;;; Streams as signals

; ex 3.73
(define (integral integrand initial-value dt)
	(define int
		(cons-stream initial-value
			(add-streams (scale-stream integrand dt) int)
		)
	)
	int
)
(define (RC r c dt)
	(define (proc i v0)
		(add-streams (integral (scale-stream i (/ 1. c)) v0 dt) (scale-stream i r))
	)
	proc
)

; ex 3.74
;?
(define zero-crossings
	(stream-map sign-change-detector sense-data (cons-stream 0 sense-data)))

; ex 3.75
; don't understand the problem formulation
(define (make-zero-crossings input-stream last-value)
	(let ((avpt (/ (+ (stream-car input-stream) last-value) 2)))
		(cons-stream (sign-change-detector avpt last-value)
			(make-zero-crossings (stream-cdr input-stream) avpt)
		)
	)
)

; ex 3.76
(define (smooth s)
	(define (iter s1 val)
		(cons-stream (/ (+ val (stream-car s1)) 2) (iter (stream-cdr s1) (stream-car s1)) )
	)
	(iter (stream-cdr s) (stream-car s))
)

;not ok:
(define (smooth s)
	(stream-map (lambda (x y) (/ (x y) 2)) (stream-cdr s) (stream-car s))
)
;?
(define zero-crossings
	(stream-map sign-change-detector (smooth sense-data) (cons-stream 0 (smooth sense-data))))


;; Streams and delayed evaluation

; ex 3.77
(define (solve f y0 dt)
	(define y (integral (delay dy) y0 dt))
	(define dy (stream-map f y))
y)

(define (integral delayed-integrand initial-value dt)
	(cons-stream initial-value
		(let
			((integrand (force delayed-integrand)))
			(if (stream-null? integrand)
				the-empty-stream
				(integral (delay (stream-cdr integrand))
					(+ (* dt (stream-car integrand)) initial-value)
					dt
				)
			)
		)
	)
)
(stream-ref (solve (lambda (y) y) 1 0.001) 1000)

; ex 3.78
(define (solve-2nd a b dt y0 dy0)
	(define y (integral (delay dy) y0 dt))
	(define dy (integral (delay ddy) dy0 dt))
	(define ddy (add-streams (scale-stream dy a) (scale-stream y b)))
	y
)

(stream-ref (solve-2nd 1 0 0.001 1 1) 1000)
(stream-ref (solve-2nd 0 1 0.001 1 1) 1000)

; ex 3.79
(define (solve-2nd f dt y0 dy0)
	(define y (integral (delay dy) y0 dt))
	(define dy (integral (delay ddy) dy0 dt))
	(define ddy (stream-map f dy y))
	y
)

(stream-ref (solve-2nd (lambda (dy y) 1) 0.001 0 0) 1000)

; ex 3.80
(define (RLC r l c dt)
	(define (proc vc0 il0)
		(define il (integral (delay dil) il0 dt))
		(define vc (integral (delay dvc) vc0 dt))
		(define dil
			(add-streams
				(scale-stream vc (/ 1. l))
				(scale-stream il (- (/ r l) ))
			)
		)
		(define dvc (scale-stream il (/ -1. c)))
		(cons vc il)
	)
	proc
)

(define rlc1 (RLC 1. 1. 0.2 0.1))
(define rlc-test (rlc1 10. 0.))

; ex 3.81
; Exercise 3.6 discussed generalizing the random-number generator to allow one to reset the random-number sequence so as to produce repeatable sequences of ``random'' numbers. 
; Produce a stream formulation of this same generator that operates on an input stream of requests to generate 
; a new random number or to reset the sequence to a specified value and that produces the desired stream of random numbers. 
; Don't use assignment in your solution.
(define (rand s)
	(define (rand-update arg)
		(define maxval 10000)
		(remainder (+ arg (random maxval)) maxval)
	)
	(define (dispatch s last-value)
		(cond	( (eq? (stream-car s) 'generate)
					(let ( (new-value (rand-update last-value)) )
						(cons-stream last-value (dispatch (stream-cdr s) new-value) )
					)
				)
				( (number? (stream-car s))
					(let ( (last-value (stream-car s)) (new-value (rand-update (stream-car s))) )
						(cons-stream last-value (dispatch (stream-cdr s) new-value))
					)
				)
				( else (error "Unsupported operation."))
		)
	)
	(dispatch s 0)
)

; ex 3.82
; Redo exercise 3.5 on Monte Carlo integration in terms of streams. 
; The stream version of estimate-integral will not have an argument telling how many trials to perform. 
; Instead, it will produce a stream of estimates based on successively more trials.

(define (monte-carlo trials experiment)
	(define (iter success total)
		(cond	( (= 0 total) (/ success trials)) 
				( (experiment) (iter (+ success 1) (- total 1) ) )
				( else (iter success (- total 1)) )
		)
	)
	(iter 0 trials)
)
(define (monte-carlo experiment-stream passed failed)
	(define (next passed failed)
		(cons-stream 
			(/ passed (+ passed failed))
			(monte-carlo (stream-cdr experiment-stream) passed failed)
		)
	)
	(if (apply (stream-car experiment-stream))
		(next (+ passed 1) failed)
		(next passed (+ failed 1))
	)
)
(define (random-in-range low high)
	(+ low (random (- high low)))
)
(define (exp-stream proc) (cons-stream proc (exp-stream proc)))
(define (estimate-integral pred x1 x2 y1 y2 trials)
	(let
		((rect-area (* (- x2 x1) (- y2 y1)) )
		(experiment (lambda () (pred (random-in-range x1 x2) (random-in-range y1 y2))) ))
		(stream-map (lambda (x) (* x rect-area))
			(monte-carlo (exp-stream experiment) 0 0)
		)
	)
)
(define (unit-circle? x y)
	(< (+ (* x x) (* y y)) 1) )
(estimate-integral unit-circle? -1.0 1.0 -1.0 1.0 100000)
