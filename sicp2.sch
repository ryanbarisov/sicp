(define (add-rat x y)
	(make-rat
		(+ (* (numer x) (denom y)) (* (numer y) (denom x)))
		(* (denom x) (denom y))
	)
)
(define (sub-rat x y)
	(make-rat
		(- (* (numer x) (denom y)) (* (numer y) (denom x)))
		(* (denom x) (denom y))
	)
)
(define (mul-rat x y)
	(make-rat
		(* (numer x) (numer y))
		(* (denom x) (denom y))
	)
)
(define (div-rat x y)
	(make-rat
		(* (numer x) (denom y))
		(* (denom x) (numer y))
	)
)
(define (equal-rat? x y) (= (* (numer x) (denom y)) (* (numer y) (denom x))))
(define (make-rat n d) (cons n d))
(define (numer x) (car x))
(define (denom x) (cdr x))
(define (print-rat x)
	(newline)
	(display (numer x))
	(display "/")
	(display (denom x))
)
(define (gcd a b)
	(cond	((> b a) (gcd b a))
		((= b 0) a)
		(else (gcd b (remainder a b)))
	)
)

; ex 2.1 - handle positive and negative arguments
(define (make-rat n d)
	(let ((div (abs (gcd n d))))
		(if (< (* n d) 0)
			(cons (/ (- (abs n)) div) (/ (abs d) div))
			(cons (/ (abs n) div) (/ (abs d) div))
		)
	)
)
; compute gcd at selection time rather than at the construction time
;(define (make-rat n d) (cons n d))
;(define (numer x)
;	(let ((g (gcd (car x) (cdr x))))
;		(/ (car x) g)
;	)
;)
;(define (denom x)
;	(let ((g (gcd (car x) (cdr x))))
;		(/ (cdr x) g)
;	)
;)

; ex 2.2
(define (make-point x y) (cons x y))
(define (x-point p) (car p))
(define (y-point p) (cdr p))
(define (print-point p)
	(newline)
	(display (string "(" (x-point p) "," (y-point p) ")" ))
)
(define (make-segment start-point end-point) (cons start-point end-point))
(define (start-segment seg) (car seg))
(define (end-segment seg) (cdr seg))
(define (midpoint-segment seg)
	(make-point
		(/ (+ (x-point (start-segment seg)) (x-point (end-segment seg))) 2)
		(/ (+ (y-point (start-segment seg)) (y-point (end-segment seg))) 2)
	)
)

; ex 2.3
;;; rectangle by two corner points
(define (make-rectangle corner-point opposite-point) (cons corner-point opposite-point))
(define (corner-point-rectangle rect) (car rect))
(define (opposite-point-rectangle rect) (cdr rect))
(define (horizontal-length rect) (abs (- (x-point (upper-right-point-rectangle rect)) (x-point (lower-left-point-rectangle rect)) )))
(define (vertical-length rect) (abs (- (y-point (upper-right-point-rectangle rect)) (y-point (lower-left-point-rectangle rect)) )))
;;; rectangle by corner point and its dimensions
(define (make-rectangle lower-left-point xlength ylength) (cons lower-left-point (cons xlength ylength)))
(define (corner-point-rectangle rect) (car rect))
(define (opposite-point-rectangle rect) (make-point
	(+ (x-point (corner-point-rectangle rect)) xlength)
	(+ (y-point (corner-point-rectangle rect)) ylength))
)
(define (horizontal-length rect) (abs xlength))
(define (vertical-length rect) (abs ylength))
;;;
(define (rectangle-perimeter rect) (* 2 (+ (horizontal-length rect) (vertical-length rect))))
(define (rectangle-area rect) (* (horizontal-length rect) (vertical-length rect)))

;(define (cons x y)
;	(define (dispatch m)
;		(cons	((= m 0) x)
;			((= m 1) y)
;			(else (error "Argument not 0 or 1 -- CONS" m))
;		)
;	)
;	dispatch
;)
;(define (car z) (z 0))
;(define (cdr x) (z 1))

; ex 2.4 - pair (cons x y) represented using lambdas
(define (cons x y) (lambda (m) (m x y)))
(define (car z) (z (lambda (p q) p)))
(define (cdr z) (z (lambda (p q) q)))

; ex 2.5 - pair (cons a b) represented as 2^a * 3^b
(define (cons a b) (* (expt 2 a) (expt 3 b)))
(define (car p)
	(define (iter3 result)
		(if (= (remainder result 3) 0)
			(iter3 (/ result 3))
			(iter2 result 0)
		)
	)
	(define (iter2 num a)
		(if (= (remainder num 2) 0)
			(iter2 (/ num 2) (+ a 1))
			a
		)
	)
	(iter3 p)
)
(define (cdr p)
	(define (iter2 result)
		(if (= (remainder result 2) 0)
			(iter2 (/ result 2))
			(iter3 result 0)
		)
	)
	(define (iter3 num b)
		(if (= (remainder num 3) 0)
			(iter3 (/ num 3) (+ b 1))
			b
		)
	)
	(iter2 p)
)

; ex 2.6 - Church numerals, numeral means amount of times input function to be applied to its argument
(define zero (lambda (f) (lambda (x) x)) )
(define (add-1 n)
	(lambda (f) (lambda (x) (f ((n f) x)) ) )
)
; ((zero f) x) -> ((lambda (x) x) x) -> x
; (add-1 zero) -> (lambda (f) (lambda (x) (f ((zero f) x)) ) ) -> (lambda (f) (lambda (x) (f x) ) )
(define one (lambda (f) (lambda (x) (f x))))

; ((one f) x) -> ((lambda (x) (f x)) x) -> (f x)
; (add-1 one) -> (lambda (f) (lambda (x) (f ((one f) x)) ) ) -> (lambda (f) (lambda (x) (f (f x)) ) )
(define two (lambda (f) (lambda (x) (f (f x)) ) ))

; addition procedure of two Church numerals
(define (add2 m n) (lambda (f) (lambda (x) ((m f) ((n f) x)) )) )

;;; interval arithmetic
; interval represents range of values of inexact quantity
; ex 2.7 - interval implementation
(define (make-interval lower upper) (cons lower upper))
(define (lower-bound intvl) (car intvl))
(define (upper-bound intvl) (cdr intvl))
(define (add-interval x y)
	(make-interval (+ (lower-bound x) (lower-bound y))
			(+ (upper-bound x) (upper-bound y))
	)
)

; ex 2.8 - difference of intervals
(define (sub-interval x y)
	(make-interval
		(- (lower-bound x) (upper-bound y))
		(- (upper-bound x) (lower-bound y))
	)
)
(define (mul-interval x y)
	(let (	(p1 (* (lower-bound x) (lower-bound y)))
		(p2 (* (lower-bound x) (upper-bound y)))
		(p3 (* (upper-bound x) (upper-bound y)))
		(p4 (* (upper-bound x) (upper-bound y))))
		(make-interval (min p1 p2 p3 p4)
				(max p1 p2 p3 p4)
		)
	)
)
; ex 2.9 - width of the interval product is not a function of widths of the intervals only
; take three different intervals with the same width
(define i1 (make-interval 0 1))
(define i2 (make-interval -1 0))
(define i3 (make-interval 1 2))

; widths of all three possible multiplications are not the same:
(define (width intvl) (/ (- (upper-bound intvl) (lower-bound intvl)) 2) )
(= (width (mul-interval i1 i2)) (width (mul-interval i1 i3))) ; #f

; ex 2.10 - corrected implementation of intervals division
(define (div-interval x y)
	(if (< (* (upper-bound y) (lower-bound y)) 0)
		(error "Divisor interval spans zero, result is undefined")
		(mul-interval
			x
			(make-interval
				(/ 1.0 (upper-bound y))
				(/ 1.0 (lower-bound y))
			)
		)
	)
)

; ex 2.11 - correct implementation of intervals multiplication
(define (mul-interval x y)
	(let
		(	(x0 (lower-bound x))
			(x1 (upper-bound x))
			(y0 (lower-bound y))
			(y1 (upper-bound y))
		)
		(cond
			((< x1 0)
				(cond	((< y1 0)	(make-interval (* x1 y1) (* x0 y0)))
					((> y0 0)	(make-interval (* x0 y1) (* x1 y0)))
					(else		(make-interval (* x0 y1) (* x0 y0)))
				)
			)
			((> x0 0)
				(cond	((< y1 0)	(make-interval (* x1 y0) (* x0 y1)))
					((> y0 0)	(make-interval (* x0 y0) (* x1 y1)))
					(else		(make-interval (* x1 y0) (* x1 y1)))
				)
			)
			(else
				(cond 	((< y1 0)	(make-interval (* x1 y0) (* x0 y0)))
					((> y0 0)	(make-interval (* x0 y1) (* x1 y1)))
					(else		
							(make-interval
								(min (* x0 y1) (* x1 y0))
								(max (* x0 y0) (* x1 y1))
							)
					)
				)
			)
		)
	)
)
(define (make-center-width c w) (make-interval (- c w) (+ c w)))
(define (center i)
	(/ (+ (upper-bound i) (lower-bound i)) 2.)
)
(define (width intvl)
	(/ (- (upper-bound intvl) (lower-bound intvl)) 2.0)
)

; ex 2.12 - interval by center and percent
(define (make-center-percent c p)
	(define w (* c (/ p 100.0)))
	(make-interval (- c w) (+ c w))
)
(define (percent i)
	(* 100 (/ (width i) (abs (center i))))
)
;;;

(define (list-ref lst n)
	(if (= n 0) (car lst)
		(list-ref (cdr lst) (- n 1))
	)
)
(define (length lst)
	(if (null? lst) 0
		(+ 1 (length (cdr lst)))
	)
)
(define (length lst)
	(define (iter lst n)
		(if (null? lst) n
			(iter (cdr lst) (+ n 1))
		)
	)
	(iter lst 0)
)
(define (append list1 list2)
	(if (null? list1)
		list2
		(cons (car list1) (append (cdr list1) list2))
	)
)

; ex 2.17 - list that contains only the last element of a given list
(define (last-pair lst)
	(let ((tail (cdr lst)))
		(if (null? tail)
			lst
			(last-pair tail)
		)
	)
)

; ex 2.18
(define (reverse lst)
	(define (iter l chain)
		(if (null? l)
			chain
			(iter (cdr l) (cons (car l) chain))
		)
	)
	(iter lst (list ))
)

; ex 2.19 - change-counting program using list of coin currency
(define us-coins (list 50 25 10 5 1))
(define uk-coins (list 100 50 20 10 5 2 1 0.5))
; answer is the amount of ways to exchange the sum by the given set of values, answer doesn't depend on order in that set
(define (cc amount coin-values)
	(cond ((= amount 0) 1)
		((or (< amount 0) (no-more? coin-values)) 0)
		(else (+ (cc amount (except-first-denomination coin-values))
				(cc (- amount (first-denomination coin-values)) coin-values))
		)
	)
)
(define (first-denomination coin-values) (car coin-values))
(define (except-first-denomination coin-values) (cdr coin-values))
(define (no-more? coin-values) (null? coin-values))

; ex 2.20 - same-parity takes one or more integers and returns a list of all the arguments that have the same even-odd parity as the first argument.
(define (same-parity p . args)
	(define parity (remainder p 2))
	(define (filter e) (= (remainder e 2) parity))
	(define (accum lst)
		(cond	((null? lst) '())
			((filter (car lst)) (cons (car lst) (accum (cdr lst))))
			(else (accum (cdr lst)))
		)
	)
	(accum (cons p args))
)


(define (map proc items)
	(if (null? items)
		'()
		(cons (proc (car items)) (map proc (cdr items)))
	)
)
(define (scale-list times items)
	(map (lambda (x) (* times x)) items)
)

; ex 2.21
(define (square-list items) (map square items))
(define (square-list items)
	(if (null? items)
		'()
		(cons (square (car items)) (square-list (cdr items)))
	)
)

; ex 2.22
(define (square-list items)
	(define (iter things answer)
		(if (null? things)
			'()
			(cons (square (car things)) (iter (cdr things) answer))
		)
	)
	(iter items '())
)

; ex 2.23
(define (for-each action items)
	(cond ((null? items) #t)
		(else
			(action (car items))
			(for-each action (cdr items))
		)
	)
)

; ex 2.25 - pick 7 from lists using car and cdr
(define lst1 (list 1 3 (list 5 7) 9))
(cadr (caddr lst1))

(define lst2 (list (list 7)))
(caar lst2)

(define lst3 (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7)))))))
(cadr (cadr (cadr (cadr (cadr (cadr lst3))))))

; ex 2.26
(define x (list 1 2 3))
(define y (list 4 5 6))
(append x y) ; (1 2 3 4 5 6)
(cons x y) ; ( (1 2 3) 4 5 6 )
(list x y) ; ( (1 2 3) (4 5 6) )

; iterative process
(define (count-leaves items)
	(define (iter lst result)	
		(cond	((null? lst) result)
			((list? (car lst)) (iter (cdr lst) (+ result (iter (car lst) 0))) )
			(else (iter (cdr lst) (+ result 1)) )
		)
	)
	(iter items 0)
)
;recursive process
(define (count-leaves items)
	(cond	((null? items) 0)
		((not (pair? items)) 1)
		(else (+ (count-leaves (car items)) (count-leaves (cdr items))) )
	)
)

; ex 2.27 - all elements and sublists are reversed
(define (deep-reverse x)
	(define (iter li res)
		(cond   ((null? li) res)
			((list? (car li)) (iter (cdr li) (cons (iter (car li) '()) res)))
			(else (iter (cdr li) (cons (car li) res)))
		)
	)
	(iter x '())
)

; ex 2.28 - fringe takes as argument a tree (represented as a list) and returns a list whose elements are all the leaves of the tree arranged in left-to-right order
(define (fringe x)
	(cond	((null? x) '())
		((not (list? (car x))) (cons (car x) (fringe (cdr x))))
		(else (append (fringe (car x)) (fringe (cdr x))))
	)
)

; ex 2.29
(define (make-mobile left right) (list left right))
(define (make-branch length structure) (list length structure))
(define is-mobile? list?)
; 2.29(a)
(define (left-branch mobile) (car mobile))
(define (right-branch mobile) (cadr mobile))
(define (branch-length branch) (car branch))
(define (branch-structure branch) (cadr branch))
; 2.29(b)
(define (total-weight mob)
	(define (branch-weight br)
		(let ((st (branch-structure br)))
			(if (is-mobile? st)
				(total-weight st)
				st
			)
		)
	)
	(+	(branch-weight (left-branch mob))
		(branch-weight (right-branch mob))
	)
)
; 2.29(c)
(define (balanced? mob)
	(define (mobile-torque m)
		(+	(branch-torque (left-branch m))
			(branch-torque (right-branch m))
		)
	)
	(define (branch-torque br)
		(let ((st  (branch-structure br))
				(len (branch-length br)))
			(if (is-mobile? st)
				(* (mobile-torque st) len)
				(* st len)
			)
		)
	)
	(=	(branch-torque (left-branch mob))
			(branch-torque (right-branch mob))
	)
)
; 2.29(d)
(define (make-mobile left right) (cons left right))
(define (make-branch length structure) (cons length structure))
; updated selectors:
(define is-mobile? pair?)
(define (right-branch mobile) (cdr mobile))
(define (branch-structure branch) (cdr branch))

(define (scale-tree tree factor)
	(cond ((null? tree) '())
		((not (pair? tree)) (* tree factor))
		(else (cons (scale-tree (car tree) factor)
				(scale-tree (cdr tree) factor)))
	)
)
(define (scale-tree tree factor)
	(map (lambda (sub-tree)
		(if (pair? sub-tree)
			(scale-tree sub-tree factor)
			(* sub-tree factor)
		))
		tree
	)
)

; ex 2.30 - square each element in a tree represented by list
(define (square-tree tree)
	(cond ((null? tree) '())
		((not (pair? tree)) (square tree))
		(else (cons (square-tree (car tree))
			(square-tree (cdr tree))
			)
		)
	)
)
(define (square-tree tree)
	(map (lambda (sub-tree)
		(if (not (pair? sub-tree))
			(square sub-tree)
			(square-tree sub-tree)
		)
	)
	tree
	)
)

; ex 2.31 - map for trees represented using lists
(define (tree-map procedure tree)
	(map (lambda (sub-tree)
		(if (pair? sub-tree)
			(tree-map procedure sub-tree)
			(procedure sub-tree)
		))
		tree
	)
)

; ex 2.32 - generate a set of subsets for a given set
(define (subsets s)
	(if (null? s)
		(list '())
		(let ((rest (subsets (cdr s))))
			(append rest (map (lambda (x) (cons (car s) x)) rest))
		)
	)
)
; any set has an empty set as its subset -> (if (null? s) (list '()) ...
; suppose we have a set of subsets for a set without current element (head of list) -> (let ((rest (subsets (cdr s)))) ... )
; subsets with this element are appended to this set (append rest ...)
; in order to form subsets with this element already formed subsets are used:
;   to each already formed subset we append current element -> (map (lambda (x) (cons (car s) x)))
; since x above is the subset (list), order should be (cons (car s) x)


(define (filter predicate sequence)
	(cond
		((null? sequence) '())
		((predicate (car sequence))
			(cons (car sequence) (filter predicate (cdr sequence)))
		)
		(else (filter predicate (cdr sequence)))
	)
)
(define (filter predicate sequence)
	(accumulate (lambda (x y) (if (predicate x) (cons x y) y)) '() sequence)
)
(define (map proc sequence)
	(accumulate (lambda (x y) (cons (proc x) y)) '() sequence)
)
(define (accumulate op initial sequence)
	(if (null? sequence)
		initial
		(op (car sequence) (accumulate op initial (cdr sequence)))
	)
)
(define (enumerate-interval low high)
	(if (> low high)
		'()
		(cons low (enumerate-interval (+ low 1) high))
	)
)
(define (append list1 list2)
	(if (null? list1)
		list2
		(cons (car list1) (append (cdr list1) list2))
	)
)
(define (enumerate-tree x)
	(cond	((null? x) '())
		((not (pair? (car x))) (cons (car x) (enumerate-tree (cdr x))))
		(else (append (enumerate-tree (car x)) (enumerate-tree (cdr x))))
	)
)
(define (append list1 list2) (accumulate cons list2 list1))
(define (enumerate-tree t)
	(accumulate (lambda (x y) (if (pair? x) (append (enumerate-tree x) y) (cons x y)) ) '() t)
)
(define (sum-odd-squares tree)	
	(accumulate + 0
		(map square
			(filter odd?
				(enumerate-tree tree)
			)
		)
	)
)
(define (even-fibs n)
	(accumulate cons '()
		(filter even?
			(map fib
				(enumerate-interval 0 n)
			)
		)
	)
)
(define (list-fib-squares n)
	(accumulate cons '()
		(map square
			(map fib
				(enumerate-interval 0 n)
			)
		)
	)
)

; ex 2.33 - list manipulations using accumulations
(define (map p sequence)
	(accumulate (lambda (x y) (cons (p x) y)) '() sequence))
(define (append seq1 seq2)
	(accumulate cons seq2 seq1))
(define (length sequence)
	(accumulate (lambda (x y) (+ 1 y)) 0 sequence))

; ex 2.34 - evaluate polynomial at x using Horner rule and accumulations
; f(x) = a_n*x^n + a_(n-1)*x^(n-1) + ... + a_1*x + a_0 =
;      =(...(a_n*x + a_(n-1))*x   + ... + a_1)*x + a_0
; b <- 0
; b <- b*x + a_n
; b <- b*x + a_(n-1)
; ...
; b <- b*x + a_0
; f(x) = b
(define (horner-eval x coefficient-sequence)
	(accumulate (lambda (this-coeff higher-terms) (+ (* x higher-terms) this-coeff))
		0
		coefficient-sequence)
)

; ex 2.35 - count-leaves using accumulate
(define (count-leaves t)
	(accumulate + 0
		(map (lambda (x) (if (list? x) (count-leaves x) 1)) t))
)

(define (map proc seq)
	(if (null? seq) '()
		(cons (proc (car seq)) (map proc (cdr seq)))
	)
)
(define (count-leaves t)
	(define (iter tree amount)
		(cond	((null? tree) amount)
			((pair? (car tree))
				(iter (cdr tree) (+ amount (iter (car tree) 0)) )
			)
			(else
				(iter (cdr tree) (+ amount 1))
			)
		)
	)
	(iter t 0)
)
(define (accumulate op initial sequence)
	(if (null? sequence)
		initial
		(op (car sequence) (accumulate op initial (cdr sequence)))
	)
)

; ex 2.36 - accumulation for sequence of sequences
(define (accumulate-n op init seqs)
	(if (null? (car seqs)) '()
		(cons (accumulate op init (map car seqs))
			(accumulate-n op init (map cdr seqs))
		)
	)
)

(define (map2 op . args) (accumulate-n op 0 args))

; ex 2.37 - build up matrix-vector and matrix-matrix products using dot-product and accumulations
(define (dot-product v w) (accumulate + 0 (map * v w)))
(define (matrix-*-vector m v) (map (lambda (row) (dot-product row v)) m))
(define (transpose mat) (accumulate-n cons '() mat))
(define (matrix-*-matrix m n)
	(let ((cols (transpose n)))
		(map (lambda (row) (matrix-*-vector cols row) ) m)
	)
)


(define (fold-right op initial sequence)
	(if (null? sequence)
		initial
		(op (car sequence) (fold-right op initial (cdr sequence)))
	)
)
(define (fold-left op initial sequence)
	(define (iter result rest)
		(if (null? rest)
			result
			(iter (op result (car rest)) (cdr rest))
		)
	)
	(iter initial sequence)
)

; ex 2.38
(fold-right / 1 (list 1 2 3))
; (/ 1 (fold-right / 1 (list 2 3)))
; (/ 1 (/ 2 (fold-right / 1 (list 3))))
; (/ 1 (/ 2 (/ 3 (fold-right / 1 '()))))
; (/ 1 (/ 2 (/ 3 1))) -> (/ 1 (/ 2 3)) -> (/ 1 2/3) -> 3/2
(fold-left / 1 (list 1 2 3))
; (iter 1 (list 1 2 3))
; (iter (/ 1 1) (list 2 3)) -> (iter 1 (list 2 3))
; (iter (/ 1 2) (list 3)) -> (iter 1/2 (list 3))
; (iter (/ 1/2 3) '()) -> (iter 1/6 '()) -> 1/6
(fold-right list '() (list 1 2 3))
; (list 1 (fold-right list '() (list 2 3)))
; (list 1 (list 2 (fold-right list '() (list 3))))
; (list 1 (list 2 (list 3 (fold-right list '() '()))))
; (list 1 (list 2 (list 3 '()))) -> (1 (2 (3 ())))
(fold-left list '() (list 1 2 3))
; (iter '() (list 1 2 3))
; (iter (list '()  1)        (list 2 3)) -> (iter (() 1) (list 2 3))
; (iter (list (() 1)  2)     (list 3))   -> (iter ((() 1) 2) (list 3))
; (iter (list ((() 1) 2)  3) '())        -> (iter (((() 1) 2) 3) '()) -> (((() 1) 2) 3)

; ex 2.39 - reverse list using fold functions
(define (reverse sequence) (fold-left (lambda (x y) (cons y x)) '() sequence))
(define (reverse sequence) (fold-right (lambda (x y) (cons y (reverse x))) '() sequence))

; nested mappings as a way to implement computations usually employing nested loops
(define (enumerate-interval a b) (if (> a b) '() (cons a (enumerate-interval (+ a 1) b))))
; given a positive integer n, find all ordered pairs of distinct positive integers i and j, where 1 < j < i < n, such that i + j is prime.
; (1) generate ordered pairs 1 <= j < i <= n
; for each 1 <= i <= n ( i \in (enumerate-interval 1 n) ) compose a list of pairs (i j), where 1 <= j < i
; resulting list of lists of pairs (i j) is accumulated (flattened) into single list of pairs (i j), 1 <= j < i <= n
(define (ordered-pairs n)
	(accumulate append '()
		(map (lambda (i)
			(map (lambda (j) (list i j)) (enumerate-interval 1 (- i 1)))
			) (enumerate-interval 1 n)
		)
	)
)
; (define (accumulate op null-value lst) (if (null? lst) null-value (op (car lst) (accumulate op null-value (cdr lst)))))

; combination of mapping and accumulating with append is sometimes called flatmap
(define (flatmap proc seq) (accumulate append '() (map proc seq)))
; (map proc seq) should be a list of lists, example:
; (accumulate append '() (list (list 1 2) (list 3 4)))
; (append (list 1 2) (accumulate append '() (list (list 3 4))))
; (append (list 1 2) (append (list 3 4) (accumulate append '() '())))
; (append (list 1 2) (append (list 3 4) '()))
; (append (list 1 2) (list 3 4)) -> (list 1 2 3 4)

; rewrite ordered-pairs using flatmap
(flatmap
	(lambda (i)
		(map (lambda (j) (list i j))
			(enumerate-interval 1 (- i 1))
		)
	)
	(enumerate-interval 1 n)
)
; (2) (list i j) -> (list i j (+ i j))
(define (make-pair-sum pair) (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))
; (3) primality check (taken from sec. 1)
(define (prime-sum? p) (prime? (+ (car p) (cadr p))))
(define (prime? n) (= (smallest-divisor n) n))
(define (smallest-divisor n) (find-divisor n 2))
(define (find-divisor n test-divisor)
	(define (next n)
		(if (= n 2) 3
			(+ n 2))
	)
	(cond ((> (square test-divisor) n) n)
		((divides? test-divisor n) test-divisor)
		(else (find-divisor n (next test-divisor)))
	)
)
(define (divides? n p) (= (remainder p n) 0))
; (4) given a positive integer n, find all ordered pairs of distinct positive integers i and j, where 1 < j < i < n, such that i + j is prime.
(define (prime-sum-pairs n)
	(map make-pair-sum
		(filter prime-sum?
			(flatmap
				(lambda (i)
					(map (lambda (j) (list i j))
						(enumerate-interval 1 (- i 1))
					)
				)
			(enumerate-interval 1 n))
		)
	)
)

; generate all permutations of the given set (represented as list, output - list of lists)
; for each item in s, recursively generate the sequence of permutations of S-x, and adjoin x at the front of each one
; generate s-x for a given element
(define (remove x s) (filter (lambda (y) (not (equal? y x))) s))
; (cons 1 (list 2 3)) -> (cons 1 (cons 2 (cons 3 '()))) -> (list 1 2 3)
; (cons x lst) -> (list x (car lst) (cadr lst) ... )
(define (permutations s)
	(if (null? s)
		(list '())
		(flatmap
			(lambda (x)					; for each x in s
				(map (lambda (sx) (cons x sx))		; append x at front of sx, which is an element
					(permutations (remove x s))	; of recursively generated list of permutations
				)
			) s
		)
	)
)
; (permutations '()) -> (list '()) -> (())
; (permutations (list 1)) -> (flatmap (lambda (x) (map (lambda (sx) (cons x sx)) (permutations (remove x (list 1)))) (list 1)))
; -> (append (cons 1 (permutations '())) '()) -> (cons 1 (list '())) -> (list (list 1)) -> ((1))
; (permutations (list 1 2) -> (flatmap (lambda (x) (map (lambda (sx) (cons x sx)) (permutations (remove x (list 1 2))))) (list 1 2))
; -> (append (map (lambda (sx) (cons 1 sx)) (permutations (list 2))) (append (map (lambda (sx) (cons 2 sx)) (permutations (list 1)) '()))
; -> (append (map (lambda (sx) (cons 1 sx)) (list (list 2))) (append (map (lambda (sx) (cons 2 sx)) (list (list 1)) '()))
; -> (append (list (cons 1 (list 2))) (append (list (cons 2 (list 1))) '()))
; -> (append (list (list 1 2)) (list (list 2 1))) - > (list (list 1 2) (list 2 1)) -> ((1 2) (2 1))

; ex 2.40 - given an integer n, generate the sequence of pairs (i,j) with 1 <= j < i <= n
(define (unique-pairs n)
	(flatmap
		(lambda (i)
			(map
				(lambda (j) (list j i))
				(enumerate-interval 1 (- i 1))
			)
		)
		(enumerate-interval 1 n)
	)
)
; use unique-pairs to simplify definition of prime-sum-pairs above
(define (prime-sum-pairs n)
	(map make-pair-sum
		(filter prime-sum? (unique-pairs n))
	)
)

; ex 2.41 - find all ordered triples of distinct positive integers i, j, k <= n that sum to a given integer s
(define (triplet-sum n s)
	(define (triplets n)
		(flatmap
			(lambda (i)
				(flatmap
					(lambda (j)
						(map (lambda (k) (list k j i)) (enumerate-interval 1 (- j 1)))
					) (enumerate-interval 1 (- i 1))
				)
			) (enumerate-interval 1 n)
		)
	)
	(filter
		(lambda (tri) (= s (+ (car tri) (cadr tri) (caddr tri))) )
		(triplets n)
	)
)

; ex 2.42 - eight-queens puzzle
; Find all ways to place eight queens on n x n chessboard so that no queen is in check from any other (i.e., no two queens are in the same row, column, or diagonal).
; Algorithm places queens column by column, looking for a safe row in the kth column.
; Assume that we have already generated the sequence of all possible ways to place k - 1 queens in the first k - 1 columns of the board.
; For each of these ways, generate an extended set of positions by placing a queen in each row of the kth column.
; Now filter these, keeping only the positions for which the queen in the kth column is safe with respect to the other queens.
(define (queens board-size)
	(define (make-position row col) (list (list row col)))
	(define (row p) (car p))
	(define (column p) (cadr p))
	(define (adjoin-position r c positions)
		(append (make-position r c) positions)
	)
	(define empty-board '())
	; test whether (top) position in kth column is safe w.r.t. each position in preceding k-1 columns
	(define (safe? k positions)
		(define qc (column (car positions)))
		(define qr (row (car positions)))
		(define (not-safe-pos? pos)
			(let
				( (c  (column pos))
				  (r  (row pos))
				)
				(or (= r qr) (= (abs (- c qc)) (abs (- r qr))))
			)
		)
		(not (accumulate (lambda (x y) (or (not-safe-pos? x) y)) #f (cdr positions)))
	)
	(define (queen-cols k)
		(if (= k 0)
			(list empty-board)
			(filter
				(lambda (positions) (safe? k positions))
				(flatmap
				 	(lambda (rest-of-queens)
						(map (lambda (new-row) (adjoin-position new-row k rest-of-queens))
							(enumerate-interval 1 board-size)
						)
					)
					(queen-cols (- k 1))
				)
			)
		)
	)
	(queen-cols board-size)
)

; ex 2.43 - interchanged order of nested mappings results in slower program
(define (queens2 board-size)
	(define (make-position row col) (list (list row col)))
	(define (row p) (car p))
	(define (column p) (cadr p))
	(define (adjoin-position r c positions)
	        (append (make-position r c) positions)
	)
	(define empty-board '())
	; test whether (top) position in kth column is safe w.r.t. each position in preceding k-1 columns
	(define (safe? k positions)
	        (define qc (column (car positions)))
	        (define qr (row (car positions)))
	        (define (not-safe-pos? pos)
	                (let
	                        ( (c  (column pos))
	                          (r  (row pos))
	                        )
	                        (or (= r qr) (= (abs (- c qc)) (abs (- r qr))))
	                )
	        )
	        (not (accumulate (lambda (x y) (or (not-safe-pos? x) y)) #f (cdr positions)))
	)
	(define (queen-cols k)
	        (if (= k 0)
	                (list empty-board)
	                (filter
	                        (lambda (positions) (safe? k positions))
	                        (flatmap
	                                (lambda (new-row)
	                                        (map (lambda (rest-of-queens) (adjoin-position new-row k rest-of-queens))
							(queen-cols (- k 1))
	                                        )
	                                )
					(enumerate-interval 1 board-size)
	                        )
	                )
	        )
	)
	(queen-cols board-size)
)

(define (timed-run f . args) (define t1 (real-time-clock)) (apply f args) (- (real-time-clock) t1))

; Problem with the second program is that (queen-cols (- k 1)) is called 8 times, one per each row
; Each time this gives the same set of positions, but because of recursive process used in implementation this leads to slower program.
; On step 1 of program 2 (queen-cols 0) is called 8 times
; On step 2 (queen-cols 1) is called 8 times, each of which calls (queen-cols 0) 8 times
; On step k (queen-cols (- k 1)) is computed 8 times, (queen-cols j) is computed 8^(k-j) times
; Let T(k) be the elapsed time to compute (queen-cols k)
; First program elapsed time is at least T1=T1(1) + .. + T(7)
; Second program elapsed time is at least T2=8^8*T2(1) + 8^7*T2(2) + .. + 8*T2(7)
; Assume that T1(k) <= 8*T1(k-1)
; Assume that T2(k) <= 8*T2(k-1)
; Program 1: T1 >= T1(1) * (1 + 8 + .. + 8^6) = T(1) * (8^7-1)/7
; Program 2: T2 >= T(1) * (8^8 + 8^8 + ... + 8^8) = T(1) * 7*8^8
; These crude estimate gives T2/T1 > 390.

; estimate
(define (queens-rel-time n)
	(if (> n 1)
		(let (	(mult1 (/ (- (expt n (- n 1)) 1) (- n 1)))
			(mult2 (* (- n 1) (expt n n)))
			)
			(/ mult2 mult1 1.)
		)
		0.
	)
)
; run each version for boards 1x1 .. 7x7
(define t1 (map (lambda (k) (timed-run queens  k)) (enumerate-interval 1 7)))
(define t2 (map (lambda (k) (timed-run queens2 k)) (enumerate-interval 1 7)))
; compute t2/t1 and its estimate for each board size
(define actual-rel
	(accumulate append '()
		(map	(lambda (n) (list (if (> (list-ref t1 (- n 1)) 0)
					(/ (list-ref t2 (- n 1)) (list-ref t1 (- n 1)) 1.)
					0.))
			) (enumerate-interval 1 7))
		)
	)
)
(define predicted-rel (accumulate append '() (map (lambda (n) (list (queens-rel-time n))) (enumerate-interval 1 7))))
actual-rel
predicted-rel




(define (memq item x)
	(cond ((null? x) false)
		((eq? item (car x)) x)
		(else (memq item (cdr x)))
	)
)

; ex 2.54 - equal? as a procedure
(define (equal1? a b)
	(cond
			((and (null? a) (null? b)) #t)
			((and (list? a) (list? b)) (and (equal1? (car a) (car b)) (equal1? (cdr a) (cdr b)) ))
			(else (eq? a b))
	)
)

; ex 2.55
; (car ''abracadabra) -> (car (quote (quote abracadabra))) -> (car '(quote abracadabra)) -> 'quote

;; Symbolic differentiation
(define (variable? x) (symbol? x))
(define (same-variable? v1 v2) (and (variable? v1) (variable? v2) (eq? v1 v2)))
(define (make-sum a1 a2) (list '+ a1 a2))
(define (make-product m1 m2) (list '* m1 m2))
(define (sum? x) (and (pair? x) (eq? (car x) '+)))
(define (product? x) (and (pair? x) (eq? (car x) '*)))
(define (addend s) (cadr s))
(define (augend s) (caddr s))
(define (multiplier p) (cadr p))
(define (multiplicand p) (caddr p))
(define (make-sum a1 a2)
	(cond
		((=number? a1 0) a2)
		((=number? a2 0) a1)
		((and (number? a1) (number? a2)) (+ a1 a2))
		(else (list '+ a1 a2))
	)
)
(define (=number? exp num) (and (number? exp) (= exp num)))
(define (make-product m1 m2)
	(cond
		((or (=number? m1 0) (=number? m2 0)) 0)
		((=number? m1 1) m2)
		((=number? m2 1) m1)
		((and (number? m1) (number? m2)) (* m1 m2))
		(else (list '* m1 m2))
	)
)
; ex 2.56 - derivative of exponentiation + update in deriv procedure
(define (exponentiation? x) (and (pair? x) (eq? (car x) '**)))
(define (base x) (cadr x) )
(define (exponent x) (caddr x) )
(define (make-exponentiation b ex)
	(cond
		((= ex 0) 1)
		((= ex 1) b)
		(else (list '** b ex))
	)
)
; ex 2.57 - handle sums and products of arbitrary numbers of (two or more) terms
; augend, multiplicand
(define (augend s)
	(define (iter x)
		(if (null? x)
			0
			(make-sum (iter (cdr x)) (car x))
		)
	)
	(iter (cddr s))
)
(define (multiplicand p)
	(define (iter x)
		(if (null? x)
			1
			(make-product (iter (cdr x)) (car x))
		)
	)
	(iter (cddr p))
)
(define (deriv exp var)
	(cond ((number? exp) 0)
		((variable? exp)
			(if (same-variable? exp var) 1 0))
		((sum? exp)
			(make-sum	(deriv (addend exp) var)
					(deriv (augend exp) var))
			)
		((product? exp)
			(make-sum
				(make-product (multiplier exp) (deriv (multiplicand exp) var))
				(make-product (deriv (multiplier exp) var) (multiplicand exp))
			)
		)
		((exponentiation? exp)
			(make-product
				(make-product (exponent exp) (deriv (base exp) var))
				(make-exponentiation (base exp) (- (exponent exp) 1))
			)
		)
		(else (error "unknown expression type -- DERIV" exp))
	)
)

; ex 2.58(a) - differentiation program to work with usual (infix) notation ( (a + b) instead of (+ a b))
(define (make-sum-infix a1 a2)
	(cond
		((=number? a1 0) a2)
		((=number? a2 0) a1)
		((and (number? a1) (number? a2)) (+ a1 a2))
		(else (list a1 '+ a2))
	)
)
(define (make-product-infix m1 m2)
	(cond
		((or (=number? m1 0) (=number? m2 0)) 0)
		((=number? m1 1) m2)
		((=number? m2 1) m1)
		((and (number? m1) (number? m2)) (* m1 m2))
		(else (list m1 '* m2))
	)
)
(define (sum-infix? x) (and (pair? x) (eq? (cadr x) '+)))
(define (product-infix? x) (and (pair? x) (eq? (cadr x) '*)))
(define (remove-parenthesis exp)
	(define (iter x)
		(cond
			((not (list? x)) x)
			((not (list? (car x))) x)
			(else (iter (car x)))
		)
	)
	(iter exp)
)
(define (addend s) (car s))
(define (augend s)
	(define (iter xx)
		(let
			((x (remove-parenthesis xx)))
			(cond
				((null? x) 0)
				((number? (car x)) (car x))
				((product-infix? x) (display x) (make-product-infix (car x) (iter (cddr x))))
				(else (make-sum-infix (car x) (iter (cddr x))))
			)
		)
	)
	(iter (cddr s))
)
(define (multiplier p) (car p))
(define (multiplicand p)
	(define (iter x)
		(if (null? x)
			1
			(make-product-infix (car x) (iter (cddr x)))
		)
	)
	(iter (cddr p))
)
(define (deriv-infix exp var)
	(cond
		((number? exp) 0)
		((variable? exp) (if (same-variable? exp var) 1 0))
		((product-infix? exp)
			(make-sum-infix
				(make-product-infix (multiplier exp) (deriv-infix (multiplicand exp) var))
				(make-product-infix (multiplicand exp) (deriv-infix (multiplier exp) var))
			)
		)
		((sum-infix? exp) (make-sum-infix (deriv-infix (addend exp) var) (deriv-infix (augend exp) var) ) )
		(else
			(error "Unrecognized operation in DERIV " exp)
		)
	)
)
; ex 2.58(b) not done


;; sets as unordered lists

(define (element-of-set? x set)
	(cond ((null? set) #f)
			((equal? x (car set)) #t)
			(else (element-of-set? x (cdr set)))
	)
)
(define (adjoin-set x set)
	(if (element-of-set? x set)
		set
		(cons x set)
	)
)
(define (intersection-set set1 set2)
	(cond
		((or (null? set1) (null? set2)) '())
		((element-of-set? (car set1) set2) (cons (car set1) (intersection-set (cdr set1) set2)) )
		(else (intersection-set (cdr set1) set2))
	)
)

; ex 2.59 - unite sets
(define (union-set set1 set2)
	(cond
		((null? set1) set2)
		((null? set2) set1)
		((element-of-set? (car set1) set2) (union-set (cdr set1) set2))
		(else (cons (car set1) (union-set (cdr set1) set2)))
	)
)

;; sets as ordered lists

(define (element-of-set? x set)
	(cond
		((null? set) #f)
		((= x (car set)) #t)
		((< x (car set)) #f)
		(else (element-of-set? x (cdr set)))
	)
)
(define (intersection-set set1 set2)
	(cond
		((or (null? set1) (null? set2)) '())
		((= (car set1) (car set2)) (cons (car set1) (intersection-set (cdr set1) (cdr set2))))
		((< (car set1) (car set2)) (intersection-set (cdr set1) set2))
		(else (intersection-set set1 (cdr set2)))
	)
)

; ex 2.61 - adjoin-set
(define (adjoin-set x set)
	(cond
		((null? set) (cons x set))
		((< x (car set)) (cons x set))
		(else (cons (car set) (adjoin-set x (cdr set))))
	)
)

; ex 2.62 - union-set \Theta(n)
(define (union-set set1 set2)
	(cond
		((null? set1) set2)
		((null? set2) set1)
		((< (car set1) (car set2)) (cons (car set1) (union-set (cdr set1) set2)))
		((> (car set1) (car set2)) (cons (car set2) (union-set set1 (cdr set2))))
		(else (cons (car set1) (union-set (cdr set1) (cdr set2))))
	)
)

;; sets as binary trees

(define (make-tree entry left right) (list entry left right))
(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))

(define (element-of-set? x set)
	(cond	((null? set) #f)
			((= x (entry set)) #t)
			((< x (entry set)) (element-of-set? x (left-branch set)))
			(else (element-of-set? x (right-branch set)))
	)
)
(define (adjoin-set x set)
	(cond
		((null? set) (make-tree x '() '()))
		((= x (entry set)) set)
		((< x (entry set)) (make-tree (entry set) (adjoin-set x (left-branch set)) (right-branch set)) )
		(else (make-tree (entry set) (left-branch set) (adjoin-set x (right-branch set))))
	)
)

; ex 2.63
(define (tree->list-1 tree)
	(if (null? tree)
		'()
		(append
			(tree->list-1 (left-branch tree))
			(cons (entry tree) (tree->list-1 (right-branch tree)))
		)
	)
)
(define (tree->list-2 tree)
	(define (copy-to-list tree result-list)
		(if (null? tree)
			result-list
			(copy-to-list (left-branch tree)
			              (cons (entry tree)
			                    (copy-to-list (right-branch tree)
			                                  result-list))
			)
		)
	)
	(copy-to-list tree '())
)
; Do the two procedures produce the same result for every tree? If not, how do the results differ? What lists do the two procedures produce for the trees x,y,z?
(define x1 (make-tree 1 '() '()))
(define x5 (make-tree 5 '() '()))
(define x11 (make-tree 11 '() '()))
(define x3 (make-tree 3 x1 x5))
(define x9 (make-tree 9 '() x11))
(define x7 (make-tree 7 x3 x9))
(tree->list-1 x7)
(tree->list-2 x7)

(define y7 (make-tree 7 x5 x9))
(define y3 (make-tree 3 x1 y7))
(tree->list-1 y3)
(tree->list-2 y3)

(define z7 (make-tree 7 '() '()))
(define z3 (make-tree 3 x1 '()))
(define z9 (make-tree 9 z7 x11))
(define z5 (make-tree 5 z3 z9))
(tree->list-1 z5)
(tree->list-2 z5)


; ex 2.64
; convert an ordered list to a balanced binary tree
(define (list->tree elements)
	(car (partial-tree elements (length elements)))
)
; partial-tree takes as arguments an integer n and list of at least n elements and constructs a balanced tree containing the first n elements of the list.
; The result returned by partial-tree is a pair (formed with cons) whose car is the constructed tree and whose cdr is the list of elements not included in the tree.
(define (partial-tree elts n)
	(if (= n 0)
		(cons '() elts)
		(let ((left-size (quotient (- n 1) 2)))
			(let ((left-result (partial-tree elts left-size)))
				(let (
					(left-tree (car left-result))
					(non-left-elts (cdr left-result))
					(right-size (- n (+ left-size 1)))
					)
					(let (	(this-entry (car non-left-elts))
						(right-result (partial-tree (cdr non-left-elts)
							right-size))
						)
						(let (	(right-tree (car right-result))
							(remaining-elts (cdr right-result))
							)
							(cons (make-tree this-entry left-tree right-tree)
								remaining-elts)
						)
					)
				)
			)
		)
	)
)


; ex 2.65 - \Theta(n) implementation of union and intersection of sets implemented as balanced binary trees
(define (union-sorted-lists list1 list2)
	(cond
		((null? list1) list2)
		((null? list2) list1)
		((< (car list1) (car list2)) (cons (car list1) (merge-sorted-lists (cdr list1) list2)))
		((> (car list1) (car list2)) (cons (car list2) (merge-sorted-lists list1 (cdr list2))))
		(else (cons (car list1) (merge-sorted-lists (cdr list1) (cdr list2))))
	)
)
(define (union-set-tree tree1 tree2)
	(list->tree (union-sorted-lists (tree->list-1 tree1) (tree->list-1 tree2)))
)
(define (intersection-sorted-lists list1 list2)
	(cond
		((null? list1) '())
		((null? list2) '())
		((< (car list1) (car list2)) (intersection-sorted-lists (cdr list1) list2))
		((> (car list1) (car list2)) (intersection-sorted-lists list1 (cdr list2)))
		(else (cons (car list1) (intersection-sorted-lists (cdr list1) (cdr list2))))
	)
)
(define (intersection-set-tree tree1 tree2)
	(list->tree (intersection-sorted-lists (tree->list-1 tree1) (tree->list-1 tree2)))
)

;; sets and information retrieval

(define (lookup given-key set-of-records)
	(cond ((null? set-of-records) false)
		((equal? given-key (key (car set-of-records)))
			(car set-of-records))
		(else (lookup given-key (cdr set-of-records))))
)
; ex 2.66 - implement the lookup procedure for the case where the set of records is structured as a binary tree, ordered by the numerical values of the keys.
(define (lookup given-key set-of-records)
	(if (null? set-of-records)
		#f
		(let ((k (key (entry tree))))
			(cond
				((= given-key k) (entry tree))
				((< given-key k) (lookup given-key (left-branch set-of-records)))
				(else (lookup given-key (right-branch set-of-records)))
			)
		)
	)
)

; Huffman encoding
; Given the alphabet of possible symbols and its relative occurence (defined by weight which is a integer number)
; , encode the message using Huffman tree

; in order to uniquely determine each encoded symbol during decoding process
; any two encoded symbols of the alphabet should have different prefixes
; this property is called prefix property

; in order to maintain this property so called Huffman binary tree is used
; in these binary tree inner nodes contains the following data: symbols of the alphabet in children nodes and sum of the weights of these symbols
; terminal leafs contains one symbol and its weight
; each symbol is encoded as binary sequence of 0 and 1, which is defined by the traversal path from the root of the tree to the terminal leaf with this symbol:
; if symbol is in the left subtree, append 0 to the encoding, otherwise it is in the right subtree, append 1 to the encoding

; terminal leaf constructor
(define (make-leaf symbol weight)
	(list 'leaf symbol weight)
)
; terminal leaf selectors
(define (leaf? object)
	(eq? (car object) 'leaf)
)
(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))

; binary tree node constructor
(define (make-code-tree left right)
	(list
		left
		right
		(append (symbols left) (symbols right))
		(+ (weight left) (weight right))
	)
)
; binary tree node selectors
(define (left-branch tree) (car tree))
(define (right-branch tree) (cadr tree))
(define (symbols tree)
	(if (leaf? tree)
		(list (symbol-leaf tree))
		(caddr tree)
	)
)
(define (weight tree)
	(if (leaf? tree)
		(weight-leaf tree)
		(cadddr tree)
	)
)

; decoding
(define (decode bits tree)
	(define (decode-1 bits current-branch)
	    (if (null? bits)
	        '()
	        (let ((next-branch (choose-branch (car bits) current-branch)))
							(if (leaf? next-branch)
								; append found symbol to the result
								; and look up for the next symbol starting from the root
								(cons (symbol-leaf next-branch) (decode-1 (cdr bits) tree))
								; look up for the symbol in the next branch
								(decode-1 (cdr bits) next-branch)
							)
					)
		)
	)
	(decode-1 bits tree)
)
(define (choose-branch bit branch)
	(cond	((= bit 0) (left-branch branch))
		((= bit 1) (right-branch branch))
		(else (error "bad bit -- CHOOSE-BRANCH" bit)))
)

; create set represented by ordered list from the list of pairs (symbol + weight)
; list is ordered by the weight value in each pairs in ascending order
(define (make-leaf-set pairs)
	(if (null? pairs)
		'()
		(let ((pair (car pairs))
					(symbol (car pair))
					(weight (cadr pair)))
			(adjoin-set
				(make-leaf symbol weight)
		    (make-leaf-set (cdr pairs))
		  )
		)
	)
)
(define (adjoin-set x set)
	(cond ((null? set) (list x))
		((< (weight x) (weight (car set))) (cons x set))
		(else (cons (car set)
		            (adjoin-set x (cdr set))))
	)
)
; encode message using Huffman tree symbol by symbol
(define (encode message tree)
	(if (null? message)
		'()
		(append
			(encode-symbol (car message) tree)
			(encode (cdr message) tree))
		)
)
; ex 2.68 - encode-symbol is a procedure, which you must write, that returns the list of bits that encodes a given symbol according to a given tree.
(define (encode-symbol symb tree)
	(define (iter msg tree)
		(cond
			((leaf? tree)
				(if (eq? symb (symbol-leaf tree))
					'()
					(error "symbol not found" symb)
				)
			)
			((element-of-set? symb (symbols (left-branch tree))) (cons 0 (iter symb (left-branch tree))))
			(else (cons 1 (iter symb (right-branch tree))))
		)
	)
	(iter '() tree)
)
(define (element-of-set? x set)
	(cond
		((null? set) #f)
		((eq? x (car set)) #t)
		(else (element-of-set? x (cdr set)))
	)
)

; generate Huffman tree from list of pairs symbol + weight
(define (generate-huffman-tree pairs)
	(successive-merge (make-leaf-set pairs))
)

; ex 2.69
; given leaf set represented as ordered list
; create tree using iterative procedure
; at start each list element is terminal leaf (node) of the resulting tree
; in each iteration two nodes of the set are grouped into the new node
; this node contains them as its children with the union of their symbols and sum of their weights
; then its node is placed back into the ordered list
; order in the list is maintained
; after at most n-1 iterations the single element in the list is left, which is the root of Huffman tree
(define (successive-merge leaf-set)
	(define (iter result set)
		(if (null? (cdr set))
			result
			(let ((new-pair (make-code-tree (car set) (cadr set))))
				(iter new-pair (adjoin-set new-pair (cddr set)))
			)
		)
	)
	(iter '() leaf-set)
)


;;; Multiple representations for abstract data


;; Operation, type -> procedure
;; Dispatch table.
;;
(define *op-table* (make-hash-table))

(define (put op type proc)
	(hash-table/put! *op-table* (list op type) proc)
)
(define (get op type)
	(hash-table/get *op-table* (list op type) '())
)


(define (add-complex z1 z2)
	(make-from-real-imag
			(+ (real-part z1) (real-part z2))
			(+ (imag-part z1) (imag-part z2)))
)
(define (sub-complex z1 z2)
	(make-from-real-imag
			(- (real-part z1) (real-part z2))
			(- (imag-part z1) (imag-part z2)))
)
(define (mul-complex z1 z2)
	(make-from-mag-ang
			(* (magnitude z1) (magnitude z2))
			(+ (angle z1) (angle z2)))
)
(define (div-complex z1 z2)
	(make-from-mag-ang
			(/ (magnitude z1) (magnitude z2))
			(- (angle z1) (angle z2)))
)


; in order to distinguish between rectangular and polar complex number representation
; we use additional type tag
; implemented using list
; procedures to attach type tag and extract it from the typed data
(define (attach-tag type-tag contents)
	(cons type-tag contents))
; selectors from the typed data
(define (type-tag datum)
	(if (pair? datum)
		(car datum)
		(error "Bad tagged datum -- TYPE-TAG" datum)))
(define (contents datum)
	(if (pair? datum)
		(cdr datum)
		(error "Bad tagged datum -- CONTENTS" datum)))
; type check
(define (rectangular? z)
	(eq? (type-tag z) 'rectangular))
(define (polar? z)
	(eq? (type-tag z) 'polar))

; one way to implement same interface over different (complex number) representation
; is to use naming conventions
; here each function is suffixed with the type name

(define (real-part-rectangular z) (car z))
(define (imag-part-rectangular z) (cdr z))
(define (magnitude-rectangular z)
	(sqrt (+
		(square (real-part-rectangular z))
		(square (imag-part-rectangular z)))))
(define (angle-rectangular z)
	(atan
		(imag-part-rectangular z)
		(real-part-rectangular z)))
(define (make-from-real-imag-rectangular x y)
	(attach-tag 'rectangular
		(cons x y)))
(define (make-from-mag-ang-rectangular r a)
	(attach-tag 'rectangular
		(cons (* r (cos a)) (* r (sin a)))))

(define (real-part-polar z)
	(* (magnitude-polar z) (cos (angle-polar z))))
(define (imag-part-polar z)
	(* (magnitude-polar z) (sin (angle-polar z))))
(define (magnitude-polar z) (car z))
(define (angle-polar z) (cdr z))
(define (make-from-real-imag-polar x y)
	(attach-tag 'polar
		(cons (sqrt (+ (square x) (square y)))
			(atan y x))))
(define (make-from-mag-ang-polar r a)
	(attach-tag 'polar (cons r a)))


; then general functions can use specialized functions using if
(define (real-part z)
	(cond ((rectangular? z) (real-part-rectangular (contents z)))
		((polar? z) (real-part-polar (contents z)))
		(else (error "Unknown type -- REAL-PART" z))))
(define (imag-part z)
	(cond ((rectangular? z) (imag-part-rectangular (contents z)))
		((polar? z) (imag-part-polar (contents z)))
		(else (error "Unknown type -- IMAG-PART" z))))
(define (magnitude z)
	(cond ((rectangular? z) (magnitude-rectangular (contents z)))
		((polar? z) (magnitude-polar (contents z)))
		(else (error "Unknown type -- MAGNITUDE" z))))
(define (angle z)
	(cond ((rectangular? z) (angle-rectangular (contents z)))
		((polar? z) (angle-polar (contents z)))
		(else (error "Unknown type -- ANGLE" z))))

(define (make-from-real-imag x y)
	(make-from-real-imag-rectangular x y))
(define (make-from-mag-ang r a)
	(make-from-mag-ang-polar r a))


; another way is to encapsulate supplementary procedures which are not the part of the user interface
; in Scheme this can be done as inner functions inside 'package' functions
; which populates the global table of types and functions with the interface implementation
; this implementation creates typed object with the internal type tag, which is needed only for internal procedures
; global table is represented here by hash-table

(define (install-rectangular-package)
	;; internal procedures
	(define (real-part z) (car z))
	(define (imag-part z) (cdr z))
	(define (make-from-real-imag x y) (cons x y))
	(define (magnitude z)
	  (sqrt (+ (square (real-part z))
	     (square (imag-part z)))))
	(define (angle z)
	  (atan (imag-part z) (real-part z)))
	(define (make-from-mag-ang r a)
	  (cons (* r (cos a)) (* r (sin a))))
	;; interface to the rest of the system
	(define (tag x) (attach-tag 'rectangular x))
	(put 'real-part '(rectangular) real-part)
	(put 'imag-part '(rectangular) imag-part)
	(put 'magnitude '(rectangular) magnitude)
	(put 'angle '(rectangular) angle)
	(put 'make-from-real-imag 'rectangular
	     (lambda (x y) (tag (make-from-real-imag x y))))
	(put 'make-from-mag-ang 'rectangular
	     (lambda (r a) (tag (make-from-mag-ang r a))))
	'done
)

(define (install-polar-package)
	;; internal procedures
	(define (magnitude z) (car z))
	(define (angle z) (cdr z))
	(define (make-from-mag-ang r a) (cons r a))
	(define (real-part z)
	  (* (magnitude z) (cos (angle z))))
	(define (imag-part z)
	  (* (magnitude z) (sin (angle z))))
	(define (make-from-real-imag x y)
	  (cons (sqrt (+ (square x) (square y)))
	  (atan y x)))
	;; interface to the rest of the system
	(define (tag x) (attach-tag 'polar x))
	(put 'real-part '(polar) real-part)
	(put 'imag-part '(polar) imag-part)
	(put 'magnitude '(polar) magnitude)
	(put 'angle '(polar) angle)
	(put 'make-from-real-imag 'polar
	     (lambda (x y) (tag (make-from-real-imag x y))))
	(put 'make-from-mag-ang 'polar
	     (lambda (r a) (tag (make-from-mag-ang r a))))
	'done
)

; then we can define generic interface functions on typed data
(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))
(define (magnitude z) (apply-generic 'magnitude z))
(define (angle z) (apply-generic 'angle z))


; then we can execute generic function using type dispatch
; depending on the type, corresponding function from the global table is called
; dot notation : variable amount of args are packed into the list
(define (apply-generic op . args)
	(let ((type-tags (map type-tag args)))
		(let ((proc (get op type-tags)))
			(if proc
				(apply proc (map contents args))
				(error "No method for these types -- APPLY-GENERIC" (list op type-tags))
			)
		)
	)
)

; symbolic differentiation package as an exercise
(define (deriv exp var)
	(cond ((number? exp) 0)
		((variable? exp) (if (same-variable? exp var) 1 0))
		(else
			((get 'deriv (operator exp)) (operands exp) var)
		)
	)
)
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))

; ex 2.73
(define (install-deriv-package)
	;; internal procedures
	(define (sum? x)
		(and (pair? x) (eq? (car x) '+))
	)
	(define (product? x)
		(and (pair? x) (eq? (car x) '*))
	)

	(define (addend s) (car s))
	(define (augend s)
		(define (iter x)
			(if (null? x)
				0
				(make-sum (iter (cdr x)) (car x))
			)
		)
		(iter (cdr s))
	)
	(define (multiplier p) (car p))
	(define (multiplicand p)
		(define (iter x)
			(if (null? x)
				1
				(make-product (iter (cdr x)) (car x))
			)
		)
		(iter (cdr p))
	)
	(define (make-sum a1 a2)
		(cond
			((=number? a1 0) a2)
			((=number? a2 0) a1)
			((and (number? a1) (number? a2)) (+ a1 a2))
			(else (list '+ a1 a2))
		)
	)
	(define (=number? exp num)
		(and (number? exp) (= exp num))
	)
	(define (make-product m1 m2)
		(cond
			((or (=number? m1 0) (=number? m2 0)) 0)
			((=number? m1 1) m2)
			((=number? m2 1) m1)
			((and (number? m1) (number? m2)) (* m1 m2))
			(else (list '* m1 m2))
		)
	)
	(define (exponentiation? x)
		(and (pair? x) (eq? (car x) '**))
	)
	(define (base x) (car x) )
	(define (exponent x) (cadr x) )
	(define (make-exponentiation b ex)
		(cond
			((= ex 0) 1)
			((= ex 1) b)
			(else (list '** b ex))
		)
	)
	(define (deriv-sum exp var)
		(make-sum	(deriv (addend exp) var) (deriv (augend exp) var))
	)
	(define (deriv-prod exp var)
		(make-sum
			(make-product (multiplier exp) (deriv (multiplicand exp) var))
			(make-product (deriv (multiplier exp) var) (multiplicand exp))
		)
	)
	(define (deriv-expon exp var)
		(make-product
			(make-product (exponent exp) (deriv (base exp) var))
			(make-exponentiation (base exp) (- (exponent exp) 1))
		)
	)
	;; interface to the rest of the system
	(define (tag x) (attach-tag 'deriv x))
	(put 'deriv '+ deriv-sum)
	(put 'deriv '* deriv-prod)
	(put 'deriv '** deriv-expon)
	'done
)


; rational package number as an exercise
(define (install-rational-package)
	;; internal procedures
	(define (gcd x y)
		(if (= y 0)
			x
			(gcd y (remainder x y))
		)
	)
	(define (numer x) (car x))
	(define (denom x) (cdr x))
	(define (make-rat p q)
		(let ((div (gcd p q)))
			(cons (/ p div) (/ q div))
		)
	)
	(define (add-rat x y)
		(make-rat (+ (* (numer x) (denom y))
			(* (numer y) (denom x)))
			(* (denom x) (denom y))))
	(define (sub-rat x y)
		(make-rat (- (* (numer x) (denom y))
			(* (numer y) (denom x)))
			(* (denom x) (denom y))))
	(define (mul-rat x y)
		(make-rat (* (numer x) (numer y))
			(* (denom x) (denom y))))
	(define (div-rat x y)
		(make-rat (* (numer x) (denom y))
			(* (denom x) (numer y))))
	;; interface to rest of the system
	(define (tag x) (attach-tag 'rational x))
	(put '+ '(rational rational)
		(lambda (x y) (tag (add-rat x y))))
	(put '- '(rational rational)
		(lambda (x y) (tag (sub-rat x y))))
	(put '* '(rational rational)
		(lambda (x y) (tag (mul-rat x y))))
	(put '/ '(rational rational)
		(lambda (x y) (tag (div-rat x y))))

	(put 'make 'rational
		(lambda (n d) (tag (make-rat n d))))
	'done)
(define (make-rational n d)
	((get 'make 'rational) n d)
)
; (apply-generic '+ (make-rational 7 5) (make-rational 5 7))


; complex number package
(define (install-complex-package)
	;; imported procedures from rectangular and polar packages
	(define (make-from-real-imag x y)
		((get 'make-from-real-imag 'rectangular) x y))
	(define (make-from-mag-ang r a)
		((get 'make-from-mag-ang 'polar) r a))
	;; internal procedures
	(define (add-complex z1 z2)
		(make-from-real-imag (+ (real-part z1) (real-part z2))
			(+ (imag-part z1) (imag-part z2))))
	(define (sub-complex z1 z2)
		(make-from-real-imag (- (real-part z1) (real-part z2))
			(- (imag-part z1) (imag-part z2))))
	(define (mul-complex z1 z2)
		(make-from-mag-ang (* (magnitude z1) (magnitude z2))
			(+ (angle z1) (angle z2))))
	(define (div-complex z1 z2)
		(make-from-mag-ang (/ (magnitude z1) (magnitude z2))
			(- (angle z1) (angle z2))))
	;; interface to rest of the system
	(define (tag z) (attach-tag 'complex z))
	(put '+ '(complex complex)
		(lambda (z1 z2) (tag (add-complex z1 z2))))
	(put '- '(complex complex)
		(lambda (z1 z2) (tag (sub-complex z1 z2))))
	(put '* '(complex complex)
		(lambda (z1 z2) (tag (mul-complex z1 z2))))
	(put '/ '(complex complex)
		(lambda (z1 z2) (tag (div-complex z1 z2))))
	(put 'make-from-real-imag 'complex
		(lambda (x y) (tag (make-from-real-imag x y))))
	(put 'make-from-mag-ang 'complex
		(lambda (r a) (tag (make-from-mag-ang r a))))
	(put 'real-part '(complex) real-part)
	(put 'imag-part '(complex) imag-part)
	(put 'magnitude '(complex) magnitude)
	(put 'angle '(complex) angle)
	'done
)
(define (make-complex-from-real-imag x y)
	((get 'make-from-real-imag 'complex) x y)
)
(define (make-complex-from-mag-ang r a)
	((get 'make-from-mag-ang 'complex) r a)
)
; (apply-generic '* (make-complex-from-mag-ang 5 90) (make-complex-from-real-imag 5 7))











;;; 2.5.1 GENERIC ARITHMETIC OPERATIONS


;; Operation, type -> procedure
;; Dispatch table.
;;
(define *op-table* (make-hash-table))
(define (put op type proc)
	(hash-table/put! *op-table* (list op type) proc))
(define (get op type)
	(hash-table/get *op-table* (list op type) '()))
(define (apply-generic op . args)
	(let ((type-tags (map type-tag args)))
		(let ((proc (get op type-tags)))
			(if proc
				(apply proc (map contents args))
				(error "No method for these types -- APPLY-GENERIC" (list op type-tags))
			)
		)
	)
)
; ex. 2.78
(define (attach-tag tag contents)
	(if (number? contents)
		contents
		(cons tag contents)
	)
)
(define (type-tag data)
	(cond ((pair? data) (car data))
		((number? data) 'scheme-number)
		(else (error "No type found " data))
	)
)
(define (contents data)
	(cond ((pair? data) (cdr data))
		((number? data) data)
		(else (error "No contents found" data))
	)
)

; ex 2.79 - define a generic equality predicate equ? that tests the equality of two numbers, and install it in the generic arithmetic package.
; This operation should work for ordinary numbers, rational numbers, and complex numbers.
; ex 2.80 - define a generic predicate =zero? that tests if its argument is zero, and install it in the generic arithmetic package.
; This operation should work for ordinary numbers, rational numbers, and complex numbers.
(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))
(define (equ? x y) (apply-generic 'equ? x y))
(define (=zero? x) (apply-generic '=zero? x))
; ex 2.83 - procedure that raises objects of that type one level in the tower (tower of types: integer->rational->real->complex)
(define (raise1 x) (apply-generic 'raise x))
; ex 2.85 - procedure that simplifies (drops) a data object by lowering it in the tower of types as far as possible ()
(define (project x) (apply-generic 'project x))
(define (drop x)
	(let ((t (type-tag x)))
		(display t)
		(newline)
		(display x)
		(newline)
		(if (and
			(lower-type? 'scheme-number t)
			(equ? x ((get 'raise' (list t)) ((get 'project (list t)) x) )))
				(drop (project x))
				x
		)
	)
)

(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))
(define (magnitude z) (apply-generic 'magnitude z))
(define (angle z) (apply-generic 'angle z))

; package for numbers implemented in Scheme
(define (install-scheme-number-package)
	(define (tag x)
		(attach-tag 'scheme-number x))
	(put 'add '(scheme-number scheme-number)
		(lambda (x y) (tag (+ x y))))
	(put 'sub '(scheme-number scheme-number)
		(lambda (x y) (tag (- x y))))
	(put 'mul '(scheme-number scheme-number)
		(lambda (x y) (tag (* x y))))
	(put 'div '(scheme-number scheme-number)
		(lambda (x y) (tag (/ x y))))
	(put 'equ? '(scheme-number scheme-number)
		(lambda (x y) (= 0 (- x y))))
	(put '=zero? '(scheme-number)
		 (lambda (x) (= x 0)))
	(put 'make 'scheme-number
		(lambda (x) (tag x)))
	(put 'exp '(scheme-number scheme-number)
		(lambda (x y) (tag (expt x y)))) ; using primitive expt
	(put 'raise '(scheme-number)
		(lambda (x) ((get-coercion 'scheme-number 'rational) (tag x))))
	(put 'project '(scheme-number)
		(lambda (x) x))
	'done
)
; constructor
(define (make-scheme-number n)
	((get 'make 'scheme-number) n)
)

; package for rational numbers
(define (install-rational-package)
	;; internal procedures
	(define (numer x) (car x))
	(define (denom x) (cdr x))
	(define (make-rat n d)
		(let ((g (gcd n d)))
			(cons (/ n g) (/ d g))
		)
	)
	(define (add-rat x y)
		(make-rat (+ (* (numer x) (denom y))
				(* (numer y) (denom x)))
				(* (denom x) (denom y))))
	(define (sub-rat x y)
		(make-rat (- (* (numer x) (denom y))
				(* (numer y) (denom x)))
				(* (denom x) (denom y))))
	(define (mul-rat x y)
		(make-rat (* (numer x) (numer y))
				(* (denom x) (denom y))))
	(define (div-rat x y)
		(make-rat (* (numer x) (denom y))
				(* (denom x) (numer y))))
	(define eq-num (get 'equ? '(scheme-number scheme-number)))
	(define (eq-rat x y)
		(and (eq-num (numer x) (numer y)) (eq-num (denom x) (denom y)))
	)
	(define (to-num r) (make-scheme-number (/ (numer r) (denom r))))
	;; interface to rest of the system
	(define (tag x) (attach-tag 'rational x))
	(put 'add '(rational rational)
		(lambda (x y) (tag (add-rat x y))))
	(put 'sub '(rational rational)
		(lambda (x y) (tag (sub-rat x y))))
	(put 'mul '(rational rational)
		(lambda (x y) (tag (mul-rat x y))))
	(put 'div '(rational rational)
		(lambda (x y) (tag (div-rat x y))))
	(put 'equ? '(rational rational)
		(lambda (x y) (eq-rat x y)))
	(put '=zero? '(rational)
		(lambda (r) ((get '=zero? '(scheme-number)) (to-num r))))
	(put 'make 'rational
		(lambda (n d) (tag (make-rat n d))))
	(put 'raise '(rational)
		(lambda (x) ((get-coercion 'rational 'complex) (tag x))))
	(put 'project '(rational)
		(lambda (x) (make-scheme-number (/ (numer x) (denom x)))))
	'done
)
(define (make-rational n d)
	((get 'make 'rational) n d)
)

; complex package

(define (install-complex-package)
	;; imported procedures from rectangular and polar packages
	(define (make-from-real-imag x y)
		((get 'make-from-real-imag 'rectangular) x y))
	(define (make-from-mag-ang r a)
		((get 'make-from-mag-ang 'polar) r a))
	;; internal procedures
	(define (add-complex z1 z2)
		(make-from-real-imag (add (real-part z1) (real-part z2))
			(add (imag-part z1) (imag-part z2))))
	(define (sub-complex z1 z2)
		(make-from-real-imag (sub (real-part z1) (real-part z2))
			(sub (imag-part z1) (imag-part z2))))
	(define (mul-complex z1 z2)
		(make-from-mag-ang (mul (magnitude z1) (magnitude z2))
			(add (angle z1) (angle z2))))
	(define (div-complex z1 z2)
		(make-from-mag-ang (div (magnitude z1) (magnitude z2))
			(sub (angle z1) (angle z2))))
	(define (eq-complex z1 z2)
		(and (equ? (real-part z1) (real-part z2)) (equ? (imag-part z1) (imag-part z2)))
	)
	(define zero-complex (make-from-real-imag 0 0))
	;; interface to rest of the system
	(define (tag z) (attach-tag 'complex z))
	(put 'add '(complex complex)
		(lambda (z1 z2) (tag (add-complex z1 z2))))
	(put 'sub '(complex complex)
		(lambda (z1 z2) (tag (sub-complex z1 z2))))
	(put 'mul '(complex complex)
		(lambda (z1 z2) (tag (mul-complex z1 z2))))
	(put 'div '(complex complex)
		(lambda (z1 z2) (tag (div-complex z1 z2))))
	(put 'equ? '(complex complex)
		(lambda (z1 z2) (eq-complex z1 z2)))
	(put 'make-from-real-imag 'complex
		(lambda (x y) (tag (make-from-real-imag x y))))
	(put 'make-from-mag-ang 'complex
		(lambda (r a) (tag (make-from-mag-ang r a))))
	(put '=zero? '(complex)
		(lambda (z) (eq-complex z zero-complex)))
	(put 'real-part '(complex) real-part)
	(put 'imag-part '(complex) imag-part)
	(put 'magnitude '(complex) magnitude)
	(put 'angle '(complex) angle)
	(put 'project '(complex)
		(lambda (z) (make-rational (real-part z) 1)))
	'done
)

(define (make-complex-from-real-imag x y)
	((get 'make-from-real-imag 'complex) x y)
)
(define (make-complex-from-mag-ang r a)
	((get 'make-from-mag-ang 'complex) r a)
)



(define (install-rectangular-package)
	;; internal procedures
	(define (real-part z) (car z))
	(define (imag-part z) (cdr z))
	(define (make-from-real-imag x y) (cons x y))
	(define (magnitude z)
		(sqrt (add (square (real-part z))
			(square (imag-part z)))))
	(define (angle z)
		(atan (imag-part z) (real-part z)))
	(define (make-from-mag-ang r a)
		(cons (mul r (cos a)) (mul r (sin a))))
	;; interface to the rest of the system
	(define (tag x) (attach-tag 'rectangular x))
	(put 'real-part '(rectangular) real-part)
	(put 'imag-part '(rectangular) imag-part)
	(put 'magnitude '(rectangular) magnitude)
	(put 'angle '(rectangular) angle)
	(put 'make-from-real-imag 'rectangular
		(lambda (x y) (tag (make-from-real-imag x y))))
	(put 'make-from-mag-ang 'rectangular
		(lambda (r a) (tag (make-from-mag-ang r a))))
	'done
)

(define (install-polar-package)
	;; internal procedures
	(define (magnitude z) (car z))
	(define (angle z) (cdr z))
	(define (make-from-mag-ang r a) (cons r a))
	(define (real-part z)
		(mul (magnitude z) (cos (angle z))))
	(define (imag-part z)
		(mul (magnitude z) (sin (angle z))))
	(define (make-from-real-imag x y)
		(cons (sqrt (add (square x) (square y)))
		(atan y x)))
	;; interface to the rest of the system
	(define (tag x) (attach-tag 'polar x))
	(put 'real-part '(polar) real-part)
	(put 'imag-part '(polar) imag-part)
	(put 'magnitude '(polar) magnitude)
	(put 'angle '(polar) angle)
	(put 'make-from-real-imag 'polar
		(lambda (x y) (tag (make-from-real-imag x y))))
	(put 'make-from-mag-ang 'polar
		(lambda (r a) (tag (make-from-mag-ang r a))))
	'done
)


(install-scheme-number-package)
(install-rational-package)
(install-complex-package)
(install-rectangular-package)
(install-polar-package)





;; Coercion, type -> type
;; Dispatch table.
;;
(define *coer-table* (make-hash-table))
(define (put-coercion from to proc)
	(hash-table/put! *coer-table* (list from to) proc)
)
(define (get-coercion from to)
	(hash-table/get *coer-table* (list from to) '())
)
(define (scheme-number->complex n)
	(make-complex-from-real-imag (contents n) 0)
)
(define (scheme-number->rational n)
	(make-rational (contents n) 1)
)
(define (rational->complex r)
	(make-complex-from-real-imag r 0)
)

(put-coercion 'scheme-number 'complex scheme-number->complex)
(put-coercion 'scheme-number 'rational scheme-number->rational)
(put-coercion 'rational 'complex rational->complex)

; ex 2.81(c)
(define (apply-generic op . args)
	(let ((type-tags (map type-tag args)))
		(let ((proc (get op type-tags)))
			(if (not (null? proc))
				(apply proc (map contents args))
				(if (and (= (length args) 2) (not (eq? (car type-tags) (cadr type-tags))))
					(let ((type1 (car type-tags))
						(type2 (cadr type-tags))
						(a1 (car args))
						(a2 (cadr args)))
							(let ((t1->t2 (get-coercion type1 type2))
								(t2->t1 (get-coercion type2 type1)))
									(cond 	( (not (null? t1->t2)) (apply-generic op (t1->t2 a1) a2))
										( (not (null? t2->t1)) (apply-generic op a1 (t2->t1 a2)))
										(else (error "No method for these types" (list op type-tags)))
									)
							)
					)
					(error "No method for these types" (list op type-tags))
				)
			)
		)
	)
)

; ex. 2.82 - generalize apply-generic to handle coercion in the general case of multiple arguments
(define (append list1 list2) (fold-right cons list2 list1))
(define (coerce-to-type type1 args)
	(define (iter result items)
		(let ((type-args (map type-tag items)))
			(if (null? items)
				result
				(let ((a2 (car items))
					(type2 (car type-args))
					(t2->t1 (get-coercion (car type-args) type1))
					)
						(cond ((eq? type1 type2) (iter (append result (list a2)) (cdr items)))
							((null? t2->t1) '())
							(else (iter (append result (list (t2->t1 a2))) (cdr items)))
						)
				)
			)
		)
	)
	(iter '() args)
)
(define (coerce-types args)
	(define (iter items)
		(if (null? items)
			'()
			(let ((attempt (coerce-to-type (type-tag (car items)) args)))
				(if (null? attempt)
					(iter (cdr items))
					attempt
				)
			)
		)
	)
	(iter args)
)
(define (apply-generic op . args)
	(let ((type-tags (map type-tag args)))
		(display type-tags)
		(newline)
		(let ((proc (get op type-tags)))
	  		(display proc)
		  	(newline)
			(if (not (null? proc))
				(apply proc (map contents args))
				(if (not (eq? (car type-tags) (cadr type-tags)))
					(let ((coerced-args (coerce-types args)))
						(display coerced-args)
						(newline)
						(if (null? coerced-args)
							(error "No method for these types" (list op (map type-tag coerced-args)))
							(apply apply-generic op coerced-args)
						)
					)
					(error "No method for these types" (list op type-tags))
				)
			)
		)
	)
)

;;; stopped on 2.84
(define (choose-lower-type type1 type2)
	(cond
		((eq? type1 type2) type1)
		((eq? type2 'complex) type1)
		((eq? type1 'complex) type2)
		((eq? type2 'rational) type1)
		((eq? type1 'rational) type2)
		(else type2)
	)
)
(define (lower-type? type1 type2)
	(and (eq? (choose-lower-type type1 type2) type1) (not (eq? type1 type2)))
)
(define (find-highest-type type-args)
	(define (iter candidate args)
		(if (null? args)
			candidate
			(iter (if (lower-type? candidate (car args)) (car args) candidate) (cdr args))
		)
	)
	(iter '() type-args)
)
; use find-highest-type to successively raise types in the list
(define (coerce-types args)
	(let	((high-type (find-highest-type (map type-tag args)))
		)
			(define (raise-till x)
				(if (lower-type? (type-tag x) high-type)
					(raise-till (raise1 x))
					x
				)
			)
			(map raise-till args)
	)
)
(define (apply-generic op . args)
	(let ((type-tags (map type-tag args)))
		(display type-tags)
		(newline)
		(let ((proc (get op type-tags)))
	  		(display proc)
		  	(newline)
		  	(display args)
		  	(newline)
			(if (not (null? proc))
				(apply proc (map contents args))
				(if (not (eq? (car type-tags) (cadr type-tags)))
					(let ((coerced-args (coerce-types args)))
						(display coerced-args)
						(newline)
						(if (null? coerced-args)
							(error "No method for these types" (list op (map type-tag coerced-args)))
							(apply apply-generic op coerced-args)
						)
					)
					(error "No method for these types" (list op type-tags))
				)
			)
		)
	)
)


;;;;;;;;;;;;;;;;;
; 2.5.3 Symbolic algebra

(define (install-polynomial-package)
	;; internal procedures
	;; representation of poly
	(define (make-poly variable term-list)
		(cons variable term-list))
	(define (variable p) (car p))
	(define (term-list p) (cdr p))
	(define (variable? x) (symbol? x))
	(define (same-variable? v1 v2)
		(and (variable? v1) (variable? v2) (eq? v1 v2))
	)
	;; representation of terms and term lists	
	(define (adjoin-term term term-list)
		(if (=zero? (coeff term))
			term-list
			(cons term term-list))
	)
	(define (the-empty-termlist) '())
	(define (first-term term-list) (car term-list))
	(define (rest-terms term-list) (cdr term-list))
	(define (empty-termlist? term-list) (null? term-list))
	(define (make-term order coeff) (list order coeff))
	(define (order term) (car term))
	(define (coeff term) (cadr term))
	; procedures
	(define (add-terms L1 L2)
		(cond 
			((empty-termlist? L1) L2)
			((empty-termlist? L2) L1)
			(else
				(let ((t1 (first-term L1)) (t2 (first-term L2)))
					(cond 
						((> (order t1) (order t2)) 
							(adjoin-term t1 (add-terms (rest-terms L1) L2)))
						((< (order t1) (order t2))
							(adjoin-term t2 (add-terms L1 (rest-terms L2))))
						(else
							(adjoin-term
								(make-term (order t1)
								(add (coeff t1) (coeff t2)))
								(add-terms (rest-terms L1)
								(rest-terms L2))))
					)
				)
			)
		)
	)
	(define (add-poly p1 p2)
		(if (same-variable? (variable p1) (variable p2))
			(make-poly (variable p1)
				(add-terms (term-list p1)
				        (term-list p2)))
			(error "Polys not in same var -- ADD-POLY"
				(list p1 p2))
		)
	)
	(define (mul-terms L1 L2)
		(if (empty-termlist? L1)
			(the-empty-termlist)
			(add-terms (mul-term-by-all-terms (first-term L1) L2)
				(mul-terms (rest-terms L1) L2))
		)
	)
	(define (mul-term-by-all-terms t1 L)
		(if (empty-termlist? L)
			(the-empty-termlist)
			(let ((t2 (first-term L)))
				(adjoin-term
					(make-term (+ (order t1) (order t2))
					(mul (coeff t1) (coeff t2)))
					(mul-term-by-all-terms t1 (rest-terms L)))
			)
		)
	)
	(define (mul-poly p1 p2)
		(if (same-variable? (variable p1) (variable p2))
			(make-poly (variable p1)
				(mul-terms (term-list p1) (term-list p2))
			)
			(error "Polys not in same var -- MUL-POLY" (list p1 p2))
		)
	)
	(define (zero? terms)
		(or 
			(empty-termlist? terms)
			(=zero? (coeff (first-term terms)))
			(zero? (rest-terms terms))
		)
	)
	;; interface to rest of the system
	(define (tag p) (attach-tag 'polynomial p))
	(put 'add '(polynomial polynomial) 
		(lambda (p1 p2) (tag (add-poly p1 p2))))
	(put 'mul '(polynomial polynomial) 
		(lambda (p1 p2) (tag (mul-poly p1 p2))))
	(put 'make 'polynomial
		(lambda (var terms) (tag (make-poly var terms))))
	(put 'zero? 'polynomial
		(lambda (p) (zero (terms p))))
	'done
)
(define (make-polynomial var terms)
	((get 'make 'polynomial) var terms)
)
;; stopped before ex 2.87
