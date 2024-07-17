; ex 1.2
;(/ (+ 5 4 (- 2 (- 3 (+ 6 (/ 1 5))))) (* 3 (- 6 2) (- 2 7) ) )

; ex 1.3
(define (sum-sq a b c) (+ (* a a) (* b b) (* c c)))

; 1.1.7 Newton method for computing square root
(define (sqrt-iter guess x)
	(if (good-enough? guess x)
		guess
		(sqrt-iter (improve-guess guess x) x)
	)
)
(define (improve-guess guess x) (average guess (/ x guess)))
(define (average x y) (/ (+ x y) 2))
(define (good-enough? guess x)
	;(display (list (cons "x" x) (cons "guess" guess) (cons "error" (abs (- (square guess) x)))))
	;(read-char)
	(< (abs (- (square guess) x)) 0.001)
)
(define (sqrt x) (sqrt-iter 1.0 x))

; ex 1.6 - why special form should be used, what if function is used instead?
(define (new-if predicate then-clause else-clause)
	(cond 
		(predicate then-clause)
		(else else-clause)
	)
)
(define (sqrt-iter guess x)
	(new-if (good-enough? guess x)
		guess
		(sqrt-iter (improve-guess guess x) x)
	)
)
; (sqrt 1) ;Aborting!: maximum recursion depth exceeded
; new-if is defined as function, which is evaluated in applicative order, i.e. arguments of new-if are evaluated before applying it
; one of the arguments needs to be evaluated is new-if itself, which leads to infinite recursion regardless of the value of x

; ex 1.7 - method above gives inaccurate results for large and small numbers, why?
(define (rel-err x y) (/ (abs (- x y)) x) )
(define (sqrt-err1 x) (rel-err (sqrt (square x)) x))
(define (sqrt-err2 x) (rel-err (square (sqrt x)) x))
(define (sqrt-err x)
	(if (> (sqrt-err1 x) (sqrt-err2 x))
		(sqrt-err1 x) (sqrt-err2 x))
)
; good-enough? in some cases with large number returns #f even if the guess is exact:
;(abs (- (square 1e25) 1e50)) ; 2.077e34 >> 0.001 - iterative procedure will never stop in this case
; good-enough? exploits absolute error, which for small numbers is greater than the number itself
;(sqrt-err 1e-3) ; .96
(define (good-enough? guess x)
	(display (list (cons "x" x) (cons "guess" guess) (cons "error" (abs (- (square guess) x)))))
	(read-char)
	(< (rel-err guess (improve-guess guess x)) 0.001)
)
; relative error gives better accuracy for small numbers and doesn't halt on large numbers:
;(sqrt-err 1e-3) ; 1.21e-3
;(sqrt-err 1e150); 4.12e-6

; ex 1.8 - Newton method for computing cubic root
(define (cbrt x) (cbrt-iter 1.0 x))
(define (cbrt-iter guess x)
	(if (good-enough3? guess x)
		guess
		(cbrt-iter (improve-guess3 guess x) x)
	)
)
(define (good-enough3? guess x)
	;(display (list (cons "x" x) (cons "guess" guess) (cons "error" (abs (- (square guess) x)))))
	;(read-char)
	(< (rel-err guess (improve-guess3 guess x)) 0.001)
)
(define (improve-guess3 guess x) (/ (+ (/ x (square guess)) (* 2 guess)) 3) )

; ex 1.9
;(define (+ a b)
;  (if (= a 0)
;      b
;      (inc (+ (dec a) b))))
; this is recursive process, layout:
; (+ 4 5) -> (inc (+ (dec 4) 5)) -> (inc (+ 3 5)) -> (inc (inc (+ (dec 3) 5))) -> (inc (inc (+ 2 5)) -> ... -> (inc (inc (inc (inc (+ 0 5))))) -> (inc (inc (inc (inc 5)) -> 9
;(define (+ a b)
;  (if (= a 0)
;      b
;      (+ (dec a) (inc b))))
; this is iterative process, layout:
; (+ 4 5) -> (+ (dec 4) (inc 5)) -> (+ 3 6) -> (+ (dec 3) (inc 6)) -> (+ 2 7) -> ... -> (+ 0 9) -> 9

; ex 1.10 - Ackermann function
(define (A x y)
	(cond
		((= y 0) 0)
		((= x 0) (* 2 y))
		((= y 1) 2)
		(else
			(A (- x 1) (A x (- y 1)))
		)
	)
)
(define (f n) (A 0 n)) ; f(n) = 2*n = (2+)(2+)..(2)_n
(define (g n) (A 1 n)) ; g(n) = 2^n = (2*)(2*)..(2)_n
(define (h n) (A 2 n)) ; h(n) = 2^(2^(2^..)) = (2^)(2^)..(2)_n

; ex 1.11
; f(n) = n if n<3, f(n) = f(n-1)+2f(n-2)+3f(n-3), n>=3
(define (f-rec n)
	(if (< n 3) n (+ (f-rec (- n 1)) (* 2 (f-rec (- n 2))) (* 3 (f-rec (- n 3)))) )
)
(define (f-iter n)
	(define (f-iter1 a b c n)
		(if (< n 3)
			a
			(f-iter1 (+ a (* 2 b) (* 3 c)) a b (- n 1))
		)
	)
	(f-iter1 2 1 0 n)
)

; ex 1.12 - compute Pascal triangle coefficients
; Pa(m,n) = 1 if m = n or m = 1 or n < 3, Pa(m-1,n-1) + Pa(m,n-1) otherwise (m, n >= 1; n - row, m - column)
; recursive process
(define (pascal-rec m n)
	(if (or (= m 1) (= m n) (< n 3)) 
		1
		(+ (pascal-rec (- m 1) (- n 1)) (pascal-rec m (- n 1)) )
	)
)

(define (fast-expt b n)
	(cond	((= n 0) 1)
		((even? n) (square (fast-expt b (/ n 2))))
		(else (* b (fast-expt b (- n 1))))
	)
)

; ex 1.16 - fast exponentiation (iterative process) using logarithmic number of steps
(define (fast-expt-iter b n)
	(define (expt-iter a b n)
		(cond
			((= n 0) a)
			((even? n) (expt-iter a (* b b) (/ n 2)))
			(else (expt-iter (* a b) b (- n 1)))
		)
	)
	(expt-iter 1 b n)
)

; ex 1.17 - fast multiplication (recursive process) using logarithmic number of steps
(define (fast-mult-rec a b)
	(define (even? n) (= (remainder n 2) 0))
	(define (double n) (+ n n))
	(define (halve n)
		(if (even? n)
			(/ n 2)
			(halve (- n 1))
		)
	)
	(cond ((= b 0) 0)
		((even? b) (fast-mult-rec (double a) (halve b)))
		(else (+ a (fast-mult-rec a (- b 1)) ) )
	)
)

; ex 1.18 - fast multiplication (iterative process) using logarithmic number of steps
(define (fast-mult-iter a b)
	(define (mult-iter result a b)
		(cond ((= b 0) result)
			((even? b) (mult-iter result (double a) (halve b)) )
			(else (mult-iter (+ result a) a (- b 1)) )
		)
	)
	(mult-iter 0 a b)
)

; ex 1.19 - Fibonacci number using logarithmic number of steps
(define (fib n) (fib-iter 1 0 0 1 n))
(define (fib-iter a b p q count)
	(cond ((= count 0) b)
		((even? count) (fib-iter a b
					(+ (* q q) (* p p))
					(* q (+ q (* 2 p)))
					(/ count 2)))
		(else (fib-iter
			(+ (* b q) (* a q) (* a p))
			(+ (* b p) (* a q))
			p q (- count 1)))
	)
)

; ex 1.22 - search-for-primes checks the primality of consecutive odd integers in a specified range
(define (search-for-primes m n)
	(cond ((> m n) true)
		( (= (remainder m 2) 0) (search-for-primes (+ m 1) n))
		(else (timed-prime-test m) (search-for-primes (+ m 2) n))
	)
)
(define (smallest-divisor n) (find-divisor n 2))
(define (divides? n p) (= (remainder p n) 0))
; ex 1.23 - test only odd numbers when searching consecutive divisors
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
(define (prime? n) (= (smallest-divisor n) n))
(define (timed-prime-test n) (start-prime-test n (real-time-clock)) )
(define (start-prime-test n start-time)
	(if (prime? n)
		(report-prime n (- (real-time-clock) start-time))
		(display "")
	)
)
(define (report-prime n elapsed-time)
	(display (cons "time " elapsed-time))
	(display " *** ")
	(display (cons "prime " n))
	(newline)
)
; amount of steps and elapsed time should grow as O(sqrt(n))
(define x (fast-expt-iter 10 9)) (search-for-primes x (+ x 1000))
(define x (fast-expt-iter 10 11)) (search-for-primes x (+ x 1000))
(define x (fast-expt-iter 10 13)) (search-for-primes x (+ x 1000))

; ex 1.24 - use fixed amount of Fermat test
(define (start-prime-test n start-time)
	(if (prime-fermat? n 20)
		(report-prime n (- (real-time-clock) start-time))
		(display "")
	)
)
; Fermat test O(log n)
(define (expmod base exp m)
	(cond	((= exp 0) 1)
		((even? exp) (remainder
			(square (expmod base (/ exp 2) m)) m))
		(else (remainder
			(* base (expmod base (- exp 1) m)) m))
	)
)
; returns true if n is (supposedly) prime, test is probabilistic
(define (fermat-test n)
	(define (try-it a) (= (expmod a n n) a))
	(try-it (+ 1 (random (- n 1))))
)
(define (fast-prime-test? n times tester)
	(cond ((= times 0) #t)
		((tester n) (prime-test? n (- times 1) tester))
		(else #f)
	)
)
(define (prime-fermat? n times) (fast-prime-test? n times fermat-test))
; O(log(n)); n = 10^k, k = 10, 20, .. should give linear time increase on average
(define x (fast-expt-iter 10 10)) (search-for-primes x (+ x 1000))
(define x (fast-expt-iter 10 20)) (search-for-primes x (+ x 1000))
(define x (fast-expt-iter 10 40)) (search-for-primes x (+ x 1000))
(define x (fast-expt-iter 10 80)) (search-for-primes x (+ x 1000))
(define x (fast-expt-iter 10 160)) (search-for-primes x (+ x 1000))
(define x (fast-expt-iter 10 320)) (search-for-primes x (+ x 1000))

; ex 1.25
(define (expmod base exp m) (remainder (fast-expt base exp) m))
; tests show O(n^2) instead of O(log n)
; possible explanation: remainder divides large a^n by large n directly instead of divide-and-conquer strategy
(define (timed-test f . args) (define t1 (real-time-clock)) (apply f args) (display (- (real-time-clock) t1)) (display newline)))
(define x 10000) (search-for-primes x (+ x 100))
(define x 20000) (search-for-primes x (+ x 100))
(define x 40000) (search-for-primes x (+ x 100))

; ex 1.26 - O(n) instead of O(log n), since same argument is computed twice inside argument, eliminating logarithmic order of growth
(define (expmod3 base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (* (expmod3 base (/ exp 2) m)
                       (expmod3 base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod3 base (- exp 1) m))
                    m)))
)
; ex 1.28 - Miller-Rabin test 
(define (miller-rabin-test n)
	(define (expmod2 base exp mod)
		(cond	((= exp 0) 1)
			((even? exp)
				(if (and (= (remainder (square base) mod) 1)
					(> (remainder base mod) 1)
					(< (remainder base mod) (- mod 1)) )
					0
					(remainder (square (expmod2 base (/ exp 2) mod)) mod))
			)
			(else (remainder (* base (expmod2 base (- exp 1) mod)) mod))
		)
	)
	(define (try-it a n) (< (expmod2 a (- n 1) n) 2))
	(try-it (+ 1 (random (- n 1))) n)
)
(define (prime-miller-rabin? n times) (fast-prime-test? n times miller-rabin-test))
; Carmichael numbers https://oeis.org/A002997/list
(define carm (list 561 1105 1729 2465 2821 6601 8911 10585 15841 29341 41041 46657 52633 62745 63973 75361 101101 115921 126217 162401 172081 188461 252601 278545 294409 314821 334153 340561 399001 410041 449065 488881 512461 530881 552721))
; Fermat test considers Carmichael numbers as prime:
(fold (lambda (x y) (and (fermat-test x) y)) #t carm) ; #t - all numbers in list are prime
(miller-rabin-test 561) ; #f

; ex 1.29 - Simpson's Rule of numerical integration
(define (integral2 f a b n)
	(define (add-dx x) (+ x dx))
	(define (add-dx2 x) (+ x (* 2 dx)))
	(define dx (/ (- b a) n))
	(* (/ dx 3)
		(+ (* 4 (sum f (+ a dx) add-dx2 b))
			(* 2 (sum f (+ a (* 2 dx)) add-dx2 (- b dx)))
			(f a) (f b)))
)

; ex 1.30 - iterative sum
(define (sum term a next b)
	(define (iter a result)
		(if (> a b) result
			(iter (next a) (+ result (term a))))
	)
	(iter a 0.0)
)

; ex 1.31 - product + factorial + pi approximations
; iterative process
(define (product-iter term a next b)
	(define (iter a result)
		(if (> a b) result
			(iter (next a) (* result (term a)))))
	(iter a 1.0)
)
; recursive process
(define (product-rec term a next b)
	(if (> a b) 1.0
		(* (term a) (product-rec term (next a) next b )))
)
(define product product-iter)
; factorial in terms of product
(define inc (lambda (x) (+ x 1)))
(define (factorial n) (product (lambda (x) x) 1 inc n))
; pi approximation in terms of product
(define (integer-part n mod) (- n (remainder n mod)))
(define (pi-numer-term n) (integer-part (+ n 3) 2))
(define (pi-denom-term n) (+ 1 (integer-part (+ n 2) 2)))
; naive unstable approach (divide two large numbers) diverges with increasing terms
(define (pi-approx-bad terms)
	(* 4 (/ (product pi-numer-term 0 inc (- terms 1) ) (product pi-denom-term 0 inc (- terms 1) ))))
(define (pi-next a)
	(if (even? a)
		(/ (+ a 2) (+ a 1))
		(/ (+ a 1) (+ a 2))
	)
)
(define (pi-approx terms)
	(* 4 (product pi-next 1 inc terms))
)

; ex 1.32 - accumulate 
; iterative process
(define (accumulate combiner null-value term a next b)
	(define (iter a result)
		(if (> a b)
			result
			(iter (next a) (combiner result (term a))))
	)
	(iter a null-value)
)
; recursive process
(define (accumulate combiner null-value term a next b)
	(if (> a b) null-value
		(combiner (term a) (accumulate combiner null-value term (next a) next b)))
)
(define (sum term a next b) (accumulate + 0.0 term a next b))
(define (product term a next b) (accumulate * 1.0 term a next b))

; ex 1.33
(define (filtered-accumulate filter combiner null-value term a next b)
	(define (iter a result)
		(cond	((> a b) result)
			((filter a) (iter (next a) (combiner result (term a))))
			(else (iter (next a) (combiner result null-value) ))
		)
	)
	(iter a null-value)
)
; 1.33(a)
(define (sum-prime-squares a b)
	(filtered-accumulate prime? + 0.0 square a (lambda (x) (+ x 1)) b)
)
; 1.33(b)
(define (gcd a b)
	(if (= b 0) a
		(gcd b (remainder a b)))
)
(define (gcd a b)
	(cond	((> b a) (gcd b a))
		((= b 0) a)
		(else (gcd b (remainder a b)))
	)
)
(define (product-rel-prime n)
	(define (rel-prime? a)
		(= (gcd a n) 1)
	)
	(filtered-accumulate rel-prime? * 1.0 identity 1 (lambda (x) (+ x 1)) (- n 1))
)


(define (search f neg-point pos-point)
	(define tolerance 0.0001)
	(define (close-enough? a b) (< (abs (- a b)) tolerance))
	(let ((midpoint (/ (+ neg-point pos-point) 2.0)))
		(if (close-enough? neg-point pos-point)
			midpoint
			(let ((test-value (f midpoint)))
				(cond   ((> test-value 0.0) (search f neg-point midpoint))
					((< test-value 0.0) (search f midpoint pos-point))
					(else midpoint)
				)
			)
		)
	)
)
(define (half-interval-method f a b)
	(let 
		((x (f a))
		 (y (f b)))
		 (cond	((and (< x 0) (> y 0)) (search f a b))
		 	((and (> x 0) (< y 0)) (search f b a))
		 	(else (error "Values are not of opposite signs."))
		 )
	)
)
(define (fixed-point f first-guess)
	(define tolerance 0.0001)
	(define (close-enough? a b) (< (abs (- a b)) tolerance))
	(define (try guess)
		(let ((next-guess (f guess)))
			(if (close-enough? guess next-guess)
				guess
				(try next-guess)
			)
		)
	)
	(try first-guess)
)

; ex 1.35 - compute golden ratio as a fixed point of x -> 1 + 1/x
(define phi (fixed-point (lambda (x) (+ 1 (/ 1 x))) 1.0))

; ex 1.36 - fixed point of functions: find x = f(x) using iterative process with initial guess, displaying approximations
(define (fixed-point f first-guess)
	(define tolerance 0.0001)
	(define (close-enough? a b) (< (abs (- a b)) tolerance))
	(define (try guess steps)
		(let ((next-guess (f guess)))
			(newline) (display guess) (display (string " (step " steps ")"))
			(if (close-enough? guess next-guess)
				guess
				(try next-guess (+ steps 1))
			)
		)
	)	
	(try first-guess 0)
)
(define (average x y) (/ (+ x y) 2))
(fixed-point (lambda (x) (/ (log 1000) (log x))) 10.0)
(fixed-point (lambda (x) 
	(average x (/ (log 1000) (log x)) )
		) 10.0)
; makes times iterations of fixed point
(define (fixed-point-times f first-guess times)
	(define tolerance 0.001)
	(define (close-enough? a b) (< (abs (- a b)) tolerance))
	(define (try guess n)
		(let ( (next-guess (f guess)))
			(newline) (display guess)
				(if (or (= n 0) (close-enough? guess next-guess))
						guess
						(try next-guess (- n 1))
				)
		)
	)	
	(try first-guess times)
)

; ex 1.37 - continuous fractions
; iterative process
(define (cont-frac-iter n d k)
	(define (step guess i)
		(if (= i 0)
			guess
			(step (/ (n i) (+ guess (d i))) (- i 1))
		)
	)
	(step 0. k)
)
; recursive process
(define (cont-frac-rec n d k)
	(if (= k 0)
			0.
			(/ (n k) (+ (cont-frac-rec n d (- k 1)) (d k)))
	)
)
(define cont-frac cont-frac-iter)
; check procedure by approximating 1/phi = (sqrt(5)-1)/2 using n_i = d_i = 1
; how large must you make k in order to get an approximation that is accurate to 4 decimal places?
(define (errphi k) (abs (- (/ (- (sqrt 5) 1) 2) (cont-frac (lambda (i) 1.0) (lambda (i) 1.0) k))))
(errphi 10) ;Value: 5.6460660007306984e-5

; ex 1.38 - e approximation using continuous fraction n_i = 1, d_i = 1 2 1 1 4 1 1 6 1 1 8 ...
(define (integer-part n mod) (- n (remainder n mod)))
(define (euler-approx k) (+ 2 (cont-frac 
	(lambda (i) 1)
	(lambda (i)
		(if (= (remainder (+ i 1) 3) 0)
			(* 2. (+ 1 (/ (integer-part i 3) 3)))
			1
		)
	)
	k))
)

; ex 1.39 - Lambert formula for tangent function using continuous fraction
(define (tan-cf x k)
	(cont-frac 
		(lambda (i) (if (= i 1) x (- (square x))))
		(lambda (i) (- (* 2 i) 1))
		k
	)
)
;* hyperbolic tangent function:
(define (tanh-cf x k)
	(cont-frac 
		(lambda (i) (if (= i 1) x (square x)) )
		(lambda (i) (- (* 2 i) 1))
		k
	)
)

(define (average-damp f) (lambda (x) (average x (f x))))
(define (average a b) (/ (+ a b) 2))
(define (sqrt x)
	(fixed-point (average-damp (lambda (y) (/ x y))) 1.0)
)
(define (cbrt x)
	(fixed-point (average-damp (lambda (y) (/ x (square y)))) 1.0)
)
(define (deriv g)
	(define dx 0.000001)
	(lambda (x)
		(/ (- (g (+ x dx)) (g x)) dx)
	)
)
(define (newton-transform f)
	(lambda (x)
		(- x (/ (f x) ((deriv f) x) ))
	)
)
(define (newton-method f guess)
	(fixed-point (newton-transform f) guess)
)
(define (fixed-point-of-transform f transform guess)
	(fixed-point (transform f) guess)
)
(define (sqrt x)
	(fixed-point-of-transform (lambda (y) (/ x y))
	average-damp 1.0)
)
(define (sqrt x)
	(fixed-point-of-transform (lambda (y) (- (square y) x))
	newton-transform 1.0)
)

; ex 1.40
(define (cubic a b c)
	(lambda (x) (+ (* x x x) (* a (square x)) (* b x) c))
)

; ex 1.41
(define (double procedure)
	(lambda (x) (procedure (procedure x)))
)
; (((double (double double)) (lambda (x) (+ x 1))) 5) -> ?
; (double f) -> (lambda (x) (f (f x)))
; (double double) -> (lambda (x) (double (double x)))
; (double (double double)) -> (lambda (x) ((double double) ((double double) x)))
; ((double (double double)) f) -> ((double double) ((double double) f)) -> ((double double) (double (double f))) -> (double (double (double (double f)) ))
; ((double (double double)) (lambda (x) (+ x 1))) -> (double (double (double (double (lambda (x) (+ x 1)))))) -> (double (double (double (lambda (x) (+ x 2))))) -> ... -> (lambda (x) (+ x 16))
; (((double (double double)) (lambda (x) (+ x 1))) 5) -> 21

; ex 1.42 - composition of functions
(define (compose f g)
	(lambda (x) (f (g x)))
)

; ex 1.43 - repeated application of given function
; iterative implementation
(define (repeated f n)
	(define (iter f m result)
		(if (= m 0)
			result
			(iter f (- m 1) (f result))
		)
	)
	(lambda (x)
		(iter f n x)
	)
)
; implementation using compose
(define (repeated f n)
	(cond ((= n 0) (lambda (x) x))
		((even? n) (repeated (compose f f) (/ n 2)))
		(else (compose f (repeated f (- n 1))) )
	)
)

; ex 1.44 - smoothed function
(define (smooth f)
	(define dx 0.0001)
	(lambda (x)
		(/ (+ (f (+ x dx)) (f x) (f (- x dx))) 3.)
	)
)
(define (smooth-n f n) ((repeated smooth n) f))

; ex 1.45 - computing nth roots using fixed point with average damping applied m times
; experiments show that m: 2^m < n < 2^(m+1) is enough
(define (fast-expt b n)
	(cond	((= n 0) 1)
		((even? n) (square (fast-expt b (/ n 2))))
		(else (* b (fast-expt b (- n 1))))
	)
)
(define (root x n)
	(define avgtimes (floor (/ (log n) (log 2))))
	(fixed-point-of-transform
		(lambda (y) (/ x (fast-expt y (- n 1))))
		(repeated average-damp avgtimes) 1.0
	)
)

; ex 1.46 - iterative improvement procedure
(define (rel-err x y) (/ (abs (- x y)) x) )
(define (abs-err x y) (abs (- x y)))
(define (good-guess? guess)
	(define rtol 0.00001)
	(define atol 0.00001)
	(lambda (f)
		(let ((fguess (f guess)))
			(and 
				(< (rel-err guess fguess) rtol)
				(< (abs-err guess fguess) atol)
			)
		)
	)
)
(define (iterative-improve good-guess? improve-guess)
	(define (iter guess)
		(if (good-guess? guess)
			guess
			(iter (improve-guess guess))
		)
	)
	iter
)
; fixed point in terms of iterative-improve
(define (fixed-point f first-guess)
	((iterative-improve (good-guess? f) f) first-guess)
)
; sqrt in terms of iterative-improve
(define (sqrt x)
	((iterative-improve
		(lambda (y) (and (< (rel-err (square y) x) 0.00001) (< (abs (- (square y) x)) 0.00001)))
		(lambda (y) (average y (/ x y))))
	1.)
)
