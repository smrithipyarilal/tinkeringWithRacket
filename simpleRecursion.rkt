#lang racket

;The following two lines are required to test the code.
(require rackunit)
(require rackunit/text-ui)

#|
CS 270
Homework 1

Create By Professors Bruce Char, Mark Boady, Jeremy Johnson, and Steve Earth

Complete each of the below functions.

Tests given are not designed to be comprehensive.
They will give you an idea if your code is right, but they do not test all possible cases.
Think about your design.
When grading, we may add additional tests for your functions.

Important Rules:
1.) You may not use any loop constructs; if used, your answer will get a zero.
2.) All these functions must be implemented recursively -- you will receive a zero if there is no recursion.
Note that for some questions, recursive helper functions are allowed
(in which case, the grader will still give credit even with the main function not being recursive).
3.) You may not use the set! command. If used, your answer will get a zero.
As a general rule, you should only be using functions taught in class (otherwise you are likely missing
the point of the question and endangering your score).
4.) Using If/Cond to explicitly pass tests instead of following the instructions will always result in a
zero for that question.  Do not assume that simply because your code passes all the example unit tests here
then that means you get 100%; we will be testing with input beyond the examples.  The graders will
still obey the input contracts.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;Part 1 - Summations ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
|#

;Question 1 (10 points)
;input contract:  n is a positive integer
;output contract: (sumsqs n) is the sum of the first n squares.
;for example, when n=4, the output would be 30 because of 1 + 4 + 9 + 16

(define (sumsqs n)
  (if (equal? n 1) 1
      (+ (* n n) (sumsqs (- n 1)))))


;Test Bed
(display "Question 1 Tests (10 points)\n")
(define-test-suite test_sum1
  (check-equal? (sumsqs 1) 1)
  (check-equal? (sumsqs 2) 5)
  (check-equal? (sumsqs 3) 14)
  (check-equal? (sumsqs 4) 30)
  (check-equal? (sumsqs 5) 55)
  (check-equal? (sumsqs 6) 91)
  (check-equal? (sumsqs 7) 140)
  (check-equal? (sumsqs 8) 204)
  (check-equal? (sumsqs 9) 285)
  (check-equal? (sumsqs 10) 385)
)
(define q1_score (- 10 (run-tests test_sum1)))



;Question 2 (10 points)
;input contract:  n is a positive integer
;output contract: (sumodds n) is the sum of the first n odd numbers.
;for example, when n=4, the output would be 16 because of 1 + 3 + 5 + 7
(define (sumodds n)
  (if (equal? (- n 1) 0) 1
      (+ ( - (* 2 n) 1) (sumodds (- n 1)))))


;Test Bed
(display "Question 2 Tests (10 points)\n")
(define-test-suite test_sum2
  (check-equal? (sumodds 1) 1)
  (check-equal? (sumodds 2) 4)
  (check-equal? (sumodds 3) 9)
  (check-equal? (sumodds 4) 16)
  (check-equal? (sumodds 5) 25)
  (check-equal? (sumodds 6) 36)
  (check-equal? (sumodds 7) 49)
  (check-equal? (sumodds 8) 64)
  (check-equal? (sumodds 9) 81)
  (check-equal? (sumodds 10) 100)
)
(define q2_score (- 10 (run-tests test_sum2 'verbose)))



#|
Question 3 (10 points)
;input contract:  n is a positive integer
;output contract: (altcubes n) is an alternating adding/subtracting of cubes
;for example, when n=5, the output would be 81  because of 1 - 8 + 27 - 64 + 125
; hint: the expt command is inefficient here and should not be used! 
|#
(define (altcubes n)
  (if (equal? n 1) 1 
  (if (equal? (remainder n 2) 0)
      (- (altcubes (- n 1)) (* n (* n n)) )
      (+ (altcubes (- n 1)) (* n (* n n)))
)))

;Test Bed
(display "Question 3 Tests (10 points)\n")
(define-test-suite test_sum3
  (check-equal? (altcubes 1) 1)
  (check-equal? (altcubes 2) -7)
  (check-equal? (altcubes 3) 20)
  (check-equal? (altcubes 4) -44)
  (check-equal? (altcubes 5) 81)
  (check-equal? (altcubes 6) -135)
  (check-equal? (altcubes 10) -575)
  (check-equal? (altcubes 100) -507500)
  (check-equal? (altcubes 347) 20981268)
  (check-equal? (altcubes 1001) 502253001)
)
(define q3_score (- 10 (run-tests test_sum3 'verbose)))


;Question 4 (10 points)
;input contract:  n is a positive integer
;output contract: (oddfact n) is the "factorial" of just the first n odds
;for example, when n=5, the output would be 945 because of 9*7*5*3*1

(define (oddfact n)
  (if (equal? (- n 1) 0) 1
      (* ( - (* 2 n) 1) (oddfact (- n 1)))))

;Test Bed
(display "Question 4 Tests (10 points)\n")
(define-test-suite test_sum4
  (check-equal? (oddfact 1) 1)
  (check-equal? (oddfact 2) 3)
  (check-equal? (oddfact 3) 15)
  (check-equal? (oddfact 4) 105)
  (check-equal? (oddfact 5) 945)
  (check-equal? (oddfact 6) 10395)
  (check-equal? (oddfact 7) 135135)
  (check-equal? (oddfact 8) 2027025)
  (check-equal? (oddfact 9) 34459425)
  (check-equal? (oddfact 10) 654729075)
)
(define q4_score (- 10 (run-tests test_sum4 'verbose)))

;Question 5 (10 points)
;input contract:  n is a positive integer
;output contract: (logfloor n) is the largest power of 2 which is less than or equal to n
;for example, when n=100, the output would be 6 because 2^6 <= 100 but 2^7 > 100
; do NOT use any math operations beyond basic arithmetic (+, - , *, /) to do this,
; however, a helper function might be useful

(define (lgh n i)
  (if (> (expt 2 i) n) (- i 1)
      (lgh n (+ i 1))))

(define (logfloor n)
  (lgh n 1))

;Test Bed
(display "Question 5 Tests (10 points)\n")
(define-test-suite test_sum5
  (check-equal? (logfloor 1) 0)
  (check-equal? (logfloor 2) 1)
  (check-equal? (logfloor 3) 1)
  (check-equal? (logfloor 4) 2)
  (check-equal? (logfloor 5) 2)
  (check-equal? (logfloor 6) 2)
  (check-equal? (logfloor 7) 2)
  (check-equal? (logfloor 8) 3)
  (check-equal? (logfloor 9) 3)
  (check-equal? (logfloor 10) 3)
)
(define q5_score (- 10 (run-tests test_sum5 'verbose)))

#|
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;Part 2 - Puzzles ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

Question 6 (10 points)
Staircase climbing

You are standing at the bottom of a staircase and heading to the top.
You can take big steps (2 at a time) or small steps.

;input contract:  n is a positive integer representing the number of steps on the staircase
;output contract: (waysup n) is the number of different ways you could get to the top from the bottom
;for example, when n=4, the output would be 5 because there are five different ways to climb the 4 stairs:
[1] ssss  [2] ssb  [3] sbs  [4] bss [5] bb.  note that s means taking a small step, and b means a big step.
|#


#|

EXPLANATION

Ok so this one was slightly tricky! 
The pattern is similar to the fibonacci series; only difference - for n number of stairs, the possible wasy to climb
it are F(n + 1) where F() is the fibonacci function.
Ok so firstly, why is this similar to fibonacci sequence? well, lets say for 3 stairs, we can get to the third step by either
taking one big step from the first step, or one small step from the second step.
So we add this additional bit to the number of wasy we could climb one stair and two stairs.

For 1 stair - s       [1 way]
For 2 stairs - ss, b  [2 ways]
For 3 stairs - (one stair + b) + (2 stairs + s)
               sb, sss, bs  [3 ways]

and when we look at these numbers - 1 + 2 = 3
This pattern of the possible ways to climb n stairs is (the possible ways to climb (n - 1) stairs) +  (the possible ways to climb (n - 2) stairs)
repeats, and the code is based on this logic. 
The reason why for n number of stairs, the possible wasy to climb
are F(n + 1) and not F(n) is because, in the fibonacci series, the starting value is 0, however, in this problem, we start with 1 stair.
and because we need atleast two values to start the sequence, thats why we have if (< n 3). 
Hope this helps :)
|#

(define (stepways n)
  (if (< n 3) n
     (+ (stepways (- n 1)) (stepways (- n 2)))))



;Test Bed
(display "Question 6 Tests (10 points)\n")
(define-test-suite test_puzzle
  (check-equal? (stepways 1) 1)
  (check-equal? (stepways 2) 2)
  (check-equal? (stepways 3) 3)
  (check-equal? (stepways 5) 8)
  (check-equal? (stepways 10) 89)
  (check-equal? (stepways 12) 233)
  (check-equal? (stepways 15) 987)
  (check-equal? (stepways 20) 10946)
  (check-equal? (stepways 25) 121393)
  (check-equal? (stepways 30) 1346269)
)
(define q6_score (- 10 (run-tests test_puzzle 'verbose)))

#|
Question 7 (20pts)
 
; input contract: n is any integer
; output contract: (is_prime n) returns #t if n is prime and false otherwise.

Here is a basic layout for your function.
1.) Negative Numbers, 0, and 1 are not primes.
2.) To determine if n is prime:
2a.) See if n is divisible by i=2
2b.) Set i=i+1
2c.) If i^2 <=n continue.
3.) If no values of i evenly divided n, then it must be prime.
Note: You can stop when i*i > n.
Why?
Take n=19 as an example.
i=2, 2 does not divide 19 evenly
i=3, 3 does not divide 19 evenly
i=4, 4 does not divide 19 evenly
i=5, we don't need to test this. 5*5=25.
If 5*x=19, the value of x would have to be smaller then 5.
We already tested those values!
No larger numbers can be factors unless one we already test is to.

Hint: You may have the recursion take place in a helper function!
In other words, define two functions, and have the "main" function
call the helper function which recursively performs the subcomputations.
|#

(define (primeh n i)
  (cond
  [(equal? (remainder n i) 0) #f]
  [(< (* i i) n) (primeh n (+ i 1))]
  [else #t]))

(define (is_prime n)
  (cond
  [(equal? n 2) #t]
  [(> n 2) (primeh n 2)]
  [else  #f]))



;Here are some tests to see if your function works.
(display "Question 7 Tests (20 points)\n")
(define-test-suite test_is_prime
  (check-equal? (is_prime -1) #f)
  (check-equal? (is_prime 0) #f)
  (check-equal? (is_prime 1) #f)
  (check-equal? (is_prime 2) #t)
  (check-equal? (is_prime 3) #t)
  (check-equal? (is_prime 4) #f)
  (check-equal? (is_prime 5) #t)
  (check-equal? (is_prime 6) #f)
  (check-equal? (is_prime 7) #t)
  (check-equal? (is_prime 8) #f)
  (check-equal? (is_prime 9) #f)
  (check-equal? (is_prime 10) #f)
  (check-equal? (is_prime 11) #t)
  (check-equal? (is_prime 12) #f)
  (check-equal? (is_prime 13) #t)
  (check-equal? (is_prime 14) #f)
  (check-equal? (is_prime 16) #f)
  (check-equal? (is_prime 18) #f)
  (check-equal? (is_prime 19) #t)
  (check-equal? (is_prime 20) #f)
)
(define q7_score (- 20 (run-tests test_is_prime 'verbose)))

#|
Question 8 (10 points)
; input contract: n is a positive integer
; output contract: (sum_digits n) returns the sum of the digits of n.
;for example, when n=3123, the output would be 9 because of 3+1+2+3
; note: you should NOT be converting to strings to do this problem -- do all calculations with INTEGERS
|#

(define (sum_digits n)
  (if (equal? (quotient n 10) 0) n
      (+ (remainder n 10) (sum_digits (quotient n 10)))))

;Test Bed
(display "Question 8 Tests (20 points)\n")
(define-test-suite test_sum_digits
  (check-equal? (sum_digits 395718860534) 59)
  (check-equal? (sum_digits 193139816415) 51)
  (check-equal? (sum_digits 22424170465) 37)
  (check-equal? (sum_digits 800187484459) 58)
  (check-equal? (sum_digits 427552056869) 59)
  (check-equal? (sum_digits 842622684442) 52)
  (check-equal? (sum_digits 412286285840) 50)
  (check-equal? (sum_digits 996417214180) 52)
  (check-equal? (sum_digits 386408307450) 48)
  (check-equal? (sum_digits 694607189265) 63)
  (check-equal? (sum_digits 773012980023) 42)
  (check-equal? (sum_digits 730616292946) 55)
  (check-equal? (sum_digits 106507053657) 45)
  (check-equal? (sum_digits 396412723003) 40)
  (check-equal? (sum_digits 944913350029) 49)
  (check-equal? (sum_digits 210936428922) 48)
  (check-equal? (sum_digits 750072072199) 49)
  (check-equal? (sum_digits 454744396973) 65)
  (check-equal? (sum_digits 736602622344) 45)
  (check-equal? (sum_digits 329844591802) 55)
)
(define q8_score (- 20 (run-tests test_sum_digits 'verbose)))


;---------------------------------------------------------------------
;---------------------------------------------------------------------
;---------------------------------------------------------------------
;;;;;;;;;;;;;;Test Summary;;;;;;;;;;;;;;;;;;;;;;;
(display "------Test Summary------\n")
(display "Q1 Passed ")
(display q1_score)
(display "/10\n")
(display "Q2 Passed ")
(display q2_score)
(display "/10\n")
(display "Q3 Passed ")
(display q3_score)
(display "/10\n")
(display "Q4 Passed ")
(display q4_score)
(display "/10\n")
(display "Q5 Passed ")
(display q5_score)
(display "/10\n")
(display "Q6 Passed ")
(display q6_score)
(display "/10\n")
(display "Q7 Passed ")
(display q7_score)
(display "/20\n")
(display "Q8 Passed ")
(display q8_score)
(display "/20\n")

(define grand_total (+ q1_score q2_score q3_score q4_score q5_score q6_score q7_score q8_score))
(display "\n")
(display "Total: ")
(display grand_total)
(display "/100\n")