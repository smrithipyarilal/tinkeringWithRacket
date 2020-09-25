#lang racket

#|
AUTHOR      - Smrithi Pyari Lal
DATE        - July 13th, 2020
COURSE      - CS270
PROFESSOR   - Steve Earth
|#


#|
CS 270 Math Foundations of CS
Homework 3
Create By Professor Bruce Char, Professor Mark Boady, and Professor Jeremy Johnson

Submit in BBLearn.

Once you write a function, you may use it in later questions.

Important Rules:
1) If the instructions state something must be recursive, you will recieve a zero if it is not recursive.
   Helper functions are sometimes allowed (and then the recursion could be in the helper rather than main).
2) If you think you need functions not taught in class, you are likely approaching the problem wrong
3) while you make cond cases to handle bases, it is not permitted to simple write
   cond cases for all the unit tests!

In this assignment students will write several functions that
recursively process numbers.  In class we provided two recursive
definitions of numbers.  The recursive definitions provide a set
of constructors which capture the ways to construct a number.
Peano numbers have two constructors, one of which, succ, is recursive,
and "DubNums" have three constructors, two of which (double and double-plus1) are recursive.

1)  Peano arithmetic

    In words:  A number is either zero, or, recursively, the successor
    of a number is a number.

    Formally:  Number := zero|(succ Number)

2)  DubNum arithmetic

    In words:  A DubNum is either zero, or recursively, doubling
    a (nonzero) number or doubling a number and adding one gives a dubNum.

    Formally:  DubNum := zero|(double DubNum)|(double-plus1 DubNum)

    We can interpret dubnums by assigning zero to the number zero
    and if B is a dubnum representing the integer value b, than
    (double B) has value 2b and (double-plus1 B) has value 2b+1.

    Note that there is more than one way to construct dubnums with
    the same value.  E.G.  (double binzero) would have the same value as zero, so our
    definition of double prevents constructions such as '(double (double binzero))

    When recursively processing either types of numbers, you must
    handle cases corresponding to the different constructors.  Recursive
    calls must have inputs whose size is smaller, where size is the
    number of constructors needed to build the number.

    In the first part of this assignment you will write recursive
    functions to subtract and divide Peano numbers.  You will also
    implement the Euclidean algorithm to compute the greatest common
    divisor, gcd, of Peano numbers.

    In the second part of this assignment you will write recursive functions
    to add and multiply DubNums using the recursive definition above.

|#

(require racket/contract)
(require rackunit)
(require rackunit/text-ui)

#|

Part I.  Peano arithmetic.

|#


(define (pzero? n)
  (equal? n 'pzero))

(define (nat? x)
  (cond
    [(zero? x) #t]
    [(pair? x) (and (equal? (first x) 'succ) (nat? (second x)))]
    [else #f]))

(define (succ n)
  (list 'succ n))

(define (pred n)
  (if (pzero? n) 'pzero (second n)))

(define zero 'pzero)
(define one (succ zero))
(define two (succ one))
(define three (succ two))
(define four (succ three))
(define five (succ four))
(define six (succ five))
(define seven (succ six))
(define eight (succ seven))
(define nine (succ eight))
(define ten (succ nine))

; addition of Peano numbers
; Input: M, N Peano numbers representing the integers m,n
; Output: the Peano number representing m+n
(define (plus M N)
  (if (pzero? M)
      N
      (succ (plus (pred M) N))))

; multiplication of Peano numbers
; Input: M, N Peano numbers representing the integers m,n
; Output: the Peano number representing m*n
(define (mult M N)
  (if (pzero? M)
      zero
      (plus N (mult (pred M) N))))
; comparison of Peano numbers
; Input: M, N Peano numbers representing the integers m,n
; Output: a boolean = #t if m < n and #f otherwise
(define (ltnat? M N)
  (cond
    [(pzero? N) #f]
    [(pzero? M) #t]
    [else (ltnat? (pred M) (pred N))]))

;Question 1.  Implement subtraction of Peano numbers.  See specification
;             below.
; subtraction of Peano numbers
; Input: M, N Peano numbers representing the integers m,n
; Output: the Peano number representing m-n if m >= n.
;         It is undefined otherwise. (note: to be undefined, that means (sub three five) gives NO RESULT.
;         It does NOT mean to print an error msg and gracefully exit or to crash!
;         
(define (sub M N)
  (cond
  [(ltnat? M N) (void)]
  [(pzero? N) M]
  [else (sub (pred M) (pred N))]
))

#|
Question 2 & 3.  Implement division of Peano numbers.  Implement functions to
             compute the quotient and remainder when dividing Peano numbers
             m and n.  I.E. compute q the quotient and r the remainder
             such that m = q*n + r with 0 <= r < n.  See specification
             below.
|#
; Division of Peano numbers
; Input: ; Input: M, N Peano numbers representing the integers m,n where n > 0
; Output: the Peano number representing q, the quotient of m divided by n.
;         in other words, m = q*n + r with 0 <= r < n.
(define (div M N)
  (if (ltnat? M N) 'pzero
  (succ (div (sub M N) N))
  
  ))

; Remainder of Peano numbers
; Input: ; Input: M, N Peano numbers representing the integers m,n where n > 0
; Output: the Peano number representing r, the remainder of m divided by n.
;         m = q*n + r with 0 <= r < n.  To be efficient, your answer must be recursive.
(define (rem M N)
  (cond
  [(equal? M N) 'pzero]
  [(ltnat? N M) (rem (sub M N) N)]
  [else M]
))

#|
Question 4.  Implement a function to compute the greatest common divisor
             of the Peano numbers M and N.  g = gcd(m,n) satisfies
             1)  g is a common divisor of m and n.
                 g divides m and g divides n.  I.E. the remainder when
                 dividing m and n by g is 0.
             2)  g is the greatest common divisor.
                 If e divides m and e divides n then e must divide g.

             MAJOR HINT:  the gcd(m,n) can be computed recursively as follows:
             1)  gcd(m,0) = m
             2)  gcd(m,n) = gcd(n,remainder of m divided by n).
|#

; Greatest common divisor of Peano numbers
; Input: ; Input: M, N Peano numbers representing the integers m,n
; Output: the Peano number representing gcd(m,n)
; note: for your function to be efficient, you should be implementing the hint given above.
(define (gcd M N)
  (if (equal? N 'pzero) M
      (gcd N (rem M N)))
)


(display "Question 1 - Subtraction (15 points)\n")
(define-test-suite peano-subtract
  (check-equal? (sub ten ten) zero)
  (check-equal? (sub ten two) eight)
  (check-equal? (sub nine nine) zero)
  (check-equal? (sub nine one) eight)
  (check-equal? (sub eight six) two)
  (check-equal? (sub eight five) three)
  (check-equal? (sub seven one) six)
  (check-equal? (sub seven five) two)
  (check-equal? (sub six six) zero)
  (check-equal? (sub six two) four)
  (check-equal? (sub five three) two)
  (check-equal? (sub four two) two)
  (check-equal? (sub three two) one)
  (check-equal? (sub two one) one)
  (check-equal? (sub one zero) one)
)
(define q1_score (- 15 (run-tests peano-subtract 'verbose)))

(display "Question 2 - Division (15 points)\n")
(define-test-suite peano-div
  (check-equal? (div ten ten) one)
  (check-equal? (div ten two) five)
  (check-equal? (div nine three) three)
  (check-equal? (div nine one) nine)
  (check-equal? (div eight six) one)
  (check-equal? (div eight four) two)
  (check-equal? (div one seven) zero)
  (check-equal? (div seven five) one)
  (check-equal? (div six six) one)
  (check-equal? (div six two) three)
  (check-equal? (div five three) one)
  (check-equal? (div four two) two)
  (check-equal? (div three two) one)
  (check-equal? (div two one) two)
  (check-equal? (div one five) zero)
)
(define q2_score  (- 15 (run-tests peano-div 'verbose)))

(display "Question 3 - Remainder (15 points)\n")
(define-test-suite peano-rem
  (check-equal? (rem ten three) one)
  (check-equal? (rem ten two) zero)
  (check-equal? (rem nine three) zero)
  (check-equal? (rem nine one) zero)
  (check-equal? (rem eight five) three)
  (check-equal? (rem eight four) zero)
  (check-equal? (rem one seven) one)
  (check-equal? (rem seven five) two)
  (check-equal? (rem six six) zero)
  (check-equal? (rem six two) zero)
  (check-equal? (rem five three) two)
  (check-equal? (rem four two) zero)
  (check-equal? (rem three two) one)
  (check-equal? (rem two one) zero)
  (check-equal? (rem one five) one)
)
(define q3_score  (- 15 (run-tests peano-rem 'verbose)))

(display "Question 4 - GCD (15 points)\n")
(define-test-suite peano-gcd
  (check-equal? (gcd nine ten) one)
  (check-equal? (gcd three nine) three)
  (check-equal? (gcd six seven) one)
  (check-equal? (gcd eight two) two)
  (check-equal? (gcd nine one) one)
  (check-equal? (gcd ten three) one)
  (check-equal? (gcd six seven) one)
  (check-equal? (gcd five ten) five)
  (check-equal? (gcd three zero) three)
  (check-equal? (gcd six one) one)
  (check-equal? (gcd eight four) four)
  (check-equal? (gcd eight eight) eight)
  (check-equal? (gcd three two) one)
  (check-equal? (gcd three five) one)
  (check-equal? (gcd three three) three)
)
(define q4_score  (- 15 (run-tests peano-gcd 'verbose)))
#|

Part II.  DubNum arithmetic.
            
|#

(define binzero 'bzero)

(define (binzero? b)
  (equal? b binzero))

(define (double b)
  (if (binzero? b) binzero (list 'D b)))

(define (double-plus1 b)
  (list 'DP1 b))

(define (double? b)
  (cond
    [(not (pair? b)) #f]
    [(equal? (first b) 'D) #t]
    [else #f]))

(define (double-plus1? b)
  (cond
    [(not (pair? b)) #f]
    [(equal? (first b) 'DP1) #t]
    [else #f]))

(define (op b)
  (second b))

(define (binone? b)
  (equal? b binone))

(define binone (double-plus1 binzero))
(define bintwo (double binone))
(define binthree (double-plus1 binone))
(define binfour (double bintwo))
(define binfive (double-plus1 bintwo))
(define binsix (double binthree))
(define binseven (double-plus1 binthree))
(define bineight (double binfour))
(define binnine (double-plus1 binfour))
(define binten (double binfive))

; increment a dubnum
; Inputs: B is a dubnum representing the integer b
; Output: the dubnum representing b + 1.

(define(inc B)
  (cond
    [(binzero? B) (double-plus1 B)]
    [(double? B) (double-plus1 (op B))]
    [(double-plus1? B) (double (inc (op B)))]))

;Question 5.  Implement a recursive function to add two dubnums.
; Inputs: A and B are dubnums representing the integers a,b
; Output: the dubnum representation of a + b.
;Note: your answer should never include a number like '(DP1 (D (D zero)))=1
;instead of '(DP1 zero). The extra doubles serve no purpose and should not appear.

(define (binplus A B)
  (cond
    [(binzero? A) B]
    [(binzero? B) A]
    [(and (double? A) (double? B)) (double (binplus (second A) (second B)))]
    [(and (double-plus1? A) (double? B)) (double-plus1 (binplus (second A) (second B)))]
    [(and (double-plus1? B) (double? A)) (double-plus1 (binplus (second A) (second B)))]
    [else (double (inc (binplus (second A) (second B))))]
))

(display "Question 5 - DubNum Plus (20 points)\n")
(define-test-suite bin-plus-test
  (check-equal? (binplus binzero binone) binone)
  (check-equal? (binplus binone bintwo) binthree)
  (check-equal? (binplus bintwo binsix) bineight)
  (check-equal? (binplus binthree bintwo) binfive)
  (check-equal? (binplus binfour binfive) binnine)
  (check-equal? (binplus binfive bintwo) binseven)
  (check-equal? (binplus binsix binone) binseven)
  (check-equal? (binplus binseven bintwo) binnine)
  (check-equal? (binplus bineight bintwo) binten)
  (check-equal? (binplus binnine binzero) binnine)
  (check-equal? (binplus binten binzero) binten)
  (check-equal? (binplus binnine binone) binten)
  (check-equal? (binplus bineight binzero) bineight)
  (check-equal? (binplus binseven binone) bineight)
  (check-equal? (binplus binsix binthree) binnine)
  (check-equal? (binplus binfive binthree) bineight)
  (check-equal? (binplus binfour bintwo) binsix)
  (check-equal? (binplus binthree binseven) binten)
  (check-equal? (binplus bintwo bintwo) binfour)
  (check-equal? (binplus binone binfive) binsix)
)
(define q5_score  (- 20 (run-tests bin-plus-test 'verbose)))

;Question 6.  Implement a recursive function to multiply two dubnums.
; Inputs: A and B are dubnums representing the integers a,b
; Output: the dubnum representing a * b.
;Note: your answer should never include a number like '(DP1 (D (D zero)))=1
;instead of '(DP1 zero). The extra doubles serve no purpose and should not appear.

(define (binmult A B)
  (cond
    [(or (binzero? A) (binzero? B)) binzero]
    [(double? B) (double (binmult A (second B)))]
    [else (binplus (double (binmult A (second B))) A)]
))


(display "Question 6 - DubNum Multiply (20 points)\n")
(define-test-suite bin-mult-test
  (check-equal? (binmult binzero binone) binzero)
  (check-equal? (binmult binone binzero) binzero)
  (check-equal? (binmult binzero binthree) binzero)
  (check-equal? (binmult binnine binzero) binzero)
  (check-equal? (binmult binone binthree) binthree)
  (check-equal? (binmult binone binseven) binseven)
  (check-equal? (binmult binfive binone) binfive)
  (check-equal? (binmult binten binone) binten)
  (check-equal? (binmult bintwo binone) bintwo)
  (check-equal? (binmult bintwo bintwo) binfour)
  (check-equal? (binmult bintwo binthree) binsix)
  (check-equal? (binmult bintwo binfour) bineight)
  (check-equal? (binmult bintwo binfive) binten)
  (check-equal? (binmult binthree bintwo) binsix)
  (check-equal? (binmult binthree binthree) binnine)
  (check-equal? (binmult binfour bintwo) bineight)
  (check-equal? (binmult binfive bintwo) binten)
  (check-equal? (binmult binfive binfour) (double binten))
  (check-equal? (binmult binfour binthree) (double binsix))
  (check-equal? (binmult binfive binthree) (double-plus1 binseven))
)
(define q6_score  (- 20 (run-tests bin-mult-test 'verbose)))

;---------------------------------------------------------------------
;---------------------------------------------------------------------
;---------------------------------------------------------------------
;;;;;;;;;;;;;;Grade Summary;;;;;;;;;;;;;;;;;;;;;;;
(display "------Grade Summary------\n")
(display "Q1 Scored: ")
(display q1_score)
(display "/15\n")
(display "Q2 Scored: ")
(display q2_score)
(display "/15\n")
(display "Q3 Scored: ")
(display q3_score)
(display "/15\n")
(display "Q4 Scored: ")
(display q4_score)
(display "/15\n")
(display "Q5 Scored: ")
(display q5_score)
(display "/20\n")
(display "Q6 Scored: ")
(display q6_score)
(display "/20\n")

(define grand_total (+ q1_score q2_score q3_score q4_score q5_score q6_score))
(display "\n")
(display "Total: ")
(display grand_total)
(display "/100\n")