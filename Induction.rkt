; PUT YOUR NAME HERE

#lang racket
(require rackunit)
(require rackunit/text-ui)
#|
CS 270
Homework 8
Professor B. Char, M. Boady,  J. Johnson, G. Long, and S. Earth

Tests given are not designed to be comprehensive.
They will give you an idea if your code is right, but they do not test all possible cases.
Think about your design.  When grading, we may add additional tests for your functions.

Important Rules:
1.) You may not use any loop constructs; if used, your answer will get a zero.
2.) If the directions state for a function to be implemented recursively, then you will receive a zero
if there is no recursion. Note that for some questions, recursive helper functions are allowed
(in which case, the grader will still give credit even with the main function not being recursive).
3.) You may not use the set! command. If used, your answer will get a zero.
As a general rule, you should only be using functions taught in class (otherwise you are likely missing
the point of the question and endangering your score).
4.) Using If/Cond to explicitly pass tests instead of following the instructions will always result in a
zero for that question.  Do not assume that simply because your code passes all the example unit tests here
then that means you get 100%; we will be testing with input beyond the examples.  The graders will
still obey the input contracts.
|#

;Each of the below questions has two parts.
;First, you will be asked to write a Racket function to solve a problem.
;Secondly, you will be asked to prove by induction with equational reaasoning that your Racket
;code is correct by exhibiting the stated property.
;Note: no helpers are necessary for any of these problems

;---------------------------------------------------------------------

;Question 1a (10 points)
; Write a recursive function satisifying the following specifications
; Input contract:  n is a positive integer
; Output contract: (cubesum n) is the sum of the first n perfect cubes
; note: it is overkill to use the expt function here.
; Example:  (cubesum 4) would be 100 because of 1 + 8 + 27 + 64

(define (cubesum n)
  (if (> n 1) (+ (* (* n n) n) (cubesum (- n 1))) 1)
)

;Test Bed
(display "Question 1a cubesum Tests (5 points)\n")
(define-test-suite test_spec_sum
  (check-equal? (cubesum 1) 1)
  (check-equal? (cubesum 2) 9)
  (check-equal? (cubesum 3) 36)
  (check-equal? (cubesum 4) 100)
  (check-equal? (cubesum 5) 225)
  (check-equal? (cubesum 6) 441)
  (check-equal? (cubesum 7) 784)
  (check-equal? (cubesum 8) 1296)
  (check-equal? (cubesum 9) 2025)
  (check-equal? (cubesum 10) 3025))
(define q1a_score (- 10 (run-tests test_spec_sum 'verbose)))

#| Question 1b (10 points)
Prove by induction that for all positive integers, (cubesum n) = (n^2)*(n+1)^2 /4
Enter your proof below:

BASE CASE
Input contract states that n is a positive integer, according to this definition, the smallest value of n is 1.

LHS:
(cubesum 1)                                            Calling function cubesum on 1
(if (> 1 1) (+ (* (* 1 1) 1) (cubesum (- 1 1))) 1)     Applying function definition of cubesum
(if #f (+ (* (* 1 1) 1) (cubesum (- 1 1))) 1)          Evaluating > [the expression evaluates to #f becasue 1 is not less than 1]
1                                                      Evaluating if

RHS:
(cubesum n) = (n^2)*(n+1)^2 /4
(cubesum 1) = (1^2)*(1+1)^2 / 4
            = 1*(2)^2 / 4
            = 4/4
            = 1

LHS = RHS; so the base case holds true.

INDUCTIVE HYPOTHESIS:
Let us assume that for an integer x such that 1 <= x <= y,
(cubesum x) = (x^2)*(x+1)^2 / 4

INDUCTIVE PROOF:
We will now show that if the theory holds true for 1 <= x <= y, it will also hold true for y+1

LHS
(cubesum (+ y 1))                                                                      Calling function cubesum on y + 1
(if (> (+ y 1) 1) (+ (* (* (+ y 1) (+ y 1)) (+ y 1)) (cubesum (- (+ y 1) 1)) 1)        Applying function definition of cubesum
(if #t (+ (* (* (+ y 1) (+ y 1)) (+ y 1)) (cubesum (- (+ y 1) 1)) 1)                   Evaluating > [the expression evaluates to #t becasue we are checking if y + 1 is greter than 1, and it is,
                                                                                       because even if y was the smallest possible value we could input, which is 1, adding a 1 to it would make it
                                                                                       greater than 1]
(+ (* (* (+ y 1) (+ y 1)) (+ y 1)) (cubesum (- (+ y 1) 1))                             Evaluating if
(+ (* (* (+ y 1) (+ y 1)) (+ y 1)) (cubesum y)                                         Evaluating + - by algebraic simplification

(y+1)^3 + ((y^2)*(y+1)^2 /4)                                                           Applying inductive hypothesis after rewriting algebraically
(((y+1)^2) * (y+1)) + ((y^2)*(y+1)^2 /4)                                               Expand
(((y+1)^2) * ((y+1) + (y^2)/4))                                                        Factor out y+1 from the terms on either side of the + sign

RHS
(cubsesum (+ y 1)) = (((y+1)^2)*(y+1+1)^2) / 4
                   = (((y+1)^2)*(y+2)^2) / 4                [Expanding (y+2)^2]
                   = (((y+1)^2)*(y^2 + 4 + 4y)) / 4         [dividing (y^2 + 4 + 4y)) by 4]
                   = (((y+1)^2)*((y^2)/4 + (y+1))           [rewriting expression in different order]
                   = (((y+1)^2)*((y+1) + (y^2)/4))
LHS = RHS as they both reduced to same expression 

we can conclude that becasue the base case and the Inductive case hold, the statement- For all positive integers, (cubesum n) = (n^2)*(n+1)^2 /4, holds true 
Quod Erat Demonstradum (Q.E.D)

|#

; Question 2 (10 points)
; Write a recursive function satisifying the following specifications
; Input:  L is a (possibly empty) list of integers.
; Output: (oddzeros L) is a boolean value which is true iff the quantity of zeros in L is an odd amount

(define (oddzeros L)
  (if (null? L) #f (xor (equal? (first L) 0) (oddzeros (rest L))))
)


;Test Bed
(display "Question 2a evenzeros Tests (10 points)\n")
(define-test-suite test_even_zeros
  
  (check-equal? (oddzeros '(1)) #f)
  (check-equal? (oddzeros '(0)) #t)
  (check-equal? (oddzeros '(0 0)) #f)
  (check-equal? (oddzeros '(7 0)) #t)
  (check-equal? (oddzeros '(1 -2)) #f)
  (check-equal? (oddzeros '(0 0 1)) #f)
  (check-equal? (oddzeros '(4 0 1)) #t)
  (check-equal? (oddzeros '(1 0 8)) #t)
  (check-equal? (oddzeros '(0 11 0 -9)) #f)
  (check-equal? (oddzeros '(0 11 0 -9 0)) #t))
(define q2a_score (- 10 (run-tests test_even_zeros 'verbose)))

#|
Question 2b (10 points)
Prove by induction, algebra, and equational reasoning that
If L contains an even number of zeros then (equal? (oddzeros L) #f)
If L contains an odd number of zeros then (equal? (oddzeros L) #t)
Hint: You need 4 cases (cons 0 E), (cons x E), (cons 0 D), (cons x D)
Where, x!=0, E is a list with an even number of zeros and D is a list with an odd number of zeros.
Enter your proof below:

We will prove this function by considering two cases-
1. When L contains even no. of zeroes
2. When L contains odd no. of zeroes

BASE CASE
*/Case 1/*

Input contract states that L is a possibly empty list fo integers. This means that the null list is the smallest list. Plus, 0 is an even number, so using the null list as our anchor for the base case
is valid

LHS:
(oddzeroes '())                                                            Calling function oddzeroes on '() 
(if (null? '()) #f (xor (equal? (first '()) 0) (oddzeros (rest '()))))     Applying function definition of oddzeroes
(if #t #f (xor (equal? (first '()) 0) (oddzeros (rest '()))))              Evaluating null? [this evaluates to #t becasue the list is '(), a null list]
#f                                                                         Evaluating if

RHS
The null list contains 0 elements in it, and 0 is an even number, thus the function (oddzeroes '()) will return #f

As LHS = RHS, the base case for the first case holds true

*/Case 2/*

Input contract states that L is a possibly empty list fo integers. This means that the null list is the smallest list. but, 0 is an even number, so using the list '(0) as our anchor for the base case
is valid as it contains 1 zero, and 1 is an odd number. 

(oddzeros ‘(0))                                                            Call oddzeros on a list with one 0
(if (null? ‘(0)) #f (xor (equal? (first ‘(0)) 0) (oddzeros (rest ‘(0)))))  Apply definition of oddzeros
(if #f #f (xor (equal? (first ‘(0)) 0) (oddzeros (rest ‘(0))))             Evaluate null? [This evaluates to #f becasue our list contains the element 0 in it and is not empty]
(xor (equal? (first ‘(0)) 0) (oddzeros (rest ‘(0))))                       Evaluate if
(xor (equal? 0 0) (oddzeros (rest ‘(0))))                                  Evaluate first [This gives us 0 becasue it is the only element in the list and can be thought of as the first element]
(xor #t (oddzeros (rest ‘(0))))                                            Evaluate equal?
(xor #t (oddzeros ‘()))                                                    Evaluate rest [There is only one element in the list, so removing it would give us a null list]
(xor #t #f)                                                                Evaluate (oddzeros ‘()) (above)
#t                                                                         Evaluate xor

RHS
The list '(0) contains 1 element in it, a 0, as there is 1 zero in the list, and 1 is an odd number, the function (oddzeroes '(0)) will return #t

As LHS = RHS, the base case for the first case holds true


From Case 1 and Case 2 we can say that:
1. (oddzeros ‘()) returns false, so the base case holds in this instance
2. (oddzeros ’(0)) returns true, so the base case also holds in this instance.

INDUCTIVE HYPOTHESIS:
Let us assume that for every list E that contains an even number of 0s, (oddzeros E) returns #f
Let us assume that for every list D that contains an odd number of 0s, (oddzeros D) returns #t

INDUCTIVE PROOF
We need 4 cases (cons 0 E), (cons x E), (cons 0 D), (cons x D)
Where, x!=0, E is a list with an even number of zeros and D is a list with an odd number of zeros.

*/Case 1/*

LHS
(oddzeros (cons 0 E))                                                                         Call function oddzeros on (cons 0 E)
(if (null? (cons 0 E)) #f (xor (equal? (first (cons 0 E)) 0) (oddzeros (rest (cons 0 E)))))   Apply definition of oddzeros
(if #f #f ...)                                                                                Evaluate null? [cannot be an empty list becasue we are perfroming a cons operation and passing that list
                                                                                              as the arugement]
(xor (equal? (first (cons 0 E)) 0) (oddzeros (rest (cons 0 E))))                              Evaluate if
(xor (equal? 0 0) (oddzeros (rest (cons 0 E))))                                               Evaluate first [This gives a 0 becasue we cons a 0 and then do a firct on it]
(xor #t (oddzeros (rest (cons 0 E))))                                                         Evaluate equal?
(xor #t (oddzeros E))                                                                         Evaluate rest
(xor #t #f)                                                                                   IH
#t                                                                                            Evaluate xor
(oddzeros (cons 0 E)) produces #t, so the first test is correct

RHS
No. of zeroes in (cons 0 E)= No. of  0s in E + 1
                           = 2*n + 1 [where n denotes that it is an even numbered value]
                           = an odd number of zeroes
                           = #t
As LHS = RHS, the theory holds for case 1

(oddzeros (cons x E))                                                                         Call oddzeros on (cons 0 E)
(if (null? (cons x E)) #f (xor (equal? (first (cons x E)) 0) (oddzeros (rest (cons x E)))))   Apply definition of oddzeros on (cons x E)
(if #f #f ...)                                                                                Evaluate null? [cannot be an empty list becasue we are perfroming a cons operation and passing that list
                                                                                              as the arugement]
(xor (equal? (first (cons x E)) 0) (oddzeros (rest (cons x E))))                              Evaluate if
(xor (equal? x 0) (oddzeros (rest (cons x E))))                                               Evaluate first [This gives a x becasue we cons an x and then do a first on it]
(xor #f (oddzeros (rest (cons x E))))                                                         Evaluate equal? [the input contract said that x!=0]
(xor #f (oddzeros E))                                                                         Evaluate rest
(xor #f #f)                                                                                   IH
#f                                                                                            Evaluate xor
(oddzeros (cons x E)) produces #f, so the second test is correct

RHS
No. of zeroes in (cons x E)= No. of 0s in E
                           = 2*n [where n denotes that it is an even numbered value]
                           = an even number of zeroes
                           = #f

As LHS = RHS, the theory holds for case 2

*/Case 2/*

LHS
(oddzeros (cons 0 D))                                                                         Call oddzeros on (cons 0 D)
(if (null? (cons 0 D)) #f (xor (equal? (first (cons 0 D)) 0) (oddzeros (rest (cons 0 D)))))   Apply definition of oddzeros on (cons 0 D)
(if #f #f ...)                                                                                Evaluate null? [cannot be an empty list becasue we are perfroming a cons operation and passing that list
                                                                                              as the arugement]
(xor (equal? (first (cons 0 D)) 0) (oddzeros (rest (cons 0 D))))                              Evaluate if
(xor (equal? 0 0) (oddzeros (rest (cons 0 D))))                                               Evaluate first [This gives a x becasue we cons an x and then do a first on i
(xor #t (oddzeros (rest (cons 0 D))))                                                         Evaluate equal?
(xor #t (oddzeros D))                                                                         Evaluate rest
(xor #t #t)                                                                                   IH
#f                                                                                            Evaluate xor

RHS
No. of zeroes in (cons 0 D)= No. of 0s in D + 1
                           = 2*n + 1 + 1 [where n in the expression denotes that it is an odd numbered value]
                           = 2*n + 2
                           = 2*(n + 1)
                           = even number of zeroes
                           = #f

As LHS = RHS, the theory holds for case 2


(oddzeros (cons x D))                                                                         Call oddzeros on (cons 0 E)
(if (null? (cons x D)) #f (xor (equal? (first (cons x D)) 0) (oddzeros (rest (cons x D)))))   Apply definition of oddzeros
(if #f #f ...)                                                                                Evaluate null? (cannot be an empty list)
(xor (equal? (first (cons x D)) 0) (oddzeros (rest (cons x D))))                              Evaluate if
(xor (equal? x 0) (oddzeros (rest (cons x D))))                                               Evaluate first
(xor #f (oddzeros (rest (cons x D))))                                                         Evaluate equal?
(xor #f (oddzeros D))                                                                         Evaluate rest
(xor #f #t)                                                                                   IH
#t                                                                                            Evaluate xor

RHS
No. of zeroes in (cons x D)= No. of 0s in D [the input contract said that x!=0]
                           = 2*n + 1[where n in the expression denotes that it is an odd numbered value]
                           = odd number of zeroes
                           = #t

As LHS = RHS, the theory holds for case 2


CONCLUSION:
As the thoeyr holds true for case 1 and case 2, We can concluded that it holds true for all cases




|#

;Q3a (10 Points)
; Write a recursive function satisifying the following specifications
; Input contract:  L a list
; Output contract: (duplicate L) is a new list with two copies of each value consecutively in L
(define (duplicate L)
  (if (null? L) null (cons (first L) (cons (first L) (duplicate (rest L)))))
)

(display "Question 3a duplicate Tests (10 points)\n")
(define-test-suite test_duplicate
  (check-equal? (duplicate '(1)) '(1 1))
  (check-equal? (duplicate '(null)) '(null null))
  (check-equal? (duplicate '(1 2)) '(1 1 2 2))
  (check-equal? (duplicate '(4 6)) '(4 4 6 6))
  (check-equal? (duplicate '((1) (2 3))) '((1) (1) (2 3) (2 3)))
  (check-equal? (duplicate '(4 5 6)) '(4 4 5 5 6 6))
  (check-equal? (duplicate '(7 8 9 10)) '(7 7 8 8 9 9 10 10))
  (check-equal? (duplicate '(1 2 3 4 5)) '(1 1 2 2 3 3 4 4 5 5))
  (check-equal? (duplicate '(9 9 9)) '(9 9 9 9 9 9))
  (check-equal? (duplicate '(1 4 5 6 4 3 4 5)) '(1 1 4 4 5 5 6 6 4 4 3 3 4 4 5 5)))
(define q3a_score (- 10 (run-tests test_duplicate 'verbose)))

#|
Q3b (10 Points)
denote (length L) = x
Prove that (length (duplicate L)) = 2*x
You may use the following properties of length, as long as you properly cite them in your proof
Length Property 1: (length '()) = 0 
Length Property 2: If a is any object and B is a list
(length (cons a B)) = (+ 1 (length B))
You may Justify lines by saying [By Length Property 1]
Enter your proof below:

(length L) = x
To prove: (length (duplicate L)) = 2*x

BASE CASE:
input contract states that L is a list, smallest list is the null list,

LHS
(length (duplicate '()))                                                                              Calling length function on (duplicate '()
(length (if (null? '()) null (cons (first '()) (cons (first '()) (duplicate (rest '()))))))           Appling function definition of (duplicate '())
(length (if #t null (cons (first '()) (cons (first '()) (duplicate (rest '()))))))                    Evaluating null? [This expression evaluates to true because our input is '() which is null]
(length (null))                                                                                       Evaluating if
(length '())                                                                                          Rewriting null as '()
0                                                                                                     Length property 1

RHS
gievn: (length L) = x
length of a null list is 0, becasue there are no elements in it
=> (length '()) = 0

(length (duplicate L)) = 2*0
                       = 0

as LHS = RHS, the base case holds true for given statement.

INDUCTIVE HYPOTHESIS:
Let us assume that for a list L with length k such that 1 <= k <= z,
(length (duplicate L)) = 2*k

INDUCTIVE PROOF:
We will now show that if the theory holds true for 1 <= k <= z, it will also hold true for a length of z+1

LHS:
when k = z, if we want to have a list with length z + 1, we must add one element to it, to do this, we cons anything to the list, in this case, we are cons-ing an element 'a

(if (null? L) null (cons (first L) (cons (first L) (duplicate (rest L)))))

(length (duplicate (cons 'a L)))                                                                                                  Calling length function on (duplicate (cons 'a L))
(length (if (null? (cons 'a L) null (cons (first (cons 'a L)) (cons (first (cons 'a L)) (duplicate (rest (cons 'a L)))))))        Applying function definition of duplicate on (cons 'a L)
(length (if #f null (cons (first (cons 'a L)) (cons (first (cons 'a L)) (duplicate (rest (cons 'a L))))))                         Evaluating null? [This evaluates to #f because the list will
                                                                                                                                  definitely have atleast 1 element in it after doing a cons
                                                                                                                                  operation on it]
(length (cons (first (cons 'a L)) (cons (first (cons 'a L)) (duplicate (rest (cons 'a L)))))                                      Evaluating if
(length (cons 'a (cons 'a (duplicate (rest (cons 'a L))))))                                                                       Evaluating first-cons twice [we get 'a becasue doing cons plugs in the
                                                                                                                                  'a to the beginning of the list and doing a first oepration returns this
                                                                                                                                  'a]
(length (cons 'a (cons 'a (duplicate L))))                                                                                        Evaluating rest-cons [This gives L because the rest simply undoes what
                                                                                                                                  the cons operation did]
(+ (length (cons 'a (duplicate L))) 1)                                                                                            Length property 2
(+ (+ (length (duplicate L)) 1) 1)                                                                                                Length property 2
(+ (+ 2*k 1) 1                                                                                                                    From Inductive hypothesis
2*k +1 + 1                                                                                                                        Rewriting algebraicallty
2*k + 2                                                                                                                           Algebraic simplification

RHS
Given length L = k
(length (duplicate L))) = 2*k
(length (duplicate (cons 'a L))) = 2* (k + 1)
                                 = 2k + 2    [expanding algebraically]

LHS = RHS as they both reduced to same expression 

we can conclude that becasue the base case and the Inductive case hold, the statement- For all positive integers, (length (duplicate L)) = 2*x, holds true 
Quod Erat Demonstradum (Q.E.D)



|#

;Question 4a (10pts)
; Write a recursive function satisifying the following specifications
; Input contract:  L is a non-empty list
; Output contract: (dropend L) is the same as L but with last element removed
(define (dropend L)
  (if (null? (rest L)) '() (cons (first L)  (dropend (rest L))))
)

(display "Question 4a dropend Tests (10 points)\n")
(define-test-suite test_cut_end
  (check-equal? (dropend '(1)) '())
  (check-equal? (dropend '(1 2)) '(1))
  (check-equal? (dropend '(3 4 (5))) '(3 4))
  (check-equal? (dropend '( (1) (2) (3) )) '( (1) (2) ))
  (check-equal? (dropend '((1 2 3 4))) '())
  (check-equal? (dropend '((1 2) (3 4))) '((1 2)))
  (check-equal? (dropend '(9 9 8)) '(9 9))
  (check-equal? (dropend '(/ 10 5)) '(/ 10))
  (check-equal? (dropend '(OR A B)) '(OR A))
  (check-equal? (dropend '(NOT X)) '(NOT))
)
(define q4a_score (- 10 (run-tests test_cut_end 'verbose)))

#|
;Question 4b
;denote (length L) = x
;Prove by Induction that (length (dropend L)) = x-1
;You may use the properties of length from Question 3
Enter your proof below:

(length L) = x
To prove: (length (dropend L)) = x-1

BASE CASE:
input contract states that L is a list, smallest list is the null list,

 (if (null? (rest L)) '() (cons (first L)  (dropend (rest L))))

LHS
(length (length (dropend L))                                                                             Calling length function on (dropend '()
(length (if (null? (rest '(a))) null (cons (first '(a)) (dropend (rest '(a))))))                         Appling function definition of (dropend '())
(length (if #t null (cons (first '(a)) (dropend (rest '())))))                                           Evaluating null? [This expression evaluates to true because our input is '(a) and performing
                                                                                                         a rest operation on it will remove the 'a and give us a null list]
(length (null))                                                                                          Evaluating if
(length '())                                                                                             Rewriting null as '()
0                                                                                                        Length property 1

RHS
gievn: (length L) = x
length of a null list with one element is 1
=> (length '(a)) = 1

(length (dropend '(a))) = 1 - 1
                        = 0

as LHS = RHS, the base case holds true for given statement.

INDUCTIVE HYPOTHESIS:
Let us assume that for a list L with length k such that 1 <= k <= z,
(length (dropend L)) = k-1

LHS:
when k = z, if we want to have a list with length z + 1, we must add one element to it, to do this, we cons anything to the list, in this case, we are cons-ing an element 'b

(if (null? (rest L)) '() (cons (first L)  (dropend (rest L))))

(length (duplicate (cons 'b L)))                                                                                                  Calling length function on (duplicate (cons 'a L))
(length (if (null? (rest (cons '(b) L))) null (cons (first (cons 'a L)) (dropend (rest (cons 'a L))))))                           Applying function definition of duplicate on (cons 'a L)
(length (if #f null (cons (first (cons 'a L)) (dropend (rest (cons 'a L))))))                                                     Evaluating null? [This evaluates to #f because the list will
                                                                                                                                  definitely have atleast 1 element in it after doing a cons
                                                                                                                                  operation on it]
(length (cons (first (cons 'a L)) (dropend (rest (cons 'a L)))))                                                                  Evaluating if
(length (cons 'a (dropend (rest (cons 'a L)))))                                                                                   Evaluating first-cons  [we get 'a becasue doing cons plugs in the
                                                                                                                                  'a to the beginning of the list and doing a first oepration returns this
                                                                                                                                  'a]
(length (cons 'a (dropend L)))                                                                                                    Evaluating rest-cons [This gives L because the rest simply undoes what
                                                                                                                                  the cons operation did]
(+ (length (dropend L)) 1)                                                                                                        Length property 2
(k-1 + 1)                                                                                                                         Applying IH to the algebraic equivalent of the previous expression
k                                                                                                                                 Algebraic Simplification

RHS
Given length L = k
(length (dropend L))) = k-1
(length (dropend (cons 'a L))) = k+1 -1
                               = k    [expanding algebraically]

LHS = RHS as they both reduced to same expression 

we can conclude that becasue the base case and the Inductive case hold, the statement- For all that for all non-empty lists L, (length (dropend L)) = x-1, holds true 
Quod Erat Demonstradum (Q.E.D)

|#

;Question 5a (10pts)
; Write a recursive function satisifying the following specifications
; Input contract:  L is a list with even length
; Output contract: (addpairs L) is new list with pairs of elements added together.
; Example:  (addpairs '(2 5 3 1)) would return '(7 4) since 2+5=7 and 3+1=4

(define (addpairs L)
  (if (null? L) null (cons (+ (first L) (second L)) (addpairs (rest (rest L)))))
)

(display "Question 5a add_pairs Tests (10 points)\n")
(define-test-suite test_add_pairs
  (check-equal? (addpairs '()) '())
  (check-equal? (addpairs '(1 2)) '(3))
  (check-equal? (addpairs '(1 2 3 4)) '(3 7))
  (check-equal? (addpairs '(2 2 2 2)) '(4 4))
  (check-equal? (addpairs '(0 -1 -2 3)) '(-1 1))
  (check-equal? (addpairs '(1 1 1 1)) '(2 2))
  (check-equal? (addpairs '(1 2 3 4 5 6 7 8)) '(3 7 11 15))
  (check-equal? (addpairs '(9 9 9 9 9 9)) '(18 18 18))
  (check-equal? (addpairs '(7 3 4 6 5 5)) '(10 10 10))
  (check-equal? (addpairs '(-9 9 -8 8)) '(0 0)))
(define q5a_score (- 10 (run-tests test_add_pairs 'verbose)))

#|
Question 5b
denote (length L) = x
Prove by Induction that (length (add_pairs L)) = x/2
You may use the properties of length from Question 3
Enter your proof below:

(length L) = x
To prove: (length (add_pairs L)) = x/2

BASE CASE:
input contract states that L is a list, with even number of elements

(if (null? L) null (cons (+ (first L) (second L)) (addpairs (rest (rest L)))))

LHS
(length (length (add_pairs L))                                                                           Calling length function on (dropend '()
(if (null? '()) null (cons (+ (first '()) (second '())) (addpairs (rest (rest '())))))                   Appling function definition of (dropend '())
(length (if #t null (cons (+ (first '(a b)) (second '(a b))) (addpairs (rest (rest '(a b))))))           Evaluating null? [This expression evaluates to false because our input is '() which is a null list]
(length (null))                                                                                          Evaluating if
(length '())                                                                                             Rewriting null as '()
0                                                                                                        Length property 1

RHS
gievn: (length L) = x
length of a null list is 0, becasue there are no elements in it
=> (length '()) = 0

(length (add_pairs L)) = 0/2
                       = 0

as LHS = RHS, the base case holds true for given statement.

INDUCTIVE HYPOTHESIS:
Let us assume that for a list L with length k such that 1 <= k <= z,
(length (duplicate L)) = k/2

INDUCTIVE PROOF:
We will now show that if the theory holds true for 1 <= k <= z, it will also hold true for a length of z+2 [we add two because our input contract will always be an even lengthed list]

LHS:
when k = z, if we want to have a list with length z + 2, we must add one element to it, to do this, we cons anything to the list twice, in this case, we are cons-ing an element 'a and then
another element 'b where a and b are integers.

(if (null? L) null (cons (+ (first L) (second L)) (addpairs (rest (rest L)))))

(length (add_pairs (cons b (cons a L))))                                                                                                                             Calling length function on (duplicate (cons 'a L))
(length (if (null? (cons b (cons a L))) null (cons (+ (first (cons b (cons a L))) (second (cons b (cons a L)))) (addpairs (rest (rest (cons b (cons a L))))))))      Applying function definition of duplicate on (cons 'a L)
(length (if #f null (cons (+ (first (cons b (cons a L))) (second (cons b (cons a L)))) (addpairs (rest (rest (cons b (cons a L))))))))                               Evaluating null? [This evaluates to #f because the list will
                                                                                                                                                                     definitely have atleast 1 element in it after doing a cons
                                                                                                                                                                     operation on it]
(length (cons (+ (first (cons b (cons a L))) (second (cons b (cons a L)))) (addpairs (rest (rest (cons b (cons a L)))))))                                            Evaluating if
(length (cons (+ b (second (cons b (cons a L)))) (addpairs (rest (rest (cons b (cons a L)))))))                                                                      Evaluating first-cons [we get b becasue doing cons plugs in the
                                                                                                                                                                     b to the beginning of the list that looks like '(a <elements from L>)
                                                                                                                                                                     and doing first operation on this would give b]
(length (cons (+ b a) (addpairs (rest (rest (cons b (cons a L)))))))                                                                                                 Evaluating second-cons [we get a becasue doing cons plugs in the
                                                                                                                                                                     a to the beginning of the list that looks like '(a <elements from L>)
                                                                                                                                                                     We do a second cons operation putiing the b into the list making it
                                                                                                                                                                     look like (b a <elements from L>) and doing second operation on
                                                                                                                                                                     this would give a]
(length (cons (+ b a) (addpairs L)))                                                                                                                                 Evaluating rest twice [We get L because we do two conss and two rest
                                                                                                                                                                     operations hence canceling each other out]
(+ (length (addpairs L)))                                                                                                                                            Length property 2
k/2 + 1                                                                                                                                                              Applying IH on the algebraic equivalent to the previous expression
                                                                                                                                                         
RHS
Given length L = k
(length (addpairs L))) = k/2
(length (add_pairs (cons b (cons a L)))) = (k+2)/2
                                         = k/2 + 1    [simplifying algebraically]

LHS = RHS as they both reduced to same expression 

we can conclude that becasue the base case and the Inductive case hold, the statement- For all positive integers, (length (addpairs L)) = x/2, holds true 
Quod Erat Demonstradum (Q.E.D)



|#

;;;;;;;;;;;;;;Grade Summary;;;;;;;;;;;;;;;;;;;;;;;
(display "------Grade Summary------\n")
(display "Q1a Scored: ")
(display q1a_score)
(display "/10\n")
(display "Q1b Scored: ?/10 (Graded by TA)\n")
(display "Q2a Scored: ")
(display q2a_score)
(display "/10\n")
(display "Q2b Scored: ?/10 (Graded by TA)\n")
(display "Q3a Scored: ")
(display q3a_score)
(display "/10\n")
(display "Q3b Scored: ?/10 (Graded by TA)\n")
(display "Q4a Scored: ")
(display q4a_score)
(display "/10\n")
(display "Q4b Scored: ?/10 (Graded by TA)\n")
(display "Q5a Scored: ")
(display q5a_score)
(display "/10\n")
(display "Q5b Scored: ?/10 (Graded by TA)\n")


(define grand_total (+ q1a_score q2a_score q3a_score q4a_score q5a_score))
(display "\n")
(display "Total: ")
(display grand_total)
(display "/100\n")