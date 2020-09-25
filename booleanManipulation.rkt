#lang racket

;smrithi

;The following two lines are required to test the code.
(require rackunit)
(require rackunit/text-ui)

#|
CS 270
Homework 1

Created By Professors Bruce Char, Mark Boady, Jeremy Johnson, and Steve Earth

Complete each of the below functions.

Tests given are not designed to be comprehensive.
They will give you an idea if your code is right, but they do not test all possible cases.
Think about your design.  When grading, we may add additional tests for your functions.

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
|#

;In this exercise, we will directly relate integers and boolean values.
;We will use this ability to generate partial truth tables.
;Specifically, we will generate the inputs to the function.
;Next week, we will create the truth table outputs.
;Afterwards, will will combine these parts to solve problems.

;Question 1
;Convert a positive Integer to a List of true/false values.
;If we want to convert the integer 9 to a list of t/f values
;we start by finding out its remainder and quotient when divided by 2.
;(remainder 9 2) = 1
;(quotient 9 2) = 4
;This tells us the least significant bit is a 1
;We will represent 1 as #t, so the list is currently (#t)
;Next, we repeat the proccess with the quotient 4
;(remainder 4 2) = 0
;(quotient 4 2) = 2
;Zero is false, so we add this to the list (#f #t)
;Repeat with the quotient 2
;(remainder 2 2) = 0
;(quotient 2 2) = 1
;Zero is false, the list becomes (#f #f #t)
;(remainder 1 2) = 1
;(quotient 1 2) = 0
;One is true, the list because (#t #f #f #t)
;The quotient was zero meaning we can stop.

; input contract:  n is a postive integer
; output contract: (int_to_bool n) is a list containing the binary representation of n in the usual
; left to right order, but with the 1s as #t and the 0s as #f.
; NOTE: a helper function would be useful here, but if you think you need to do a reverse somewhere,
; then that means you have implemented your function incorrectly!
; Since 0 is not in the input contract for this function, we will deal with it later as a special case.
; Example: since 6 in binary is 110, (int_to_bool 6) would return (#t #t #f)
(define (int_to_bool n)
    (cond
      [ (and (zero? (quotient n 2)) (zero? (remainder n 2)))  '(#f) ]
      [ (and (zero? (quotient n 2)) (equal? 1 (remainder n 2)))  '(#t) ]
      [ (and (not(zero? (quotient n 2))) (zero? (remainder n 2))) (append (int_to_bool (quotient n 2)) '(#f)) ]
      [ (and (not(zero? (quotient n 2))) (equal? 1 (remainder n 2))) (append (int_to_bool (quotient n 2)) '(#t)) ]     
    )
)



;Test to see if you function works correctly
(define-test-suite test_int_to_bool
  (check-equal? (int_to_bool 1) '(#t))
  (check-equal? (int_to_bool 2) '(#t #f))
  (check-equal? (int_to_bool 3) '(#t #t))
  (check-equal? (int_to_bool 4) '(#t #f #f))
  (check-equal? (int_to_bool 5) '(#t #f #t))
  (check-equal? (int_to_bool 6) '(#t #t #f))
  (check-equal? (int_to_bool 7) '(#t #t #t))
  (check-equal? (int_to_bool 8) '(#t #f #f #f))
  (check-equal? (int_to_bool 9) '(#t #f #f #t))
  (check-equal? (int_to_bool 10) '(#t #f #t #f))
  (check-equal? (int_to_bool 11) '(#t #f #t #t))
  (check-equal? (int_to_bool 12) '(#t #t #f #f))
  (check-equal? (int_to_bool 13) '(#t #t #f #t))
  (check-equal? (int_to_bool 14) '(#t #t #t #f))
  (check-equal? (int_to_bool 15) '(#t #t #t #t))
  (check-equal? (int_to_bool 16) '(#t #f #f #f #f))
)
(display "Question 1.) int_to_bool Results (8 points)\n")
(define q1_score (* (/ 1.0 2.0) (- 16 (run-tests test_int_to_bool 'verbose))))


;Question 2
;Only significant binary digits are stored by the above function.
;In reality, we would want every number to have the same bit length.

; input contract:  n is a nonnegative integer
; output contract:  (pad n) is a FUNCTION which takes a list L. if L has less than n members, then the
; output of that function would be L but with enough #f's in front to make it have exactly a length of n.
; if L already has n or more members, then it just returns L. 
; examples:  ( (pad 7) '(w x y z) ) would come out to be '(#f #f #f w x y z)
; and ( (pad 3) '(w x y z) ) returns '(w x y z) as would ( (pad 4) '(w x y z) )
; It is permitted to call the length function in your implementation

(define (pad n)
    (lambda (x) (if (< (length x) n) ((pad n) (cons #f x)) x))
)


;Check your function with the below tests
(define-test-suite test_pad
  (check-equal? ((pad 5) (int_to_bool 0))  '(#f #f #f #f #f))
  (check-equal? ((pad 5) (int_to_bool 1))  '(#f #f #f #f #t))
  (check-equal? ((pad 5) (int_to_bool 2))  '(#f #f #f #t #f))
  (check-equal? ((pad 5) (int_to_bool 3))  '(#f #f #f #t #t))
  (check-equal? ((pad 5) (int_to_bool 4))  '(#f #f #t #f #f))
  (check-equal? ((pad 5) (int_to_bool 5))  '(#f #f #t #f #t))
  (check-equal? ((pad 5) (int_to_bool 6))  '(#f #f #t #t #f))
  (check-equal? ((pad 5) (int_to_bool 7))  '(#f #f #t #t #t))
  (check-equal? ((pad 5) (int_to_bool 8))  '(#f #t #f #f #f))
  (check-equal? ((pad 5) (int_to_bool 9))  '(#f #t #f #f #t))
  (check-equal? ((pad 5) (int_to_bool 10)) '(#f #t #f #t #f))
  (check-equal? ((pad 5) (int_to_bool 11)) '(#f #t #f #t #t))
  (check-equal? ((pad 5) (int_to_bool 12)) '(#f #t #t #f #f))
  (check-equal? ((pad 5) (int_to_bool 13)) '(#f #t #t #f #t))
  (check-equal? ((pad 5) (int_to_bool 14)) '(#f #t #t #t #f))
  (check-equal? ((pad 5) (int_to_bool 15)) '(#f #t #t #t #t))
)
(display "Question 2.) pad Results (8 points)\n")
(define q2_score (* (/ 1.0 2.0) (- 16 (run-tests test_pad 'verbose))))

;Question 3
; input contract:  n is a nonnegative integer
; output contract: (tt_inputs n) is a list of all 2^n possible boolean settings with n variables,
; in the standard top to bottom order (the line breaks here in the comments are just for readability)
; Examples:  (tt_inputs 3) would be
; '( (#t #t #t)
;    (#t #t #f)
;    (#t #f #t)
;    (#t #f #f)
;    (#f #t #t)
;    (#f #t #f)
;    (#f #f #t)
;    (#f #f #f) )
; HINT: the best way to do this is to create helper functions which count down '(7 6 5 4 3 2 1 0)
; and then have the mother function convert each of those individually into a list of booleans.
; Note:  it is permitted to use the expt mathematical function in your implementation
; expt is the exponential function, e.g. (expt 2 3) would return 8.  Do you see why that'd be useful?

(define (tt_inputs n)
  (if (zero? n) '(()) (map (lambda (x) ((pad n) (int_to_bool x))) (tth (expt 2 n))))
 )
 

(define (tth n)
  (if (zero? n) '() (cons (- n 1) (tth (- n 1))))
  )


;Check your function with the following tests
(define-test-suite test_tt
  (check-equal? (tt_inputs 0)
                '(())
  )
  (check-equal? (tt_inputs 1)
                '( (#t) (#f) )
  )
  (check-equal? (tt_inputs 2)
                '( (#t #t) (#t #f) (#f #t) (#f #f))
  )
  (check-equal? (tt_inputs 3)
                '( (#t #t #t)
                   (#t #t #f)
                   (#t #f #t)
                   (#t #f #f)
                   (#f #t #t)
                   (#f #t #f)
                   (#f #f #t)
                   (#f #f #f)
                   )
   )
   (check-equal? (tt_inputs 4)
                '(
                   (#t #t #t #t)
                   (#t #t #t #f)
                   (#t #t #f #t)
                   (#t #t #f #f)
                   (#t #f #t #t)
                   (#t #f #t #f)
                   (#t #f #f #t)
                   (#t #f #f #f)
                   (#f #t #t #t)
                   (#f #t #t #f)
                   (#f #t #f #t)
                   (#f #t #f #f)
                   (#f #f #t #t)
                   (#f #f #t #f)
                   (#f #f #f #t)
                   (#f #f #f #f)
                   )
   )
)
(display "Question 3.) tt_inputs Results (10 points)\n")
(define q3_score (- 10 (* 2 (run-tests test_tt 'verbose))))

;Question 4
;The inputs we made above have the format '(#t #f #f #t).
;We need boolean expressions that work with this format.
;Example: suppose we wish to verify that (a -> b) always gives the same answer as (~a \/ b)
;first we should write a function that returns whether the lefthand side (the implies)
;is equal the output of the righthand side (the disjunction).

(define (implies_verify L) ; note that (first L) is the "a" and (second L) is the "b"
    (equal? (implies (first L) (second L) ) (or (not (first L)) (second L)))
 )

;next, we write out own test suite that checks ALL possible inputs for a,b to make certain that the
;equality always holds (i.e. our equality verification function always outputs #t)
;Note: we could have automated this process using our tt_inputs and map, but since there are only two
;variables here, we will enter the four cases manually (later in this homework we'll code it automatically)
(define-test-suite test_implies
  (check-equal? (implies_verify '(#t #t)) #t)
  (check-equal? (implies_verify '(#t #f)) #t)
  (check-equal? (implies_verify '(#f #t)) #t)
  (check-equal? (implies_verify '(#f #f)) #t)
)
(display "Example.) Results of Implies Verify\n")
(run-tests test_implies)


;similar to the example above, verify the following three boolean algebra laws by creating
;a verification function and also a test-suite (note that it will look like this default test-suite
;passes all the tests since it's empty, but you still need to populate it with all the cases!)

;a.) Demorgan's Law
;implement ~(a /\ b) = (~a \/ ~b)
(define (demorgan_verify bool_vars)
  (equal? (not(and (first bool_vars) (second bool_vars))) (or (not(first bool_vars)) (not(second bool_vars))))
)


;Test Implies Def
(define-test-suite test_demorgan
  (check-equal? (demorgan_verify '(#t #t)) #t)
  (check-equal? (demorgan_verify '(#t #f)) #t)
  (check-equal? (demorgan_verify '(#f #t)) #t)
  (check-equal? (demorgan_verify '(#f #f)) #t) 
)
(display "4a.) Results of Demorgan Verify (8 points)\n")
(define q4a_score (- 8 (* 2 (run-tests test_demorgan 'verbose))))






;b.) Absorption
;implement (x /\ (x \/ y)) = x
(define (absorp_verify bool_vars)
 (equal? (and (first bool_vars) (or (first bool_vars) (second bool_vars))) (first bool_vars))
)


;Test Implies Def
(define-test-suite test_absorp
  (check-equal? (absorp_verify '(#t #t)) #t)
  (check-equal? (absorp_verify '(#t #f)) #t)
  (check-equal? (absorp_verify '(#f #t)) #t)
  (check-equal? (absorp_verify '(#f #f)) #t) 
)
(display "4b.) Results of Absorption Verify (8 points)\n")
(define q4b_score (- 8 (* 2 (run-tests test_absorp 'verbose))))





;c.) Associativity
;implement x \/ (y \/ z) = (x \/ y) \/ z
(define (assoc_verify bool_vars)
 (equal? (or (first bool_vars) (or (second bool_vars) (second (rest bool_vars)))) (or (or (first bool_vars) (second bool_vars)) (second (rest bool_vars))))
)
;Test Implies Def
(define-test-suite test_assoc
  (check-equal? (assoc_verify '(#t #t #t)) #t)
  (check-equal? (assoc_verify '(#t #t #f)) #t)
  (check-equal? (assoc_verify '(#t #f #t)) #t)
  (check-equal? (assoc_verify '(#t #f #f)) #t)
  (check-equal? (assoc_verify '(#f #t #t)) #t)
  (check-equal? (assoc_verify '(#f #t #f)) #t)
  (check-equal? (assoc_verify '(#f #f #t)) #t)
  (check-equal? (assoc_verify '(#f #f #f)) #t) 
)
(display "4c.) Results of Associativity Verify (16 points)\n")
(define q4c_score (- 16 (* 2 (run-tests test_assoc 'verbose))))




;Question 5
;Write a function that takes
;fun - a function that takes a list of boolean values and returns a boolean
;tt - a truth table (list of lists of T/F values)
;And returns a list of T/F values with results
;For example if fun computes (not a)
;and tt = ( (#t) (#f) )
;Then the return of
;(evaluate_tt fun tt) should be (#f #t)
(define (evaluate_tt fun T)
  (map (lambda (x) (fun x)) T)
)

;Test your function
(define-test-suite test_eval_tt
  (check-equal?
   (evaluate_tt implies_verify '( (#t #t) (#t #f) (#f #t) (#f #f)))
   '(#t #t #t #t)
  )
  (check-equal?
   (evaluate_tt demorgan_verify '( (#t #t) (#t #f) (#f #t) (#f #f)))
   '(#t #t #t #t)
  )
  (check-equal?
   (evaluate_tt absorp_verify '( (#t #t) (#t #f) (#f #t) (#f #f)))
   '(#t #t #t #t)
  )
  (check-equal?
   (evaluate_tt assoc_verify '( (#t #t #t)
                   (#t #t #f)
                   (#t #f #t)
                   (#t #f #f)
                   (#f #t #t)
                   (#f #t #f)
                   (#f #f #t)
                   (#f #f #f)
                   ))
   '(#t #t #t #t #t #t #t #t)
  )
)
(display "5.) Results of Evaluate on Truth Table (12 points)\n")
(define q5_score (- 12 (* 3 (run-tests test_eval_tt 'verbose))))

;Question 6
;Write a function that takes a list of true/false values
;and determines if that list could be the outcome of values of a tautology
;Note: a null list IS in fact comprised of all #t's (it has no #f's. in fact there's nothing in it at all!)
(define (tautology result_list)
  (foldr (lambda (x y) (and x y)) #t result_list) 
)


;Test your function
(define-test-suite test_taut
  (check-equal? (tautology '()) #t)
  (check-equal? (tautology '(#t)) #t)
  (check-equal? (tautology '(#f)) #f)
  (check-equal? (tautology '(#t #t)) #t)
  (check-equal? (tautology '(#t #f)) #f)
  (check-equal? (tautology '(#f #t)) #f)
  (check-equal? (tautology '(#f #f)) #f)
  (check-equal? (tautology '(#t #t #t #t #t #t #t)) #t)
  (check-equal? (tautology '(#t #t #t #t #t #f #t)) #f)
)
(display "6.) Results of Evaluation on Truth Table (18 points)\n")
(define q6_score (- 18 (* 2 (run-tests test_taut 'verbose))))

;Question 7
;Write a function that takes a function and the number of variables it has
;and determines if the function is a tautology.  (Here is the promised automation from question #4)

(define (is_taut func n)
  (foldr (lambda (x y) (and (func x) y)) #t (tt_inputs n))
)

;Test your function
(define-test-suite test_is_taut
  (check-equal? (is_taut implies_verify 2) #t)
  (check-equal? (is_taut demorgan_verify 2) #t)
  (check-equal? (is_taut absorp_verify 2) #t)
  (check-equal? (is_taut assoc_verify 3) #t)
  (check-equal? (is_taut (lambda (X) (and (first X) (second X))) 2) #f)
  (check-equal? (is_taut (lambda (X) (or (first X) (second X))) 2) #f)
)
(display "7.) Results of Is Tautology Question (12 points)\n")
(define q7_score (- 12 (* 2 (run-tests test_is_taut 'verbose))))


;---------------------------------------------------------------------
;---------------------------------------------------------------------
;---------------------------------------------------------------------
;;;;;;;;;;;;;;Grade Summary;;;;;;;;;;;;;;;;;;;;;;;
(display "------Grade Summary------\n")
(display "Q1 Scored: ")
(display q1_score)
(display "/8\n")
(display "Q2 Scored: ")
(display q2_score)
(display "/8\n")
(display "Q3 Scored: ")
(display q3_score)
(display "/10\n")
(display "Q4A Scored: ")
(display q4a_score)
(display "/8\n")
(display "Q4B Scored: ")
(display q4b_score)
(display "/8\n")
(display "Q4C Scored: ")
(display q4c_score)
(display "/16\n")
(display "Q5 Scored: ")
(display q5_score)
(display "/12\n")
(display "Q6 Scored: ")
(display q6_score)
(display "/18\n")
(display "Q7 Scored: ")
(display q7_score)
(display "/12\n")

(define grand_total (+ q1_score q2_score q3_score q4a_score q4b_score q4c_score q5_score q6_score q7_score))
(display "\n")
(display "Total: ")
(display grand_total)
(display "/100\n")