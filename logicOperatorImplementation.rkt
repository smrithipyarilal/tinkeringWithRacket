#lang racket

;The following two lines are required to test the code.
(require rackunit)
(require rackunit/text-ui)

#|
CS 270
Homework 2
Create By Professor Bruce Char, Professor Mark Boady, and Professor Jeremy Johnson

Complete each of the below functions.

Tests given are not resigned to be comprehensive.
They will give you an idea if your code is write, but they do not test all possible cases.

Think about your design.

When grading, we may add additional tests for your functions.

Once you write a function, you may use it in later questions.

Important Rules:
1.) You may not use loop constructs like while/for/sum. If used, your answer will get a zero.
2.) If the instructions state something must be recursive, you will recieve a zero if it is not recursive.
    Recursive helper functions are allowed (the main function not being recursive).
3.) You may not use the set! command. If used, your answer will get a zero.
4.) Using If/Cond to explicitly pass tests instead of following the instructions
    will always result in a zero for that question.
|#


;Question 1
;Complete the definition of noti to implement logical NOT.
;Questions 1-6:
;You may only use (if ...), #t, #f, input arguments. All other functions are forbidden.
;For example, do not use =, eq?, or equal?


;You can write boolean functions within these rules.
;The NAND Operator 
;Input: (and (boolean? a) (boolean? b)) - Note (boolean? e) returns true if e is a boolean value
;Output: (boolean? (nandi a n))
;True when a is false
;True when b is false
(define (nandi a b)
  (if a (if b #f #t)
   #t)
)


; logical negation
; input:  (boolean? e) 
; output:  (boolean? (noti e))
; When e is true return false
; When e is false return true
(define (noti e)
  (if e #f #t)
 )

;Tests
(define-test-suite testnoti
  (check-equal?
    (noti #f) #t)
  (check-equal?
    (noti #t) #f)
)
(display "Question 1 noti (2 points)")
(newline)
(define q1_score (- 2 (run-tests testnoti 'verbose)))


;Question 2
;Complete the definition of andi to implement logical AND.  
;You may only use (if ...), #t, #f, input arguments. All other functions are forbidden.
;For example, do not use =, eq?, or equal?

; logical and
; input:  e1 and e2 are boolean expressions
; output:  (boolean? (andi e1 e2))
; True when e1 and e2 are both true
; False otherwise
(define (andi e1 e2)
  (if e1
      (if e2 #t #f)
      #f)
)

;Tests
(define-test-suite testandi
  (check-equal?
    (andi #f #f) #f)
  (check-equal?
    (andi #f #t) #f)
  (check-equal?
    (andi #t #f) #f)
  (check-equal?
    (andi #t #t) #t)
)
(display "Question 2 andi (4 points)")
(newline)
(define q2_score (- 4 (run-tests testandi 'verbose)))


;Question 3
;Complete the definition of ori to implement logical OR.
;You may only use (if ...), #t, #f, input arguments. All other functions are forbidden.
;For example, do not use =, eq?, or equal?

; logical or
; input:  e1 and e2 are boolean expressions
; output:  (boolean? (ori e1 e2))
; return true if either e1 or e2 are true

(define (ori e1 e2)
  (if e1 #t
      (if e2 #t #f)
))

;Tests
(define-test-suite testori
  (check-equal?
    (ori #f #f) #f)
  (check-equal?
    (ori #f #t) #t)
  (check-equal?
    (ori #t #f) #t)
  (check-equal?
    (ori #t #t) #t)
)
(display "Question 3 ori (4 points)")
(newline)
(define q3_score (- 4 (run-tests testori 'verbose)))


;Question 4
;Complete the definition of xori to implement logical exclusive XOR.
;You may only use (if ...), #t, #f, input arguments. All other functions are forbidden.
;For example, do not use =, eq?, or equal?


; logical xor
; input:  e1 and e2 are boolean expressions
; output:  (boolean? (xori e1 e2))
; return true if exactly one of e1 or e2 are true

(define (xori e1 e2)
  (cond 
  [(andi e1 e2) #f]
  [(andi (noti e1) (noti e2)) #f]
  [else #t]
))


        ;Tests
(define-test-suite testxori
  (check-equal?
    (xori #f #f) #f)
  (check-equal?
    (xori #f #t) #t)
  (check-equal?
    (xori #t #f) #t)
  (check-equal?
    (xori #t #t) #f)
)
(display "Question 4 xori (4 points)")
(newline)
(define q4_score (- 4 (run-tests testxori 'verbose)))


;Question 5
;Complete the definition of impliesi to implement logical implication.
;You may only use (if ...), #t, #f, input arguments. All other functions are forbidden.
;For example, do not use =, eq?, or equal?


; logical implication
; input:  e1 and e2 are boolean expressions
; output:  (boolean? (impliesi e1 e2))
; return true if e1 is false
; or e1 is true and e2 is true
(define (impliesi e1 e2)
  (if e1 e2 #t)
)

;Tests
(define-test-suite testimpliesi
  (check-equal?
    (impliesi #f #f) #t)
  (check-equal?
    (impliesi #f #t) #t)
  (check-equal?
    (impliesi #t #f) #f)
  (check-equal?
    (impliesi #t #t) #t)
)

(display "Question 5 impliesi (4 points)")
(newline)
(define q5_score (- 4 (run-tests testimpliesi 'verbose)))


;Question 6
;Complete the definition of iffi to implement logical equivalence (if and only if).
;You may only use (if ...), #t, #f, input arguments. All other functions are forbidden.
;For example, do not use =, eq?, or equal?


; logical iffi
; input:  e1 and e2 are boolean expressions
; output:  (boolean? (iffi e1 e2)) true if e1 and e2 are both true of
;           both false
(define (iffi e1 e2)
  (noti (xori e1 e2))
)

;Tests
(define-test-suite testiffi
  (check-equal?
    (iffi #f #f) #t)
  (check-equal?
    (iffi #f #t) #f)
  (check-equal?
    (iffi #t #f) #f)
  (check-equal?
    (iffi #t #t) #t)
)
(display "Question 6 iffi (4 points)")
(newline)
(define q6_score (- 4 (run-tests testiffi 'verbose)))


;Question 7.
; Use foldr to implement (andlist L).
; andlist takes a list and ANDs all the elements in the list.
; You only use foldr, andi, #t, #f, and input arguments.
(define (andlist L)
  (foldr andi #t L)
)

;Tests
(define-test-suite testandlist
  (check-equal?
    (andlist '()) #t)
  (check-equal?
    (andlist '(#t #t #t)) #t)
  (check-equal?
    (andlist '(#t #f #t)) #f)
  (check-equal?
    (andlist '(#t #t #t #t #f)) #f)
)
(display "Question 7 andlist (4 points)")
(newline)
(define q7_score (- 4 (run-tests testandlist 'verbose)))



;Question 8.
; Use foldr to implement (orlist L).
; andlist takes a list and ORs all the elements in the list.
; You only use foldr, ori, #t, #f, and input arguments.
(define (orlist L)
  (foldr ori #f L)
)

;Tests
(define-test-suite testorlist
  (check-equal?
    (orlist '()) #f)
  (check-equal?
    (orlist '(#f #f #f)) #f)
  (check-equal?
    (orlist '(#t #f #t)) #t)
  (check-equal?
    (orlist '(#t #t #t #t #f)) #t)
)
(display "Question 8 orlist (4 points)")
(newline)
(define q8_score (- 4 (run-tests testorlist 'verbose)))

;Question 9
; Write a recursive function all_q to check if a list of symbols
; contains all q characters.
; Don't forget the base case and the necessary recursion.
; You may use any previously written function.

;Hint: You can use equal? on symbols
;(equal? 'q 'b) returns #f
;(equal? 'b 'b) returns #t

; Check if a list contains all q characters
; Input:  L is a list of symbols (a,b,c,...,z).
; Output: a boolean value which is true when all of the elements in L
;         are equal to q and false otherwise.
; The empty list should return true.
(define (all_q L)
  (cond
  [(null? L) #t]
  [(equal? (first L) 'q) (all_q (rest L))]
  [else #f]
  ))


(define-test-suite test_all_q
  (check-equal? (all_q '()) #t)
  (check-equal? (all_q '(q)) #t)
  (check-equal? (all_q '(b)) #f)
  (check-equal? (all_q '(b c)) #f)
  (check-equal? (all_q '(c q)) #f)
  (check-equal? (all_q '(q q)) #t)
  (check-equal? (all_q '(q x q)) #f)
  (check-equal? (all_q '(q q q)) #t)
  (check-equal? (all_q '(q q q q q q q q)) #t)
  (check-equal? (all_q '(q q q q w q q q)) #f)
)

(display "Question 9 all_q (10 points)")
(newline)
(define q9_score (- 10 (run-tests test_all_q 'verbose)))

;Question 10
;Solve question 9 again. This time you may not write a recursive function.
;Solve the problem using map/foldr/foldl/lambda instead of recursion.
;You may use andlist/orlist defined above.
;Do not write helper functions for this question. Use Lambda instead.

; Check if a list contains all q characters
; Input:  L is a list of symbols (a,b,c,...,z).
; Output: a boolean value which is true when all of the elements in L
;         are equal to q and false otherwise.
; The empty list should return true. Do you see why this base case makes sense?
(define (all_q_v2 L)
  (foldr (lambda (x y) (andi (equal? x 'q) y)) #t L))

(define-test-suite test_all_q_v2
  (check-equal? (all_q_v2 '()) #t)
  (check-equal? (all_q_v2 '(q)) #t)
  (check-equal? (all_q_v2 '(b)) #f)
  (check-equal? (all_q_v2 '(b c)) #f)
  (check-equal? (all_q_v2 '(c q)) #f)
  (check-equal? (all_q_v2 '(q q)) #t)
  (check-equal? (all_q_v2 '(q x q)) #f)
  (check-equal? (all_q_v2 '(q q q)) #t)
  (check-equal? (all_q_v2 '(q q q q q q q q)) #t)
  (check-equal? (all_q_v2 '(q q q q w q q q)) #f)
)

(display "Question 10 all_q_v2 (10 points)")
(newline)
(define q10_score (- 10 (run-tests test_all_q_v2 'verbose)))


;Question 11
; Write a recursive function at_least_one_q to check if a list of symbols
; contains at least one q symbol.
; Don't forget the base case and the necessary recursion. 
; You may use any previously written function.

; Check if a list contains at least one q
; Input:  L is a list of symbols (a,b,c,...z).
; Output: a boolean value which is true when at least one of the elements
;          in L is equal to q and false otherwise.
; An empty list should return false. Think about why this is a good base case.
(define (at_least_one_q L)
  (cond 
  [(null? L) #f]
  [(equal? (first L) 'q) #t]
  [else (at_least_one_q (rest L))]

))

(define-test-suite test_at_least_one_q
  (check-equal? (at_least_one_q '(q)) #t)
  (check-equal? (at_least_one_q '(b)) #f)
  (check-equal? (at_least_one_q '(x y)) #f)
  (check-equal? (at_least_one_q '(v q)) #t)
  (check-equal? (at_least_one_q '(q q)) #t)
  (check-equal? (at_least_one_q '(x x d)) #f)
  (check-equal? (at_least_one_q '(c t q)) #t)
  (check-equal? (at_least_one_q '(q q q)) #t)
  (check-equal? (at_least_one_q '(q w q q)) #t)
  (check-equal? (at_least_one_q '(x y d w)) #f)
)

(display "Question 11 at_least_one_q (10 points)")
(newline)
(define q11_score (- 10 (run-tests test_at_least_one_q 'verbose)))


;Question 12
;Solve question 11 again. This time you may not write a recursive function.
;Solve the problem using map/foldr/foldl/lambda instead of recursion.
;You may use andlist/orlist defined above.
;Do not write helper functions for this question. Use Lambda instead.

; Check if a list contains at least one q
; Input:  L is a list of symbols (a,b,c,...z).
; Output: a boolean value which is true when at least one of the elements
;          in L is equal to q and false otherwise.
; An empty list should return false.

(define (at_least_one_q_v2 L)
  (foldr (lambda (x y) (ori (equal? x 'q) y)) #f L))

(define-test-suite test_at_least_one_q_v2
  (check-equal? (at_least_one_q_v2 '(q)) #t)
  (check-equal? (at_least_one_q_v2 '(b)) #f)
  (check-equal? (at_least_one_q_v2 '(x y)) #f)
  (check-equal? (at_least_one_q_v2 '(v q)) #t)
  (check-equal? (at_least_one_q_v2 '(q q)) #t)
  (check-equal? (at_least_one_q_v2 '(x x d)) #f)
  (check-equal? (at_least_one_q_v2 '(c t q)) #t)
  (check-equal? (at_least_one_q_v2 '(q q q)) #t)
  (check-equal? (at_least_one_q_v2 '(q w q q)) #t)
  (check-equal? (at_least_one_q_v2 '(x y d w)) #f)
)

(display "Question 12 at_least_one_q_v2 (10 points)")
(newline)
(define q12_score (- 10 (run-tests test_at_least_one_q_v2 'verbose)))


;Question 13
; Write a recursive function exactly_one_q to check if a list of symbols
; contains exactly one q symbol.
; Don't forget the base case and the necessary recursion.
; You may use any previously written function.

;Hint: The answer to question 11 is helpful to use here.

; Check if a list contains exactly one q
; Input:  L is a list of symbols (a,b,c,...,z).
; Output: a boolean value which is true when exactly one of the elements
;          in L is equal to q and false otherwise.
; The empty list should return false.
(define (exactly_one_q L)
  (cond
    [(null? L) #f]
    [(equal? (first L) 'q) (noti (at_least_one_q (rest L)))]
    [else (exactly_one_q (rest L))]
))


(define-test-suite test_exactly_one_q
  (check-equal? (exactly_one_q '(q)) #t)
  (check-equal? (exactly_one_q '(x)) #f)
  (check-equal? (exactly_one_q '(z r)) #f)
  (check-equal? (exactly_one_q '(q d)) #t)
  (check-equal? (exactly_one_q '(q q)) #f)
  (check-equal? (exactly_one_q '(d e p)) #f)
  (check-equal? (exactly_one_q '(q b q)) #f)
  (check-equal? (exactly_one_q '(q q q)) #f)
  (check-equal? (exactly_one_q '(q n q q)) #f)
  (check-equal? (exactly_one_q '(m n m q)) #t)
)

(display "Question 13 exactly_one_q (10 points)")
(newline)
(define q13_score (- 10 (run-tests test_exactly_one_q 'verbose)))


;Question 14.
; Write a recursive function odd_amt_q to check if a list of symbols
; contains an odd number of a symbols.
; Don't forget the base case and the necessary recursion. 
; You may use any previously written function.

; Check if a list contains an odd number of q symbols
; Input:  L is a list of symbols (a,b,c,...,z).
; Output: a boolean value which is true when an odd number of the elements
;          in L is equal to q and false otherwise.
; 0 is even, so the Null list should return false

(define (odd_amt_q L)
  (cond
  [(null? L) #f]
  [(equal? (first L) 'q) (noti (odd_amt_q (rest L)))]
  [else (odd_amt_q (rest L))]
))

(define-test-suite test_odd_amt_q
  (check-equal? (odd_amt_q '(q)) #t)
  (check-equal? (odd_amt_q '(h i)) #f)
  (check-equal? (odd_amt_q '(k q)) #t)
  (check-equal? (odd_amt_q '(q z)) #t)
  (check-equal? (odd_amt_q '(t f b)) #f)
  (check-equal? (odd_amt_q '(u w q)) #t)
  (check-equal? (odd_amt_q '(q r q)) #f)
  (check-equal? (odd_amt_q '(q q q)) #t)
  (check-equal? (odd_amt_q '(q x q)) #f)
  (check-equal? (odd_amt_q '(q q q q)) #f)
)

(display "Question 14 odd_amt_q (10 points)")
(newline)
(define q14_score (- 10 (run-tests test_odd_amt_q 'verbose)))


;Question 15.
;Solve question 14 again. This time you may not write a recursive function.
;Solve the problem using map/foldr/foldl/lambda instead of recursion.
;Do not write helper functions for this question. Use Lambda instead.

; Check if a list contains an odd number of q symbols
; Input:  L is a list of symbols (a,b,c,...,z).
; Output: a boolean value which is true when an odd number of the elements
;          in L is equal to q and false otherwise.
; 0 is even, so the Null list should return false

(define (odd_amt_q_v2 L)
  (foldr (lambda (x y) (xori (equal? x 'q) y)) #f L))


(define-test-suite test_odd_amt_q_v2
  (check-equal? (odd_amt_q_v2 '(q)) #t)
  (check-equal? (odd_amt_q_v2 '(h i)) #f)
  (check-equal? (odd_amt_q_v2 '(k q)) #t)
  (check-equal? (odd_amt_q_v2 '(q z)) #t)
  (check-equal? (odd_amt_q_v2 '(t f b)) #f)
  (check-equal? (odd_amt_q_v2 '(u w q)) #t)
  (check-equal? (odd_amt_q_v2 '(q r q)) #f)
  (check-equal? (odd_amt_q_v2 '(q q q)) #t)
  (check-equal? (odd_amt_q_v2 '(q x q)) #f)
  (check-equal? (odd_amt_q_v2 '(q q q q)) #f)
)

(display "Question 15 odd_amt_q_v2 (10 points)")
(newline)
(define q15_score (- 10 (run-tests test_odd_amt_q_v2 'verbose)))


;---------------------------------------------------------------------
;---------------------------------------------------------------------
;---------------------------------------------------------------------
(display "------Test Summary------\n")
(display "Q1 Scored: ")
(display q1_score)
(display "/2\n")
(display "Q2 Scored: ")
(display q2_score)
(display "/4\n")
(display "Q3 Scored: ")
(display q3_score)
(display "/4\n")
(display "Q4 Scored: ")
(display q4_score)
(display "/4\n")
(display "Q5 Scored: ")
(display q5_score)
(display "/4\n")
(display "Q6 Scored: ")
(display q6_score)
(display "/4\n")
(display "Q7 Scored: ")
(display q7_score)
(display "/4\n")
(display "Q8 Scored: ")
(display q8_score)
(display "/4\n")
(display "Q9 Scored: ")
(display q9_score)
(display "/10\n")
(display "Q10 Scored: ")
(display q10_score)
(display "/10\n")
(display "Q11 Scored: ")
(display q11_score)
(display "/10\n")
(display "Q12 Scored: ")
(display q12_score)
(display "/10\n")
(display "Q13 Scored: ")
(display q13_score)
(display "/10\n")
(display "Q14 Scored: ")
(display q14_score)
(display "/10\n")
(display "Q15 Scored: ")
(display q15_score)
(display "/10\n")

(define grand_total (+ q1_score q2_score q3_score q4_score q5_score q6_score q7_score q8_score q9_score q10_score q11_score q12_score q13_score q14_score q15_score))
(display "\n")
(display "Total: ")
(display grand_total)
(display "/100\n")