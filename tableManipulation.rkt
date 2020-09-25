;  ********** Smrithi Pyari Lal **********

#lang racket
#|
;CS 270 Math Foundations of CS
;Create By Professor Bruce Char, Professor Mark Boady, Professor Jeremy Johnson, and Steve Earth
;Drexel University
Homework 5A

- insert your solutions into this file where indicated (for instance as "'replace-this-with-your-implementation")

- make sure the entire file is accepted by DrRacket. If you don't finish some problems, comment them out.
The same is true for any English text that you may add. This file already contains many comments, so you
can see what the syntax is.

Submit this HW5A racketfile through Blackboard (learning.drexel.edu), and HW5B (SAT solver) via Gradescope

Important Rules:
0) All function names are given, do not change the names or signatures of any functions.
1) If the instructions state something must be recursive, you will recieve a zero if it is not recursive.
   Helper functions are sometimes allowed (and then the recursion could be in the helper rather than main).
2) If you think you need functions not taught in class, you are likely approaching the problem wrong
3) while you make cond cases to handle bases, it is not permitted to simple write
   cond cases for all the unit tests!
4) Once you write a function, you may use it in later questions.

|#

;; We use rackunit package to do unit tests. When you start,
;; all the tests will be failing. Once you implement the required
;; functions, the unit tests associated with those functions should
;; pass. Do not modify the unit test blocks except where indicated.
;; 
;;
(require rackunit)
(require rackunit/text-ui)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Assignment Overview
;
; The goal of this assignment is to create a boolean evaluator
; and use it to determine if a boolean expression is satisfied or not. 
; An expression is satisified if it is true for a setting of 
; variables.
;
; This framework could also be used to determine if it is 
; possible to make a statement true for all setting of 
; variables, i.e. a tautology checker.
; Either directly by observing that Phi is a tautology iff
; ~Phi is not satisfiable or though minor modification of the code.
;
; We will slowly build up all the tools needed by solving a
; series of smaller problems first. You need to do this 
; assignment from top to bottom. Functions are defined to be 
; used later on.
;
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Part 1 Lookup
;
; If we want to set the values of variables in our boolean
; expressions, we need a way to store variable values.
;
; The lookup functions takes a variable name and an environment. 
; It returns the value the variable is assigned.
; If the variable is not assigned it returns an error.
;
; The environment is a list of pairs 
;    (variable_name variable_value)
;
; Inputs: A variable name and environment
; Outputs: The value of the variable or an error
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Hints: Here is an example of how to throw an error.
; Copy it into your session and see what it does
;
; (error 'lookup "Variable Name Not Found")
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (lookup target environment)
  (cond
    [(null? environment) (error 'lookup "Environment is null")]
    [(equal? target (first (first environment))) (second (first environment))]
    [else (lookup target (rest environment))]
  )
 )
;You can test the error with the next line.
;Make sure to comment it back out so you can complete the rest of the assignment
;(lookup 'c '( (a #t) (b #f)))
;Test Implementation
(define-test-suite lookup-suite
  ;Check Error is correct
  (check-equal? (with-handlers ([exn:fail? (lambda (exn) 101)]) (lookup 'a '()) ) 101)
  (check-equal? (with-handlers ([exn:fail? (lambda (exn) 204)]) (lookup 'c '((a #t) (b #f))) ) 204)
  ;Check Lookups
  (check-equal? (lookup 'a '( (a #t) (b #f))) #t)
  (check-equal? (lookup 'b '( (a #t) (b #f))) #f)
)
(run-tests lookup-suite 'verbose)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Part 2 bool-eval
;
; Next we need to evaluate a boolean expression given a list of variables
; We will build up an evaluator that can evaluate boolean expressions containing
; Variables, Constants (#t,#f), not, or, and, implies, iff
;
; We will start with just variables, constants and "or".
; The new expression you should add is "and".
; Test your evaluator after each new case is added.
;
; Input: A boolean expression and an environment
; Output: The result of evaluating the expression with the variable values 
;          from the environment
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;General Helper Function
;The following words cannot be used as variable names
; (or and not implies iff #t #f)
; We need a test so that the system doesn't treat these as variables
(define (is-reserved-word? word)
  (cond
   [ (equal? word '#t) #t]
   [ (equal? word '#f) #t]
   [ (equal? word 'OR) #t]
   [ (equal? word 'AND) #t]
   [ (equal? word 'NOT) #t]
   [ (equal? word 'IMPLIES) #t]
   [ (equal? word 'IFF) #t]
   ;Otherwise
   [ else #f]
  )
 )

;Helper Functions for Constants
;is-constant: Returns true if the expression is a constant
(define (is-constant? expression)
  ;This is true when the expression is #t or #f
  ;It is false otherwise
  (or (equal? expression #t) (equal? expression #f))
 )
;Tests
(is-constant? '#t) ;true
(is-constant? '#f) ;true
(is-constant? 'OR) ;false

;eval-constant: returns the boolean value of the constant
;Note: we don't need environment here, but for consistency all 
; our eval functions will take environment as their second input
(define (eval-constant expression environment)
  expression
)
;Tests
(eval-constant '#t '( (a #t) (b #f))) ;returns #t
(eval-constant '#f '( (a #t) (b #f))) ;returns #f


;Helper Functions for Variables
;is-variable?: Returns true when the expression is a symbol
(define (is-variable? expression)
  (and
   (symbol? expression)
   (not (is-reserved-word? expression))
  )
)
;Tests
(is-variable? 'a)
(is-variable? 'b)
(is-variable? '#t)
(is-variable? '#f)
(is-variable? 'OR)
(is-variable? 'AND)
(is-variable? 'NOT)
(is-variable? 'IMPLIES)
(is-variable? 'IFF)
;eval-variable: returns the value associated with a variable
;This is why you wrote the lookup function!
(define (eval-variable variable environment)
  (lookup variable environment)
)

;From Here Down, the functions call each other

;Helper Functions for the Or Statement
;is-or: returns true if the statement is an or
(define (is-or? expression)
  (equal? (first expression) 'OR)
)
;Evaluate the or statement
;Here is where it gets interesting
;To evaluate an or statement, we need to evaluate the inputs first
(define (eval-or expression environment)
  ;Use the built in or to find the actual value
  ;so the or looks like (or something1 something2)
  ; (first expression) the word or
  ; (first (rest expression)) the expression something1
  ; (first (rest (rest expression))) the expression something2
  (or (bool-eval (first (rest expression)) environment)
      (bool-eval (first (rest (rest expression))) environment)))

;You have to implement the remaining definitions
;Uncomment this code as you complete parts. You don't have to do it all at once.
;For example, you can implement and test "and" without "implies" if you just
;leave all the implies code commented out.

;is-and: returns true if the statement is an and
(define (is-and? expression)
  (equal? (first expression) 'AND)
)

;eval-and: evaluate an and statement
(define (eval-and expression environment)
  (and (bool-eval (first (rest expression)) environment)
      (bool-eval (first (rest (rest expression))) environment))
)

;is-not: returns true if the statement is a not
(define (is-not? expression)
 (equal? (first expression) 'NOT)
)

;eval-not: evaluate a not expression
(define (eval-not expression environment)
 (not (bool-eval (first (rest expression)) environment))
)

;is-implies: returns true if the statement is a implies
(define (is-implies? expression)
  (equal? (first expression) 'IMPLIES)
)

;eval-implies: evaluate an implies expression
(define (eval-implies expression environment)
  (implies (bool-eval (first (rest expression)) environment)
      (bool-eval (first (rest (rest expression))) environment))
)

;is-iff: returns true if the statement is an iff
(define (is-iff? expression)
  (equal? (first expression) 'IFF)
)

;eval-iff; evaluate an iff expression
(define (eval-iff expression environment)
  (if (or (and (bool-eval (first (rest expression)) environment)
      (bool-eval (first (rest (rest expression))) environment))
          (and (not (bool-eval (first (rest expression)) environment))
      (not (bool-eval (first (rest (rest expression))) environment)) )) #t #f)
)

(define (bool-eval expression environment)
  (cond
    [;Case 1 Constants
     (is-constant? expression)
     (eval-constant expression environment)
    ]
    [;Case 2 Variables
     (is-variable? expression)
     (eval-variable expression environment)
    ]
    [;Case 3 or statements
     (is-or? expression)
     (eval-or expression environment)
    ]
    [;Case 4 not statements
    (is-not? expression)
     (eval-not expression environment)
     ]
    [;Case 5 and statements
    (is-and? expression)
     (eval-and expression environment)
    ]
    [;Case 6 implies statements
    (is-implies? expression)
     (eval-implies expression environment)
     ]
    [;Case 7 iff
    (is-iff? expression)
     (eval-iff expression environment)
     ]
    [;Else Case
     else
     (display "Expression given was invalid")
    ]
  )
)
;Comment the Sections of the tests to only test certain functions.

;Test Constants
(define-test-suite bool-eval-suite
(check-equal? 
(bool-eval '#t '( (a #t) (b #f))) ;True
#t)
(check-equal? 
(bool-eval '#f '( (a #t) (b #f))) ;False
#f)
;Test Variables
(check-equal? 
(bool-eval 'a '( (a #t) (b #f))) ;True
#t)
(check-equal? 
(bool-eval 'b '( (a #t) (b #f))) ;False
#f)
;Test Or
(check-equal? 
(bool-eval '(OR a a) '( (a #t) (b #f))) ;True
#t)
(check-equal? 
(bool-eval '(OR a b) '( (a #t) (b #f))) ;True
#t)
(check-equal? 
(bool-eval '(OR b a) '( (a #t) (b #f))) ;True
#t)
(check-equal? 
(bool-eval '(OR b b) '( (a #t) (b #f))) ;False
#f)
;Test not
(check-equal? 
(bool-eval '(NOT a) '( (a #t) (b #f))) ;False
#f)
(check-equal? 
(bool-eval '(NOT b) '( (a #t) (b #f))) ;True
#t)
;Test and
(check-equal? 
(bool-eval '(AND a a) '( (a #t) (b #f))) ;True
#t)
(check-equal? 
(bool-eval '(AND a b) '( (a #t) (b #f))) ;False
#f)
(check-equal? 
(bool-eval '(AND b a) '( (a #t) (b #f))) ;False
#f)
(check-equal? 
(bool-eval '(AND b b) '( (a #t) (b #f))) ;False
#f)
;Test implies
(check-equal? 
(bool-eval '(IMPLIES a a) '( (a #t) (b #f))) ;True
#t)
(check-equal? 
(bool-eval '(IMPLIES a b) '( (a #t) (b #f))) ;False
#f)
(check-equal? 
(bool-eval '(IMPLIES b a) '( (a #t) (b #f))) ;True
#t)
(check-equal? 
(bool-eval '(IMPLIES b b) '( (a #t) (b #f))) ;True
#t)
;Test iff
(check-equal? 
(bool-eval '(IFF a a) '( (a #t) (b #f))) ;True
#t)
(check-equal? 
(bool-eval '(IFF a b) '( (a #t) (b #f))) ;False
#f)
(check-equal? 
(bool-eval '(IFF b a) '( (a #t) (b #f))) ;False
#f)
(check-equal? 
(bool-eval '(IFF b b) '( (a #t) (b #f))) ;True
#t)
)
(run-tests bool-eval-suite 'verbose)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Part 3: get-variables
;
; If we want to test all setting for variables, then we need to know what
; the variables are
;
; Inputs: a boolean expression
; Outputs: a list with the variables from the expression
; The output should contain NO duplicate variable names.
;
; Hint: What does append do?
; Hint: What does remove-duplicates do?
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (get-variables expression)
  (cond
    [(is-variable? expression) (cons expression '())]
    [(or (is-not? expression) (is-iff? expression) ) (remove-duplicates (get-variables (second expression)))]
    [else (remove-duplicates (append (get-variables (second expression)) (get-variables (third expression))))]
))

(define-test-suite get-variables-suite
(check-equal? 
(get-variables 'a); returns (a)
'(a))

(check-equal? 
(get-variables '(OR a b)); returns the list (a b)
'(a b))

(check-equal? 
(get-variables '(AND (OR a (NOT b)) (IMPLIES c (IFF d #t)))); returns the list (a b c d)
'(a b c d))

(check-equal? 
(get-variables '(AND (NOT a) a)); returns (a)
'(a))

(check-true 
(or
  (equal? (get-variables '(AND (OR a b) (AND b a)));returns (b a) (Either one of these is correct)
          '(b a))
  (equal? (get-variables '(AND (OR a b) (AND b a)));returns (a b) (Either one of these is correct)
          '(a b))
))
;(b a) or (a b) is correct, but you will find (b a) is the 
;easier approach.

)
(run-tests get-variables-suite 'verbose)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Part 4: make-truth-table
;
; Given a list of variables give the truth table with all settings of varaibles
;
; Input: A list of variables
; Output: A list of environments with all possible settings for the variables
;
;Hint: What does the list command do? For example (list 1 2 3)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;This sounds easy, but having the output look right is actually a little difficult
;We will make some helper functions to get us there

;Variable Bindings
;For any variable, it can be either true or false
; Given a variable, we want to create a set of bindings with the possibilities
; ( ( (var_name_1 value_1) ) ( (var_name_1 value_2) ) )
;This seems like it has a lot of nested lists, but we will need them all. this is just a helper function.
;At the top-most point we return a list of two environments
; (make_bindings 'a) returns a table (i.e. list of environments)
; (environments for a) = ( (environment where a=t) (environment where a=f))
; = ( ((a t)) ((a f)))
; note: there is no need to make this function recursive!
(define (make_bindings name)
  (list (cons (cons name '(#t)) '()) (cons (cons name '(#f)) '()))
)

(define-test-suite make-bindings-suite
(check-equal? 
(make_bindings 'a) ; returns (((a #t)) ((a #f))) 
'(((a #t)) ((a #f))) 
)

(check-equal? 
(make_bindings 'b) ; returns (((b #t)) ((b #f))) 
'(((b #t)) ((b #f))) 
)
)
(run-tests make-bindings-suite 'verbose)



;Insert Binding
; Given a binding and a list of environments, 
; insert the binding to the beginning of each environment
; (insert_binding X ( (a b c) (m p q) ...))
; returns 
; ( (X a b c) (X m p q) ...)
; note: there is no need to make this function recursive!
(define (insert_binding binding environment)
  (if (null? (rest binding))
  (map (lambda (x) (cons (first binding) x)) environment)
  (map (lambda (x) (cons binding x)) environment)    
))

(define-test-suite insert-bindings-suite
(check-equal? 
(insert_binding '(a #t) '()) ; ( )
'()
)

(check-equal? 
(insert_binding '(oranges #t) '( ((grapes #f)) )) ; ( ((oranges #t) (grapes #f)) )
'( ((oranges #t) (grapes #f)) ) 
)

(check-equal? 
(insert_binding '(a #t) '( ((b #t)) ((b #f)) )) ;( ((a #t) (b #t)) ((a #t) (b #f)) )
'( ((a #t) (b #t)) ((a #t) (b #f)) ))
)
(run-tests insert-bindings-suite 'verbose)


;Insert Multiple Bindings
; When we are extending the truth table, we are adding more then one binding at a time
; Next, we define a function that takes a list of bindings and adds then to the environment.
; Case 1: When the environment is null, return the bindings
; Case 2: When the bindings are null, return null
; Case 3: Neither bindings nor environment are null
; 
; This is extending the truth table. Lets look at an example for 2 variables
; The empty table look like ()
; The table with just b looks like
; ( ; start of table
;   ( (b #t) ) ;First row has one column
;   ( (b #f) ) ;Second row has one column
; ) ; end of table
; This is the same as inserting the bindings of b into the null environment
; The table for a and b looks like
; (;start of table
;   ( (a #t) (b #t)) ;Row 1
;   ( (a #t) (b #f)) ;Row 2
;   ( (a #f) (b #t)) ;Row 3
;   ( (a #f) (b #f)) ;Row 4
;); End of table
; The rows 1-2 are inserting the first element in the binding into each row in the previous table
; Then we need to append this to the next value in each row of the previous table (rows 3-4)
(define (insert_multiple_bindings bindings environments)
  (cond
    [(null? environments) bindings]
    [(null? bindings) `()]
    [else (append (insert_binding (first bindings) environments) (insert_multiple_bindings (rest bindings) environments))]
))

(define-test-suite insert-multiple-bindings-suite
(check-equal? 
 (insert_multiple_bindings '( ((a #t)) ((a #f))) '())
 '( ((a #t)) ((a #f)))
)

(check-equal? 
 (insert_multiple_bindings '() '( ((b #t)) ((b #f))))
 '()
)

(check-equal? 
(insert_multiple_bindings '( ((a #t)) ((a #f))) '( ((b #t)) ((b #f))) )
'( 
  ((a #t) (b #t))
  ((a #t) (b #f))
  ((a #f) (b #t))
  ((a #f) (b #f))
)
);end of check
)
(run-tests insert-multiple-bindings-suite 'verbose)


;Extend Table
;Given a table of truth values, extend it by adding a new variable name.
;(extend_table var_name current_table)
;Use the functions you have previously defined.

(define (extend_table var_name current_table)
      (insert_multiple_bindings (make_bindings var_name) current_table)
  )

(define-test-suite extend-table-suite
(check-equal? 
(extend_table 'a '()) 
'( (( a #t)) ((a #f)) )
)

(check-equal?  (extend_table 'a '( (( b #t)) ((b #f)) ))
'( 
  ((a #t) (b #t))
  ((a #t) (b #f))
  ((a #f) (b #t))
  ((a #f) (b #f))
 )
)
)
(run-tests extend-table-suite 'verbose)

;Gen Truth Table
; Given a list of variables generate the truth table for them
; do NOT make this a recursive function. use the functions you have already built.

(define (make-truth-table var_names)
  (foldr (lambda (var_name current_table) (extend_table var_name current_table)) '() var_names)
)

(define-test-suite make-truth-table-suite

(check-equal? 
(make-truth-table '(a));returns ( ((a #t)) ((a #f)))
'( ((a #t)) ((a #f))))

(check-equal? 
(make-truth-table '(a b))
;Result with pretty spacing
;( 
;      ( ( a #t) ( b #t))
;      ( ( a #t) ( b #f))
;      ( ( a #f) ( b #t))
;      ( ( a #f) ( b #f))
;)
'( ((a #t) (b #t)) ((a #t) (b #f)) ((a #f) (b #t)) ((a #f) (b #f)))
);end of check

;This answer is very long (2^4 possibilities)
(check-equal? 
(make-truth-table '(a b c d)) 
'(
    ((a #t) (b #t) (c #t) (d #t))
    ((a #t) (b #t) (c #t) (d #f))
    ((a #t) (b #t) (c #f) (d #t))
    ((a #t) (b #t) (c #f) (d #f))
    ((a #t) (b #f) (c #t) (d #t))
    ((a #t) (b #f) (c #t) (d #f))
    ((a #t) (b #f) (c #f) (d #t))
    ((a #t) (b #f) (c #f) (d #f))

    ((a #f) (b #t) (c #t) (d #t))
    ((a #f) (b #t) (c #t) (d #f))
    ((a #f) (b #t) (c #f) (d #t))
    ((a #f) (b #t) (c #f) (d #f))
    ((a #f) (b #f) (c #t) (d #t))
    ((a #f) (b #f) (c #t) (d #f))
    ((a #f) (b #f) (c #f) (d #t))
    ((a #f) (b #f) (c #f) (d #f))
))
)
(run-tests make-truth-table-suite 'verbose)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Part 5: is_satisfied
;
; Given a boolean expression, decide if it is satisfied (true for atleast
;                             one assignment of the variables)
;
; Input: A boolean expression with variables
; Output: True if the expression is satisfied, false otherwise
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;A few more helper functions
;Given an expression and a truth table
;Run the expression on each item and make a list of the results
(define (run-on-truth-table expression tt)
  (if (pair? tt) (append (cons (bool-eval expression (first tt)) '()) (append (run-on-truth-table expression (rest tt)) '() ))
    '()))

;Tests
(define-test-suite run-on-truth-table-suite

(check-equal? 
(run-on-truth-table '(NOT a) (make-truth-table '(a))) ;returns (#f #t)
'(#f #t))

(check-equal? 
(run-on-truth-table '(OR (NOT a) a) (make-truth-table '(a))); returns (#t #t)
'(#t #t))

(check-equal? 
(run-on-truth-table '(AND a b) (make-truth-table '(a b)));returns (#t #f #f #f)
'(#t #f #f #f))
)
(run-tests run-on-truth-table-suite 'verbose)

;Now you have a list with the results all all your tests
;we need to decide if atleast one is true
;Given a list of boolean values, return true if atleast one is true 
; Return false otherwise
; note: there is no need to make this function recursive
(define (atleast-one-true my_list)
  (foldr (lambda (x y) (or (equal? x #t) y)) #f my_list))

;Tests
(define-test-suite atleast-one-true-suite

(check-equal? 
(atleast-one-true '());returns #f
#f)

(check-equal? 
(atleast-one-true '(#t));returns #t
#t)

(check-equal? 
(atleast-one-true '(#f #f #f)); returns #f
#f)

(check-equal? 
(atleast-one-true '(#t #f #t)); returns #t
#t)

(check-equal? 
(atleast-one-true '(#f #f #t)); returns #t
#t)
)
(run-tests atleast-one-true-suite 'verbose)

;Finally, determine if the expression is satisfied using all the components
; you have built up
(define (is-satisfied? bool-expression)
    (atleast-one-true (run-on-truth-table bool-expression (make-truth-table (get-variables bool-expression))))                                    
)

;Test Case
(define-test-suite is-satisfied-suite

(check-equal? 
(is-satisfied? '(OR (NOT a) a)) ;returns true
#t)

(check-equal? 
(is-satisfied? '(AND (NOT a) a)) ;returns false
#f)

(check-equal? 
 (is-satisfied? 
  '(NOT (IFF (IMPLIES a b) (OR (NOT a) b)))
  );returns false
#f)

(check-equal?
  (is-satisfied?
   '(AND (OR p (OR q (NOT p))) (OR (NOT p) (OR q r)))
  ); returns true
 #t)

(check-equal?
  (is-satisfied?
   '(AND (OR p (OR q (NOT r))) (OR (NOT p) (OR q r)))
  ); returns true
 #t)

(check-equal?
  (is-satisfied?
   '(AND (OR p q) (AND (NOT p) (NOT q)))
  ); returns false
 #f)
  
(check-equal?
  (is-satisfied?
   '(AND (OR p q) (AND (OR (NOT p) (NOT q)) (AND (OR p (NOT q))
                                                 (OR (NOT p) q))))
  ); returns false
 #f)
)
(run-tests is-satisfied-suite 'verbose)


;;----------------------------------------------------------------------
;;-------------Grading Results ----------------------------------------
;;----------------------------------------------------------------------
(display "\nHomework Grade\n")


;#Q01 Question 1
;Note: There should be an error for variable not found at the end
(display "Q01 Question 1: Lookup (4 Points)\n")
(define-test-suite q1_test_suite
  (check-equal? (lookup 'a '( ( a #t) (b #f))) #t)
  (check-equal? (lookup 'b '( ( a #t) (b #f))) #f)
  (check-equal? (lookup 'c '( (a #t) (b #t) (c #t) )) #t)
  (check-equal? (lookup 'd '( (a #t) (d #f) (c #f) (b #t))) #f)
)
(define q1_points (- 4 (run-tests q1_test_suite 'verbose)))

;;Part 2
;;#Q02 Question 2
(display "Q02 Question 2: bool-eval supports AND (4 Points)\n")

(define-test-suite q2_test_suite
  (check-equal? (bool-eval '(AND a c) '( (a #t) (b #f) (c #t) (d #f))) #t)
  (check-equal? (bool-eval '(AND a d) '( (a #t) (b #f) (c #t) (d #f))) #f)
  (check-equal? (bool-eval '(AND b c) '( (a #t) (b #f) (c #t) (d #f))) #f)
  (check-equal? (bool-eval '(AND b d) '( (a #t) (b #f) (c #t) (d #f))) #f)
)
(define q2_points (- 4 (run-tests q2_test_suite 'verbose)))

;;#Q03 Question 3
(display "Q03 Question 3: bool-eval supports NOT (2 Points)\n")

(define-test-suite q3_test_suite
  (check-equal? (bool-eval '(NOT a) '( (a #t) (b #f) (c #t) (d #f))) #f)
  (check-equal? (bool-eval '(NOT b) '( (a #t) (b #f) (c #t) (d #f))) #t)
)
(define q3_points (- 2 (run-tests q3_test_suite 'verbose)))

;;#Q04 Question 4
(display "Q04 Question 4: bool-eval supports IMPLIES (4 Points)\n")

(define-test-suite q4_test_suite
  (check-equal? (bool-eval '(IMPLIES a c) '( (a #t) (b #f) (c #t) (d #f))) #t)
  (check-equal? (bool-eval '(IMPLIES a d) '( (a #t) (b #f) (c #t) (d #f))) #f)
  (check-equal? (bool-eval '(IMPLIES b c) '( (a #t) (b #f) (c #t) (d #f))) #t)
  (check-equal? (bool-eval '(IMPLIES b d) '( (a #t) (b #f) (c #t) (d #f))) #t)
)
(define q4_points (- 4 (run-tests q4_test_suite 'verbose)))

;;#Q05 Question 5
(display "Q05 Question 5: bool-eval supports IFF (4 Points)\n")
(define-test-suite q5_test_suite
  (check-equal? (bool-eval '(IFF a c) '( (a #t) (b #f) (c #t) (d #f))) #t)
  (check-equal? (bool-eval '(IFF a d) '( (a #t) (b #f) (c #t) (d #f))) #f)
  (check-equal? (bool-eval '(IFF b c) '( (a #t) (b #f) (c #t) (d #f))) #f)
  (check-equal? (bool-eval '(IFF b d) '( (a #t) (b #f) (c #t) (d #f))) #t)
)
(define q5_points (- 4 (run-tests q5_test_suite)))

;;Part 3
;;#Q06 Question 6
(display "Q06 Question 6: get_variables (4 Points)\n")
(define-test-suite q6_test_suite
  (check-equal? (get-variables 'a) '(a))
  (check-equal? (get-variables '(OR a b)) '(a b))
  (check-equal? (get-variables '(AND (OR a (NOT b)) (IMPLIES c (IFF d #t)))) '(a b c d))
  (check-equal? (get-variables '(AND (NOT a) a)) '(a))
)
(define q6_points (- 4 (run-tests q6_test_suite)))

;;Part 4
;;#Q07 Question 7
(display "Q07 Question 7: make_bindings (4 Points)\n")
(define-test-suite q7_test_suite
  (check-equal? (make_bindings 'a) '(((a #t)) ((a #f))))
  (check-equal? (make_bindings 'b) '(((b #t)) ((b #f))))
  (check-equal? (make_bindings 'cake) '(((cake #t)) ((cake #f))))
  (check-equal? (make_bindings 'orange) '(((orange #t)) ((orange #f))))
)
(define q7_points (- 4 (run-tests q7_test_suite)))

;;#Q08 Question 8
(display "Q08 Question 8: insert_binding (3 Points)\n")
(define-test-suite q8_test_suite
  (check-equal? (insert_binding '((a #t)) '()) '() )
  (check-equal? (insert_binding '((robot #t)) '( ((grapes #f)) )) '( ((robot #t) (grapes #f)) ) )
  (check-equal? (insert_binding '((a #t)) '( ((b #t)) ((b #f)) )) '( ((a #t) (b #t)) ((a #t) (b #f)) ))
)
(define q8_points (- 3 (run-tests q8_test_suite)))

;;#Q09 Question 9
(display "Q09 Question 9: insert_multiple_bindings (4 Points)\n")
(define-test-suite q9_test_suite
  (check-equal? (insert_multiple_bindings '( ((a #t)) ((a #f))) '()) '( ((a #t)) ((a #f))))
  (check-equal? (insert_multiple_bindings '() '( ((b #t)) ((b #f)))) '())
  (check-equal? (insert_multiple_bindings '( ((a #t)) ((a #f))) '( ((b #t)) ((b #f))) )
                '(
                  ((a #t) (b #t))
                  ((a #t) (b #f))
                  ((a #f) (b #t))
                  ((a #f) (b #f))
                  )
                )
  (check-equal? (insert_multiple_bindings '( ((c #t)) ((c #f)))
'(((a #t) (b #t))((a #t) (b #f))((a #f) (b #t))((a #f) (b #f)))
)
'(
((c #t) (a #t) (b #t))
((c #t) (a #t) (b #f))
((c #t) (a #f) (b #t))
((c #t) (a #f) (b #f))
((c #f) (a #t) (b #t))
((c #f) (a #t) (b #f))
((c #f) (a #f) (b #t))
((c #f) (a #f) (b #f))
)
)

) 
(define q9_points (- 4 (run-tests q9_test_suite)))

;;#Q10 Question 10
(display "Q10 Question 10: extend_table (3 Points)\n")
(define-test-suite q10_test_suite
  (check-equal? (extend_table 'a '()) '( (( a #t)) ((a #f)) ))
  (check-equal? (extend_table 'a '( (( b #t)) ((b #f)) ))
'(
((a #t) (b #t))
((a #t) (b #f))
((a #f) (b #t))
((a #f) (b #f))
)
)
  (check-equal? (extend_table 'c '(
((a #t) (b #t))
((a #t) (b #f))
((a #f) (b #t))
((a #f) (b #f))
)
)
'(
((c #t) (a #t) (b #t))
((c #t) (a #t) (b #f))
((c #t) (a #f) (b #t))
((c #t) (a #f) (b #f))
((c #f) (a #t) (b #t))
((c #f) (a #t) (b #f))
((c #f) (a #f) (b #t))
((c #f) (a #f) (b #f))
)
)
)
(define q10_points (- 3 (run-tests q10_test_suite)))

;;Q11 Question 11
(display "Q11 Question 11: make-truth-table (3 Points)\n")
(define-test-suite q11_test_suite
  (check-equal? (make-truth-table '(a));returns ( ((a #t)) ((a #f)))
'( ((a #t)) ((a #f))))
  (check-equal? (make-truth-table '(a b))
;Result with pretty spacing
;(
; ( ( a #t) ( b #t))
; ( ( a #t) ( b #f))
; ( ( a #f) ( b #t))
; ( ( a #f) ( b #f))
;)
'( ((a #t) (b #t)) ((a #t) (b #f)) ((a #f) (b #t)) ((a #f) (b #f)))
)
  (check-equal? (make-truth-table '(a b c d))
'(
((a #t) (b #t) (c #t) (d #t))
((a #t) (b #t) (c #t) (d #f))
((a #t) (b #t) (c #f) (d #t))
((a #t) (b #t) (c #f) (d #f))
((a #t) (b #f) (c #t) (d #t))
((a #t) (b #f) (c #t) (d #f))
((a #t) (b #f) (c #f) (d #t))
((a #t) (b #f) (c #f) (d #f))

((a #f) (b #t) (c #t) (d #t))
((a #f) (b #t) (c #t) (d #f))
((a #f) (b #t) (c #f) (d #t))
((a #f) (b #t) (c #f) (d #f))
((a #f) (b #f) (c #t) (d #t))
((a #f) (b #f) (c #t) (d #f))
((a #f) (b #f) (c #f) (d #t))
((a #f) (b #f) (c #f) (d #f))
))

)
(define q11_points (- 3 (run-tests q11_test_suite)))

;;Part 5
;;#Q12 Question 12
(display "Q12 Question 12: run-on-truth-table (3 Points)\n")
(define-test-suite q12_test_suite
  (check-equal? (run-on-truth-table '(NOT a) (make-truth-table '(a))) ;returns (#f #t)
'(#f #t))
  (check-equal? (run-on-truth-table '(OR (NOT a) a) (make-truth-table '(a))); returns (t #t)
'(#t #t))
  (check-equal? (run-on-truth-table '(AND a b) (make-truth-table '(a b)));returns (t #f #f #f)
'(#t #f #f #f))
)
(define q12_points (- 3 (run-tests q12_test_suite)))

;;#Q13 Question 13
(display "Q13 Question 13: atleast-one-true (5 Points)\n")
(define-test-suite q13_test_suite
  (check-equal? (atleast-one-true '()) #f)
  (check-equal? (atleast-one-true '(#t)) #t)
  (check-equal? (atleast-one-true '(#f #t #f)) #t)
  (check-equal? (atleast-one-true '(#f #f #f #f #f)) #f)
  (check-equal? (atleast-one-true '(#f #f #f)) #f)
)
(define q13_points (- 5 (run-tests q13_test_suite)))

;;#Q14 Question 14
(display "Q14 Question 14: is-satisfied? (5 Points)\n")
(define-test-suite q14_test_suite
  (check-equal? (is-satisfied? '(OR (NOT a) a)) ;returns true
#t)

  (check-equal? (is-satisfied?
'(IFF (AND a b) (NOT (OR (NOT a) (NOT b))))
);returns true
#t)
  (check-equal? (is-satisfied?
'(IFF (OR x y) (NOT (AND (NOT x) (NOT y))))
);Returns true
#t)
  (check-equal? (is-satisfied?
'(IFF (AND m n) (OR m n))
)
#t)
  (check-equal? (is-satisfied? '(AND (NOT a) a)) #f)
)
(define q14_points (- 5 (run-tests q14_test_suite)))
;Total
(define total_points (+
          q1_points
          q2_points
          q3_points
          q4_points
          q5_points
          q6_points
          q7_points
          q8_points
          q9_points
          q10_points
          q11_points
          q12_points
          q13_points
          q14_points))
(display "Q1: ")(display q1_points)(display "/4\n")
(display "Q2: ")(display q2_points)(display "/4\n")
(display "Q3: ")(display q3_points)(display "/2\n")
(display "Q4: ")(display q4_points)(display "/4\n")
(display "Q5: ")(display q5_points)(display "/4\n")
(display "Q6: ")(display q6_points)(display "/4\n")
(display "Q7: ")(display q7_points)(display "/4\n")
(display "Q8: ")(display q8_points)(display "/3\n")
(display "Q9: ")(display q9_points)(display "/4\n")
(display "Q10: ")(display q10_points)(display "/3\n")
(display "Q11: ")(display q11_points)(display "/3\n")
(display "Q12: ")(display q12_points)(display "/3\n")
(display "Q13: ")(display q13_points)(display "/5\n")
(display "Q14: ")(display q14_points)(display "/5\n")
(display "Total: ")(display total_points)(display "/52\n")
(display "Testing Percentage: ")(display (exact->inexact (* 100 (/ total_points 52))))(display "%\n")
(display "the other 48 points are for the SAT solver problem located on Gradescope as HW5B")