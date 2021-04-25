#lang pl

;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Question 1 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; Section 1.1
(: append5 : Char Char Char Char Char -> String)
;; The "append5" function, takes 5 characters and returns the concatenation of them by 1 string.
;; I used the "string" function to make a string from characters.
;; My function will work with 5 characters only.
;; The main difficult to solve that question is to know to string function by the documentation.
;; I solved it after a hour of read doc and tries..
(define (append5 a b c d e)
  (string a b c d e))

;; ~~~ Tests ~~~ append5
(test (append5 #\e #\d #\c #\b #\a) => "edcba")
(test (append5 #\a #\a #\a #\a #\a) => "aaaaa")
(test (append5 #\5 #\5 #\5 #\5 #\5) => "55555")
(test (append5 #\e #\5 #\c #\2 #\a) => "e5c2a")
(test (append5 #\e #\space #\c #\2 #\a) => "e c2a")



;; Section 1.2
(: permute3 : Char Char Char -> (Listof String))
;; The "permute3" function, takes 3 characters and returns the 6 permutations of them by 6 strings.
;; I used "the long way" to return the permutations, by the "cons" function. i did not success in any other way.
;; My function will work with 3 characters only.
;; The main difficult to solve that question is to calculate the permutation options.
;; I solved it after half- a hour by tries.
(define (permute3 a b c)
  (cons (string a b c)(cons (string a c b)(cons (string b a c)(cons (string b c a)(cons (string c a b)(cons (string c b a) null)))))))

;; ~~~ Tests ~~~ permute3
(test (permute3 #\a #\b #\c) => '("abc" "acb" "bac" "bca" "cab" "cba"))
(test (permute3 #\1 #\2 #\3) => '("123" "132" "213" "231" "312" "321"))
(test (permute3 #\1 #\b #\3) => '("1b3" "13b" "b13" "b31" "31b" "3b1"))
(test (permute3 #\1 #\space #\3) => '("1 3" "13 " " 13" " 31" "31 " "3 1"))




;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Question 2 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; Section 2.a
(: count-3lists : (Listof (Listof Any)) -> Natural)
;; The "count-3lists" function, get an input of "List of lists", and returns the natural number of inner lists that contain exactly 3 elements.
;; I used a function signature that binding the user to input List-of-lists
;; (because of the problem to go over an element that is not list, and for example- try to check its length..)
;; My function will work with list of lists (by 1 depth) only, and checks for every list-
;; if there is 3 elements (i used the "length" function)- if its true- add 1 and continue the check.. false- continue the check without add 1..
;; I used "match" function, to check if the list is empty (return and stop check) or there is another elements to check-
;; if true- i used "cons" to check by first (list) and rest (other lists..)
;; The function is recursive and return the correct number of 3 length lists.
;; The main difficult to solve that question is to know how to work with "cons" and to decide the function signature.
;; I solved it about a 3 hours.
(define (count-3lists list)
  (match list
    ['() 0]
    [(cons first rest)
     (cond
       [(= (length first) 3) (+ 1 (count-3lists rest))]
       [else (count-3lists rest)])]))

;; ~~~ Tests ~~~ count-3lists
(test (count-3lists '((1 3 4) (() (1 2 3)) ("tt" "Three" 7) (2 4 6 8) (1 2 3))) => 3)
(test (count-3lists '((1 3 4) (() (1 2 3) (1 2 4)) ("tt" "Three" 7) (2 4 6 8) (1 2 3))) => 4)
(test (count-3lists '((1 3 4))) => 1)
(test (count-3lists '()) => 0)
(test (count-3lists '((1 3 4) (5 5 5) (7 6 5) (1 2 ()))) => 4)
(test (count-3lists '(()()()())) => 0)




;; Section 2.b
;; Shell function
(: count-3lists-tail : (Listof (Listof Any)) -> Natural)
;; The function "count-3lists-tail" is the same as "count-3lists" from 2.a section, with 1 different feature- Tail-Recursive.
;; That function make an recursive call, but after the function makes the "depth-dive" its not makes "folding", its return the answer.
;; The function get as input List of lists, and return a natural number.
;; The shell function call to the auxiliary function with the input list and "0" as an "answer" variable.
(define (count-3lists-tail list)
  (helper list 0))


;; Auxiliary function
(: helper : (Listof (Listof Any)) Natural -> Natural)
;; The function "helper" its an auxiliary function, that do the same check like "2.a section" and the only change is that this function
;; get as input an "ans" natural number, that is hold the answer to return at the end-time (for the tail-recursive feature).
;; I used the same terminology as "2.a section".
;; My function will work only with a correct input get from the Shell function, "List of lists" and natural number.
;; The main difficult to solve that question is to understand the "tail-recursive" definition.
;; I solved it about a hour.
;; I consulted with my friend about the definition of "tail-recursive" 
(define (helper list ans)
  (match list
    ['() ans]
    [(cons first rest)
     (cond
       [(= (length first) 3) (helper rest (+ 1 ans))]
       [else (helper rest ans)])]))

;; ~~~ Tests ~~~ count-3lists-tail
(test (count-3lists-tail '((1 3 4) (() (1 2 3)) ("tt" "Three" 7) (2 4 6 8) (1 2 3))) => 3)
(test (count-3lists-tail '((1 3 4) (() (1 2 3) (1 2 4)) ("tt" "Three" 7) (2 4 6 8) (1 2 3))) => 4)
(test (count-3lists-tail '((1 3 4))) => 1)
(test (count-3lists-tail '('()'()'())) => 0)
(test (count-3lists-tail '('('()'() 2))) => 0)
(test (count-3lists-tail '('(1 3 3))) => 0)
(test (count-3lists-tail '('(0 0 0) '() '(1 1 2))) => 0)




;; Section 2c
(: count-3listsRec : (Listof Any) -> Natural)
;; The "count-3listsRec" function, get an input of "List of Any", and returns the natural number of inner lists that contain exactly 3 elements, recursively.
;; My function will work with list of any element (by any depth), and checks for every list-
;; if there is 3 elements (i used the "length" function)- if its true- add 1 and continue the check.. false- continue the check without add 1..
;; I used "match" function, to check if the list is empty (return and stop check) or there is another elements to check-
;; if true- i used "cons" to check by first element and rest (other elements..)
;; For every element- i checked if this is list?
;; for lists- i maked the length check, add 1 for the answer if its 3-length.. and call the recursive-call for first (this list) and rest.
;; if there is not a list- do not check the length.. and do not add 1 for the answer.. but make the recursive call for the other  first (depth) and rest..
;; The function is recursive and return the correct number of 3 length lists all over the depth.
;; The main difficult to solve that question is to know how to work with depth-recursive.
;; This question consumes knowledge about-
;; depth-recursive, work with the "+" (that can get more than 2 arguments), cons(first,rest), cond() etc..
;; Another difficult that i experienced was to decide about the function signature.
;; I solved it about a 3 hours, and i cunsulted with my friends about the definition and test cases examples (to calculate the outer list or not..).
;; (I was helped a lot by the forum. i didnt ask questions, but other's questions helped me). 
(define (count-3listsRec list)
  (match list
    ['() 0]
    [(cons first rest)
     (cond
       [(list? first) (cond
                        [(= (length first) 3) (+ 1 (count-3listsRec first) (count-3listsRec rest))]
                        [else (+ 0 (count-3listsRec first) (count-3listsRec rest))]
                        )]
       [else 0])]))

;; ~~~ Tests ~~~ count-3listsRec
(test (count-3listsRec '((1 3 4) (() (1 2 3)) ("tt" "Three" 7) (2 4 6 8) (1 2 3))) => 4)
(test (count-3listsRec '()) => 0)
(test (count-3listsRec '((1 3 4) (() () (1 2 3)) ("tt" "Three" 7) (2 4 6 8) (1 2 3))) => 5)
(test (count-3listsRec '(() (() (1 2 3) (1)) ("tt" "Three" 7))) => 3)
(test (count-3listsRec '(1 2 '(2))) => 0)
(test (count-3listsRec '('(1) '(2) '(2))) => 0)




;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Question 3 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; Sections 3.1 + 3.2
;; Implementation of empty stack- "EmptyKS" and push operation- "Push" constructors,
;; that are variants of data type KeyStack.
;; I think the solution here is very simple, and all i need to know is how to implement new data-type.
;; For this question i saw the practice lesson again, and helped by the presentation a lot.
;; I solved it about 2 hours. (most of the time is to over again the presentation..)
(define-type KeyStack
  [EmptyKS]
  [Push Symbol String KeyStack])




;; Section 3.3
(: search-stack : Symbol KeyStack -> (U Boolean String))
;; The search-stack function, get as input- Symbol and KeyStack, and return as output- Boolean OR String.
;; The function returns Boolean (#f- false) if there is EmptyKS or if there no wanted key.
;; My function will work with the correct input only.
;; The function check the input- if this is EmptyKS case- return false,
;; otherwise- if this is from the correct pattern- check cond:
;; if the wanted key is equal to the existing key- return the value, else-
;; keep search the key at the rest stack..
;; The main difficult to solve that question is to understand the syntax of "cases" function.
;; I learned from the presentation and from the examples there.
;; I solved it after 2 hours.
(define (search-stack key KS)
  (cases KS
    [(EmptyKS) #f]
    [(Push key2 value KS2) (cond
                             [(eq? key key2) value]
                             [else (search-stack key KS2)])]))




;; Section 3.4
(: pop-stack : KeyStack -> (U Boolean KeyStack))
;; The pop-stack function, get as input- KeyStack, and return as output- Boolean OR KeyStack.
;; The function returns Boolean (#f- false) if there is EmptyKS.
;; else- returns KeyStack (after the pop action).
;; My function will work with the correct input only- KeyStack.
;; The function check the input- if this is EmptyKS case- return false,
;; otherwise- make the pop and return the KeyStack.
;; I didnt have any difficult to solve that question.
;; I solved it after half- a hour.
(define (pop-stack KS)
  (cases KS
    [(EmptyKS) #f]
    [(Push key2 value KS2) KS2]))
  
;; ~~~ Tests ~~~ Question 3 - EmptyKS, Push, search-stack and pop-stack.
(test (EmptyKS) => (EmptyKS))
(test (Push 'b "B" (Push 'a "A" (EmptyKS))) => (Push 'b "B" (Push 'a "A" (EmptyKS))))
(test (Push 'a "AAA" (Push 'b "B" (Push 'a "A"   (EmptyKS)))) => (Push 'a "AAA" (Push 'b "B" (Push 'a "A" (EmptyKS)))))
(test (search-stack 'a (Push 'a "AAA" (Push 'b "B" (Push 'a "A" (EmptyKS))))) => "AAA")
(test (search-stack 'c (Push 'a "AAA" (Push 'b "B" (Push 'a "A" (EmptyKS))))) => #f)
(test (search-stack 'b (Push 'a "AAA" (Push 'b "B" (Push 'a "A" (EmptyKS))))) => "B")
(test (pop-stack (Push 'a "AAA" (Push 'b "B" (Push 'a "A" (EmptyKS))))) => (Push 'b "B" (Push 'a "A" (EmptyKS))))
(test (pop-stack (Push 'a "AAA" (Push 'a "AAA" (Push 'a "AAA" (EmptyKS))))) => (Push 'a "AAA" (Push 'a "AAA" (EmptyKS))))
(test (pop-stack (EmptyKS)) => #f)
(test (search-stack 'a (EmptyKS)) => #f)
(test (search-stack 'b (Push 'a "AAA"(EmptyKS))) => #f)




;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Question 4 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(: is-odd? : Natural -> Boolean)
;; input: Natural number
;; output: Boolean- true/false. true if the nubmer is odd, false otherwise
;; purpose: The function made to check if the number is odd. true if is odd, false else (is even).
;; operates: The function checks if the number is zero? if yes- return false (because zero is even)
;;           else- check if the number minus 1 is even, and go into a "ping-pong" with the other function- "is-even?".
;; The number of times that the functions make the "ping-pong" makes the return answer- if the number is even/odd.
(define (is-odd? x)
  (if (zero? x)
      false
      (is-even? (- x 1))))


(: is-even? : Natural -> Boolean)
;; input: Natural number
;; output: Boolean- true/false. true if the nubmer is even, false otherwise.
;; purpose: The function made to check if the number is even. true if is even, false else (is odd).
;; operates: The function checks if the number is zero? if yes- return true (because zero is even)
;;           else- check if the number minus 1 is odd, and go into a "ping-pong" with the other function- "is-odd?".
;; The number of times that the functions make the "ping-pong" makes the return answer- if the number is even/odd.
(define (is-even? x)
  (if (zero? x)
      true
      (is-odd? (- x 1))))

;; ~~~ Tests ~~~ is-odd?/is-even?
(test (not (is-odd? 12)))
(test (is-even? 12))
(test (not (is-odd? 0)))
(test (is-even? 0))
(test (is-odd? 1))
(test (not (is-even? 1)))




(: every? : (All (A) (A -> Boolean) (Listof A) -> Boolean))
;; input: A predicate that accepts arguments of type A, and a list of A members
;; output: Returns true if the predicate returns true for all list members
;; purpose: Check that a specific predicate is true for all list members
;; operates: Executes the predicate against the first list item, and if true is returned, recursively repeat the process
;;           for the rest of the list
(define (every? pred lst)
  (or(null? lst)
     (and (pred (first lst))
          (every? pred (rest lst)))))

;; ~~~ Tests ~~~ every?
(test (every? null? '()) => #t)
(test (not (every? null? '())) => #f)
(test (every? boolean? '(#t #t #f)) => #t)
(test (not(every? boolean? '(#t #t #f))) => #f)
(test (not(every? boolean? '(#t #\t #f))) => #t)
(test (not(not(every? boolean? '(#t #t #f)))) => #t)


;; An example for the usefulness of this polymorphic function
(: all-even? : (Listof Natural) -> Boolean)
;; input: List of natural integers
;; output: Boolean, true/false
;; purpose: To check if all the list members all are even and accpet the predicate from the functions above 
;; operates: That function uses the functions above, "every?" and "is-even?" and return true if all the list members are checked and returned true.
(define (all-even? lst)
  (every? is-even? lst))

;; ~~~ Tests ~~~ all-even?
(test (all-even? null))
(test (all-even? (list 0)))
(test (all-even? (list 2 4 6 8)))
(test (not (all-even? (list 1 3 5 7))))
(test (not (all-even? (list 1))))
(test (not (all-even? (list 2 4 1 6))))




(: every2? : (All (A B) (A -> Boolean) (B -> Boolean) (Listof A) (Listof B) -> Boolean))
;; input: 2 generic lists of 2 types- A,B
;; output: Boolean, true/false. Returns true if the predicate returns true for all lists members respectively
;; purpose: Check that a specific predicate is true for all list members, at 2 lists
;; operates: Executes the predicate against the first list item, (on both of the lists) and if true is returned, recursively repeat the process
;;           for the rest of the lists.
(define (every2? pred1 pred2 lst1 lst2)
  (or (null? lst1) ; both lists assumed to be of same length
      (and (pred1 (first lst1))
           (pred2 (first lst2))
           (every2? pred1 pred2 (rest lst1) (rest lst2)))))

;; ~~~ Tests ~~~ every2?
(test (every2? null? null? '() '()) => #t)
(test (not(every2? null? null? '(1) '(2))) => #t)
(test (every2? boolean? boolean? '(#t #t #f) '(#f #f #f)) => #t)
(test (every2? boolean? boolean? '(#t #t #\f) '(#f #f #\f)) => #f)
(test (every2? boolean? boolean? '(#t #t #f) '(#f #t #f)) => #t)