#| Assignment 1 - Racket Query Language Tests (due Oct 14, 11:50pm)

***Write the names, CDF accounts and student id for each of your group members below.***
Shray Sharma, sharm558, 1001430472
John Chen, chenjoh6, 1001636651
|#

; Note the use of plai here; this is required for "test"
#lang plai
(abridged-test-output #t)

; This imports your file; do not change this line!
(require "database.rkt")

; Function versions for common syntactic forms.
; *Use these in your queries instead of the syntactic forms!!!*
(define (And x y) (and x y))
(define (Or x y) (or x y))
(define (If x y z) (if x y z))

; Sample tables - add more tables!
; Ideas: empty table; table with just 1 attribute; 0 attributes; duplicates
(define Person
  '(("Name" "Age" "LikesChocolate") 
    ("David" 20 #t) 
    ("Jen" 30 #t) 
    ("Paul" 100 #f)))

(define Teaching
  '(("Name" "Course")
    ("David" "CSC324")
    ("Paul" "CSC108")
    ("David" "CSC343")
    ))

; empty table
(define Empty 
  '(()
    ()))

; table with empty tuples
(define Valid-Empty
  '(("Name" "Price")
    ("john" 23)
    ("shray" 33)
    ()
    ))

(define period-table
  '(("Name" ".Price")
    ("seamus" 44)))

; one tuple table
(define Games
  '(("Name" "Price")
    ("Overwatch" 40)))

; one attribute table
(define One-Column
  '(("sName")
    ("Messi")
    ("Neymar")
    ("Suarez")))

; multiple duplicates
(define Sports
  '(("Name" "Sport" "Age")
    ("Edd" "Soccer" 18)
    ("Edd" "Football" 19)
    ("Tom" "Rugby" 20)
    ("Tom" "Soccer" 20)
    ("Tom" "Soccer" 20)
    ("Phil" "Tennis" 20)))

#|
All tests go below. 
We have divided our tests into five sections:
- No WHERE/ORDER BY
- WHERE clause, but no ORDER BY
- ORDER BY clause, but no WHERE
- Both WHERE and ORDER BY
- Nested queries

Please respect this categorization when you add your tests,
and your TAs will appreciate it!
|#


; ---- SELECT/FROM tests ----

(printf "~n ---- SELECT/FROM TESTS ---- ~n~n")

; Select all
(test (SELECT * FROM Person)
      '(("Name" "Age" "LikesChocolate") 
        ("David" 20 #t) 
        ("Jen" 30 #t) 
        ("Paul" 100 #f)))

; Reordering columns
(test (SELECT '("Age" "LikesChocolate" "Name") FROM Person)
      '(("Age" "LikesChocolate" "Name")
        (20 #t "David") 
        (30 #t "Jen") 
        (100 #f "Paul")))

; Select creates duplicates
(test (SELECT '("Name") FROM Teaching)
      '(("Name")
        ("David")
        ("Paul")
        ("David")))

; Select given a literal table
(test
 (SELECT '("A" "B")
         FROM '(("C" "A" "B" "D")
                (1 "Hi" 5 #t)
                (2 "Bye" 5 #f)
                (3 "Hi" 10 #t)))
 '(("A" "B")
   ("Hi" 5)
   ("Bye" 5)
   ("Hi" 10)))

; Select all from two product of two tables
(test (SELECT * FROM [Person "P"] [Teaching "T"])
      '(("P.Name" "Age" "LikesChocolate" "T.Name" "Course")
        ("David" 20 #t "David" "CSC324")
        ("David" 20 #t "Paul" "CSC108")
        ("David" 20 #t "David" "CSC343")
        ("Jen" 30 #t "David" "CSC324")
        ("Jen" 30 #t "Paul" "CSC108")
        ("Jen" 30 #T "David" "CSC343")
        ("Paul" 100 #f "David" "CSC324")
        ("Paul" 100 #f "Paul" "CSC108")
        ("Paul" 100 #f "David" "CSC343")))

; Select some from two tables
(test (SELECT '("P.Name" "Course" "Age") FROM [Person "P"] [Teaching "T"])
      '(("P.Name" "Course" "Age")
        ("David" "CSC324" 20)
        ("David" "CSC108" 20)
        ("David" "CSC343" 20)
        ("Jen" "CSC324" 30)
        ("Jen" "CSC108" 30)
        ("Jen" "CSC343" 30)
        ("Paul" "CSC324" 100)
        ("Paul" "CSC108" 100)
        ("Paul" "CSC343" 100)))

; Take the product of a table with itself
(test (SELECT '("E.Course" "E1.Course") FROM [Teaching "E1"] [Teaching "E"])
      '(("E.Course" "E1.Course")
        ("CSC324" "CSC324")
        ("CSC108" "CSC324")
        ("CSC343" "CSC324")
        ("CSC324" "CSC108")
        ("CSC108" "CSC108")
        ("CSC343" "CSC108")
        ("CSC324" "CSC343")
        ("CSC108" "CSC343")
        ("CSC343" "CSC343")))

; Take the product of a literal table with an identifier
(test
 (SELECT *
         FROM ['(("Age" "A" "Name" "D")
                 (1 "Hi" 5 #t)
                 (2 "Bye" 5 #f)
                 (3 "Hi" 10 #t))
               "T1"]
         [Person "T2"])
 '(("T1.Age" "A" "T1.Name" "D" "T2.Name" "T2.Age" "LikesChocolate")
   (1 "Hi" 5 #t "David" 20 #t)
   (1 "Hi" 5 #t "Jen" 30 #t)
   (1 "Hi" 5 #t "Paul" 100 #f)
   (2 "Bye" 5 #f "David" 20 #t)
   (2 "Bye" 5 #f "Jen" 30 #t)
   (2 "Bye" 5 #f "Paul" 100 #f)
   (3 "Hi" 10 #t "David" 20 #t)
   (3 "Hi" 10 #t "Jen" 30 #t)
   (3 "Hi" 10 #t "Paul" 100 #f)))

; ---- SELECT/FROM ADDITIONAL NEW TESTS ----

; Select all from empty
(test (SELECT * FROM Empty)
      '(()
        ()))

; Take the product of an empty table with itself
(test (SELECT * FROM [Empty "E"] [Empty "E1"])
      '(()
        ()))

; Take the product of an empty table with itself multiple times
(test (SELECT * FROM [Empty "E"] [Empty "E1"] [Empty "E2"])
      '(()
        ()))

; Select all from one row table
(test (SELECT * FROM Games)
      '(("Name" "Price")
        ("Overwatch" 40)))

; Select all from duplicate table
(test (SELECT * FROM Sports)
      '(("Name" "Sport" "Age")
        ("Edd" "Soccer" 18)
        ("Edd" "Football" 19)
        ("Tom" "Rugby" 20)
        ("Tom" "Soccer" 20)
        ("Tom" "Soccer" 20)
        ("Phil" "Tennis" 20)))

; Select all from product of three tables
(test (SELECT * FROM [Person "P"] [Teaching "T"] [Games "G"])
      '(("P.Name" "Age" "LikesChocolate" "T.Name" "Course" "G.Name" "Price")
        ("David" 20 #t "David" "CSC324" "Overwatch" 40)
        ("David" 20 #t "Paul" "CSC108" "Overwatch" 40)
        ("David" 20 #t "David" "CSC343" "Overwatch" 40)
        ("Jen" 30 #t "David" "CSC324" "Overwatch" 40)
        ("Jen" 30 #t "Paul" "CSC108" "Overwatch" 40)
        ("Jen" 30 #t "David" "CSC343" "Overwatch" 40)
        ("Paul" 100 #f "David" "CSC324" "Overwatch" 40)
        ("Paul" 100 #f "Paul" "CSC108" "Overwatch" 40)
        ("Paul" 100 #f "David" "CSC343" "Overwatch" 40)))

; Select all from duplicate table joined with a one row table
(test (SELECT * FROM [Sports "S"] [Games "G"])
      '(("S.Name" "Sport" "Age" "G.Name" "Price")
        ("Edd" "Soccer" 18 "Overwatch" 40)
        ("Edd" "Football" 19 "Overwatch" 40)
        ("Tom" "Rugby" 20 "Overwatch" 40)
        ("Tom" "Soccer" 20 "Overwatch" 40)
        ("Tom" "Soccer" 20 "Overwatch" 40)
        ("Phil" "Tennis" 20 "Overwatch" 40)))

; Select all from valid table joined with an Empty table
(test (SELECT * FROM [Person "P"] [Empty "E"])
      '(("Name" "Age" "LikesChocolate") 
        ("David" 20 #t) 
        ("Jen" 30 #t) 
        ("Paul" 100 #f)))

; Select one from valid table joined with an Empty table
(test (SELECT '("Name") FROM [Person "P"] [Empty "E"])
      '(("Name")
        ("David")
        ("Jen")
        ("Paul")
        ))

; Select * from valid table joined with a valid table
; containing an Empty tuple
(test (SELECT '("P.Name") FROM [Person "P"] [Valid-Empty "E"])
      '(("P.Name")
        ("David")
        ("David")
        ("David")
        ("Jen")
        ("Jen")
        ("Jen")
        ("Paul")
        ("Paul")
        ("Paul")
        ))

; Select * from valid table joined with a valid table
; containing an Empty tuple
(test (SELECT * FROM [Games "G"] [Valid-Empty "E"])
      '(("G.Name" "G.Price" "E.Name" "E.Price")
        ("Overwatch" 40 "john" 23)
        ("Overwatch" 40 "shray" 33)
        ("Overwatch" 40))
        )

; Cross product a table with a period attribute 
(test (SELECT * FROM [period-table "p"] [Valid-Empty "E"])
      '(("p.Name" ".Price" "E.Name" "Price")
        ("seamus" 44 "john" 23)
        ("seamus" 44 "shray" 33)
        ("seamus" 44))
        )

; Select a conflicting attribute from a cross-product of two tables,
; one table being named "."
(test (SELECT '("..Name") FROM [Games "."] [Valid-Empty "E"])
      '(("..Name")
        ("Overwatch")
        ("Overwatch")
        ("Overwatch"))
        )

; Cross product a table with attributes starting with a period,
; and also named starting with a period,with a table with empty tuples
(test (SELECT * FROM [period-table "."] [Valid-Empty "E"])
      '(("..Name" ".Price" "E.Name" "Price")
        ("seamus" 44 "john" 23)
        ("seamus" 44 "shray" 33)
        ("seamus" 44))
        )

; Take the product of a table with itself multiple times
(test (SELECT '("G.Name" "G1.Name" "G2.Name") 
              FROM [Games "G"] [Games "G1"] [Games "G2"])
      '(("G.Name" "G1.Name" "G2.Name")
        ("Overwatch" "Overwatch" "Overwatch")))

; Take the product of a literal table with an Empty table
(test
 (SELECT *
         FROM ['(("Name" "Age")
                 ("Danny" 55)
                 ("Karen" 60)
                 ("Michelle" 50))
               "T1"]
         [Empty "E"])
 '(("Name" "Age")
   ("Danny" 55)
   ("Karen" 60)
   ("Michelle" 50)))

; Select some from the product of a literal table with an Empty table
(test
 (SELECT '("Age")
         FROM ['(("Name" "Age")
                 ("Danny" 55)
                 ("Karen" 60)
                 ("Michelle" 50))
               "T1"]
         [Empty "E"])
 '(("Age")
   (55)
   (60)
   (50)))

; ---- WHERE ----

(printf "~n ---- WHERE TESTS ---- ~n~n")

; Attribute as condition, select all
(test (SELECT *
              FROM Person
              WHERE "LikesChocolate")
      '(("Name" "Age" "LikesChocolate")
        ("David" 20 #t)
        ("Jen" 30 #t)))

; Attribute as condition, select subset
(test (SELECT '("LikesChocolate" "Name")
              FROM Person
              WHERE "LikesChocolate")
      '(("LikesChocolate" "Name")
        (#t "David")
        (#t "Jen")))

; Condition as function of one attribute, select all
(test (SELECT *
              FROM Person
              WHERE (< 50 "Age"))
      '(("Name" "Age" "LikesChocolate")
        ("Paul" 100 #f)))

; Condition as function of one attribute, select none
(test (SELECT '()
              FROM Teaching
              WHERE (equal? "Name" "David"))
      '(()
        ()
        ()))

; Constant true condition
(test (SELECT *
              FROM Person
              WHERE #t)
      Person)

; Constant false compound condition
(test (SELECT *
              FROM Person
              WHERE (> (string-length "David") 20))
      '(("Name" "Age" "LikesChocolate")))

; Condition on a literal table
(test (SELECT '("C" "B")
              FROM '(("A" "B" "C") 
                     (1 2 3)
                     (3 10 40)
                     (4 4 4)
                     (2 3 -1))
              WHERE (odd? "A"))
      '(("C" "B")
        (3 2)
        (40 10)))

; Simple condition on joined tables
(test (SELECT *
              FROM [Person "P"] [Teaching "T"]
              WHERE (equal? "P.Name" "T.Name"))
      '(("P.Name" "Age" "LikesChocolate" "T.Name" "Course")
        ("David" 20 #t "David" "CSC324")
        ("David" 20 #t "David" "CSC343")
        ("Paul" 100 #f "Paul" "CSC108")))

; Compound condition on three joined tables, two of which are the same
(test (SELECT '("P1.Name" "P1.LikesChocolate" "P.Age" "Course")
              FROM [Person "P"] [Teaching "T"] [Person "P1"]
              WHERE (And "P.LikesChocolate" (equal? "P1.Name" "T.Name")))
      '(("P1.Name" "P1.LikesChocolate" "P.Age" "Course")
        ("David" #t 20 "CSC324")
        ("Paul" #f 20 "CSC108")
        ("David" #t 20 "CSC343")
        ("David" #t 30 "CSC324")
        ("Paul" #f 30 "CSC108")
        ("David" #t 30 "CSC343")))

; ---- WHERE ADDITIONAL NEW TESTS ----

; Condition as function of one attribute, select all from
; table joined with empty

(printf "~n ---- Where Additional TESTS ---- ~n~n")
(test (SELECT * 
              FROM [Person "P"] [Empty "E"]
              WHERE (< 50 "Age"))
      '(("Name" "Age" "LikesChocolate")
        ("Paul" 100 #f)))

; Condition as function of one attribute, select none
(test (SELECT '()
              FROM Teaching
              WHERE (equal? "Name" "Paul"))
      '(()
        ()))

; Constant true condition for empty table, select all
(test (SELECT *
              FROM Empty
              WHERE #t)
      Empty)

; Constant false compound condition for valid table, select all
(test (SELECT *
              FROM Person
              WHERE #f)
      '(("Name" "Age" "LikesChocolate")))

; Condition on a literal table joined with an Empty table
(test (SELECT '("C" "B")
              FROM ['(("A" "B" "C") 
                      (1 2 3)
                      (3 10 40)
                      (4 4 4)
                      (2 3 -1))
                    "T1"]      
              [Empty "E"]
              WHERE (odd? "A"))
      '(("C" "B")
        (3 2)
        (40 10)))

; ---- ORDER BY ----

(printf "~n ---- ORDER BY TESTS ---- ~n~n")

; Order by attribute
(test (SELECT *
              FROM Person
              ORDER BY "Age")
      '(("Name" "Age" "LikesChocolate")
        ("Paul" 100 #f)
        ("Jen" 30 #t)
        ("David" 20 #t)))

; Order by attribute, not selected
(test (SELECT '("Name")
              FROM Person
              ORDER BY "Age")
      '(("Name")
        ("Paul")
        ("Jen")
        ("David")))

; Order by a function of an attribute
(test (SELECT *
              FROM Person
              ORDER BY (string-length "Name"))
      '(("Name" "Age" "LikesChocolate")
        ("David" 20 #t)
        ("Paul" 100 #f)
        ("Jen" 30 #t)))

; Order with duplicate
(test (SELECT *
              FROM Teaching
              ORDER BY (+ (string-length "Name") (string-length "Course")))
      '(("Name" "Course")
        ("David" "CSC324")
        ("David" "CSC343")
        ("Paul" "CSC108")))

; Order on a literal table
(test (SELECT *
              FROM '(("A" "B" "C") 
                     (1 2 3)
                     (3 10 40)
                     (4 4 4)
                     (2 3 -1))
              ORDER BY "C")
      '(("A" "B" "C")
        (3 10 40)
        (4 4 4)
        (1 2 3)
        (2 3 -1)))

; Order on two tables
(test (SELECT *
              FROM [Person "P"] [Teaching "T"]
              ORDER BY "Age")
      '(("P.Name" "Age" "LikesChocolate" "T.Name" "Course")
        ("Paul" 100 #f "David" "CSC324")
        ("Paul" 100 #f "Paul" "CSC108")
        ("Paul" 100 #f "David" "CSC343")
        ("Jen" 30 #t "David" "CSC324")
        ("Jen" 30 #t "Paul" "CSC108")
        ("Jen" 30 #t "David" "CSC343")
        ("David" 20 #t "David" "CSC324")
        ("David" 20 #t "Paul" "CSC108")
        ("David" 20 #t "David" "CSC343")))

; ---- ORDER BY ADDITIONAL NEW TESTS ----

; Order by on valid table joined with an Empty table, select all
(test (SELECT *
              FROM [Person "P"] [Empty "E"]
              ORDER BY "Age")
      '(("Name" "Age" "LikesChocolate")
        ("Paul" 100 #f)
        ("Jen" 30 #t)
        ("David" 20 #t)))

; Order by on three valid tables joined together, select all
(test (SELECT *
              FROM [Person "P"] [Teaching "T"] [Games "G"]
              ORDER BY "Age")
      '(("P.Name" "Age" "LikesChocolate" "T.Name" "Course" "G.Name" "Price")
        ("Paul" 100 #f "David" "CSC324" "Overwatch" 40)
        ("Paul" 100 #f "Paul" "CSC108" "Overwatch" 40)
        ("Paul" 100 #f "David" "CSC343" "Overwatch" 40)
        ("Jen" 30 #t "David" "CSC324" "Overwatch" 40)
        ("Jen" 30 #t "Paul" "CSC108" "Overwatch" 40)
        ("Jen" 30 #t "David" "CSC343" "Overwatch" 40)
        ("David" 20 #t "David" "CSC324" "Overwatch" 40)
        ("David" 20 #t "Paul" "CSC108" "Overwatch" 40)
        ("David" 20 #t "David" "CSC343" "Overwatch" 40)))

; ---- ORDER BY and WHERE ----

(printf "~n ---- ORDER BY/WHERE TESTS ---- ~n~n")

; Use attributes, select all 
(test
 (SELECT * 
         FROM Person 
         WHERE "LikesChocolate" 
         ORDER BY "Age")
 '(("Name" "Age" "LikesChocolate")
   ("Jen" 30 #t)
   ("David" 20 #t)))

; Use attributes, select one unused attribute
(test
 (SELECT '("Name") 
         FROM Person 
         WHERE "LikesChocolate" 
         ORDER BY "Age")
 '(("Name")
   ("Jen")
   ("David")))

; Two joined tables, select all
(test
 (SELECT * 
         FROM [Person "P"] [Teaching "T"] 
         WHERE (equal? "P.Name" "T.Name")
         ORDER BY "Age")
 '(("P.Name" "Age" "LikesChocolate" "T.Name" "Course")
   ("Paul" 100 #f "Paul" "CSC108")
   ("David" 20 #t "David" "CSC324")
   ("David" 20 #t "David" "CSC343")))

; Two joined tables, select some attributes
(test
 (SELECT '("P.Name" "Course" "LikesChocolate")
         FROM [Person "P"] [Teaching "T"] 
         WHERE (equal? "P.Name" "T.Name")
         ORDER BY "Age")
 '(("P.Name" "Course" "LikesChocolate")
   ("Paul" "CSC108" #f)
   ("David" "CSC324" #t)
   ("David" "CSC343" #t)))

; ---- ORDER BY/WHERE ADDITIONAL NEW TESTS ----

; Two valid tables joined with Empty table, select some attributes
(test
 (SELECT '("P.Name" "Course" "LikesChocolate")
         FROM [Person "P"] [Teaching "T"] [Empty "E"]
         WHERE (equal? "P.Name" "T.Name")
         ORDER BY "Age")
 '(("P.Name" "Course" "LikesChocolate")
   ("Paul" "CSC108" #f)
   ("David" "CSC324" #t)
   ("David" "CSC343" #t)))


; ---- Nested queries ----

(printf "~n ---- NESTED QUERY TESTS ---- ~n~n")

(test
 (SELECT * 
         FROM (SELECT '("Age" "Name") FROM Person))
 '(("Age" "Name")
   (20 "David")
   (30 "Jen")
   (100 "Paul")))

(test
 (SELECT '("Person.Name" "Course")
         FROM [(SELECT '("Name") FROM Person) "Person"]
         [(SELECT * FROM Teaching WHERE (Or (equal? "Course" "CSC343")
                                            (equal? "Course" "CSC108")))
          "Teaching"])
 '(("Person.Name" "Course")
   ("David" "CSC108")
   ("David" "CSC343")
   ("Jen" "CSC108")
   ("Jen" "CSC343")
   ("Paul" "CSC108")
   ("Paul" "CSC343")))

; Nested query containing a literal
(test
 (SELECT *
         FROM [(SELECT '("A") 
                       FROM '(("A" "B") 
                              (1)
                              (10)))
               "Table1"]
         [(SELECT *
                  FROM '(("C" "A")
                         ("Hi" "Bye")
                         ("Dog" "Cat")
                         ("Red" "Blue")))
          "Table2"]
         WHERE (And (equal? (string-length "Table2.A") 3) (< 0  "Table1.A")))
 '(("Table1.A" "C" "Table2.A")
   (1 "Hi" "Bye")
   (1 "Dog" "Cat")
   (10 "Hi" "Bye")
   (10 "Dog" "Cat")))
