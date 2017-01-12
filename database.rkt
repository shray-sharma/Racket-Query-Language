#| Assignment 1 - Racket Query Language  (due Oct 17, 11:50pm)

Shray Sharma, sharm558, 1001430472
John Chen, chenjoh6, 1001636651
|#

#lang racket

; Tables used for testing and example calls

(define Person
  '(("Name" "Age" "LikesChocolate")
    ("David" 20 #t)
    ("Jen" 30 #t) 
    ("Paul" 100 #f)))

(define Teaching
  '(("Name" "Course")
    ("David" "CSC324")
    ("Paul" "CSC108")
    ("David" "CSC343")))

(define Players '(("fName" "sName" "Team" "Goals")
                  ("Sergio" "Aguero" "Manchester City" 20)
                  ("Cristiano" "Ronaldo" "Real Madrid CF" 48)
                  ("David" "Silva" "Manchester City" 8)
                  ("Lionel" "Messi" "FC Barcelona" 51)))

; Function versions for common syntactic forms.
; *Use these in your queries instead of the syntactic forms!!!*
(define (And x y) (and x y))
(define (Or x y) (or x y))
(define (If x y z) (if x y z))

(provide attributes
         tuples
         size
         SELECT)

; Part 0: Semantic aliases

#|
(attributes table)
  table: a valid table (i.e., a list of lists as specified by assigment)
  Returns a list of the attributes in 'table', in the order they appear.
|#

(define attributes first)

#|
(tuples table)
  table: a valid table
  Returns a list of all tuples in 'table', in the order they appear.
  Note: it is possible for 'table' to contain no tuples.
|#

(define tuples rest)

#|
(size table)
  table: a valid table
  Returns the number of tuples in 'table'.
|#

(define (size x) 
  (length (tuples x)))

#|
(find lst item)
A function that takes:
  lst: a list
  item: some value

  and returns true if the item is found in lst.

> (find 1 '(1 2 3))
#t
> (find '(4 2) '((1 2 3) (4 2) (3 3 4) (5 3 2 1)))
#t
|#
(define (find item lst)
  (cond [(empty? lst) #f]
        [(equal? (first lst) item) #t]
        [else (find item (rest lst))]))

#|
A function that takes: 
  - a list of attributes
  - a string (representing an attribute)
  - a tuple

  and returns the value of the tuple corresponding to that attribute.

> (get-value '("fName" "sName" "age")
               "sName"
               '("John" "Smith" "25"))
"Smith"
|#
(define (get-value attributes attr tuple)
  (if (equal? (first attributes) attr)
      (first tuple)
      (get-value (rest attributes) attr (rest tuple))))

#|
(filter-table f table)
A function that takes:
  - f: a unary function that takes a tuple and returns a boolean value
  - table: a valid table

  and returns a new table containing only the tuples in 'table'
  that satisfy 'f'.
|#

(define (filter-table f table)
  (cons (attributes table)
        (filter f (tuples table))))

#|
A function 'replace-attr' that takes:
  - x 
  - a list of attributes

  and returns a function 'f' which takes a tuple and does the following:
    - If 'x' is in the list of attributes, return the corrresponding value 
      in the tuple.
    - Otherwise, just ignore the tuple and return 'x'.

> (define f (replace-attr "sName" '("fName" "sName" "Team")))
> (f '("Lionel" "Messi" "FC Barcelona"))
"Messi"
|#

(define (replace-attr x attrs)
  (lambda (tuple)
    (if (find x attrs)
        (get-value attrs x tuple)
        x)))

#|
(order-by-helper func table)
A function that takes:
  - func: a function that specifies the #:key within a tuple for sorting   
  - table: a valid table

  and returns a new table sorted in descending order as specified by func.
|#

(define (order-by-helper func table)
  (cons (attributes table)
        (sort (tuples table)
              (lambda (x y)
                (if (string? x)
                    (string>? x y)
                    (> x y)))
              #:key func)))

#|
(count-occurrences lst target)
 A function that takes:
  lst: the list to look through
  target: the target item to find

  and returns the number of occurrences of target in list
|#
(define (count-occurrences lst target)
  (define (add-if-equal lst-elt accum)
    (if (equal? lst-elt target)
        (+ 1 accum)
        (+ 0 accum)
        ))
  (foldl add-if-equal 0 lst)
  )

#|
(check-all-duplicates list-to-be-renamed list-of-list-of-attributes name)
 A function that takes:
  list-to-be-renamed: the attribute list that may be renamed
  list-of-list-of-attributes: the total list of all attributes
  name: the name of the table of which list-to-be-renamed is the attribute list 

  Returns a new list that has a  1-to-1, same-order mapping with list-to-be-renamed.
  The mapping is as follows: if any element in list-to-be-renamed was found
  in the (flattened) list-of-list-of-attributes, then this element is renamed
  as "[name].[element]". Otherwise, it is left alone. As mentioned previously,
  the order is left unchanged--if list-to-be-renamed did not have any elements in
  common with the list-of-list-of-attributes, list-to-be-renamed is returned
  unmolested. 

|#
(define (check-all-duplicates list-to-be-renamed list-of-list-of-attributes name)
  ; inner function which renames the element if it occurs more than once
  ; in the total list-of-list-of-attributes
  (define (rename-duplicates target)
    (if (> (count-occurrences (flatten list-of-list-of-attributes) target) 1)
        (string-append name "." target)
        target
        )
    )
  (map rename-duplicates list-to-be-renamed))

#|
(rename-attributes-helper list-of-list-of-attributes list-of-names position
accumulator)

 A function that takes:
  list-of-list-of-attributes: the total list of all attributes
  list-of-names: the list of names representing the table names corresponding
  to the attributes
  position: the position in the list-of-names we are currently at
  accumulator: the list of attributes renamed so far
 
  Note that these are parallel arrays: our position in list-of-names is
  the same as our position in list-of-list-of-attributes.

  The return type depends on the value of position.

  If position is > length of the list-of-names - 1, the function returns a new
  list representing a modified and flattened list-of-list-of-attributes,
  satisfying the calling function.

  Otherwise, this function calls check-all-duplicates and appends the results
  of that function to accumulator, building the new total list of all attributes
  list by list.
|#

(define (rename-attributes-helper list-of-1ist-of-attributes list-of-names position accumulator)
  (if (> position (- (length list-of-names) 1))
      accumulator
      (rename-attributes-helper  list-of-1ist-of-attributes list-of-names
                                 (+ position 1) (append accumulator
                                                        (check-all-duplicates
                                                         (list-ref list-of-1ist-of-attributes position)
                                                         list-of-1ist-of-attributes
                                                         (list-ref list-of-names position))
                                                        ))))

#|
(rename-attributes list-of-list-of-attributes list-of-names)
 A function that takes:
  list-of-list-of-attributes: the total list of all attributes
  list-of-names: the list of names representing the table names corresponding
  to the attributes

  Note that these are parallel arrays: each element (name) in the list of names
  corresponds to a list in the list-of-list-of-attributes.
  
  Returns a new list representing a modified and flattened
  list-of-list-of-attributes. The modification is given in the handout, but
  is repeated here for clarity. Essentially, the new list is guaranteed
  to have no identically named elements, with collisions resolved by prepending
  the corresponding name given in list-of-names followed by "." (dot)
  to the element.
|#

(define (rename-attributes list-of-list-of-attributes list-of-names)
  (rename-attributes-helper list-of-list-of-attributes list-of-names 0 '())) 

#|
(cartesian-product-helper table1 table2)
A function that takes:
  table1: a valid table
  table2: a valid table

  and returns the cartesian product of the two tables.

|#

(define (cartesian-product-helper table1 table2)
  (define (add-to-all-lists-i target-list)
    (define (my-append lst2)
      (append target-list lst2)) 
    (if (empty? table1)
        '()
        (map my-append table1)))
  (map add-to-all-lists-i table2))

#|
(cartesian-product-helper table1 table2)
A function that takes:
  table1: a valid table
  table2: a valid table

  and returns the cartesian product of the two tables.
|#

(define (cartesian-product table1 table2)
  (apply append (cartesian-product-helper table2 table1)))

#|
(n-cartesian-product listoftables)
A function that takes:
  listoftables: a list of tables to be joined together

  and returns the cross product of all the tables.
  How it works is that the first table is cross-producted with the
  cartesian-product of the remaining tables, and so on.
|#

(define (n-cartesian-product listoftables)
  (if (empty? (rest listoftables))
      (first listoftables) ; 
      (cartesian-product (first listoftables) (n-cartesian-product (rest listoftables)))))

#|
(from list-of-tables list-of-names)
A function that takes:
  list-of-tables: a list of tables to be joined
  list-of-names: the corresponding names of the list of tables

  and returns a table with all the tables joined together.

  More specifically, the first list will be the new total list of attributes.
  The remaining list will be a new table formed from the cross product of the tuples.
|#

(define (from list-of-tables list-of-names)
  (cons (rename-attributes (map attributes list-of-tables) list-of-names)       
        (n-cartesian-product (map tuples list-of-tables))))

#|
(first-col table)
A function that takes:
  table: a valid table

  and returns a list containing only the first column of table.

> (first-col '(("fName" "sName")
               ("Sergio" "Aguero")
               ("Lionel" "Messi")))
|#

(define (first-col table)
  (map (lambda (l) (first l)) table))

#|
(rest-col table)
A function that takes:
  table: a valid table

  and returns table with the first column removed.

> (rest-col '(("fName" "sName" "Team")
              ("Sergio" "Aguero" "Manchester City")
              ("Lionel" "Messi" "FC Barcelona")))
'(("sName"  "Team")
  ("Aguero" "Manchester City")
  ("Messi"  "FC Barcelona"))
|#

(define (rest-col table)
  (map (lambda (lst) (rest lst)) table))

#|
(create-list n)
A function that takes:
  n: a number

  and returns a list containing n empty lists.

> (create-list 4)
'(() () () ())
|#

(define (create-list n)
  (map (lambda (x) (append '() '())) (range n))) 

#|
(join-cols col1 col2)
A function that takes:
  col1: a list of lists
  col2: a list

  and returns a table containing columns col1 and col2.

> (join-cols '(("fName") ("Sergio") ("Lionel"))
             '("sName" "Aguero" "Messi"))
'(("fName"  "sName")
  ("Sergio" "Aguero")
  ("Lionel" "Messi"))
|#

(define (join-cols col1 col2)
  (map (lambda (lst1 lst2) (append lst1 (list lst2))) col1 col2))

(define (join-table cols final-table)
  (if (empty? cols)
      final-table
      (join-table (rest cols) (join-cols final-table (append* (first cols))))))

#|
(select attrs table)
A function that takes:
  attrs: a list
  table: a valid table

  and returns a subset of the columns of table corresponding to
  attrs or the entire table if attrs is "*".

> (select '("Team") Players)
'(("Team")
  ("Manchester City")
  ("Real Madrid CF")
  ("Manchester City")
  ("FC Barcelona"))
|#

(define (select attrs table)
  (if (equal? attrs "*")
      table
      (join-table
       (map (lambda (attr)
              (select-helper attr table (create-list (length table))))
            attrs)
       (create-list (length table)))))

(define (select-helper attr table attr-col)
  (if (empty? (first table))
      attr-col
      (if (equal? attr (caar table))
          (join-cols attr-col (first-col table))
          (select-helper attr (rest-col table) attr-col))))

#|
replace:
a macro that uses the 'replace-attr' function recursively
to reduce an expression to a value by replacing attributes
in the expression with their corresponding values

ex. (equal? "Name" "Sam") -> (equal? "David" "Sam") -> #f
|#

(define-syntax replace
  (syntax-rules ()
    [(replace (expr ...) <attrs>)
     (lambda (tuple)
       (if (empty? tuple)
           tuple
           (let ([cond (list ((replace expr <attrs>) tuple) ...)])
             (apply (first cond) (rest cond))
             )))]
    [(replace <atom> <attrs>)
     (lambda (tuple)
       (if (empty? tuple)
           tuple
           ((replace-attr <atom> <attrs>) tuple)))]))

#|
(where cond table)
A macro that takes:
  expr: an expression
  table: a valid table

  and returns table with only tuples that satisfied expr.

> (where (equal? "Team" "Manchester City") Players)
'(("fName"  "sName"  "Team"            "Goals")
  ("Sergio" "Aguero" "Manchester City" 20)
  ("David"  "Silva"  "Manchester City" 8))

> (where "LikesChocolate" Person)
'(("Name"  "Age" "LikesChocolate")
  ("David" 20    #t)
  ("Jen"   30    #t))
|#

(define-syntax where
  (syntax-rules ()
    [(where <cond> <table>)
     (filter-table (replace <cond> (attributes <table>)) <table>)]))

#|
(order-by ord table)
A macro that takes:
  ord: an expression
  table: a valid table

  and returns table with tuples sorted in descending order
  as specified by ord.

> (order-by "Goals" Players)
'(("fName"     "sName"   "Team"            "Goals")
  ("Lionel"    "Messi"   "FC Barcelona"    51)
  ("Cristiano" "Ronaldo" "Real Madrid CF"  48)
  ("Sergio"    "Aguero"  "Manchester City" 20)
  ("David"     "Silva"   "Manchester City" 8))

> (order-by (string-length "fName") Players)
'(("fName"     "sName"   "Team"            "Goals")
  ("Cristiano" "Ronaldo" "Real Madrid CF"  48)
  ("Sergio"    "Aguero"  "Manchester City" 20)
  ("Lionel"    "Messi"   "FC Barcelona"    51)
  ("David"     "Silva"   "Manchester City" 8))
|#

(define-syntax order-by
  (syntax-rules ()
    [(order-by <ord> <table>)
     (order-by-helper (replace <ord> (attributes <table>)) <table>)]))

#|
SELECT:
a macro to handle all queries.
|#
(define-syntax SELECT
  (syntax-rules (FROM WHERE ORDER BY)
    ; FROM -> SELECT
    [(SELECT (attrs ...) FROM table)
     (select (attrs ...) table)]
    [(SELECT (attrs ...) FROM [table name] ...)
     (SELECT (attrs ...) FROM (from (list table ...) (list name ...)))]
    
    ; FROM -> SELECT * (all attributes)
    [(SELECT atom FROM table)
     (if (equal? * atom) (select (attributes table) table) "select: invalid arg")]
    [(SELECT atom FROM [table name] ...)
     (if (equal? * atom) (from (list table ...) (list name ...)) "select: invalid arg")]
    
    ; FROM -> WHERE -> SELECT
    [(SELECT (attrs ...) FROM table WHERE pred)
     (SELECT (attrs ...) FROM (where pred table))]
    [(SELECT (attrs ...) FROM [table name] ... WHERE pred)
     (SELECT (attrs ...) FROM (where pred (from (list table ...) (list name ...))))]
    [(SELECT atom FROM table WHERE pred)
     (SELECT atom FROM (where pred table))]
    [(SELECT atom FROM [table name] ... WHERE pred)
     (SELECT atom FROM (where pred (from (list table ...) (list name ...))))]
    
    ; FROM -> ORDER BY -> SELECT
    [(SELECT (attrs ...) FROM table ORDER BY ord)
     (SELECT (attrs ...) FROM (order-by ord table))]
    [(SELECT (attrs ...) FROM [table name] ... ORDER BY ord)
     (SELECT (attrs ...) FROM (order-by ord (from (list table ...) (list name ...))))]
    [(SELECT atom FROM table ORDER BY ord)
     (SELECT atom FROM (order-by ord table))]
    [(SELECT atom FROM [table name] ... ORDER BY ord)
     (SELECT atom FROM  (order-by ord (from (list table ...) (list name ...))))]
    
    ; FROM -> WHERE -> ORDER BY -> SELECT
    [(SELECT (attrs ...) FROM table WHERE pred ORDER BY ord)
     (SELECT (attrs ...) FROM (order-by ord table) WHERE pred)]
    [(SELECT (attrs ...) FROM [table name] ... WHERE pred ORDER BY ord)
     (SELECT (attrs ...) FROM (order-by ord (where pred (from (list table ...) (list name ...)))))]
    [(SELECT atom FROM table WHERE pred ORDER BY ord)
     (SELECT atom FROM (order-by ord (where pred table)))]
    [(SELECT atom FROM [table name] ... WHERE pred ORDER BY ord)
     (SELECT atom FROM (order-by ord (where pred (from (list table ...) (list name ...)))))]  
    ))