# Racket-Query-Language
An assignment for CSC324. Note to UofT students, please **do not plagarize**. MarkUs ***will*** catch you.  
---
An implementation of a subset of the query language portion of SQL, in Racket. Wrote Racket functions to perform queries on "tables" and used Racket's powerful macro system to mimic the syntax of SQL.

### Querying in RQL
We use <attrs> to specify the columns we want to see, <tables> to specify the table(s) to get the data from, <condition> to lter the data, and <order-expr> to order the results. The SELECT and FROM clauses are required for every query, but WHERE and ORDER BY are optional: zero, one, or both of them may appear in a query. However, the keywords must follow this order; so FROM cannot appear before SELECT and ORDER BY cannot appear before WHERE.
```
(SELECT <attrs>
 FROM <tables>
 WHERE <condition>
 ORDER BY <order-expr>)
```

### Tables
Tables are represented with a list-of-lists where The first list is a list of strings naming the attributes of the relation and each following list represents one tuple in the table.
```
'(( "Name" "Age" "LikesChocolate")
 ("David" 20 #t)
 ("Jen" 30 #t)
 ("Paul" 50 #f))
```
