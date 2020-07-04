# More-Queries

Project used to build basic queries, using Prolog. The project is used to practice Prolog coding.

## Description

The program evaluates the result of applying certain queries on given tables. Testing means, along with tables and expected outputs are provided in order to test the functionality.

## Provided queries

The following queries are available:

* table - The most basic query, which takes the name of the table and return the table itself.
* tprint - It receives a query and prints the table resulted from the evaluated query.
* select - It takes a header and a query, it returns a table that contains only the columns from the given header, extracted from the table resulted from the received query's evaluation.
* join - It receives a predicate, a header and two queries and it returns a table with the given header, with the new values resulted by satisfying the predicate using the values on each coresponding entry of the two tables.
* tfilter - It receives a list of uninstantiated variables, a predicate structured based on the previous list and a query. It returns a table which has contains only the entries that satisfy the given predicate, starting from the table resulted from the received query's evaluation. 
