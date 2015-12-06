LeafDB is a functional local file storage system implemented in OCaml with
SQL-like commands to query.

TO RUN:

install menhir if not already installed

opam install menhir

install csv

opam install csv

THEN:
ocamlbuild -use-ocamlfind -pkgs csv -use-menhir -libs str,unix main.ml

THEN:
ocamlbuild -use-ocamlfind -pkgs csv -use-menhir -libs str,unix main.byte

THEN:
./main.byte

SUPPORTED COMMANDS
Our file storage system uses SQL-like commands; some vary from the traditional
SQL style. This is the full list of commands we support.

NOTE: keywords can be written in CAPS or full lowercase. Semicolons are optional
at the end of the command.

In the DBMS:

  SHOW DATABASES

  USE <database_name>

  CREATE DATABASE <database_name>

  DROP DATABASE <database_name>

  To exit the program:

  EXIT

Inside a database:

  SHOW TABLES

  CREATE TABLE <table_name> (<column_1> <value_1>, <column_2> <value_2>, ...)

  SELECT <column_1>, <column_2>, ..., <column_n> FROM <table_name>
  WHERE <column> <operation> <value>

  To select all columns:

  SELECT * FROM <table_name> WHERE <column> <operation> <value>

  INSERT INTO <table_name> (<column_1>, <column_2>, ..., <column_n>)
  VALUES (<value_1>, <value_2>, ..., <value_n>)

  To insert into all columns:

  INSERT INTO <table_name>
  VALUES (<value_1>, <value_2>, ..., <value_n>)

  <a select statement or table name>
  JOIN
  <a select statement or table name>
  ON <column_name from 1st select/table> = <column_name from 2nd select/table>

  (NOTE: our JOIN strays away from conventional SQL. Also, if using select
  statements, the columns in ON column1 = column2 must be selected.)

  UPDATE <table_name>
  SET <column_1>=<value_1>, <column_2>=<value_2>, ..., <column_n>=<value_n>
  WHERE <some_column>=<some_value>

  DELETE FROM <table_name>
  WHERE <some_column> = <some_value>

  DROP TABLE <table_name>

  To exit the database:

  EXIT

SUPPORTED VALUES:
These are the values that can be specified when creating tables.
  FLOAT
  BOOLEAN
  STRING
  INT

SUPPORTED OPERATORS:
These operators can be used in the WHERE conditions of SELECT statements
  =
  >
  >=
  <
  <=
  !=
  LIKE
  NOT LIKE

OTHER NOTES:
The test cases in interpret.ml, map.ml, and table.ml are commented out
because the ocamlbuild does not support the test case syntax

