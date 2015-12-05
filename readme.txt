-- CamlDB --
Nianze Liu, Lydia Tang and Andrew Wang

-- Compiling and Running --
CamlDB requires jsonm and menhir, so if needed, install these through opam:

    opam install jsonm
    opam install menhir

To compile the project, use ocamlbuild:

    ocamlbuild -use-menhir -pkg jsonm main.byte

To run, run main.byte:

    ocamlrun main.byte

-- Tutorial --
Let's use CamlDB to help with an experiment. We want to see how amount of sleep affects test scores. We'll ask people for their names, average hours slept and their score on the last exam. So run main.byte to start the REPL, and create a table:

    CREATE TABLE data (name STRING, hours FLOAT, score INT);

CREATE TABLE
  Syntax:
    CREATE TABLE
    CREATE TABLE [table name]
    ([column name 1] [column type 1],
    [column name 2] [column type 2],
    ...
    [column name n] [column type n]);

  Syntax Rules:
    [table name]: case-sensitive; support use any string except for the
    built-in words reserved for instructions, such as INT, CREATE.
    [column name]: case-sensitive; support use any string except for the
    built-in words reserved for instructions, such as INT, CREATE.
    [column type]: support 4 types: INT, FLOAT, STRING, BOOL

  Example:
    CREATE TABLE data (name STRING, hours FLOAT, score INT);


Note that queries are case-sensitive: keywords are all caps and identifiers start with a lowercase letter.

Now let's add some data to the table. We give the survey to some people and record the results:

    INSERT INTO data VALUES ("Alex", 5., 72);
    INSERT INTO data VALUES ("Bob", 80E-1, 80);
    INSERT INTO data VALUES ("Cindy", .6e+1, 70);
    INSERT INTO data VALUES ("David", 600.e-2, 75);
    INSERT INTO data VALUES ("Eve", .07E+2, 76);
    INSERT INTO data VALUES ("Fiona", 9e+0, 88);

Great, now we have some data in the table. Let's check what we've done with a SELECT query:

    SELECT * FROM data;

We get a tabular display of our table:

+-------+-------+-------+
| name  | hours | score |
+-------+-------+-------+
| Fiona | 9.    | 88    |
+-------+-------+-------+
| Eve   | 7.    | 76    |
+-------+-------+-------+
| David | 6.    | 75    |
+-------+-------+-------+
| Cindy | 6.    | 70    |
+-------+-------+-------+
| Bob   | 8.    | 80    |
+-------+-------+-------+
| Alex  | 5.    | 72    |
+-------+-------+-------+

Let's try some more complex queries. Select all students who slept less than 7 hours:

    SELECT * FROM data WHERE hours < 7.;

Note that the table uses an optimization strategy, moving the most recently accessed entries to the front. We can see this by viewing the whole table again:

    SELECT * FROM data;

Note that the light sleepers have moved to the top of the table. This helps improve speed since recent accesses are more likely to be accessed later.

We can also select only their hours and scores:

    SELECT hours, score FROM data WHERE hours < 7.;

We can also combine conditions:

    SELECT hours, score FROM data WHERE hours < 7. OR score > 75;

Or select only distinct entries in a column:

    SELECT DISTINCT hours FROM data;

We see that there are many ways to filter and query data.

Turns out Alex scored higher than he thought: he actually got a 76. Let's update the table:

    UPDATE data SET score=76 WHERE name="Alex";

Oops, David isn't even in the class. Delete his entry:

    DELETE FROM data WHERE name="David";

Now that our survey is complete, we'd like to have a better understanding of the data. One way CamlDB lets us do this is with visualizations, specifically scatterplots and line graphs. We can draw a scatterplot with this syntax:

    SELECT hours, score FROM data #SCATTER;

We get a graphical display:

       89.8 |
            |
            |                                                     *
            |
            |
            |
            |
            |
            |
            |                                        *
            |
            |
            |
            |   *                        *
            |
            |
            |
            |
            |                *
       68.2 |-----------------------------------------------------------
            4.6                                                        9.4

Let's draw a line graph to get a sense for the overall trend. Let's sort our data and plot:

    SELECT hours, score FROM data ORDER BY hours ASC #LINE;

We get this curve:

       89.8 |
            |
            |                                                     *
            |                                                   **
            |                                                 **
            |                                               **
            |                                             **
            |                                           **
            |                                         **
            |                                       **
            |                                    ***
            |                                 ***
            |                              ***
            |                           ***
            |                         **
            |                      ***
            |   ****             **
            |       ******     **
            |             *****
       68.2 |-----------------------------------------------------------
            4.6                                                        9.4

To save your work, simply type EXIT; at the REPL, and CamlDB will save your tables for the next time you start the REPL.

We've seen how CamlDB helps with data management through insert/delete, access and visualization features. We hope it can make data more accessible and fun to work with.

* Example input:
  SELECT * FROM tb;
  SELECT col1,col2 FROM tb #BAR;
  SELECT TOP 10 col1 FROM tb;
  SELECT TOP 10 * FROM tb #LINE;
  SELECT TOP 50 PERCENT co1,col2 FROM tb;
  SELECT TOP 13 PERCENT col1 FROM tb #HISTOGRAM;
  SELECT TOP 20 PERCENT * FROM tb WHERE col1 = 2.0 OR col2 = 4;
  SELECT DISTINCT col1 FROM tb #SCATTER;
  SELECT DISTINCT col1 FROM tb WHERE col1 = 2 OR col2 = 4;
  SELECT col1,col2 FROM a
   WHERE col1 = 2 AND col2 < 3.1e+10 OR col3 > 1E-2 AND col4 = 5;
  SELECT col1,col2 FROM a
   WHERE col1 = 2 OR col2 < 3 AND col3 > 1 OR col4 = 5;
  SELECT col1,col2 FROM a
   WHERE col1 = 2 AND (col2 < 3 OR col3 > 1) AND col4 = 5;
  SELECT col1,col2 FROM a ORDER BY col2 ASC;
  SELECT col1,col2 FROM a ORDER BY col4 DESC;
  SELECT col1,col2 FROM a WHERE col1=10 ORDER BY col3 DESC;
  INSERT INTO tb VALUES (\"val1\",2,true,false);
  INSERT INTO tb (col1,col2,col3)
   VALUES (\"val1\",2,true,false);
  UPDATE tb SET col1=1, col2=2;
  UPDATE tb SET col1=1, col2=2 WHERE col3<10;
  DELETE FROM tb1;
  DELETE FROM tb1 WHERE col1=1 AND col2<4;
  CREATE TABLE tb1
   (col1_name INT,
    col2_name STRING,
    col3_name BOOL
    col4_name FLOAT
   );
  SELECT col1,col3 FROM tb1 UNION ALL SELECT * FROM tb2;
  SELECT tb1.col1, tb2.col2 FROM tb1
   JOIN tb2 ON tb1.col3=tb2.col2;