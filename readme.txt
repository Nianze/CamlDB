-- CamlDB --
Nianze Liu, Lydia Tang and Andrew Wang

-- Compiling and Running --
To compile the project, use ocamlbuild:

    ocamlbuild -use-menhir -pkg jsonm main.byte

To run, run main.byte:

    ocamlrun main.byte

-- Tutorial --
Let's use CamlDB to help with an experiment. We want to see how amount of sleep affects test scores. We'll ask people for their names, average hours slept and their score on the last exam. So run main.byte to start the REPL, and create a table:

    CREATE TABLE data (name STRING, hours FLOAT, score INT)
