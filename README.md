# Compiler

This project contains 4 parts of a compiler:

1. An Extended Reader for Scheme:
I created an ocaml procedure nt_sexpr, that implements a reader for the extended language of S-expressions, and answers the parser interface given within the parsing combinator package.

2. A tag-parser for Scheme:
I created the procedure tag_parse_expression that takes an sexpr as an argument, and returns a tag-parsed sexpr of type expr. 
Parsing in this sense means annotating the sexpr with tags, so that the type of expression can be known easily and the various sub-expressions can be accessed with confidence (that is, without having to check each time to make sure that they syntactically correct and legal), and of course, parsing the sub-expressions as well.
The tag parser include the macro-expansion code according to the specifications.

3. The semantic-analysis module:
This component shall compute and annotate the lexical addresses of all variables, annotate applications in tail-position, and box variables that are copied from the stack to the heap,
and that changes to which may be visible elsewhere in the code.

4. Code generation:
Create a table of constant variables, create a table of free variables and generate the assembly string representing a single expr ', using the constants table and the free variables table.

In the end, I combined the four parts of the compilation pipeline.
