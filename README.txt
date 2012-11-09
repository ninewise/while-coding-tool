FLP Project I - Haskell
=======================

Felix Van der Jeugt, 3de bachelor of Science, Computer Science.

Extra features:
---------------

- You can include comments in your while code. (Because my while
  programs were turning out a bit to cryptic.) Comments are included
  between /* */.

- You can include (non-recursive) functions in your while code. The
  syntax is:

    function <name> (<varname>[,<varname>]) -> <rvarname> {
        <statement>
    }

  Where <name> is the name of the function, <varname>'s are the names
  of the variables, <rvarname> is the name of the return variable and
  statement is a regular statement.

  The statement inside should assign a value to the return variable,
  which will be the value of the function when it's called like so:

    <name>(<varvalue>[,<varvalue])

  as part of an expression.

  An example program with some functions can be found in
  `functions.while`.

- You can include recursive functions in your while code, with the same
  syntax. An example can be found in `fibonacci.while`. This was
  implemented with Haskell's laziness.

Extra files:
------------

- functions.while & fibonacci.while

    Files containing example programs.

- AbstractSyntaxTree.hs

    Containing the AbstractSyntaxTree module and datatype. This
    datatype represents a complete program. This module also contains
    some functionality, being fold definitions and some class
    instances.

- AnnotatedSyntaxTree.hs

    Containing an Annotated variant of the AbstractSyntaxTree, and
    functions to convert an AST to an Annotated AST.

- VariableMap.hs

    Contains an interface to the Data.Map datastructure. Used to map
    my variables onto their values.

- Evaluator.hs

    Containing the logic to calculate expressions and check conditions
    in the context of a Variable Map.

- PrettyPrinter.hs

    Containing the logic to convert an annotated AST to a nice output,
    as described in the assignment.

- ParserM.hs

    The ParserM module, as used in the lessons.

- WhileParser.hs

    A module containing the logic to parse a While program file into a
    AST as defined in my AbstractSyntaxTree module.

