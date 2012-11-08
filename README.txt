FLP Project I - Haskell
=======================

Felix Van der Jeugt, 3de bachelor of Science, Computer Science.

Extra files:
------------

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

Extra features:
---------------

- You can include comments in your while code. (Because my while
  programs were turning out a bit to cryptic.)
