============================
Assignment 1b: Monads + DPLL
============================

This assignment is an amalgamation of learning how to use monads + implementing
an SAT solver with the DPLL algorithm.

Getting started
===============

This assignment runs using stack and runs much like the last assignment.
Just clone this repository ``git clone ...``
and perform the usual commands, e.g.

.. code:: sh

    $ stack test

Assignment: Monads
==================

This part of the assignment is similar to assignment 1a: implement stubs
of functions, sometimes under constraints. This is purely intended for you
to understand how monads work as well as how to use them with both bind and do-notation.

The files for this part are found in the ``src/monads`` folder. We recommend going
through them in the following order:

1. ``src/monads/Maybe.hs``
2. ``src/monads/List.hs``
3. ``src/monads/Reader.hs``
4. ``src/monads/Writer.hs``
5. ``src/monads/State.hs``

Assignment: DPLL
================

The second part of the assignment also requires you to implement stubs. This
time though, we give a skeleton that strings your stubs together. In the end,
you will have written an SAT solver implementing the DPLL algorithm! What
a function is expected to do is again annotated above it. You will go through
the following steps to implement the SAT solver:

1. Implement CNF conversion in ``src/sat/Prop.hs``
2. Implement Tseitins Transformation in ``src/sat/Tseitin.hs``
3. Implement the DPLL algorithm ``src/sat/DPLL.hs``

Remember that you may individually run the functions via ``stack ghci``. Specifically,
there is a ``parse`` function available that allows you to easily specify propositional
formulas in a string, e.g.

.. code:: haskell

    ghci> cnf $ parse "x & -x | z"

Which will output your implementation of the cnf transformation.

The ``src/sat/CNF.hs`` file contains the rigid CNF datastructure, which is a
propositional type that can only ever be in CNF. Do make sure to read and
understand this structure, as it is the output of the Tseitin Transformation
and the main computational structure of the DPLL procedure.

Running the SAT solver
----------------------
If you have a (semi) working product. You can run the following to run the SAT solver:

.. code:: sh

    $ stack run

Do provide it input!

Grading
=======

We have yet to decide on the exact grading scheme, we can say that the PDLL part
of this assignment will weigh significantly more towards the final grade.
