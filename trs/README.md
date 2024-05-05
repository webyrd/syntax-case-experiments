This directory contains multiple steps in the writing of a
term-rewriting macro that expands to a procedure that defines a
miniKanren relation, written by Arunava Gantait, William E. Byrd, and
Michael Ballantyne.

Arunava proposed this macro, and the original specification, in the
context of a project to implement the Knuth-Bendix completion
algorithm in miniKanren.  This project is supervised by Professor
Madhavan Mukund at Chennai Mathematical Institute, along with Will
Byrd.  Brandon Willard proposed implementing Knuth-Bendix in
miniKanren.  Arunava's work has been greatly influenced by this
[implementation of Knuth-Bendix in
Prolog](https://www.metalevel.at/trs/) by Markus Triska:

Arunava's original specification is that the syntax:

```
(trs reduces (* + exp log)
  ((+ x x) -> (* 2 x))
  ((exp (log x)) -> x))
```

should expand to a definition that is equivalent to:

```
(define reduces
  (lambda (a b)
    (conde
      ((fresh (x)
         (== a `(+ ,x ,x))
         (== b `(* 2 ,x))))
      ((fresh (x)
         (== a `(exp (log ,x)))
         (== b x))))))
```

Arunava and Will Byrd individually attempted versions of this macro in
`syntax-rules`.  Will proposed a simplified syntax that made it easier
to implement the macro in `syntax-rules`, but which makes the
assumption that operator names can only apply in head position, and
which requires the user to explicitly specify a list of pattern
variables for each clause.

Will then tried a `syntax-case` version of the macro, but quickly got
stuck.

The file `trs1.scm` contains Will's simplified `syntax-rules` macro.

Will asked Michael Ballantyne for help, which resulted in a
pair-programming session in which Michael broke down the original
macro to the simpler, core problem, resulting in increasingly
sophisticated macros and syntax in `trs2.scm` and `trs3.scm`.

At the point Michael said the rest of the macro should be
straight-forward.  Will implemented the remainder of the original
macro syntax in `trs4.scm` and `trs5.scm`.

`trs5.scm` implements the entire original syntax proposed by Arunava.

Michael explained to Will a few subtleties of writing macros, to work
around unspecified behavior in the R6RS Scheme Report, and work around
incompatibilities between Chez Scheme and Racket.  You will find a few
notes to this effect in the code.  Will tested all code in Chez Scheme
10.0.0.  Michael and Will also used Racket when developing the code in
`trs2.scm` and `trs3.scm`.

`trs6.scm` cleans up the macro in `trs5.scm` slightly, to make the
code easier to read, based on suggestions from Michael Ballantyne.