SimSat
====
SimSat is an tool for synthesizing winning strategies to logical games.

The strategy synthesis algorithms are implemented in
[duet](https://github.com/zkincaid/duet).  This repository contains the ark
library of duet along with a command line interface to the strategy synthesis
algorithms.

Building
========

### Dependencies

Duet depends on several software packages.  The following dependencies need to be installed manually.

 + [opam](http://opam.ocaml.org) (with OCaml >= 4.02 & native compiler)
 + GMP and MPFR
 + [MathSAT](http://mathsat.fbk.eu) **version 5.3.14** (version 5.4 is incompatible)

On Ubuntu, you can install these packages (except Java and MathSAT) with:
```
 sudo apt-get install opam libgmp-dev libmpfr-dev
```

On MacOS, you can install these packages (except Java and MathSAT) with:
```
 brew install opam gmp mpfr
```

Next, add the [sv-opam](https://github.com/zkincaid/sv-opam) OPAM repository, and install the rest of SimSat's dependencies.  These are built from source, so grab a coffee &mdash; this may take a long time.
```
 opam remote add sv git://github.com/zkincaid/sv-opam.git
 opam install ocamlgraph batteries oasis ppx_deriving Z3 apron ounit menhir mathsat OCRS
```

### Building SimSat

After SimSat's dependencies are installed, it can be built using `make`.

Satisfiability
==============

SimSat is a satisfiability testing procedure for quantified formulas in linear
integer arithmetic and linear rational arithmetic.  It accepts input in
SMT-LIB2 format, and can be executed as follows:

    simsat.native -sat FILE.smt2

A quantified formula can be viewed as a game between two players -- SAT and
UNSAT -- whose goals are to prove that formula is satisfiable / unsatisfiable,
respectively.  The SimSat algorithm is based on synthesizing a winning
strategy for one of the two players by mutually improving strategies for both.
The procedure is described in
* Azadeh Farzan, Zachary Kincaid: [Linear Arithmetic Satisfiability via Strategy Improvement](http://www.cs.princeton.edu/~zkincaid/pub/ijcai16.pdf).  IJCAI 2016.


Synthesis (SimSynth)
====================

SimSat is also capable of synthesizing winning strategies to satisfiability
games as well as reachability games.  It accepts satisfiability in SMT-LIB2
format (with a mandatory .smt2 file extension), and reachability games in a
format that will be described below (with a mandatory file .rg extension).  To
synthesize a strategy, execute simsat as follows:

    simsat.native -sat FILE.[smt2|rg]

Syntax of reachability games:
-----------------------------
```vars: <var-list>     # comma-separated list of variables
init: <formula>      # formula describing initial positions of the game
safe: <formula>      # formula describing moves of the safety player
reach: <formula>     # formula describing moves of the reachability player
```

The init formula is defined over the variables that appear in `vars'.  The
safe and reach formulas are defined over the variables in `vars' plus primed
copies.  For example, if vars is x and y, then safe and reach are formulas
over the vocabulary {x,y,x',y'}, and safe (reach) may move from (a,b) to
(a',b') exactly when the the formula safe is satisfied by the assignment
        { x -> a, y -> b, x' -> a', y' -> b' }.

The syntax of formulas is as follows:
```<formula> ::= <formula> && <formula> | <formula> || <formula>
            | !(<formula>) | ( <formula> )
            | <term> <= <term> | <term> < <term>
            | <term> >= <term> | <term> > <term>
            | <term> = <term>
<term> ::= <int> | <var>
         | <term> + <term> | <term> - <term>
	 | <term> * <term> | <term> / <term>
	 | ( term )
```
