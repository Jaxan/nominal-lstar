Learning Nominal Automata
=========================

*NOTE*: Please download the archive `popl-artifact.zip`. This contains the
specific versions (of this and nlambda) used for the POPL submission.
This archive should contain a similar README, with simpler instructions.
If you want the newest version of the software, then don't use that
archive, but use the code in this repository.


# Dependencies

This artifact was tested on a Debian system. During development both Mac and
Windows have been used, so it should work on these operating systems too. Note
that you will need the Z3 solver (as executable). The algorithms are
implemented in Haskell and you will need a recent GHC (at least 7.10).

We use the library [nlambda](https://github.com/szynwelski/nlambda). It
is recommended to use the most recent version. Just grab the source and
put it somewhere (we build it together with nominal-lstar).

You will need to install the [Z3](https://github.com/Z3Prover/z3) theorem
prover. The executable should be locatable through the PATH environment.
Follow the build guide on their website.


# Building

You can use the stack tool. Make sure to include nlambda as a package.
It should be a matter of `stack build`, if not, stack will probably
tell you what to do. (If you need any help, send me a message.)


# Running

Stack will produce a binary in the `.stack-works` directory, which can
be invoked directly. Alternatively one can run `stack exec nominal-lstar`.
There is two modes of operation: Running the examples, or running it
interactively.

## Examples

The executable expects three arguments:

```
stack exec NominalAngluin -- <Learner> <Oracle> <Example>
```

There are three learners:
- `NomLStar` is the nominal L* algorithm as described in the paper.
- `NomLStarCol` is the nominal L* algorithm where counter examples are added
  as columns (instead of rows). This is often a bit faster.
- `NomNLStar` learns nominal NFAs.

There are two oracles:
- `EqDFA` is an equivalence oracle which returns shortest counter examples by
  trying to prove two DFAs bisimilar. This method does *not* work for
  `NomNLStar`.
- `EqNFA n` is a bounded equivalence oracle for NFAs. Deciding equivalence
  between NFAs is undecidable, so one has to fix a bound `n` for termination.

There is an additional oracle which poses the queries to stdout, so that a
human can answer them. Since this oracle is a bit buggy (and not described
in the paper), it is not part of main.

There is a bunch of examples (also described in the paper, except for the
stack data structure):
- `Fifo n` is a FIFO queue of capacity `n`.
- `Stack n` is a Stack data structure of capacity `n`.
- `Running n` is the running example from the paper with parameter `n`.
- `NFA1` accepts the language uavaw, where u,v,w are any words and a any atom.
- `Bollig n` is the language where the `n`-last symbol equals the first. This
  can be encoded efficiently with an NFA. The corresponding DFA is exponential
  in `n`.

For example:
```
stack exec nominal-lstar -- NomLStar EqDFA "Fifo 2"
```

The program will output all the intermediate hypotheses. And will terminate
once the oracle cannot find any counter examples. Printing the automaton is
done with the NLambda library, it is not the most human-friendly output.

You can define your own automaton in Haskell by using NLambda. Then it can be
learnt, and the minimal automaton will be printed.

In our paper we ran the algorithm on the examples `Fifo`, `Running`, `Bollig`
and `NFA1` with the bounds as mentioned in the paper. The first two families
are given by DFAs and we used all three learners with the `EqDFA` teacher.
For the latter two we used the `EqNFA` teacher with a bound of at most 10.
We proved by hand that the learnt model did indeed accept the language.

## Interactively

Run the tool like so:
```
stack exec nominal-lstar -- <Leaner>
```
(So similar to the above case, but without specifying the equivalence
checker and example.) The tool will ask you membership queries and
equivalence queries through the terminal. The alphabet is fixed in
`Main.hs`, so change it if you need a different alphabet (it should
work generically for any alphabet).

Additionally, one can run the `nominal-lstar2` executable instead,
if provides an easier to parse protocol for membership queries. Hence
it is more suitable for automation. This will first ask for the alphabet
which should be either `ATOMS` or `FIFO`.

A run might look like the following. The lines with `Q:` are queries,
answered by myself on the lines with `A:` or `>`.
```
##################
1. Making it complete and consistent
2. Constructing hypothesis

# Membership Queries:
# Please answer each query with "True" or "False" ("^D" for quit)
Q: []
A: True
Q: [0]
A: True
Automaton {states = {{([],True)}}, alphabet = {a₁ : for a₁ ∊ 𝔸}, delta = {({([],True)},a₁,{([],True)}) : for a₁ ∊ 𝔸}, initialStates = {{([],True)}}, finalStates = {{([],True)}}}
3. Equivalent? 

# Is the following automaton correct?
# Automaton {states = {{([],True)}}, alphabet = {a₁ : for a₁ ∊ 𝔸}, delta = {({([],True)},a₁,{([],True)}) : for a₁ ∊ 𝔸}, initialStates = {{([],True)}}, finalStates = {{([],True)}}}
# "^D" for equivalent, "[...]" for a counter example (eg "[0,1,0]")
> [0,1]
Just {[a₁,a₂] : a₁ ≠ a₂ for a₁,a₂ ∊ 𝔸}
##################
1. Making it complete and consistent
2. Constructing hypothesis
Using ce: {[a₁,a₂] : a₁ ≠ a₂ for a₁,a₂ ∊ 𝔸}
add columns: {[a₁] : for a₁ ∊ 𝔸, [a₁,a₂] : a₁ ≠ a₂ for a₁,a₂ ∊ 𝔸}

# Membership Queries:
# Please answer each query with "True" or "False" ("^D" for quit)
Q: [0,0]
A: True
Q: [1,0]
A: False
Q: [1,0,1]
A: 
```

# Changes since first release

* Better support for interactive communication.
* Optimisation: add only one row/column to fix closedness/consistency
* Simpler observation table
* More efficient nominal NLStar
