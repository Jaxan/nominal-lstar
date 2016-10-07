Learning Nominal Automata
=========================

*NOTE*: Please download the archive `popl-artifact.zip`. This contains the
same source code, but is bundled with the NLambda library (the specific version
used for the paper). The remainder of this README assumes you are using that
archive.

We have bundled the implementation of the learning algorithm and the
implementation of the NLambda library in this artifact. Note that our
version of NLambda is slightly different from the one on the [NLambda
website](http://www.mimuw.edu.pl/~szynwelski/nlambda/). Some bugs were
fixed in our version and possibly some new features have appeared.

This artifact was tested on a Debian system. During development both Mac and
Windows have been used, so it should work on these operating systems too. Note
that you will need the Z3 solver (as executable). The algorithms are
implemented in Haskell and you will need a recent GHC (at least 7.10).


# Building

Should be just as easy as `stack build`, assuming one has installed Haskell
stack. I noticed that the linker needed libtinfo. So you might need to install
the libtinfo package, for example through apt. (I do not know which haskell
package depends on this.) Building may take a while.

Stack for haskell can be installed as described on
[their website](http://haskellstack.org/).

You will need to install the [Z3](https://github.com/Z3Prover/z3) theorem
prover. The executable should be locatable through the PATH environment.
Follow the build guide on their website.


# Running

Stack will produce a binary in the `.stack-works` directory, which can
be invoked directly. Alternatively one can run `stack exec NominalAngluin`.
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
stack exec NominalAngluin -- NomLStar EqDFA "Fifo 2"
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

