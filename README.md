# CS 162 Regex repo

This repository contains the material from the livecoding sessions
during the class and the assignment templates provided to you. The
`master` branch (which is where you are reading this from) contains
the livecoding material. Each assignment has its own branch (`assign2`
for assignment 2, `assign3` for assignment 3, and so on). To switch to
(checkout in git terminology) a branch, you can run the following
command inside your clone of the repository:


partitions (which are character sets), and if two characters `a` and
`b` are in the same partition, the derivative of `r` with respect to `a`
and `b` result in the same regular expression. Formally, if `a, b ∈ S
∈ C(r)` then `δ_a(r) = δ_b(r)`.

So, if `a` and `b` are in the same partition then the state
corresponding to `r` should transition to the same state when reading
either `a` or `b` in the DFA you generate.

The definition of `C` for different cases of regular expressions is as
follows:

```
    C(∅) = { Σ }
    C(ε) = { Σ }
C(S ⊆ Σ) = { S, Σ \ S } // S is a character set
   C(r*) = C(r)
   C(!r) = C(r)
C(r | s) = C(r) ∧ C(s)
C(r & s) = C(r) ∧ C(s)
C(r ~ s) = C(r) if !r.nullable, else C(r) ∧ C(s)
```

where `∧` binary operator is pairwise union on sets of character sets.
It is defined as:

```
P ∧ Q = { S ∩ T | S ∈ P, T ∈ Q }
```

For example, {A, B} ∧ {C, D} = {A ∩ C, A ∩ D, B ∩ C, B ∩ D} where A,
B, C, D are character sets.

Based on this definition, you need to implement the `computeNext` method
which needs to compute `C`, use it to compute the transitions, and the next states by picking an element from each partition and deriving current regular expression w.r.t. the picked character.

## Part 3: Tests

You will need to write your own unit tests. Testing your own code *is
part of the grade*. We are looking for the following:
  - Non-trivial tests, i.e., a test that demonstrates that creating a
    charset creates a charset is not very useful.
  - Decent coverage - we don't expect you to go for a 100% test
    coverage, but we do expect you to cover edge cases.

Ultimately, the quality of the tests is as important as the quantity. You
need to test:
 - the normalization code given to you and how it interacts with the
   simplifications you implement. The tests for this should go into
   `RegexSpec.scala`.
 - the public API of `DerivativeAnalysis` that you will implement. The
   tests for this should test both the strucure and the behavior of the
   resulting DFA to and these tests should go into the skeleton code
   we gave you in `DerivativeAnalysisSpec.scala`

You can keep the tests you wrote for the previous assignment but you
will not be graded on those for this assignment. For grading purposes,
we will consider only the tests in `RegexSpec.scala` and
`DerivativeAnalysisSpec.scala`.

## Rules

You must use only the purely functional subset of Scala. This means
that you are not allowed to use mutations; more explicitly you must
not use any of:
  - Mutable variables, i.e., those created using the `var` keyword,
  - Mutable collections, e.g. anything under
    `scala.collection.mutable`,
  - The `Array` data type.

If you use any mutation, you will automatically fail the assignment.

Your code must compile. Invoking `compile` and `test:compile` in the
SBT shell (as described in the first tutorial) must
succeed. Otherwise, you will automatically fail the assignment.

## Submission

We will use only the contents of the `src` directory for grading so
make sure that all your code is in the proper directories under it.
You will use `turnin` on CSIL to submit your assignment.  To submit
your assignment, on the root directory of the repository you cloned,

  1. Make sure that you run the unit tests on CSIL and get the result
       you expect.
  2. run `turnin assign3@cs162 src`.
  3. Read the instructions on the screen and the list of files you are
       submitting carefully and submit the assignment only if you are
       sure that you are submitting all the files.


For example, `git checkout assign3` will switch to the branch
containing assignment 3's template and description.
