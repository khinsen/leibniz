# Implementation notes

## Code structure

The code consists of several layers that build on each other (top to
bottom):

 - `sorts.rkt` implements the sort-graph, which is a DAG of subsort
   relations.

 - `operators.rkt` mplements operators and term algebra signatures,
   which define which operators are valid in an algebra and what sort
   the resulting terms have as a function of argument sorts.

 - `terms.rkt` and `builtins.rkt` implement standard and built-in
   terms.

 - `term-syntax.rkt` is a syntax layer that simplifies writing complex
   terms inside Racket code.

 - `equations.rkt` implements equations and rules.

 - `contexts.rkt` implements contexts, which combine a signature for a
   term algebra and a list of rules for term simplification.
   
 - `rewrite.rkt` implements term rewriting.

 - `context-syntax.rkt` implements convenience syntax for doing computations
   inside a context.

 - `builtin-contexts.rkt` implements a few built-in contexts
   (truth, boolean, numbers).

Three modules are not part of this stack:

 - `condd.rkt` and `lightweight-class.rkt` provide generic utilities
   that could well be used in other projects.

 - `test-examples.rkt` contains data structures for testing, used in
   various places.

## Changes to consider

### Term creation

The API for term creation is far from definitive. Error handling in
particular needs to be defined properly.

### Merging

Merging signatures or varsets implies merging their sort graphs, which
ends up getting done twice when contexts are merged.

### Labels in rules and equations

Labels are not required to be unique, which may turn out to be a bad choice.
