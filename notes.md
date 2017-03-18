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

 - `rewrite-syntax.rkt` implements convenience syntax for doing computations
   inside a context.

 - `builtin-contexts.rkt` implements a few built-in contexts
   (truth, boolean, numbers).

 - `documents.rkt` implements the interface between the low-level support
   code and the Scribble-based language that Leibniz authors use.
   
 - `lang.rkt` and everything under `lang` implement the Leibniz language,
   which is an extension of `scribble/base` that adds commands for
   defining sorts, operators, rules etc.
 
Three modules are not part of this stack:

 - `condd.rkt` and `lightweight-class.rkt` provide generic utilities
   that could well be used in other projects.

 - `test-examples.rkt` contains data structures for testing, used in
   various places.

## Changes to consider

### Term creation

The API for term creation is far from definitive. Error handling in
particular needs to be defined properly.

### Labels in rules and equations

Labels are not required to be unique, which may turn out to be a bad choice.

### The context data structure

There are two internal data structures for contexts at this time: the low-level
one is defined in `contexts.rkt`, and is used in validation and rewriting.
The higher-level one is defined in `documents.rkt`. It is a parsed version
of what Leibniz authors write, and it is also what the XML representation encodes.
t isn't clear to me yet what the best data structure is for implementing code
transformations, which will be an important part of the Leibniz infrastructure.
These data structures are therefore likely to change over time, and perhaps just
one will survive.
