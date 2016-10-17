# A quick guide to the Leibniz examples

First of all, these are first examples, written with the main intention to explore the possibilities of Leibniz as it evolves. Expect change.

Second, the examples are presented as modules for the [Racket](http://racket-lang.org/) language, so it may seem as if Leibniz is a [domain-specific language (DSL)](https://en.wikipedia.org/wiki/Domain-specific_language) embedded into Racket. That's indeed what the current implementation is, because Racket is an excellent environment for playing with DSLs. But ultimately, Leibniz contexts (the main information items) are data structures, and even relatively simple ones. They can be stored in files or databases, using standard formats such as XML or JSON, and processed in any programming language whatsoever. They can also be embedded into scientific articles, deposited on [Zenodo](http://zenodo.org/) and cited through a DOI, integrated into [nanopublications](http://nanopub.org/), and used in many other ways - just like other data.

## Syntax

The Racket-based syntax you see in the example files is a very superficial aspect of Leibniz. What you should look at is the semantics, i.e. *what* kind of information you can express using Leibniz. In the long run, Leibniz will have a syntax resembling standard mathematical notation, and authoring tools inspired by [literate programming](https://en.wikipedia.org/wiki/Literate_programming).

That said, syntax is what hits the eye, so you must at least understand its principles to read the examples. Racket being a language of the Lisp family, it processes code in the form of [s-expressions](http://en.wikipedia.org/wiki/S-expression). You can think of s-expressions as a much less verbose version of XML. Both formats represent data in a single but very flexible data structure: a [tree](http://en.wikipedia.org/wiki/Tree_(data_structure)). To see an example of Leibniz written in plan s-expressions, look at `quantities/mass-plain-syntax.rkt`.

S-expressions seem weird at first contact, and most people rather quickly come to either love or hate them. But even s-expression-lovers tend to admit that they are difficult to read for some use cases, in particular mathematical formula. Since Leibniz uses lots of these, I have chosen a variant called [sweet-expressions](http://readable.sourceforge.net/) for the examples. Take a look at `quantities/mass.rkt`, and you will probably agree that it's more pleasant to read.

Sweet-expressions add syntactic variants to plain s-expressions, which remain valid as well. The s-expression `(a b c)` can alternatively be written as `a(b c)`, resembling the syntax of mathematical functions, or as `{b a c}`, resembling the syntax of mathematical operators. The latter makes sense mainly for, well, mathematical operators, yielding e.g. `{x + y}` for the s-expression `(+ x y)`. In contrast to traditional mathematics (and most programming languages), there are no precedence rules that would give meaning to something like `a + b*c`. You have to write either `{a + {b * c}}` or `{{a + b} * c}` and thus make the order of operations explicit. Sweet-expressions also make use of indentation for structuring longer s-expressions, much like the popular [Python](http://python.org/) language.

## Terms, sorts, rules, and equations

At the heart, Leibniz is a [term-rewriting system](http://en.wikipedia.org/wiki/Rewriting#Term_rewriting_systems). All data is represented as terms. A term is anything of the form `operator(argument1 argument2 ...)` (or, using different sweet-expressions, `(operator argument1 argument2 ...)` or `{argument1 operator argument2}`). A list of valid operators with a specification of how many and which kind of arguments they take is what defines a [term algebra](http://en.wikipedia.org/wiki/Term_algebra). In Leibniz, each context defines a term algebra via the lines starting with `op`.

The term algebras used in Leibniz have the particularity of being *sorted* term algebras. Each term has a label attached to it, called its sort. Each operator declaration states how many arguments the operator takes, but also of which sort the arguments must be and what the sort of the complete term will be. In Leibniz, different sorts represent different types of information. There are a few built-in contexts that define basic sorts such as `Boolean` (`true` or `false`) or number sorts (`Natural`, `Integer`, `Rational`, `Real`). Most sorts are defined specifically for a scientific application domain.

Sorts need not be completely independent from each other. A sort can be declared a subsort of another one. Think of subsorts as resembling subsets in mathematics. For example, `PositiveReal` is a subsort of `Real`, reflecting the fact that the positive real numbers are a subset of the real numbers.

Rules are used to implement algorithms in Leibniz. A rule consists of a *pattern* i.e. a term that potentially contains *variables* that can stand for arbitrarily complex sub-terms, and a *replacement* term. During rewriting, whenever a term matches a pattern, it is replaced by the replacement term, into which the variables values determined during matching are inserted.

A rule conveys two important messages: it states that the left and right hand sides are equal, and it states that the right-hand side is in some sense more useful or preferred. In many cases, the right-hand side is simpler than the left-hand side, and the rule serves to simplify complex terms.

One way to look at rules is as equations with a direction attached to them. Leibniz also provides undirected equations, which unlike rules are not used in rewriting, but merely state the equality of two patterns, much like an equation in traditional mathematical writing.

## Leibniz vs. computer algebra systems

Anyone familiar with computer algebra systems has probably noticed many similarities with Leibniz. In fact, computer algebra systems are also based on term rewriting systems, and used to manipulate terms and equations. So why invent something new but apparently similar?

The main difference between Leibniz and computer algebra systems is that Leibniz is a scientific notation whereas computer algebra systems are tools for doing computations. A notation needs to be clear, a tool needs to be flexible and efficient. Different priorities lead to different design decisions. In a computer algebra system, you never see a definition of a term algebra or a list of rewrite rules. They are hidden from the user, and in the case of commercial software even heavily encoded to make them inaccessible. A computer algebra system is the equivalent of a single giant unpublished Leibniz context. In contrast, Leibniz encourages small, readable, and remixable contexts that can be examined and understood by scientists with reasonable effort. However, Leibniz lacks the huge database of simplification rules that make up the bulk of a computer algebra system.

Perhaps one day computer algebra systems will read problem specifications in the form of Leibniz contexts, and return their results *plus the reasoning leading to the results* in Leibniz as well. But for now, that is just wishful thinking.

