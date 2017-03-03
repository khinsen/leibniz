# A quick guide to the Leibniz library

Leibniz is a scientific notation, to be used in scientific documents much like traditional mathematical notation. The main difference to mathematical notation is that Leibniz is a formal language with well-defined semantics.

Authors compose documents using a markup language that is similar in spirit to LaTeX. More precisely, the Leibniz markup language is an extension to [Scribble](https://docs.racket-lang.org/scribble/), the documentation language of the [Racket](http://racket-lang.org/) programming language ecosystem. You can use all Scribble markup in addition to the Leibniz-specific markup that is used to express the computationally relevant aspects.

The files in this directory are Racket modules written in the Leibniz language. The command line tool `scribble` produces human-readable HTML files from them. In the HTML version, Leibniz input is highlighted by a light blue background, whereas Leibniz output is shown with a light green background. All the blue-background information can also be exported as an XML file for use by other software, but there is no convenient interface yet for this operation.

In the long run, the XML files will be the standard exchange format for Leibniz data. Everything else can and should evolve. In particular, I hope there will be much more convenient authoring tools than my current prototype. Software wishing to read Leibniz data will not have to worry about anything else than XML.

Leibniz has a simple two-level structure. The top level is the *document*, which can contain any number of *contexts* which are identified by a name. Scribble modules, their HTML representations, and Leibniz XML files all contain one document with all its contexts. Other documents can be imported, as shown in these examples. The import mechanism is very basic for now, but should later allow to import published documents through a DOI or some other permanent published reference.

## Syntax

The syntax for Leibniz terms, rules, and equations should be considered experimental. It aims at a compromise between the readability, similarity to mathematical notation, simplicity, and generality. The current syntax makes heavy use of Unicode symbols. Experience will show how well this works out in practice. Editing Leibniz input requires an editor with good Unicode support.

## Terms, sorts, rules, and equations

At the heart, Leibniz is a [term-rewriting system](http://en.wikipedia.org/wiki/Rewriting#Term_rewriting_systems). All data is represented as terms. A term is anything of the form `operator(argument1 argument2 ...)`, with each argument being another term. In order to represent mathematical formulae in a somewhat familiar way, Leibniz also supports two-argument infix operators of the form `argument1 operator argument2`, plus three special-syntax operators: subscript, superscript, and square-bracket postfix.

However, Leibniz differs in one important way from mathematical notation: there are no precedence rules for operators. The right-hand argument of an infix operator is everything to its right. For example, `2 × 3 + 4` means `2 × (3 + 4)`, whereas in traditional mathematical notation, multiplication has higher precedence than addition, leading to the interpretation `(2 × 3) + 4`. Precedence rules work well when there are few infix operators that are used very frequently, such that the rules are well assimilated by writers and readers. Computation requires more precision than mathematics and therefore a larger number of specialized operators, making precedence rules much less convenient.

A list of valid operators with a specification of how many and which kind of arguments they take is what defines a [term algebra](http://en.wikipedia.org/wiki/Term_algebra). In Leibniz, each context defines a term algebra.

The term algebras used in Leibniz have the particularity of being *sorted* term algebras. Each term has a label attached to it, called its sort. Each operator declaration states how many arguments the operator takes, but also of which sort the arguments must be and what the sort of the complete term will be. In Leibniz, different sorts represent different types of information. There are a few built-in contexts that define basic sorts such as `boolean` (`true` or `false`) or number sorts (`ℕ` for the natural numbers, `ℤ` for the integers, `ℚ` for the rational numbers, and `ℝ` for the real numbers). Most sorts are defined specifically for a scientific application domain. In the HTML output, sorts are written in italics.

Sorts need not be completely independent from each other. A sort can be declared a subsort of another one. Think of subsorts as resembling subsets in mathematics. For example, the sort of positive reals (`ℝp`) is a subsort of `ℝ`, reflecting the fact that the positive real numbers are a subset of the real numbers.

Rules are used to implement algorithms in Leibniz. A rule consists of a *pattern* i.e. a term that potentially contains *variables* that can stand for arbitrarily complex sub-terms, and a *replacement* term. During rewriting, whenever a term matches a pattern, it is replaced by the replacement term, into which the variables values determined during matching are inserted.

A rule conveys two important messages: it states that the left and right hand sides are equal, and it states that the right-hand side is in some sense more useful or preferred. In many cases, the right-hand side is simpler than the left-hand side, and the rule serves to simplify complex terms.

One way to look at rules is as equations with a direction attached to them. Leibniz also provides undirected equations, which unlike rules are not used in rewriting, but merely state the equality of two patterns, much like an equation in traditional mathematical writing.

## Leibniz vs. computer algebra systems

Anyone familiar with computer algebra systems has probably noticed many similarities with Leibniz. In fact, computer algebra systems are also based on term rewriting systems, and used to manipulate terms and equations. So why invent something new but apparently similar?

The main difference between Leibniz and computer algebra systems is that Leibniz is a scientific notation whereas computer algebra systems are tools for doing computations. A notation needs to be clear, a tool needs to be flexible and efficient. Different priorities lead to different design decisions. In a computer algebra system, you never see a definition of a term algebra or a list of rewrite rules. They are hidden from the user, and in the case of commercial software even heavily encoded to make them inaccessible. A computer algebra system is the equivalent of a single giant unpublished Leibniz context. In contrast, Leibniz encourages small, readable, and remixable contexts that can be examined and understood by scientists with reasonable effort. However, Leibniz lacks the huge database of simplification rules that make up the bulk of a computer algebra system.

Perhaps one day computer algebra systems will read problem specifications in the form of Leibniz contexts, and return their results *plus the reasoning leading to the results* in Leibniz as well. But for now, that is just wishful thinking.
