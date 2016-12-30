
![<Lᵉ>](https://github.com/khinsen/leibniz/raw/master/logo/horizontal-leibniz-logo-500-x-150-png.png)

# A digital scientific notation

Leibniz is an attempt to define a digital scientific notation, i.e. a
formal language for writing down scientific models in terms of
equations and algorithms. Such models can be published, cited, and
discussed, in addition to being manipulated by software.

Although Leibniz can express algorithms, it is **not** a programming
language. It is more similar to
a [specification language](https://en.wikipedia.org/wiki/Specification_language) in
that it allows to express what some program is supposed to compute.

Leibniz is named after
[Gottfried Wilhelm Leibniz](https://en.wikipedia.org/wiki/Gottfried_Wilhelm_Leibniz),
who made important contributions to science, mathematics, formal
logic, and computation, topics that are all relevant to this project.
He also invented a widely used [notation for calculus](https://en.wikipedia.org/wiki/Leibniz%27s_notation).

If you are interested in the development of digital scientific
notations, even if your ideas are very different from what I envisage
with Leibniz, please consider joining my
[Open Science project](https://www.guaana.com/projects/scientific-notations-for-the-digital-era)
at [Guaana](https://www.guaana.com/).

## Status

The support code for Leibniz is now sufficiently advanced that first
examples for digital scientific knowledge can be developed. They
are located (no surprise) under `examples`. For a first contact
with Leibniz, I suggest looking at `examples/quantities/mass.rkt`,
which shows how physical quantities and units can be defined. For a much
more elaborate example, see `examples/mechanics/solar-system.rkt`, which
defines gravitational interactions in a system of celestial bodies. In
`examples/quick-guide.md` you can find essential background information
for understanding the examples.

Readers interested in the implementation should start by looking at
the file `notes.md` for an overview of the code structure.

I will announce any significant progress on my
[Guaana project](https://www.guaana.com/projects/scientific-notations-for-the-digital-era).

## Background

The following articles are helpful to understand the context in which
this code is developed:

 - My essay
   [Scientific notations for the digital era](http://sjscience.org/article?id=527)
   explains the concept of digital scientific notations, in particular
   as opposed to scientific software.

 - Mark Buchanan wrote an excellent one-page summary of this essay for
   [Nature Physics](http://www.nature.com/nphys/index.html), under the
   title
   [Digital Science](http://www.nature.com/doifinder/10.1038/nphys3815).

 - I have written two short essays on related topics:
   [Scientific communication in the digital age](http://dx.doi.org/10.1063/PT.3.3181)
   and
   [Verifiable research: The missing link between replicability and reproducibility](http://dx.doi.org/10.15200/winn.146857.76572)

Leibniz is based on
[equational logic](https://en.wikipedia.org/wiki/Equational_logic) and
[term rewriting](https://en.wikipedia.org/wiki/Rewriting#Term_rewriting_systems).
This seems an appropriate choice for scientific models that are
traditionally written as mathematical equations. Algorithms are
expressed by giving a direction to certain equations, indicating that
the left-hand side is supposed to be replaced by the right-hand side
in simplifying an expression. Term rewriting has been used for a long
time in computer algebra, notably by
[Mathematica](https://www.wolfram.com/mathematica/).

Leibniz differs from Mathematica and most other computer algebra
systems in using an order-sorted term algebra, in which each term is
assigned a **sort**, which is similar to what is called a **type** in
programming languages. For a detailed discussion of order-sorted
algebra, see

 - [Order-sorted algebra I: Equational deduction for multiple inheritance, overloading, exceptions and partial operations](http://dx.doi.org/10.1016/0304-3975(92)90302-V) by J. A. Goguen and J. Meseguer

Term rewriting in order-sorted algebras has been implemented in the
specification languages
[OBJ](http://cseweb.ucsd.edu/~goguen/sys/obj.html) and its modern
offshoot [Maude](http://maude.cs.illinois.edu/). For readers familiar
with these languages, a Leibniz "context" is roughly the same as an "object"
in OBJ or a
"[functional module](http://maude.cs.uiuc.edu/maude2-manual/html/maude-manualch4.html)"
in Maude. Reading the Maude documentation is currently the best
preparation for understanding Leibniz.

However, Leibniz is much simpler than Maude, lacking both Maude's
flexible syntax and its support for non-functional modules.  This is
due to a very different focus: Maude is a language for writing
specifications for complex software, whereas Leibniz is a notation for
scientific models. Scientific models are much simpler than most
software, but they can be processed by a wide range of
software. Leibniz must therefore be easy to implement in a wide range
of software packages, whereas reimplementing Maude is of little
interest, given that its source code is open.

## Required software

This first implementation of Leibniz is written in
[Racket](http://racket-lang.org/), whose support for implementing
languages and language extensions is particularly useful for this
project. In addition to Racket itself, Leibniz depends on the
following libraries:

 - [threading](https://github.com/lexi-lambda/threading)
 - [sxml](http://github.com/jbclements/sxml/tree/master)
 - [chk](https://github.com/jeapostrophe/chk)

The examples also use

 - [sweet-exp](http://github.com/takikawa/sweet-racket)

for better readability.

To install Leibniz and its dependencies, type:
```bash
   raco pkg install git://github.com/khinsen/leibniz\?path=leibniz
```

To run the Leibniz test suite, type
```bash
   raco test -c leibniz
```
You can run an individual library module's tests by typing
```bash
   raco test -l leibniz/sorts
```
etc.

## License

I expect to properly document and release this code at some time,
under a meaningful license. But for now, it is research code covered
by the [CRAPL](http://matt.might.net/articles/crapl/) license.

## Branch notes

Most branches of this repository contain experiments that test the
utility and feasibility of ideas for improvements and new
features. Each branch has a short note in this place that explains its
reason for being. This branch (master) always contains the version
currently considered most useful.

Note that all branches except master may be rebased, or modified in
other ways. If you want to fork this repository, please don't rely on
any branch other than master.
