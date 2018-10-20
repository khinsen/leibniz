
![<Lᵉ>](https://github.com/khinsen/leibniz/raw/master/logo/horizontal-leibniz-logo-500-x-150-png.png)

# A digital scientific notation

Leibniz is an attempt to define a digital scientific notation, i.e. a
formal language for writing down scientific models in terms of
equations and algorithms. Such models can be published, cited, and
discussed, in addition to being manipulated by software.

The best way to get an impression of what Leibniz is and what
you can do with it is to read the one-page introduction
["Leibniz by example"](http://khinsen.net/leibniz-examples/examples/leibniz-by-example.html).
Then you can move on to the [other examples](http://khinsen.net/leibniz-examples/)
and to [the manual](http://khinsen.net/leibniz/). Readers interested
in the studying the implementation (which needs and will get
a serious cleanup) should start by looking at the file `notes.md` for
an overview of the code structure.

Leibniz is named after
[Gottfried Wilhelm Leibniz](https://en.wikipedia.org/wiki/Gottfried_Wilhelm_Leibniz),
who made important contributions to science, mathematics, formal
logic, and computation, topics that are all relevant to this project.
He invented a widely used [notation for calculus](https://en.wikipedia.org/wiki/Leibniz%27s_notation),
laid the foundation of equational logic by his [definition of equality](https://en.wikipedia.org/wiki/Equality_(mathematics)),
and anticipated formal logic with his ["calculus ratiocinator"](https://en.wikipedia.org/wiki/Calculus_ratiocinator).


## Status

In a word: experimental. The major milestone that the implementation has
reached is to play its role of a digital scientific notation: Leibniz specifications
are embedded into the plain text discourse written for human readers, just
like traditional semi-formal mathematical notation. This matters because
it eliminates an important source of mistakes: the translation from human-readable
and peer-reviewed descriptions of models and methods into computer-readable code.

However, many features that I have planned for the language are still missing: built-in
collection types (lists/arrays, sets, ...) interfaces to databases and external datasets,
support for workflows. Although in principle today's Leibniz can be used for everything
(given that it's Turing-complete), it is still insufficient to express many
aspects of computational science in a sufficiently concise and convenient form.

## Required software, installation

This first implementation of Leibniz is written in
[Racket](http://racket-lang.org/), whose support for implementing
languages and language extensions is particularly useful for this
project. In addition to Racket itself, Leibniz depends on the
following libraries:

 - [megaparsack](https://github.com/lexi-lambda/megaparsack)
 - [threading](https://github.com/lexi-lambda/threading)
 - [sxml](https://github.com/jbclements/sxml/tree/master)
 - [chk](https://github.com/jeapostrophe/chk)

To install Leibniz and its dependencies, first install the Racket system
on your computer, and then type, in a terminal window:
```bash
   raco pkg install git://github.com/khinsen/leibniz\?path=leibniz
```

To run the Leibniz test suite, type
```bash
   raco test -c leibniz
```

You can then use Leibniz in two ways:

 - In Racket's IDE, called DrRacket. Any file starting with
     ```
     #lang leibniz
     ```
   is treated as a Leibniz document. Clicking on the Leibniz button
   creates a human-readable HTML version and a machine-readable XML
   version of the document, and opens the HTML file immediately in
   a browser for inspection.
   
 - Write your Leibniz documents using any text editor, and generate
   the HTML/XML files using the `leibniz` command line utility. It
   is part of installation process, but the location where it ends up
   is very platform-dependent. The good news is that the precise location
   is indicated near the end of the installation process, so have a
   careful look at the log output of `raco pkg install ...`.

For more information, see the [Leibniz manual](http://khinsen.net/leibniz/):

  - In DrRacket, go to the "Help" menu and select "Racket
    Documentation". This will open the table of contents of the Racket
    documentation in a browser. Search for "Leibniz" and click the link.
    
  - From a terminal command line, run "raco docs leibniz"

## License

I expect to properly document and release this code at some time,
under a meaningful license. But for now, it is research code covered
by the [CRAPL](http://matt.might.net/articles/crapl/) license.

## Background

The following articles are helpful to understand the context in which
Leibniz is developed:

 - My essay
   [Scientific notations for the digital era](http://sjscience.org/article?id=527)
   explains the concept of digital scientific notations, in particular
   as opposed to scientific software.

 - Mark Buchanan wrote an excellent one-page summary of this essay for
   [Nature Physics](http://www.nature.com/nphys/index.html), under the
   title
   [Digital Science](http://www.nature.com/doifinder/10.1038/nphys3815).

 - My article [Verifiability in computer-aided research: the role of digital scientific notations at the human-computer interface](https://peerj.com/articles/cs-158/) reports on the research that has lead to the development of Leibniz.

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

## Branch notes

Most branches of this repository contain experiments that test the
utility and feasibility of ideas for improvements and new
features. Each branch has a short note in this place that explains its
reason for being.

Note that all branches except master may be rebased, or modified in
other ways. If you want to fork this repository, please don't rely on
any branch other than master.

This branch replaces Scribble by Pollen as the underlying document
processing platform. The advantage of using Pollen is that all processing
can be done at the xexpr level. This means in particular that the
messy macro system for defining contexts can be replaced by much
simpler plain functions. Another advantage is that Pollen allows precise
control over the HTML output, which makes it possible to embed the XML
representation as a script in the HTML file.
