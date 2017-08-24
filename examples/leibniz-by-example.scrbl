#lang leibniz

@title{Leibniz by example}
@author{Konrad Hinsen}

@import["functions" "functions.xml"]

@context["predator-prey" #:use "functions/derivatives-ℝ→ℝ"]{

Let's start right away with an example, the explanations will follow in the
next section.

@section{Example: the predator-prey equations}

The predator-prey equations, also known as the Lotka-Volterra equations, describe the dynamics of two interacting species in an ecosystem in terms of non-linear differential equations.

The two interacting time-dependent observables are the number of prey, @op{prey : ℝ→ℝ}, and the number of predators, @op{predators : ℝ→ℝ}. Although the number of individuals of a species is really an integer, it is taken to be a real number for the benefit of using differential equations. The two coupled equations for @term{prey} and @term{predators}
are
@inset{
  @equation[pp1]{𝒟(prey) = (prey-growth-rate × prey) - (predation-rate × predators × prey)}
  @equation[pp2]{𝒟(predators) = (predator-growth-rate × predators × prey) - (predator-loss-rate × predators)}}

These equations are based on a few assumptions:
@itemlist[
  @item{In the absence of predators, the prey exihibits exponential growth described by @op{prey-growth-rate : ℝp}.}
  @item{The number of prey decreases by predation, which is @op{predation-rate : ℝp} times the number of encounters between individuals of each species. The latter is taken to be proportional to both @term{prey} and @term{predators}.}
  @item{In the absence of prey, the number of predators decreases by starvation, described by @op{predator-loss-rate : ℝp}.}
  @item{The number of predators grows with the availability of food, which, like predation, is proportional to both @term{prey} and @term{predators} with the proportionality constant @op{predator-growth-rate : ℝp}.}]

}

@context["predator-prey-explanation" #:extend "predator-prey"]{
                                                
@section{A guide to reading this example}

@itemlist[
  @item{Everything typeset on a light blue background is Leibniz code. Everything else is plain text.}
  @item{The boldface parts (@bold{pp1}, @bold{pp2}) are equation labels that can be used to refer to a specific equation.}
  @item{@sort{ℝ} stands for the real numbers, @sort{ℝp} for the positive real numbers, and @sort{ℝ→ℝ} for real functions
        of one real variable. If @op{f : ℝ→ℝ} is such a function, then @term{𝒟(f)} is the derivative of @term{f}. All these
        definitions come from the imported context "functions/ℝ→ℝ" whose definition you can see
        @hyperlink["http://khinsen.net/leibniz-examples/examples/functions.html"]{here}.
        Yes, that link should be in the example itself, and it will be.}]
}

@section{Why Leibniz?}

Compare the example in the first section with the beginning of the
@hyperlink["https://en.wikipedia.org/wiki/Lotka%E2%80%93Volterra_equations"]{Wikipedia entry}
on the same topic, which uses traditional mathematical notation. If Wikipedia adopted Leibniz, what would it gain?

@itemlist[#:style 'ordered
   @item{A machine-readable version of the predator-prey equations, generated from the same input and therefore identical in content.
         You can look at it @hyperlink["http://khinsen.net/leibniz-examples/examples/leibniz-by-example.xml"]{here}. It's an XML file,
         which your browser may not display nicely, but you can always download it and open it in a text editor.
         A Leibniz-aware solver for differential equations could read this file, prompt you for parameter values and initial values,
         and compute and plot a solution. And if you had two Leibniz-aware solvers, you could compare their output, knowing for sure
         that they work on the same equations. Better yet, they work on the same equations that you have read and understood
         while reading the explanation. No more mistakes in transcribing equations to code!}
   @item{A more precise notation. For example, in the Wikipedia text, it is not immediately clear that @italic{x} and @italic{y} are
         functions of time, whereas @italic{α}, @italic{β}, @italic{γ}, @italic{δ} are constants. In Leibniz, you have to say
         what each object is, and you get an error message if you try to take the derivative of something that is not a function.
         More generally, everything typeset on a blue background has been checked for consistency and completeness.}]

@section{What else can you do with Leibniz?}

This simple example doesn't illustrate all the features of Leibniz. Moreover, Leibniz is far from complete at this time.
Here are some things that you can do, or will soon be able to do, using Leibniz:

@itemlist[
   @item{Express algorithms. Many computational models and methods in science combine equations with algorithms.
         We write the equations in papers using mathematical notation, and the algorithms in software source code
         using programming languages. Leibniz can express both, and render both in a human-readable and in a
         machine-readable form. See @hyperlink["http://khinsen.net/leibniz-examples/examples/euclid_gcd.html"]{here}
         and @hyperlink["http://khinsen.net/leibniz-examples/examples/heron.html"]{here} for simple examples of
         algorithms in Leibniz.}
   @item{Transform equations and algorithms. Start from a general model and then specialize it. Introduce approximations.
         Switch from exact arithmetic to floating-point arithmetic (as in
         @hyperlink["http://khinsen.net/leibniz-examples/examples/heron.html"]{this example}). Most importantly,
         do all this in a human-readable document that your peers can verify, rather than in hard-to-read
         software source code.}
   @item{Document all levels of a computational research project, from basic concepts to data anlysis workflows,
         using a single language, at the level of detail that you consider appropriate. You can just write an equation
         and stop there, as in the above example. But you can also use Leibniz to write an algorithm for solving differential
         equations. The general idea is that you document your research in Leibniz, but delegate what you consider technical
         details to Leibniz-aware software tools.}
   @item{Work with complex models using computational tools. Suppose your scientific model is an equation with 2000 terms.
         Writing it down on paper is pointless. Writing it down as a Fortran subroutine is more useful, but you cannot
         do anything else with that subroutine than compute specific values. If you write it down in Leibniz, you can
         write software to analyze it, e.g. check that every second term is positive or whatever else you know about your
         model. You can also compute specific values in Leibniz, and use them to test your Fortran code.}
    @item{Define scientific concepts with the precision of a formal language. There is no better remedy against sloppy
          thinking than a computer that forces you to respect your own definitions. See
          @hyperlink["http://sjscience.org/memberPage?uId=90&jId=6#journal"]{this article collection} for essays on this
          little-known aspect of computing and for practical examples.}]
