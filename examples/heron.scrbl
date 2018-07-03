#lang leibniz

@title{Heron's algorithm}
@author{Konrad Hinsen}

Heron's algorithm computes the square root of an input number @italic{x} iteratively,
starting from an initial estimate @italic{e}, until the result is
correct within a given tolerance @italic{ε}. It is a special case of Newton's method
for finding roots of algebraic equations.

@context["heron" #:use "builtins/real-numbers"]{

@section{Heron's algorithm using exact arithmetic}

Let @op{heron(x:ℝnn, ε:ℝp, e:ℝnn) : ℝnn}
be the result of Heron's algorithm for computing the square root of @term{x}
up to tolerance @term{ε}, starting from estimate @term{e}.

The first step of the algorithm is to check if the current approximation is good enough,
in which case it is the final result:
  @inset{@rule{heron(x, ε, e) ⇒ e
               if abs(x - e^{2}) < ε}}
Note that the tolerance applies to @term{x} and not to @term{√(x)}.

Otherwise, a new estimate is computed by taking the average
of @italic{e} and @italic{x} ÷ @italic{e}:
  @inset{@rule{heron(x, ε, e) ⇒ heron(x, ε, 1/2 × (e + (x ÷ e)))}}

For convenience, we also allow no initial estimate to be supplied, using
a default value of 1:
  @inset{@op{heron(x:ℝnn, ε:ℝp) : ℝnn}
         @rule{heron(x, ε) ⇒ heron(x, ε, 1)}}
The iteration starting from 1 will always converge but could well be inefficient.

@subsection{Tests}

We can use this algorithm with rational number arguments:
  @inset{@test{heron(2, 1/2)^{2} - 2 ⇒ 1/4}
         @test{heron(2, 1/10)^{2} - 2 ⇒ 1/144}
         @test{heron(2, 1/100)^{2} - 2 ⇒ 1/144}
         @test{heron(2, 1/200)^{2} - 2 ⇒ 1/166464}}
We see that decreasing @term{ε} leads to better approximations of √2, which are
always within the prescribed tolerance.
}

@context["fp-heron"
         #:insert-extend ["heron" (real->float FP64)]]{

@section{Heron's algorithm using floating-point arithmetic}

A floating-point version of Heron's algorithm can be obtained by automatic
conversion:

@show-context["fp-heron"]

@subsection{Tests}

We can use this version with floating-point arguments:
@inset{@test{heron(2., 0.5)^{2} - 2. ⇒ 0.25}
       @eval-term{heron(2., 0.5) - √(2.)}
       @test{abs(heron(2., 0.1)^{2} - 2.) < 0.1  ⇒ true}
       @eval-term{heron(2., 0.1) - √(2.)}
       @test{abs(heron(2., 0.01)^{2} - 2.) < 0.01  ⇒ true}
       @eval-term{heron(2., 0.01) - √(2.)}
       @test{abs(heron(2., 0.001)^{2} - 2.) < 0.001  ⇒ true}
       @eval-term{heron(2., 0.001) - √(2.)}}

Again we see that decreasing @term{ε} leads to better approximations of @term{√(2.)}.
The deviation is always smaller than the prescribed tolerance.
}

@;signature-graphs["heron.sig"]
