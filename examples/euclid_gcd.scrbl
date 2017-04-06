#lang leibniz

@title{The greatest common divisor of two natural numbers}
@author{Euclid}

@context["gcd" #:use "builtins/integers"]{

The greatest common divisor @op{gcd(a:ℕ, b:ℕ) : ℕ} of two natural numbers @var{a:ℕ} and @var{b:ℕ} can be obtained by applying the following rules:
@itemlist[#:style 'ordered
  @item{If the two numbers are equal, their GCD is equal to them as well:
            @linebreak[]
            @rule{gcd(a, a) ⇒ a}}
  @item{If a > b, replace a by a-b:
            @linebreak[]
            @rule{gcd(a, b) ⇒ gcd(a - b, b) if a > b}}
  @item{Otherwise we have b > a and replace b by b-a:
            @linebreak[]
            @rule{gcd(a, b) ⇒ gcd(a, b - a)}}]

Here are some application examples:
@itemlist[
  @item{@eval-term{gcd(2, 3)}}
  @item{@eval-term{gcd(3, 2)}}
  @item{@eval-term{gcd(42, 7)}}]

}