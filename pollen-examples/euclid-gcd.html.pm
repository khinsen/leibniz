#lang pollen

◊(require leibniz/pollen)

◊define-meta[title]{The greatest common divisor of two natural numbers}
◊define-meta[author]{Euclid}

◊+context{gcd}
◊+use{builtins/integers}

The greatest common divisor ◊+op{gcd(a:ℕ, b:ℕ) : ℕ} of two natural numbers ◊+var{a:ℕ} and ◊+var{b:ℕ} can be obtained by applying the following rules:
◊ol{
  ◊li{If the two numbers are equal, their GCD is equal to them as well:
      ◊+rule{gcd(a, a) ⇒ a}}
  ◊li{If ◊+term{a > b}, replace ◊+term{a} by ◊+term{a - b}:
      ◊+rule{gcd(a, b) ⇒ gcd(a - b, b) if a > b}}
  ◊li{Otherwise we have ◊+term{b > a} and replace ◊+term{b} by ◊+term{b - a}:
      ◊+rule{gcd(a, b) ⇒ gcd(a, b - a)}}}

Here are some application examples:
◊ul{
  ◊li{◊+eval-term{gcd(2, 3)}}
  ◊li{◊+eval-term{gcd(3, 2)}}
  ◊li{◊+eval-term{gcd(42, 7)}}}
