#lang pollen

◊(require leibniz/pollen)

◊define-meta[title]{Physical quantities}
◊define-meta[author]{Konrad Hinsen}

◊+context{quantities}
◊+use{builtins/real-numbers}

This document defines a framework for working with physical quantities, including functions from quantities to quantities, derivatives of these functions, and numerical finite differences. It does not define any specific quantities. For an example of the use of this framework, see the document
◊a[#:href "mechanics.html"]{◊i{mechanics}}.

◊section{Generic quantities}

We define ◊+sort{Q} to represent any physical quantity, and ◊+sort{Q.nz ⊆ Q} to represent the subset of non-zero quantities by which it is admissible to divide. All quantities will be subsorts of ◊+sort{Q}.

The product and quotient of any two quantities is then again a quantity, with appropriate special cases for quantities that can be proven to be non-zero:
  ◊blockquote{◊+op{Q × Q : Qℝ}
              ◊+op{Q.nz × Q.nz : Qℝ.nz}
              ◊+op{Q ÷ Q.nz : Qℝ}
              ◊+op{Q.nz ÷ Q.nz : Qℝ.nz}}

The result sort of these operators is not ◊+sort{Q} but ◊+sort{Qℝ} or ◊+sort{Qℝ.nz ⊆ Qℝ}, because in the special case of a quotient of same-kind quantities, the result is a pure number. We therefore define
  ◊blockquote{◊+sort{Q ⊆ Qℝ}
              ◊+sort{Q.nz ⊆ Qℝ.nz}
              ◊+sort{ℝ ⊆ Qℝ}
              ◊+sort{ℝ.nz ⊆ Qℝ.nz}}

We can also multiply or divide quantities by numbers:
  ◊blockquote{◊+op{ℝ × Q : Q}
              ◊+op{ℝ.nz × Q.nz : Q.nz}
              ◊+op{Q ÷ ℝ.nz : Q}
              ◊+op{Q.nz ÷ ℝ.nz : Q.nz}}

The simplification strategy is to reduce quantities to the form f × q, with q a non-reducible quantity, wherever possible.

Combine multiple numerical prefactors into one:
  ◊blockquote{◊+rule{f1 × (f2 × q) ⇒ (f1 × f2) × q
               ∀ q:Q  ∀ f1:ℝ  ∀ f2:ℝ}
              ◊+rule{f1 × ((f2 × q1) ÷ q2) ⇒ (f1 × f2) × (q1 ÷ q2)
               ∀ q1:Q  ∀ q2:Q.nz  ∀ f1:ℝ  ∀ f2:ℝ}}

Replace division by multiplication:
  ◊blockquote{◊+rule{q ÷ f ⇒ (1 ÷ f) × q
               ∀ q:Q  ∀ f:ℝ.nz}
              ◊+rule{q1 ÷ (f × q2) ⇒ (1 ÷ f) × (q1 ÷ q2)
               ∀ q1:Q  ∀ q2:Q.nz  ∀ f:ℝ.nz}}

Remove quantities of zero magnitude from sums:
  ◊blockquote{◊+rule{q1 + (0 × q2) ⇒ q1
                     ∀ q1:Q  ∀ q2:Q}
              ◊+rule{q1 - (0 × q2) ⇒ q1
                     ∀ q1:Q  ∀ q2:Q}
              ◊+rule{(0 × q2) + q1 ⇒ q1
                     ∀ q1:Q  ∀ q2:Q}
              ◊+rule{(0 × q2) - q1 ⇒ q1
                     ∀ q1:Q  ∀ q2:Q}}

◊+context{quantity-template}
◊+extend{quantities}

◊section{Defining specific quantities}

The definitions and rules for specific quantities such as mass or time are essentially the same. We define a template for a generic quantity plus a context for defining the real quantities by name substitution.

◊subsection{The quantity template}

This template defines a fictitious quantity ◊+sort{SQ ⊆ Q} with
◊+sort{SQ.nz ⊆ Q.nz} and ◊+sort{SQ.nz ⊆ SQ}.

The sum and difference of two same-kind quantities is again a quantity of the same kind:
  ◊blockquote{◊+op{SQ + SQ : SQ}
              ◊+op{SQ - SQ : SQ}}

Multiplication and division by numbers also yields same-kind quantities:
  ◊blockquote{◊+op{ℝ × SQ : SQ}
              ◊+op{ℝ.nz × SQ.nz : SQ.nz}
              ◊+op{-(SQ) : SQ}
              ◊+op{SQ ÷ SQ.nz : ℝ}
              ◊+op{SQ.nz ÷ SQ.nz : ℝ.nz}
              ◊+op{SQ ÷ ℝ.nz : SQ}
              ◊+op{SQ.nz ÷ ℝ.nz : SQ.nz}}

Finally, same-kind quantities can be compared:
  ◊blockquote{◊+op{SQ < SQ : boolean}
              ◊+op{SQ > SQ : boolean}
              ◊+op{SQ ≤ SQ : boolean}
              ◊+op{SQ ≥ SQ : boolean}}

In the simplification rules, we use the variables ◊+var{sq:SQ}, ◊+var{sq1:SQ},
◊+var{sq2:SQ} and ◊+var{f:ℝ}, ◊+var{f1:ℝ}, ◊+var{f2:ℝ}.

Combine sums and differences of the same ◊+sort{SQ} with different numerical prefactors:
  ◊blockquote{◊+rule{(f1 × sq) + (f2 × sq) ⇒ (f1 + f2) × sq}
              ◊+rule{(f1 × sq) - (f2 × sq) ⇒ (f1 - f2) × sq}}

Reduce quotients of two ◊+sort{SQ}s to a number:
  ◊blockquote{◊+rule{sq1 ÷ (f × sq2) ⇒ (sq1 ÷ f) ÷ sq2}
              ◊+rule{(f × sq1) ÷ sq2 ⇒ f × (sq1 ÷ sq2)}
              ◊+rule{sq ÷ sq ⇒ 1}}

◊+context{template-test}
◊+extend{quantity-template}

◊subsubsection{Tests}

Given two quantities ◊+op{a : SQ} and ◊+op{b : SQ} whose quotient we define as ◊+rule{b ÷ a ⇒ 10}, we can test the simplification rules:

  ◊blockquote{◊+test{2 × (3 × a) ⇒ 6 × a}
              ◊+test{2 × (a ÷ 3) ⇒ 2/3 × a}
              ◊+test{(2 × a) ÷ (3 × a) ⇒ 2/3}
              ◊+test{(2 × b) ÷ (3 × a) ⇒ 20/3}
              ◊+test{(2 × a) + (3 × a) ⇒ 5 × a}
              ◊+test{(2 × a) - (3 × a) ⇒ -1 × a}}

◊+context{apply-quantity-template}
◊+use{builtins/contexts}

◊subsection{Using the quantity template}


A new context for a quantity is defined as ◊+op{define-quantity(string) : context}, using the rule
◊blockquote{◊+rule{define-quantity(name)
                    ⇒ replace-sort-prefix(remove-vars(template), "SQ", name)
                    ∀ name:string}}

where ◊+context-ref{template ⇒ context("quantity-template")} is the template context. This rule removes the context-level vars, which are not needed and can lead to name clashes if several instantiations of the template are used together. Next, it replaces the sorts with the prefix SQ, i.e. SQ and SQ.nz by corresponding sorts whose prefix is the given quantity name.

◊+context{-function-template-domain-quantity}
◊+use{apply-quantity-template}

◊section{A template for functions from one quantity to another}

This template defines functions from a domain quantity  ◊+substitute-context{define-quantity("SQD")} ...


◊+context{-function-template-image-quantity}
◊+use{apply-quantity-template}
...to an image quantity ◊+substitute-context{define-quantity("SQI")}


◊+context{quantity-function-template}
◊+use{-function-template-domain-quantity}
◊+use{-function-template-image-quantity}

The sort for such functions is ◊+sort{SQD→SQI ⊆ Q→Q}. Function application is defined by ◊+op{SQD→SQI[SQD] : SQI}.

It is convenient to provide some arithmetic:
  ◊ul{
    ◊li{Addition and subtraction of functions:
        ◊ul{
          ◊li{◊+op{(f:SQD→SQI) + (g:SQD→SQI) : SQD→SQI} with
              ◊+rule{(f + g)[x] ⇒ f[x] + g[x] ∀ x:SQD}}
          ◊li{◊+op{(f:SQD→SQI) - (g:SQD→SQI) : SQD→SQI} with
              ◊+rule{(f - g)[x] ⇒ f[x] - g[x] ∀ x:SQD}}}}
    ◊li{Addition and subtraction of constants:
        ◊ul{
          ◊li{◊+op{(f:SQD→SQI) + (q:SQI) : SQD→SQI} with
              ◊+rule{(f + q)[x] ⇒ f[x] + q ∀ x:SQD}}
          ◊li{◊+op{(f:SQD→SQI) - (q:SQI) : SQD→SQI} with
              ◊+rule{(f - q)[x] ⇒ f[x] + q ∀ x:SQD}}
          ◊li{◊+op{(q:SQI) + (f:SQD→SQI) : SQD→SQI} with
              ◊+rule{(q + f)[x] ⇒ q + f[x] ∀ x:SQD}}
          ◊li{◊+op{(q:SQI) - (f:SQD→SQI) : SQD→SQI} with
              ◊+rule{(q - f)[x] ⇒ q - f[x] ∀ x:SQD}}}}
    ◊li{Multiplication with scalars:
        ◊ul{
          ◊li{◊+op{(s:ℝ) × (f:SQD→SQI) : SQD→SQI} with
              ◊+rule{(s × f)[x] ⇒ s × f[x] ∀ x:SQD}}
          ◊li{◊+op{-(f:SQD→SQI) : SQD→SQI} with
              ◊+rule{-(f)[x] ⇒ -(f[x])  ∀ x:SQD}}}}}

◊+context{apply-quantity-function-template}
◊+use{builtins/contexts}

◊subsection{Using the quantity function template}

A new context for a quantity function is defined via ◊+op{define-quantity-function(domain-quantity-sort:string, domain-quantity-context:context, image-quantity-sort:string, image-quantity-context:context) : context}. This involves several steps:
◊ol{
  ◊li{Retrieve ◊+context-ref{template ⇒ context("quantity-function-template")} and remove its variables.}
  ◊li{Replace the template sort prefix SQD by the domain quantity sort ◊+term{domain-quantity-sort}.}
  ◊li{Replace the template sort SQI prefix by the image quantity sort ◊+term{image-quantity-sort}.}
  ◊li{Replace ◊+context-ref{sqd-context ⇒ context("-function-template-domain-quantity")} by ◊+term{domain-quantity-context}.}
  ◊li{Replace ◊+context-ref{sqi-context ⇒ context("-function-template-image-quantity")} by ◊+term{image-quantity-context}.}
  ◊li{Replace the function sort SQD→SQI by a sort constructed from ◊+term{domain-quantity-sort} and ◊+term{image-quantity-sort}.}}

These steps are performed by the following somewhat lengthy rule:
◊blockquote{
  ◊+rule{define-quantity-function(domain-quantity-sort, domain-quantity-context, image-quantity-sort, image-quantity-context)
         ⇒ replace-sort-prefix(
              replace-sort-prefix(
                replace-sort(
                  replace-include(
                    replace-include(
                      remove-vars(template),
                      sqd-context, domain-quantity-context),
                    sqi-context, image-quantity-context),
                  "SQD→SQI", domain-quantity-sort + "→" + image-quantity-sort),
                "SQI", image-quantity-sort),
              "SQD", domain-quantity-sort)}}


◊section{A template for derivatives of quantity functions}

◊+context{-function-template-image-div-domain-quantity}
◊+use{apply-quantity-template}

Given SQI as a function of SQD, the derivative function has the image quantity SQID defined as the quotient of SQI and SQD. Therefore we must first define SQID:

◊blockquote{
◊+substitute-context{define-quantity("SQID")}}

◊+context{-function-template-SQD→SQID}
◊+use{apply-quantity-function-template}

Next, we define the new quantity function ◊+sort{SQD→SQID}:

◊blockquote{
◊+substitute-context{define-quantity-function("SQD", domain-quantity, "SQID", image-div-domain-quantity)}}

Like all quantity functions, its definition relies on the contexts defining its domain and image quantities:
◊blockquote{
◊+context-ref{domain-quantity ⇒ context("-function-template-domain-quantity")} ◊br{}
◊+context-ref{image-div-domain-quantity ⇒ context("-function-template-image-div-domain-quantity")}}

◊+context{quantity-derivative-template}
◊+use{quantity-function-template}
◊+use{-function-template-SQD→SQID}

The derivative of a function is given by ◊+op{𝒟(SQD→SQI) : SQD→SQID}. The derivative operator is linear, i.e. for ◊+var{f:SQD→SQI}, ◊+var{g:SQD→SQI}, and ◊+var{s:ℝ} we have
  ◊blockquote{◊+rule{𝒟(f + g) ⇒ 𝒟(f) + 𝒟(g)}
              ◊+rule{𝒟(f - g) ⇒ 𝒟(f) - 𝒟(g)}
              ◊+rule{𝒟(s × f) ⇒ s × 𝒟(f)}}

In numerical approximations, the derivative operator ◊+op{𝒟(SQD→SQI) : SQD→SQID} is replaced by the finite-difference operator ◊+op{Δ(f:SQD→SQI, h:SQD.nz) : SQD→SQID}. A finite-difference approximation is characterized by a parameter ◊+var{h:SQD.nz} that is assumed to be a sufficiently small quantity.

Like the derivative operator, the finite-difference operator is linear:
  ◊blockquote{◊+rule{Δ(f + g, h) ⇒ Δ(f, h) + Δ(g, h)}
              ◊+rule{Δ(f - g, h) ⇒ Δ(f, h) - Δ(g, h)}
              ◊+rule{Δ(s × f, h) ⇒ s × Δ(f, h)}}

◊+context{apply-quantity-derivative-template}
◊+use{builtins/contexts}

A new context for a quantity function is defined via ◊+op{define-quantity-derivative(domain-quantity-sort:string, domain-quantity-context:context, quantity-function-sort:string, quantity-function-context:context, quantity-derivative-sort:string, quantity-derivative-context:context) : context}. This involves several steps:
◊ol{
  ◊li{Retrieve ◊+context-ref{template ⇒ context("quantity-derivative-template")} and remove its variables.}
  ◊li{Replace the template sort SQD by ◊+term{domain-quantity-sort}.}
  ◊li{Replace the template sort SQD→SQI by ◊+term{quantity-function-sort}.}
  ◊li{Replace the template sort SQD→SQID by ◊+term{quantity-derivative-sort}.}
  ◊li{Replace ◊+context-ref{dq-template ⇒ context("-function-template-domain-quantity")} by
◊+term{domain-quantity-context}.}
  ◊li{Replace ◊+context-ref{qf-template ⇒ context("quantity-function-template")} by ◊+term{quantity-function-context}.}
  ◊li{Replace ◊+context-ref{qd-template ⇒ context("-function-template-SQD→SQID")} by ◊+term{quantity-derivative-context}.}}

These steps are performed by the following somewhat lengthy rule:
◊blockquote{
  ◊+rule{define-quantity-derivative(domain-quantity-sort, domain-quantity-context, quantity-function-sort, quantity-function-context, quantity-derivative-sort, quantity-derivative-context)
         ⇒ replace-sort-prefix(
              replace-sort-prefix(
                 replace-sort(
                   replace-include(
                     replace-include(
                       replace-include(
                         remove-vars(template),
                         qf-template, quantity-function-context),
                       dq-template, domain-quantity-context),
                     qd-template, quantity-derivative-context),
                   "SQD→SQI", quantity-function-sort),
                 "SQD→SQID", quantity-derivative-sort),
               "SQD", domain-quantity-sort)}}
