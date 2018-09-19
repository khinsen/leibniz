#lang pollen

◊(require leibniz/pollen)

◊define-meta[title]{Physical quantities}
◊define-meta[author]{Konrad Hinsen}

◊+context{quantities}
◊+use{builtins/real-numbers}

◊section{Generic quantities}

We define ◊+sort{Q} to represent any physical quantity, and ◊+sort{Q.nz ⊆ Q} to represent the subset of non-zero quantities by which it is admissible to divide. The product and quotient of any two quantities is then again a quantity, with appropriate special cases for quantities that can be proven to be non-zero:
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
               ∀ q1:Q  ∀ q2:Q  ∀ f1:ℝ  ∀ f2:ℝ}}

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

The definitions and rules for specific quantities such as mass or time are essentially the same. We define a template for a generic quantity plus a context for defining the real quantities by substitution.

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

◊subsection{Handling the name substitution}

A new context for a quantity is defined as ◊+op{define-quantity(string) : context}, using the rule
◊blockquote{◊+rule{define-quantity(name)
                    ⇒ replace-sort-prefix(remove-vars(context("quantity-template")),
                                           "SQ", name)
                    ∀ name:string}}

This rule first retrieves the quantity template. Next, it removes the context-level vars, which are not needed and can lead to name clashes if several instantiations of the template are used together. In the end, it replaces the sorts with the prefix SQ by corresponding sorts whose prefix is the given quantity name.

◊section{Quantities defined via the template}

◊+context{mass}
◊+use{apply-quantity-template}
◊b{Mass:} ◊+substitute-context{define-quantity("M")}

◊+context{time}
◊+use{apply-quantity-template}
◊b{Time:} ◊+substitute-context{define-quantity("T")}

◊+context{length}
◊+use{apply-quantity-template}
◊b{Length:} ◊+substitute-context{define-quantity("L")}

◊+context{velocity}
◊+use{apply-quantity-template}
◊b{Velocity:} ◊+substitute-context{define-quantity("V")}

◊+context{acceleration}
◊+use{apply-quantity-template}
◊b{Acceleration:} ◊+substitute-context{define-quantity("A")}

◊+context{force}
◊+use{apply-quantity-template}
◊b{Force:} ◊+substitute-context{define-quantity("F")}

◊+context{angle-base}
◊+use{apply-quantity-template}
◊b{Angle:} ◊+substitute-context{define-quantity("angle")}

◊+context{angle}
◊+use{angle-base}

A useful constant for dealing with angles is ◊+op{π : angle}.

◊+context{frequency-base}
◊+use{apply-quantity-template}
◊b{Frequency:} ◊+substitute-context{define-quantity("frequency")}

◊+context{frequency}
◊+use{frequency-base}
◊+use{time}

Frequency is the inverse of time:
◊blockquote{◊+op{frequency × T : ℝ}
            ◊+op{frequency.nz × T.nz : ℝ.nz}
            ◊+op{T × frequency : ℝ}
            ◊+op{T.nz × frequency.nz : ℝ.nz}}

◊+context{angular-frequency-base}
◊+use{apply-quantity-template}
◊b{Angular frequency:} ◊+substitute-context{define-quantity("angular-frequency")}

◊+context{angular-frequency}
◊+use{angular-frequency-base}
◊+use{time}

Angular frequency is angle per time:
◊blockquote{◊;◊+op{angular-frequency × T : angle}
            ◊;◊+op{angular-frequency.nz × T.nz : angle.nz}
            ◊;◊+op{T × angular-frequency : angle}
            ◊;◊+op{T.nz × angular-frequency.nz : angle.nz}
            }

◊+context{function-template-SQD}
◊+use{apply-quantity-template}

◊section{A template for functions from one quantity to another}

This template defines functions from a domain quantity  ◊+substitute-context{define-quantity("SQD")} ...


◊+context{function-template-SQI}
◊+use{apply-quantity-template}
...to an image quantity ◊+substitute-context{define-quantity("SQI")}


◊+context{function-template}
◊+use{function-template-SQD}
◊+use{function-template-SQI}

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

◊subsection{Handling the name substitution}

A new context for a quantity-to-quantity function is defined as ◊+op{define-quantity-function(sqd:string, sqi:string) : context}, using the rule

◊;◊+op{sqd-to-sqi(string, string) : string}
◊;◊+rule{sqd-to-sqi(sqd, sqi) ⇒ sqd + "→" + sqi}

◊blockquote{◊+rule{define-quantity-function(sqd, sqi)
                    ⇒ replace-sort-prefix(
                         replace-sort-prefix(
                           replace-sort(
                             remove-vars(context("quantity-function-template")),
                                         "SQD→SQI", sqd + "→" + sqi),
                             "SQI", sqi),
                           "SQD", sqd)}}
