#lang pollen

◊(require leibniz/pollen)

◊define-meta[title]{Boolean algebra}
◊define-meta[author]{Konrad Hinsen}

◊+context[#:name "boolean" #:use "builtins/truth"]{

◊section{Logical operations}

The following operators are defined on terms of sort ◊+sort{boolean}:

◊table[◊tr[◊td{NOT:} ◊td[◊+op{¬(boolean) : boolean}]]
       ◊tr[◊td{AND:} ◊td[◊+op{boolean ∧ boolean : boolean}]]
       ◊tr[◊td{OR:}  ◊td[◊+op{boolean ∨ boolean : boolean}]]
       ◊tr[◊td{XOR:} ◊td[◊+op{boolean ⊻ boolean : boolean}]]]

◊subsection{Rewrite rules}

◊subsubsection{Eliminate NOT and OR}

NOT is replaced by XOR with ◊+term{true}:
  ◊blockquote{◊+rule{¬(X) ⇒ true ⊻ X ∀ X:boolean}}

OR is replaced by XOR and AND:
  ◊blockquote{◊+rule{X ∨ Y ⇒ X ⊻ Y ⊻ (X ∧ Y) ∀ X:boolean  ∀ Y:boolean}}

◊subsubsection{Simplify AND relations}

AND is ◊+term{false} if one of its arguments is ◊+term{false}:
  ◊blockquote{◊+rule{X ∧ false ⇒ false ∀ X:boolean}
              ◊+rule{false ∧ X ⇒ false ∀ X:boolean}}

If one argument of AND is ◊+term{true}, the result is the other argument:
  ◊blockquote{◊+rule{X ∧ true ⇒ X ∀ X:boolean}
              ◊+rule{true ∧ X ⇒ X ∀ X:boolean}}

If the two arguments to AND are equal, they are also equal to the result:
  ◊blockquote{◊+rule{X ∧ X ⇒ X ∀ X:boolean}}

◊subsubsection{Simplify XOR relations}

XOR with ◊+term{false} leaves truth values unchanged:
  ◊blockquote{◊+rule{X ⊻ false ⇒ X ∀ X:boolean}
              ◊+rule{false ⊻ X ⇒ X ∀ X:boolean}}

If the two arguments to XOR are equal, the result is ◊+term{false}:
  ◊blockquote{◊+rule{X ⊻ X ⇒ false ∀ X:boolean}}

◊subsubsection{Standardize combinations of XOR and AND}

The above rules will reduce any boolean expression to a combination of XOR and AND
operations that allow no further simplification. However, it is still possible that
logically equal expressions are rewritten into distinct syntactical forms, making it
difficult to verify that they are equal. The following rule standardizes results
by replacing XOR inside AND by AND inside XOR:
  ◊blockquote{◊+rule{X ∧ (Y ⊻ Z) ⇒ (X ∧ Y) ⊻ (X ∧ Z)
                     ∀ X:boolean  ∀ Y:boolean   ∀ Z:boolean}}


◊subsection{Tests}

Truth table for NOT:
  ◊blockquote{◊+test{¬(false) ⇒ true}
              ◊+test{¬(true) ⇒ false}}

Truth table for AND:
  ◊blockquote{◊+test{false ∧ false ⇒ false}
              ◊+test{false ∧ true ⇒ false}
              ◊+test{true ∧ false ⇒ false}
              ◊+test{true ∧ true ⇒ true}}

Truth table for OR:
  ◊blockquote{◊+test{false ∨ false ⇒ false}
              ◊+test{false ∨ true ⇒ true}
              ◊+test{true ∨ false ⇒ true}
              ◊+test{true ∨ true ⇒ true}}

Truth table for XOR:
  ◊blockquote{◊+test{false ⊻ false ⇒ false}
              ◊+test{false ⊻ true ⇒ true}
              ◊+test{true ⊻ false ⇒ true}
              ◊+test{true ⊻ true ⇒ false}}
}


