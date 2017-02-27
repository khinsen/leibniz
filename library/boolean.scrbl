#lang leibniz

@title{Boolean algebra}
@author{Konrad Hinsen}

Adapted from the @hyperlink["http://maude.cs.uiuc.edu/maude1/manual/maude-manual-html/maude-manual_16.html"]{Maude manual}.

@context["truth"]{
@section{Truth values}

The truth values are @op{true : boolean} and @op{false : boolean}.
}

@context["boolean" #:use "truth"]{
@section{Logical operations}

The following operators are defined on terms of sort @sort{boolean}:

@tabular[#:sep @hspace[1]
         @list[ @list["Negation:" @op{¬(boolean) : boolean}]
                @list["And:"      @op{boolean ∧ boolean : boolean}]
                @list["Or:"       @op{boolean ∨ boolean : boolean}]
                @list["Xor:"      @op{boolean ⊻ boolean : boolean}] ]]

@subsection{Simplifiction rules}

And is @term{false} if one of its arguments is @term{false}:
  @inset{@rule{X ∧ false ⇒ false ∀ X:boolean}
         @rule{false ∧ X ⇒ false ∀ X:boolean}}

If one argument of And is @term{true}, the result is the other argument:
  @inset{@rule{X ∧ true ⇒ X ∀ X:boolean}
         @rule{true ∧ X ⇒ X ∀ X:boolean}}

If the arguments to And are equal to each other, they are also equal to the result:
  @inset{@rule{X ∧ X ⇒ X ∀ X:boolean}}

Xor with @term{false} leaves truth values unchanged:
  @inset{@rule{X ⊻ false ⇒ X ∀ X:boolean}
         @rule{false ⊻ X ⇒ X ∀ X:boolean}}

If the arguments to Xor are equal, the result is @term{false}:
  @inset{@rule{X ⊻ X ⇒ false ∀ X:boolean}}

Negation is replaced by Xor with @term{true}:
  @inset{@rule{¬(X) ⇒ true ⊻ X ∀ X:boolean}}

Or is replaced by Xor and And:
  @inset{@rule{X ∨ Y ⇒ X ⊻ Y ⊻ X ∧ Y ∀ X:boolean  ∀ Y:boolean}}

The above set of rules must be complemented by one more rule to cover all situations:
  @inset{@rule{X ∧ Y ⊻ Z ⇒ (X ∧ Y) ⊻ X ∧ Z
               ∀ X:boolean  ∀ Y:boolean   ∀ Z:boolean}}

}
