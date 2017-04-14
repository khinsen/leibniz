#lang leibniz

@title{Boolean algebra}
@author{Konrad Hinsen}

@context["boolean" #:use "builtins/truth"]{
@section{Logical operations}

The following operators are defined on terms of sort @sort{boolean}:

@tabular[#:sep @hspace[1]
         @list[ @list["NOT:"  @op{¬(boolean) : boolean}]
                @list["AND:"  @op{boolean ∧ boolean : boolean}]
                @list["OR:"   @op{boolean ∨ boolean : boolean}]
                @list["XOR:"  @op{boolean ⊻ boolean : boolean}] ]]

@subsection{Rewrite rules}

@subsubsection{Eliminate NOT and OR}

NOT is replaced by XOR with @term{true}:
  @inset{@rule{¬(X) ⇒ true ⊻ X ∀ X:boolean}}

OR is replaced by XOR and AND:
  @inset{@rule{X ∨ Y ⇒ X ⊻ Y ⊻ (X ∧ Y) ∀ X:boolean  ∀ Y:boolean}}

@subsubsection{Simplify AND relations}

AND is @term{false} if one of its arguments is @term{false}:
  @inset{@rule{X ∧ false ⇒ false ∀ X:boolean}
         @rule{false ∧ X ⇒ false ∀ X:boolean}}

If one argument of AND is @term{true}, the result is the other argument:
  @inset{@rule{X ∧ true ⇒ X ∀ X:boolean}
         @rule{true ∧ X ⇒ X ∀ X:boolean}}

If the two arguments to AND are equal, they are also equal to the result:
  @inset{@rule{X ∧ X ⇒ X ∀ X:boolean}}

@subsubsection{Simplify XOR relations}

XOR with @term{false} leaves truth values unchanged:
  @inset{@rule{X ⊻ false ⇒ X ∀ X:boolean}
         @rule{false ⊻ X ⇒ X ∀ X:boolean}}

If the two arguments to XOR are equal, the result is @term{false}:
  @inset{@rule{X ⊻ X ⇒ false ∀ X:boolean}}

@subsubsection{Standardize combinations of XOR and AND}

The above rules will reduce any boolean expression to a combination of XOR and AND
operations that allow no further simplification. However, it is still possible that
logically equal expressions are rewritten into distinct syntactical forms, making it
difficult to verify that they are equal. The following rule standardizes results
by replacing XOR inside AND by AND inside XOR:
  @inset{@rule{X ∧ (Y ⊻ Z) ⇒ (X ∧ Y) ⊻ (X ∧ Z)
               ∀ X:boolean  ∀ Y:boolean   ∀ Z:boolean}}


@subsection{Tests}

Truth table for Not:
  @inset{@test{¬(false) ⇒ true}
         @test{¬(true) ⇒ false}}

Truth table for AND:
  @inset{@test{false ∧ false ⇒ false}
         @test{false ∧ true ⇒ false}
         @test{true ∧ false ⇒ false}
         @test{true ∧ true ⇒ true}}

Truth table for OR:
  @inset{@test{false ∨ false ⇒ false}
         @test{false ∨ true ⇒ true}
         @test{true ∨ false ⇒ true}
         @test{true ∨ true ⇒ true}}

Truth table for XOR:
  @inset{@test{false ⊻ false ⇒ false}
         @test{false ⊻ true ⇒ true}
         @test{true ⊻ false ⇒ true}
         @test{true ⊻ true ⇒ false}}

}
