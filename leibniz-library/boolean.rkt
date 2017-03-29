#lang leibniz

@title{Boolean algebra}
@author{Konrad Hinsen}

@context["boolean" #:use "builtins/truth"]{
@section{Logical operations}

The following operators are defined on terms of sort @sort{boolean}:

@tabular[#:sep @hspace[1]
         @list[ @list["Negation:" @op{¬(boolean) : boolean}]
                @list["And:"      @op{boolean ∧ boolean : boolean}]
                @list["Or:"       @op{boolean ∨ boolean : boolean}]
                @list["Xor:"      @op{boolean ⊻ boolean : boolean}] ]]

@subsection{Rewrite rules}

@subsubsection{Eliminate Negation and Or}

Negation is replaced by Xor with @term{true}:
  @inset{@rule{¬(X) ⇒ true ⊻ X ∀ X:boolean}}

Or is replaced by Xor and And:
  @inset{@rule{X ∨ Y ⇒ X ⊻ Y ⊻ X ∧ Y ∀ X:boolean  ∀ Y:boolean}}

@subsubsection{Simplify And relations}

And is @term{false} if one of its arguments is @term{false}:
  @inset{@rule{X ∧ false ⇒ false ∀ X:boolean}
         @rule{false ∧ X ⇒ false ∀ X:boolean}}

If one argument of And is @term{true}, the result is the other argument:
  @inset{@rule{X ∧ true ⇒ X ∀ X:boolean}
         @rule{true ∧ X ⇒ X ∀ X:boolean}}

If the two arguments to And are equal, they are also equal to the result:
  @inset{@rule{X ∧ X ⇒ X ∀ X:boolean}}

@subsubsection{Simplify Xor relations}

Xor with @term{false} leaves truth values unchanged:
  @inset{@rule{X ⊻ false ⇒ X ∀ X:boolean}
         @rule{false ⊻ X ⇒ X ∀ X:boolean}}

If the two arguments to Xor are equal, the result is @term{false}:
  @inset{@rule{X ⊻ X ⇒ false ∀ X:boolean}}

@subsubsection{Standardize combinations of Xor and And}

The above rules will reduce any boolean expression to a combination of Xor and And
operations that allow no further simplification. However, it is still possible that
logically equal expressions are rewritten into distinct syntactical forms, making it
difficult to verify that they are equal. The following rule standardizes results
by replacing Xor inside And by And inside Xor:
  @inset{@rule{X ∧ Y ⊻ Z ⇒ (X ∧ Y) ⊻ X ∧ Z
               ∀ X:boolean  ∀ Y:boolean   ∀ Z:boolean}}


@subsection{Tests}

Truth table for Not:
  @inset{@test{¬(false) ⇒ true}
         @test{¬(true) ⇒ false}}

Truth table for And:
  @inset{@test{false ∧ false ⇒ false}
         @test{false ∧ true ⇒ false}
         @test{true ∧ false ⇒ false}
         @test{true ∧ true ⇒ true}}

Truth table for Or:
  @inset{@test{false ∨ false ⇒ false}
         @test{false ∨ true ⇒ true}
         @test{true ∨ false ⇒ true}
         @test{true ∨ true ⇒ true}}

Truth table for Xor:
  @inset{@test{false ⊻ false ⇒ false}
         @test{false ⊻ true ⇒ true}
         @test{true ⊻ false ⇒ true}
         @test{true ⊻ true ⇒ false}}

}

@xml["boolean.xml"]
