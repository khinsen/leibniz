#lang leibniz

@title{Understanding Leibniz errors}
@author{Konrad Hinsen}

@context["ambiguous-sorts"]{

@section{Ambiguous sorts}

Given three sorts @sort{A}, @sort{B}, and @sort{C} with @sort{A ⊆ B} and @sort{A ⊆ C},
the operator definition
 @inset{@op{foo(B) : B}
        @op{foo(C) : C}}
is ambiguous because it allows terms such as @term{foo(an-A)} with @op{an-A : A}
for which the sort could be either @sort{B} and @sort{C}, with no rule
to decide between these choices. This will lead to the error message
@tt{operator foo(A) has ambiguous sorts (B C)}.
  @inset{@italic{Note: If you remove the last paragraph of this document, you will
                 actually see this error message.}}

Sometimes such ambigious sorts are the consequence of a mistake in defining the
sort graph. For example, @sort{B} might have been intended to be a subsort of @sort{C}
(and then sort of @term{foo(an-A)} would be @sort{B}) but the declaration
@comment-sort{B ⊆ C} was forgotten.

If the sort graph is correct, then the easiest way to fix an irregular operator is to add
a declaration that makes the sort unique. In this example, that could be the declaration
@op{foo(A) : A}.

}
