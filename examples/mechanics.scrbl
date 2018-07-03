#lang leibniz

@title{Basic mechanics}
@author{Konrad Hinsen}

@import["quantities" "quantities.xml"]

@context["kinematics"
         #:use "quantities/length"
         #:use "quantities/time"
         #:use "quantities/velocity"
         #:use "quantities/acceleration"]{

@section{Kinematics}

The derived quantities @sort{V} and @sort{A} are obtained as quotients
of the fundamental quantities @sort{L} and @sort{T}.

Velocities are obtained by dividing a length by a time:
  @inset{@op{L ÷ Tnz : V}
         @op{Lnz ÷ Tnz : Vnz}}

Accelerations are the result of dividing a velocity by a time:
  @inset{@op{V ÷ Tnz : A}
         @op{Vnz ÷ Tnz : Anz}}

}

@context["kinematics-example"
         #:extend "kinematics"]{

@subsection{Example}

We consider a point that moves on a straight line starting at time 0 from the origin.
At time @op{t1 : Tnz} it has distance @op{d1 : L} from the origin, at
time @op{t2 : Tnz}, @term{t2 > t1} the distance is @op{d2 : L}.

The average velocity from time 0 to @term{t1} is then
  @inset{@op{v1 : V}
         @rule{v1 ⇒ d1 ÷ t1}.}
and the average velocity between 0 and @term{t2} is
  @inset{@op{v2 : V}
         @rule{v2 ⇒ d2 ÷ t2}.}
The average acceleration is given by
  @inset{@op{a : A}
         @rule{a ⇒ 2 × ((v2 - v1) ÷ (t2 - t1))}
         @eval-term{a}.}

}

@context["kinematics-nummerical-example"
         #:extend "kinematics-example"]{

Introducing a time unit @op{s : Tnz} and a length unit @op{m : Lnz}, we
can assign numerical values:
  @inset{@rule{t1 ⇒ 3 × s},  @rule{d1 ⇒ 20 × m}
         @rule{t2 ⇒ 6 × s},  @rule{d2 ⇒ 50 × m}
         @eval-term{v1}
         @eval-term{v2}
         @eval-term{a}.}

}

@context["time-dependent-kinematics"
         #:extend "kinematics"
         #:insert-use ["quantities/function-with-finite-difference-template"
                       (rename-sort SQD T)
                       (rename-sort SQDnz Tnz)
                       (rename-sort SQI L)
                       (rename-sort SQInz Lnz)
                       (rename-sort SQID V)
                       (rename-sort SQIDnz Vnz)
                       (rename-sort SQD→SQI T→L)
                       (rename-sort SQD→SQID T→V)]
         #:insert-use ["quantities/function-with-finite-difference-template"
                       (rename-sort SQD T)
                       (rename-sort SQDnz Tnz)
                       (rename-sort SQI V)
                       (rename-sort SQInz Vnz)
                       (rename-sort SQID A)
                       (rename-sort SQIDnz Anz)
                       (rename-sort SQD→SQI T→V)
                       (rename-sort SQD→SQID T→A)]]{

@section{Time-dependent kinematics}

When describing motion, quantities @sort{L}, @sort{V}, and @sort{A}
become functions of @sort{T}. These time-dependent quantities
are written as @sort{T→L ⊆ T→Q}, @sort{T→V ⊆ T→Q}, and @sort{T→A ⊆ T→Q},
with each one being the time derivative of its predecessor. The sort
@sort{T→Q ⊆ Q→Q} covers all these time-dependent quantities.
}

@context["dynamics"
         #:extend "time-dependent-kinematics"
         #:use "quantities/mass"
         #:use "quantities/force"
         #:insert-use ["quantities/function-template"
                       (rename-sort SQD T)
                       (rename-sort SQDnz Tnz)
                       (rename-sort SQI F)
                       (rename-sort SQInz Fnz)
                       (rename-sort SQD→SQI T→F)]]{

@section{Dynamics}

Extending kinematics to dynamics requires @sort{M} for masses, @sort{F} for forces, and
@sort{T→F ⊆ T→Q} for time-dependent forces, plus the following relations between these
quantities:
  @inset{@op{M × A : F}
         @op{Mnz × Anz : Fnz}
         @op{M × T→A : T→F}
         @rule{(m × f)[t] ⇒ m × f[t]  ∀ m:M  ∀ f:T→A  ∀ t:T}}

}
