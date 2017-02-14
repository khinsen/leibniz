#lang leibniz

@title[#:version ""]{Classical point mechanics}
@author{Konrad Hinsen}

@section{Kinematics}

@context["test"]{

A @sort{point-system} consists of @sort{point}s.  The
smallest possible system is a single point: @sort{point ⊆ point-system}.
Composition of point systems is achieved with
@op{point-system and point-system : point-system}.

We introduce @sort{positions} such that @op{positions of point : position}
selects the @sort{position} of a specific @sort{point}. Likewise, we define
@sort{velocity}/@sort{velocities} with @op{velocities of point : velocity} and
@sort{acceleration}/@sort{accelerations} with @op{accelerations of point : acceleration}.

For moving points, the @sort{positions} as a function of @sort{time} are
called a @sort{trajectory}: @op{trajectory at time : positions}.
We will also need the first and second time derivatives of a @sort{trajectory}:
@itemlist[
  @item{@sort{v-trajectory}, @op{𝒟(trajectory) : v-trajectory}}
  @item{@sort{a-trajectory}, @op{𝒟(v-trajectory) : a-trajectory}}]

@subsection[#:style 'unnumbered]{Additional definitions}

@itemlist[
  @item{@smaller[@op{v-trajectory at time : velocities}]}
  @item{@smaller[@op{a-trajectory at time : accelerations}]}]

@section{Dynamics}

Points move under the influence of @sort{forces}. How a @sort{point}
reacts to a @sort{force} depends on its @sort{mass}, as described
by Newton's law of motion:

   @equation{f at t = (m * 𝒟(𝒟(r))) at t ∀ t:time}

where the force trajectory (@sort{f-trajectory}) @op{f : f-trajectory} depends
on the trajectory @op{r : trajectory}.

@subsection{Additional definitions}

@itemlist[
  @item{@smaller[@op{forces of point : force}]}
  @item{@smaller[@op{masses of point : mass}]}] (@sort{masses})

}
