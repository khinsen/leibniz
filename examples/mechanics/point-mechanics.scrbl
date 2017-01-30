#lang leibniz

@title[#:version ""]{Classical point mechanics}
@author{Konrad Hinsen}

@section{Kinematics}

A @sort{point_system} consists of @sort{point}s.  The
smallest possible system is a single point: @sort{point ⊆ point_system}.
Composition of point systems is achieved with
@op{point_system and point_system : point_system}.

We introduce @sort{positions} such that @op{positions of point : position}
selects the @sort{position} of a specific @sort{point}. Likewise, we define
@sort{velocities} with @op{velocities of point : velocity} and
@sort{accelerations} with @op{accelerations of point : acceleration}.

For moving points, the @sort{positions} as a function of time are
called a @sort{trajectory}: @op{trajectory at time : positions}.
We will also need the first and second time derivatives of a @sort{trajectory}:
@itemlist[
  @item{@sort{v_trajectory}, @op{𝒟(trajectory) : v_trajectory}}
  @item{@sort{a_trajectory}, @op{𝒟(v_trajectory) : a_trajectory}}]

@subsection[#:style 'unnumbered]{Additional definitions}

@itemlist[
  @item{@smaller[@op{v_trajectory at time : velocities}]}
  @item{@smaller[@op{a_trajectory at time : accelerations}]}]

@section{Dynamics}

Points move under the influence of @sort{forces}. How a @sort{point}
reacts to a @sort{force} depends on its @sort{mass}, as described
by Newton's law of motion:

   @equation{f at t = (m * 𝒟(𝒟(r))) at t ∀ t:time}

@subsection{Additional definitions}

@itemlist[
  @item{@smaller[@op{forces of point : force}]}
  @item{@smaller[@op{masses of point : mass}]}]
