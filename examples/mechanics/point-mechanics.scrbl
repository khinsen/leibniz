#lang leibniz

@title[#:version ""]{Classical point mechanics}
@author{Konrad Hinsen}

@context["kinematics"]{

@section{Kinematics}

A @sort{point-system} consists of @sort{point}s.  The
smallest possible system is a single point: @sort{point ⊆ point-system}.
Composition of point systems is achieved with

  @inset{
    @op{point-system and point-system : point-system}.}

The motion of the @sort{point}s is described by a @sort{trajectory},
i.e. by @sort{positions} that change over @sort{time}:

   @inset{
     @op{trajectory[time] : positions}
     @op{positions_{point} : position}}

We will also need the first and second time derivatives of these
trajectories:

@itemlist[
  @item{@op{𝒟(trajectory) : v-trajectory} with @op{v-trajectory[time] : velocities} and @op{velocities_{point} : velocity}}
  @item{@op{𝒟(v-trajectory) : a-trajectory} with  @op{a-trajectory[time] : accelerations} and @op{accelerations_{point} : acceleration}}]

}

@context["dynamics" #:use "kinematics"]{

@section{Dynamics}

Points move under the influence of @sort{forces} (@op{forces_{point} :
force}) according to Newton's law of motion,

   @equation{f[t] = m * 𝒟(𝒟(r))[t] ∀ t:time},

where @op{m : masses} (op{masses_{point} : mass}) is a
time-independent non-negative property of each @sort{point}.  The
@sort{force} trajectories @op{f : f-trajectory} are in general functions of the
position trajectories @op{r : trajectory} as we will see in the next section,
and Newton's law of motion therefore is a differential equation
that, together with suitable initial conditions, defines @term{r}.

}

