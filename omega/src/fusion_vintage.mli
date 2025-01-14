(* fusion.mli --

   Copyright (C) 1999-2025 by

       Wolfgang Kilian <kilian@physik.uni-siegen.de>
       Thorsten Ohl <ohl@physik.uni-wuerzburg.de>
       Juergen Reuter <juergen.reuter@desy.de>
       with contributions from
       Christian Speckner <cnspeckn@googlemail.com>

   WHIZARD is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   WHIZARD is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.  *)

(* \thocwmodulesection{Signature of [Fusion.T]} *)

module type T =
  sig

    val options : Options.t

(* JRR's implementation of Majoranas needs a special case. *)
    val vintage : bool

(* Wavefunctions are an abstract data type, containing a momentum~[p]
   and additional quantum numbers, collected in~[flavor]. *)
    type wf

(* Return the wave function with the the same momentum and a
   charge conjugated [flavor]. *)
    val conjugate : wf -> wf

(* Obviously, [flavor] is not restricted to the physical notion of
   flavor, but can carry spin, color, etc.  See the implementation of
   [Model.T] for the physics. *)
    type flavor
    val flavor : wf -> flavor

(* If [flavor] contains powers of coupling orders, it is sometimes useful
   for organizing the output and for diagnostics to be able to strip it
   away. *)
    type flavor_all_orders
    val flavor_all_orders : wf -> flavor_all_orders

(* If [flavor] contains $\textrm{SU}(3)$ color, it is sometimes useful
   for organizing the output and for diagnostics to be able to strip it
   away. *)
    type flavor_sans_color
    val flavor_sans_color : wf -> flavor_sans_color

(* Momenta are represented by an abstract datatype (defined
   in~[Momentum]) that is optimized for performance.  They can be
   accessed either abstractly or as lists of indices of the external
   momenta.  These indices are assigned sequentially by [amplitude] below. *)
    type p
    val momentum : wf -> p
    val momentum_list : wf -> int list

(* Coupling constants *)
    type constant

(* and right hand sides of assignments.  The latter are formed from a sign from
   Fermi statistics, a coupling (constand and Lorentz structure) and wave
   functions of the children. *)
    type coupling
    type rhs

(* \begin{dubious}
     There is no deep reason for defining a polymorphic
     [type 'a children], since we will only ever use [wf children].
   \end{dubious} *)       
    type 'a children

(* Keep track of statistics. *)
    val sign : rhs -> int

(* Extract the coupling (constant and structure) fusing the children. *)
    val coupling : rhs -> constant Coupling.t

(* In renormalized perturbation theory, couplings come in different orders
   of the loop expansion.  Be prepared: [val order : rhs -> int] *)

(* \begin{dubious}
     The concrete return type [wf list] is here only for the benefit
     of [Target] and could become [wf children] in a more refined
     interface \ldots
   \end{dubious} *)
    val children : rhs -> wf list

(* Fusions come in two types: fusions of wave functions to off-shell wave
   functions:
   \begin{equation*}
     \phi'(p+q) = \phi_1(p)\phi_2(q)
   \end{equation*} *)
    type fusion
    val lhs : fusion -> wf
    val rhs : fusion -> rhs list

(* and products at the keystones:
   \begin{equation*}
     \braket{\phi'(-p-q)|\phi_1(p)\phi_2(q)}
   \end{equation*} *)
    type braket
    val bra : braket -> wf
    val ket : braket -> rhs list

(* [amplitude goldstones incoming outgoing] calculates the
   amplitude for scattering of [incoming] to [outgoing].  If
   [goldstones] is true, also non-propagating off-shell Goldstone
   amplitudes are included to allow the checking of Slavnov-Taylor
   identities.  [selectors] is an instance of [Cascade.T.selectors]
   and used to select certain parts of an amplitude, see
   section~\ref{sec:cascades}. *)
    type amplitude
    type amplitude_sans_color
    type selectors
    type slicings
    val amplitudes : bool -> selectors -> slicings option ->
      flavor_sans_color list -> flavor_sans_color list -> amplitude list
    val amplitudes_all_orders : bool -> selectors ->
      flavor_sans_color list -> flavor_sans_color list -> amplitude list
    val amplitude_sans_color : bool -> selectors ->
      flavor_sans_color list -> flavor_sans_color list -> amplitude_sans_color

(* How a given wave function depends on other wave functions and
   couplings.   This is used for finding subexpressions common
   among different color flow amplitudes. *)
    val dependencies : amplitude -> wf -> (wf, coupling) Tree2.t

(* We should be precise regarding the semantics of the following functions, since
   modules implementating [Target] must not make any mistakes interpreting the
   return values.  Instead of calculating the amplitude
   \begin{subequations}
   \begin{equation}
   \label{eq:physical-amplitude}
     \Braket{f_3,p_3,f_4,p_4,\ldots|T|f_1,p_1,f_2,p_2}
   \end{equation}
   directly, O'Mega calculates the---equivalent, but more symmetrical---crossed
   amplitude 
   \begin{equation}
     \Braket{\bar f_1,-p_1,\bar f_2,-p_2,f_3,p_3,f_4,p_4,\ldots|T|0}
   \end{equation}
   For the benefit of the people implementing [Model]s, however,
   all flavors are represented internally by the charge conjugates
   \begin{equation}
   \label{eq:internal-amplitude}
     A(f_1,-p_1,f_2,-p_2,\bar f_3,p_3,\bar f_4,p_4,\ldots)
   \end{equation}
   \end{subequations}
   Indeed, the vertex and corresponding term in the lagrangian
   \begin{equation}
     \parbox{26\unitlength}{%
       \fmfframe(5,3)(5,3){%
         \begin{fmfgraph*}(15,20)
           \fmfleft{v}
           \fmfright{p,A,e}
           \fmflabel{$\mathrm{e}^-$}{e}
           \fmflabel{$\mathrm{e}^+$}{p}
           \fmflabel{$\mathrm{A}$}{A}
           \fmf{fermion}{p,v,e}
           \fmf{photon}{A,v}
           \fmfdot{v}
         \end{fmfgraph*}}}: \bar\psi\fmslash{A}\psi
   \end{equation}
   suggests to denote the \emph{outgoing} particle by the flavor of the
   \emph{anti}particle and the \emph{outgoing} \emph{anti}particle by the
   flavor of the particle, since this choice allows to represent the vertex
   by a triple
   \begin{equation}
     \bar\psi\fmslash{A}\psi: (\mathrm{e}^+,A,\mathrm{e}^-)
   \end{equation}
   which is more intuitive than the alternative $(\mathrm{e}^-,A,\mathrm{e}^+)$.
   Also, when thinking in terms of building wavefunctions from the outside in,
   the outgoing \emph{antiparticle} is represented by a \emph{particle}
   propagator and vice versa\footnote{Even if this choice will appear slightly
   counter-intuitive on the [Target] side, one must keep in mind that much more
   people are expected to prepare [Model]s.}.
   Note that [incoming] and [outgoing] are the physical flavors as
   in~(\ref{eq:physical-amplitude}) or in the argument of [amplitudes],
   but with the color flow quantum numbers added. *)
    val incoming : amplitude -> flavor list
    val outgoing : amplitude -> flavor list

(* In contrast, [externals] are flavors and momenta as
   in~(\ref{eq:internal-amplitude}) *)
    val externals : amplitude -> wf list

(* Return all off-shell wave functions so that [Target] can allocate
   variables for them. *)
    val variables : amplitude -> wf list

(* Return all [fusion]s in an order so that all right hand sides
   have been computed before they are used. *)
    val fusions : amplitude -> fusion list

(* Return all [braket]s. *)
    type 'a slices
    val brakets : amplitude -> braket list slices

(* Test if an off-shell wave function has been forced on-shell
   or is smeared as as gaussian. *)
    val on_shell : amplitude -> wf -> bool
    val is_gauss : amplitude -> wf -> bool

(* Describe the constraints in the [selectors] argument to [amplitudes]. *)
    val constraints : amplitude -> string option

(* Human readable description of the requested slicings of type [Orders.Conditions.] *)
    val slicings : amplitude -> string list

(* Compute the symmetry factor $\prod_i n_i!$ for identical outgoing
   particles. *)
    val symmetry : amplitude -> int

(* Quickly test whether an amplitude vanishes. *)
    val allowed : amplitude -> bool

(*i
(* \thocwmodulesubsection{Performance Hacks} *)

    val initialize_cache : string -> unit
    val set_cache_name : string -> unit
i*)

(* \thocwmodulesubsection{Diagnostics} *)

(* Compute a list of all charge conservation violating vertices in the [Model]. *)
    val check_charges : unit -> flavor_sans_color list list

(* Count the fusions and propagators that are computed and compare
   to the number of Feynman diagrams appearing in the amplitude. *)
    val count_fusions : amplitude -> int
    val count_propagators : amplitude -> int
    val count_diagrams : amplitude -> int

(* Expand the [DAG] beneath an off-shell wave function into the corresponding
   forest.  \textit{Use with caution for complicated processes!} *)
    val forest : wf -> amplitude -> ((wf * coupling option, wf) Tree.t) list

(* A list of all combinations of off-shell wave functions in the
   Feynman diagrams described by the [DAG].  This could be used for
   phase space mappings, but lies dormant at the moment.
   \begin{dubious}
     At the moment, the result contains empty lists and many
     redundancies.  This should be cleaned up!
   \end{dubious} *)
    val poles : amplitude -> wf list list

(* A list of all $s$-channel poles in the [DAG].  Helpful
   for phase space mappings and for fudging widths. *)
    val s_channel : amplitude -> wf list

(* Prepare \texttt{.dot} files as input fot \texttt{graphviz}
   to draw graphical representations of the tower of of-shell
   wavefunctions and the dag corresponding to the amplitude. *)
    val tower_to_dot : out_channel -> amplitude -> unit
    val amplitude_to_dot : out_channel -> amplitude -> unit

(* \thocwmodulesubsection{WHIZARD} *)

(* Phase space descriptions for \texttt{WHIZARD}.  Once as written
   and once with the incoming particles exchanged.  This way
   we can write a tree starting from the first and one from
   the second incoming particle. *)
    val phase_space_channels : out_channel -> amplitude_sans_color -> unit
    val phase_space_channels_flipped : out_channel -> amplitude_sans_color -> unit

  end

(* \thocwmodulesection{Various Functors generating [Fusion.T]} *)

(* There is more than one way to make fusions, differing in the
   unterlying topology of diagrams. *)

module type Maker =
    functor (P : Momentum.T) -> functor (M : Model.T) ->
      T with type p = P.t
      and type flavor = Orders.Slice(Colorize.It(M)).flavor
      and type flavor_all_orders = Colorize.It(M).flavor
      and type flavor_sans_color = M.flavor
      and type constant = M.constant
      and type selectors = Cascade.Make(M)(P).selectors
      and type slicings = Orders.Conditions(Colorize.It(M)).t
      and type 'a slices = (Orders.Slice(Colorize.It(M)).orders * 'a) list

(*i If we want or need to expose [Make], here's how to do it:

module type Stat =
  sig
    type flavor
    type stat
    exception Impossible
    val stat : flavor -> int -> stat
    val stat_fuse : stat -> stat -> flavor -> stat
    val stat_sign : stat -> int
  end

module type Stat_Maker = functor (M : Model.T) ->
  Stat with type flavor = M.flavor

module Make : functor (PT : Tuple.Poly) (Stat : Stat_Maker)
                      (T : Topology.T with type 'a children = 'a PT.t) -> Maker

i*)

(* Straightforward Dirac fermions vs. slightly more complicated
   Majorana fermions: *)

module Binary : Maker
module Binary_Majorana : Maker

module Mixed23 : Maker
module Mixed23_Majorana : Maker

module Nary : functor (B : Tuple.Bound) -> Maker
module Nary_Majorana : functor (B : Tuple.Bound) -> Maker

(* We can also proceed \'a la~\cite{HELAC:2000}.  Empirically,
   this will use slightly~($O(10\%)$) fewer fusions than the
   symmetric factorization.  Our implementation uses
   significantly~($O(50\%)$) fewer fusions than reported
   by~\cite{HELAC:2000}.  Our pruning of the DAG might
   be responsible for this.  *)

module Helac : functor (B : Tuple.Bound) -> Maker
module Helac_Majorana : functor (B : Tuple.Bound) -> Maker

(* \thocwmodulesection{Multiple Amplitudes} *)

module type Multi =
  sig
    exception Mismatch
    val options : Options.t

    type flavor
    type process = flavor list * flavor list
    type amplitude
    type fusion
    type wf
    type selectors
    type slicings
    type amplitudes

    (* Construct all possible color flow amplitudes for a given process. *)
    val amplitudes : bool -> int option ->
      selectors -> slicings option -> process list -> amplitudes
    val empty : amplitudes

(*i
    (* Precompute the vertex table cache. *)
    val initialize_cache : string -> unit
    val set_cache_name : string -> unit
i*)

    (* The list of all combinations of incoming and outgoing particles
       with a nonvanishing scattering amplitude. *)
    val flavors : amplitudes -> process list

    (* The list of all combinations of incoming and outgoing particles that
       don't lead to any color flow with non vanishing scattering amplitude. *)
    val vanishing_flavors : amplitudes -> process list

    (* The list of all color flows with a nonvanishing scattering amplitude. *)
    val color_flows : amplitudes -> Color.Flow.t list

    (* The list of all valid helicity combinations. *)
    val helicities : amplitudes -> (int list * int list) list

    (* The list of all amplitudes. *)
    val processes : amplitudes -> amplitude list

    (* [(process_table a).(f).(c)] returns the amplitude for the [f]th
       allowed flavor combination and the [c]th allowed color flow as
       an [amplitude option]. *)
    val process_table : amplitudes -> amplitude option array array

    (* The list of all non redundant fusions together with the amplitudes
       they came from. *)
    val fusions : amplitudes -> (fusion * amplitude) list

    (* If there's more than external flavor state, the wavefunctions are
       \emph{not} uniquely specified by [flavor] and [Momentum.t].  This
       function can be used to determine how many variables must be allocated. *)
    val multiplicity : amplitudes -> wf -> int

    (* This function can be used to disambiguate wavefunctions with the same
       combination of [flavor] and [Momentum.t]. *)
    val dictionary : amplitudes -> amplitude -> wf -> int

    (* [(color_factors a).(c1).(c2)] power of~$N_C$ for the given product
       of color flows. *)
    val color_factors : amplitudes -> Color.Flow.factor array array

    (* A description of optional diagram selectors. *)
    val constraints : amplitudes -> string option

    (* Human readable description of the requested slicings of type [Orders.Conditions.] *)
    val slicings : amplitudes -> string list

  end

module type Multi_Maker = functor (Fusion_Maker : Maker) ->
  functor (P : Momentum.T) ->
    functor (M : Model.T) ->
      Multi with type flavor = M.flavor
      and type amplitude = Fusion_Maker(P)(M).amplitude
      and type fusion = Fusion_Maker(P)(M).fusion
      and type wf = Fusion_Maker(P)(M).wf
      and type selectors = Fusion_Maker(P)(M).selectors
      and type slicings = Orders.Conditions(Colorize.It(M)).t

module Multi : Multi_Maker

