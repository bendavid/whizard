(* arrow.mli --

   Copyright (C) 2022-2025 by

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

(* The datatypes [Arrow.free] and [Arrow.factor] will be used as
   building blocks for [Birdtracks.t] below. *)

(* For fundamental and adjoint representations, the endpoints
   of arrows are uniquely specified by a vertex (which will
   be represented by a number).  For representations with more
   than one outgoing or incoming arrow, we need an additional index.
   This is abstracted in the [endpoint] type. *)
type endpoint = private
  | I of int
  | M of int * int

(* Provide a canonical ordering of endpoints: *)
val compare_endpoints : endpoint -> endpoint -> int

(* Endpoints can be the the tip or tail of an arrow or a ghost.
   Using incompatible types for each forces us to export three
   identical copies of some functions, but should help to avoid
   some simple mistakes, in which tips and tails are confused. *)
type tip = private endpoint
type tail = private endpoint
type ghost = private endpoint

(* Type safe aliases for [compare_endpoints]. *)
val compare_tips : tip -> tip -> int
val compare_tails : tail -> tail -> int
val compare_ghosts : ghost -> ghost -> int

(* The position of the endpoint is encoded as an integer, which
   can be mapped, if necessary. *)
val position_tip : tip -> int
val position_tail : tail -> int
val position_ghost : ghost -> int
val relocate_tip : (int -> int) -> tip -> tip
val relocate_tail : (int -> int) -> tail -> tail
val relocate_ghost : (int -> int) -> tail -> tail

(* An [Arrow.t] is either a genuine arrow or a ghost. The rationale
   for the polymorphic definition is explained below. *)

type ('tail, 'tip, 'ghost) t =
  | Arrow of 'tail * 'tip
  | Ghost of 'ghost

(* $\epsilon_{i_1i_2\cdots i_n}$ and $\bar\epsilon^{i_1i_2\cdots i_n}$
   are represented by lists~$\lbrack i_1; i_2; \ldots; i_n \rbrack$. *)

type 'tip eps = 'tip list
type 'tail eps_bar = 'tail list

(* We distinguish [free] arrows, $\epsilon$s and $\bar\epsilon$s
   that must not contain
   summation indices from [factor]s that may.  Indices are
   opaque.  [('tail, 'tip, 'ghost) t] has been defined polymorphic
   above so that we can use richer ['tail], ['tip] and ['ghost] in
   [factor] to identify summation indices.
   Not that it is \emph{not} enough to identify summation indices
   by negative integers alone.  Due to the presence of double arrows
   representing gluons, we must distinguish summation indices
   in the left factor of a product from those in the right factor. *)

type free = (tail, tip, ghost) t
type free_eps = tip eps
type free_eps_bar = tail eps_bar
type factor
type factor_eps
type factor_eps_bar

val epsilon : tip list -> free_eps
val epsilon_bar : tail list -> free_eps_bar

val relocate : (int -> int) -> free -> free
val rev : free -> free
val rev_eps : free_eps -> free_eps_bar
val rev_eps_bar : free_eps_bar -> free_eps

(* Useful for testing compatibility when adding terms. *)
val tips : free -> tip list
val tips_eps : free_eps -> tip list
val tails : free -> tail list
val tails_eps_bar : free_eps_bar -> tail list

(* For debugging, logging, etc. *)
val free_to_string : free -> string
val free_eps_to_string : free_eps -> string
val free_eps_bar_to_string : free_eps_bar -> string
val factor_to_string : factor -> string
val factor_eps_to_string : factor_eps -> string
val factor_eps_bar_to_string : factor_eps_bar -> string

(* Turn the [endpoint]s satisfying the predicate into a
   left or right hand side summation index.  Left and right
   refer to the two factors in a product and
   we must only match arrows with [endpoint]s in both
   factors, not double lines on either side.
   Typically, the predicate will be set up to select only the
   summation indices that appear on both sides.*)
    
val to_left_factor : (endpoint -> bool) -> free -> factor
val to_left_factor_eps : (endpoint -> bool) -> free_eps -> factor_eps
val to_left_factor_eps_bar : (endpoint -> bool) -> free_eps_bar -> factor_eps_bar
val to_right_factor : (endpoint -> bool) -> free -> factor
val to_right_factor_eps : (endpoint -> bool) -> free_eps -> factor_eps
val to_right_factor_eps_bar : (endpoint -> bool) -> free_eps_bar -> factor_eps_bar

(* The incomplete inverse [of_factor] raises an exception
   if there are remaining summation indices.  [is_free] can
   be used to check first. *)
val of_factor : factor -> free
val of_factor_eps : factor_eps -> free_eps
val of_factor_eps_bar : factor_eps_bar -> free_eps_bar
val is_free : factor -> bool
val is_free_eps : factor_eps -> bool
val is_free_eps_bar : factor_eps_bar -> bool

(* Return all the endpoints of the arrow that have a [position]
   encoded as a negative integer.  These are treated as summation
   indices in our applications. *)
val negatives : free -> endpoint list
val negatives_eps : free_eps -> endpoint list
val negatives_eps_bar : free_eps_bar -> endpoint list

(* Return the list of all positions of endpoints corresponding to
   adjoint representations.  To be precise, it's the list of
   integers~[i] in endpoints [I i] that appear at least once as tip
   and as tail.  While it is an error to appear \emph{more}
   than once as either, this is not checked in the current implementation. *)
val adjoints : free list -> int list
val adjoints_eps : free list -> free_eps NEList.t -> int list
val adjoints_eps_bar : free list -> free_eps_bar NEList.t -> int list

(* We will need to test whether an arrow represents a ghost. *)
val is_ghost : free -> bool

(* An arrow looping back to itself. *)
val is_tadpole : factor -> bool

(* Check if the [tip]s and [tail]s of a list of arrows that
   belong to the same positions are in a canonical order.
   This can be used to weed out color flows that are equivalent
   after applying the symmetrizations and antisymmetrizations
   in irreps described by Young tableaux. *)
val in_canonical_order : free list -> bool

(* [endpoints (position, n)] construct a list of [n] endpoints at
   [position] that can be concatenated with other such lists and
   then permuted.  Examples: [endpoints (42,1) = [I 42] ] and
   [endpoints (42,2) = [M (42,0); M (42,1)] ]. *)
val endpoints : int * int -> endpoint list
val make_tips : int * int -> tip list
val make_tails : int * int -> tail list

(* Merging an arrow with another arrow, $\epsilon$ or $\bar\epsilon$
   can give a variety of results: *)

type merge =
  | Match of factor (* a tip fits the other's tail: make one arrow out of two *)
  | Ghost_Match (* two matching ghosts *)
  | Loop_Match (* both tips fit both tails: drop the arrows *)
  | Mismatch (* ghost meets arrow: discard *)
  | No_Match (* nothing to be done *)

val merge_arrow_arrow : factor -> factor -> merge

(* We can narrow this for $\epsilon$ and $\bar\epsilon$,
   where [Loop_Match] and [Ghost_Match] are impossible! *)

type 'a merge_eps =
  | Match_Eps of 'a  (* a tip fits the other's tail: make one arrow out of two *)
  | Mismatch_Eps (* ghost meets arrow: discard *)
  | No_Match_Eps (* nothing to be done *)

val merge_arrow_eps : factor -> factor_eps -> factor_eps merge_eps
val merge_arrow_eps_bar : factor -> factor_eps_bar -> factor_eps_bar merge_eps

(* In order to merge an~$\epsilon$ with an $\bar\epsilon$, we use
   \begin{equation}
      \forall n, N \in\mathbf{N}, 2\le n \le N:\;
      \epsilon_{i_1i_2\cdots i_n} \bar\epsilon^{j_1j_2\cdots j_n}
        = \sum_{\sigma\in S_n} (-1)^{\varepsilon(\sigma)}
            \delta_{i_1}^{\sigma(j_1)} 
            \delta_{i_2}^{\sigma(j_2)} 
            \cdots
            \delta_{i_n}^{\sigma(j_n)}\,,
   \end{equation}
   where~$N=\delta_i^i$ is the dimension, to replace the pair by two lists of
   lists of arrows: the first corresponding to the even permutations, the
   second to the odd ones.
   Return [None], if the rank of $\epsilon$ and $\bar\epsilon$ don't match. *)

(* See section~\ref{sec:evaluation-of-epsilon-tensors}
   on pages~\pageref{sec:evaluation-of-epsilon-tensors}ff for a justification
   for using it also in the case~$n\not=N$. *)

val merge_eps_eps_bar : factor_eps -> factor_eps_bar -> (factor list list * factor list list) option

(* Break up an arrow [tee a (i => j) -> [i => a; a => j]], i.\,e.~insert
   a gluon. Returns an empty list for a ghost and raises an exception
   for~$\epsilon$ and~$\bar\epsilon$. *)
val tee : int -> free -> free list

(* [dir i j arrow] returns the direction of the arrow relative to [j => i].
   Returns 0 for a ghost and raises an exception for~$\epsilon$
   and~$\bar\epsilon$. *)
val dir : int -> int -> free -> int

(* It's intuitive to use infix operators to construct the lines. *)
val single : tail -> tip -> free
val double : endpoint -> endpoint -> free list
val ghost : endpoint -> free

module Infix : sig

  (* [single i j] or [i => j] creates a single line from [i] to [j] and
     [i ==> j] is a shorthard for [[i => j]]. *)
  val (=>) : int -> int -> free
  val (==>) : int -> int -> free list

  (* [double i j] or [i <=> j] creates a double line from [i] to [j] and back. *)
  val (<=>) : int -> int -> free list

  (* Single lines with subindices at the tip and/or tail *)
  val (>=>) : int * int -> int -> free
  val (=>>) : int -> int * int -> free
  val (>=>>) : int * int -> int * int -> free

  (* [?? i] creates a ghost at [i]. *)
  val (??) : int -> free

(* NB: I wanted to use [~~] instead of [??], but ocamlweb can't handle
   operators starting with [~] in the index properly. *)

end

(* These used to be called [epsilon] and [epsilon_bar], but they are
   not general enough! *)
val epsilon0 : int list -> free_eps
val epsilon0_bar : int list -> free_eps_bar

(* [chain [1;2;3]] is a shorthand for [[1 => 2; 2 => 3]] and
   [cycle [1;2;3]] for [[1 => 2; 2 => 3; 3 => 1]].  Other lists
   and edge cases are handled in the natural way. *)
val chain : int list -> free list
val cycle : int list -> free list

type matching_adjoint_arrows = Tee | Reflex

(* [adjoint_arrows_opt a arrows] searches for arrows starting and ending
   at [I a]. If a matching pair is found, the arrows are connected and
   the resulting [arrow] is returned together with the remaining arrows
   [other] as [Some (Tee, arrow :: other)].  If both tip and tail belong
   to the same arrow, [Some (Reflex, other)] is returned instead.
   If there is no match [None] is returned.  In the case of multiple
   matches, the exception [Invalid_Arg] is raised. *)
  
val adjoint_arrows_opt : int -> free list -> (matching_adjoint_arrows * free list) option
val adjoint_eps_opt : int -> free list -> free_eps NEList.t ->
                      (matching_adjoint_arrows * free list * free_eps NEList.t) option
val adjoint_eps_bar_opt : int -> free list -> free_eps_bar NEList.t ->
                      (matching_adjoint_arrows * free list * free_eps_bar NEList.t) option

module Test : sig val suite : OUnit.test val suite_long : OUnit.test end

(* Pretty printer for the toplevel. *)
val pp_free : Format.formatter -> free -> unit
val pp_factor : Format.formatter -> factor -> unit
