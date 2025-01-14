(* birdtracks.mli --

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

(* In this module, we implement birdtracks operations on expressions
   of [type t] as generally as possible.
   Module [SU3] (cf.~chapter~\ref{sec:su3}), will provide the group
   specific constructors for [type t] in the special
   case $\mathrm{SU}(N_C)$ or $\mathrm{SU}(3)$. *)

(* \thocwmodulesection{Types} *)

(* If there are no $\epsilon$s or $\bar\epsilon$s, a term is simply
   a list of arrows with a coefficient that is a polynomial,
   allowing negative powers, in $N_C$.  Here the type ['a] of arrows
   is polymorphic, because [Arrow] has both [free] arrows without
   summation indices and [factor] arrows that contain summation
   indices. *)
type 'a aterm = { coeff : Algebra.Laurent.t; arrows : 'a list }

(* If there are $\epsilon$s, we add them \ldots *)
type ('a, 'e) eterm = 'a aterm * 'e NEList.t

(* \ldots{} and the same for $\bar\epsilon$s. *)
type ('a, 'b) bterm = 'a aterm * 'b NEList.t

(* Assuming that $\epsilon$-$\bar\epsilon$-pairs are always
   reduced as soon as possible, these three alternatives
   are exhaustive. *)
type ('a, 'e, 'b) term =
  | Arrows of 'a aterm
  | Epsilons of ('a, 'e) eterm
  | Epsilon_Bars of ('a, 'b) bterm

(* In the public interface, we deal only with [free] indices, without
   summation indices. *)
type free = (Arrow.free, Arrow.free_eps, Arrow.free_eps_bar) term

(* An expression is just a sum of terms. *)
type t = free list

(* \thocwmodulesection{Functions} *)

(* Reverse all arrows and exchange $\epsilon$s and $\bar\epsilon$. *)
val rev : t -> t

(* Map the ['a aterm] component and leave the epsilons alone. *)
val map_term : ('a aterm -> 'c aterm) -> ('a, 'e, 'b) term -> ('c, 'e, 'b) term
val map_term_opt : ('a aterm -> 'c aterm option) -> ('a, 'e, 'b) term -> ('c, 'e, 'b) term option

(* Return the list of all positions of endpoints corresponding to
   adjoint representations (cf.~[Arrow.adjoints]). *)
val adjoints : t -> int list

(* Test for ghosts in an expression. *)
val haunted : t -> bool

(* Filter out all terms containing a ghost. *)
val exorcise : t -> t

(* Strip out redundancies. *)
val canonicalize : t -> t

(* Substitute a specific value for $N_C$.  Mainly for debugging. *)
val with_nc : int -> t -> t

(* Debugging, logging, etc. *)
val to_string : t -> string
val to_string_raw : t -> string

(* Extract the number if the birdtrack contains no arrows, $\epsilon$s or $\bar\epsilon$s. *)
val number : t -> Algebra.Laurent.t option

(* Test for trivial color flows that correspond to unity. *)
val is_unit : t -> bool

(* Test for vanishing coefficients. *)
val is_null : t -> bool

(* [is_multiple x y] returns [Some (cx, cy)] iff [const cy *** x = const cx *** y]
   and [None] otherwise. *)
val is_multiple : t -> t -> (Algebra.Laurent.t * Algebra.Laurent.t) option

(* Purely numeric factors, implemented as Laurent polynomials
   (cf.~[Algebra.Laurent] in~$N_C$ with complex rational
   coefficients and without arrows. *)
val const : Algebra.Laurent.t -> t
val null : t (* $0$ *)
val one : t (* $1$ *)
val two : t (* $2$ *)
val minus : t (* $-1$ *)
val int : int -> t (* $n$ *)
val fraction : int -> t (* $1/n$ *)
val nc : t (* $N_C$ *)
val over_nc : t (* $1/N_C$ *)
val imag : t (* $\ii$ *)

(* Shorthand: $\{(c_i,p_i)\}_i\to \sum_i c_i (N_C)^{p_i}$*)
val ints : (int * int) list -> t

val scale : Algebra.Laurent.c -> t -> t

val sum : t list -> t
val diff : t -> t -> t
val times : t -> t -> t
val multiply : t list -> t

(* For convenience, here are infix versions of the above operations. *)
module Infix : sig
  val ( +++ ) : t -> t -> t
  val ( --- ) : t -> t -> t
  val ( *** ) : t -> t -> t
end

(* We can compute the $f_{abc}$ and $d_{abc}$ invariant tensors
   from the generators of an arbitrary representation:
   \begin{subequations}
     \begin{align}
       f_{a_1a_2a_3} &=
        - \ii \tr\left(T_{a_1}\left\lbrack T_{a_2},T_{a_3}\right\rbrack_-\right)
          = - \ii \tr\left(T_{a_1}T_{a_2}T_{a_3}\right)
            + \ii \tr\left(T_{a_1}T_{a_3}T_{a_2}\right) \\
       d_{a_1a_2a_3} &=
         \tr\left(T_{a_1}\left\lbrack T_{a_2},T_{a_3}\right\rbrack_+\right)
          =   \tr\left(T_{a_1}T_{a_2}T_{a_3}\right)
            + \tr\left(T_{a_1}T_{a_3}T_{a_2}\right)\,
     \end{align}
   \end{subequations}
   assuming the normalization $ \tr(T_aT_b) = \delta_{ab}$.

   NB: this uses the summation indices $-1$, $-2$ and $-3$.  Therefore
   it \emph{must not} appear unevaluated more than once in a product! *)
val f_of_rep : (int -> int -> int -> t) -> int -> int -> int -> t
val d_of_rep : (int -> int -> int -> t) -> int -> int -> int -> t

(* Rename the indices of endpoints in a birdtrack.  This is required
   by our application in [Colorize.It] to match the permutations
   of lines at a vertex. *)
val relocate : (int -> int) -> t -> t

(* Pretty printer for the toplevel. *)
val pp : Format.formatter -> t -> unit

(* Support for unit tests. *)
val equal : t -> t -> unit
val assert_zero_vertex : t -> unit

module Test : sig val suite : OUnit.test val suite_long : OUnit.test end
