(* color.mli --

   Copyright (C) 1999-2019 by

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

(* \thocwmodulesection{Quantum Numbers} *)

(* Color is not necessarily the~$\textrm{SU}(3)$ of QCD.  Conceptually,
   it can be any \emph{unbroken} symmetry (\emph{broken} symmetries correspond
   to [Model.flavor]).  In order to keep the group theory simple, we confine
   ourselves to the fundamental and adjoint representation
   of a single~$\textrm{SU}(N_C)$ for the moment.  Therefore,
   particles are either color singlets or live in the defining
   representation of $\textrm{SU}(N_C)$: [SUN]$(|N_C|)$, its conjugate
   [SUN]$(-|N_C|)$ or in the adjoint representation of
   $\textrm{SU}(N_C)$: [AdjSUN]$(N_C)$. *)

type t = Singlet | SUN of int | AdjSUN of int

val conjugate : t -> t
val compare : t -> t -> int

(* \thocwmodulesection{Color Flows} *)

module type Flow =
  sig

    type color
    type t = color list * color list
    val rank : t -> int

    val of_list : int list -> color
    val ghost : unit -> color
    val to_lists : t -> int list list
    val in_to_lists : t -> int list list
    val out_to_lists : t -> int list list
    val ghost_flags : t -> bool list
    val in_ghost_flags : t -> bool list
    val out_ghost_flags : t -> bool list

(* A factor is a list of powers
   \begin{equation}
     \sum_{i}
        \left( \frac{\ocwlowerid{num}_i}{\ocwlowerid{den}_i}
                  \right)^{\ocwlowerid{power}_i}
   \end{equation} *)
    type power = { num : int; den : int; power : int }
    type factor = power list

    val factor : t -> t -> factor
    val zero : factor

  end

module Flow : Flow

(* \thocwmodulesection{Color Structure of Vertices } *)

(* In order for the [Colorize]r to work on fusions, we must
   permit to choose any permutation of the color tensors. *)

(* Since $f_{a_1a_2a_3}$ and $\epsilon_{i_1i_2i_3}$ are totally
   antisymmetric, we can take care of the permutations with a sign.

   For the other invariant tensors of rank $\le 3$, it suffices
   to specify a pair, which is symmetric in the case of the adjoint
   representation, but \emph{not} in the case of $N\otimes\bar N$.
   We can however disambiguate the order in the latter case by
   looking at the color representation of of the particles involved.  *)

(* TODO: support $d_{abc}$. *)

type pair3 =
  | P3_12 | P3_23 | P3_31
  | P3_21 | P3_32 | P3_13

type vertex3 =
  | Legacy3 (* only for debugging *)
  | Trivial3
  | Delta3 of pair3 (* $\delta_{\bar\imath_2i_3}$ *)
  | Delta8 of pair3 (* $\delta^{a_2a_3}$ *)
  | T of pair3 (* $T^{a_1}_{\bar\imath_2i_3}$ *)
  | F (* $f^{a_1a_2a_3}$ *)
  | Eps (* $\epsilon_{i_2i_3i_4}$
       and $\epsilon_{\bar\imath_2\bar\imath_3\bar\imath_4}$ *)

(* For invariant tensors of rank $\le 4$, there are more
   possibilities.  We can choose a pair, which is equivalent
   to choosing two pairs, as long as the order is irrelevant
   or can be recovered. *)

type pair4 =
  | P4_12
  | P4_13
  | P4_14
  | P4_23
  | P4_24
  | P4_34

(* We can choose a triplet.  *)

type triplet4 =
  | P4_123
  | P4_234
  | P4_341
  | P4_412

(* We can choose a cyclic permutation of three indices, when the
   choice of the first index is irrelevant by symmetry. *)

type cyclic4 =
  | C4_234
  | C4_342
  | C4_423

type vertex4 =
  | Legacy4 (* only for debugging *)
  | Trivial4
  | Delta13 of pair4 (* $\delta_{\bar\imath_3i_4}$ *)
  | Delta18 of pair4 (* $\delta^{a_3a_4}$ *)
  | Delta38 of pair4 (* $\delta_{\bar\imath_1i_2}\delta^{a_3a_4}$ *)
  | Delta33 of cyclic4 (* $\delta_{\bar\imath_1i_2}\delta_{\bar\imath_3i_4}$ *)
  | Delta88 of cyclic4 (* $\delta^{a_1a_2}\delta^{a_3a_4}$ *)
  | TT of cyclic4 (* $T^a_{\bar\imath_1i_2}T^a_{\bar\imath_3i_4}$ *)
  | FF of (int * int) * (int * int) (* $f^{aa_1a_2}f^{aa_3a_4}$ *)
  | TF of pair4 (* $T^a_{\bar\imath_1i_2}f^{aa_3a_4}$ *)
  | T4 of triplet4 (* $T^{a_2}_{\bar\imath_3i_4}$ *)
  | F4 of triplet4 (* $f^{a_2a_3a_4}$ *)
  | Eps4 of triplet4 (* $\epsilon_{i_2i_3i_4}$
                    and $\epsilon_{\bar\imath_2\bar\imath_3\bar\imath_4}$ *)

type vertex =
  | Legacy (* only for debugging *)
  | Trivial

val canonicalize_ff :
  (int * int) * (int * int) -> int * ((int * int) * (int * int))
