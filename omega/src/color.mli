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

(* \thocwmodulesection{Vertex Color Flows} *)

(* \begin{dubious}
     It might be beneficial, to use the color flow representation
     here.  This will simplify the colorizer at the price of
     some complexity in [UFO] or here.
   \end{dubious} *)

module type Test =
  sig
    val suite : OUnit.test
  end

module type Arrow =
  sig
    type endpoint
    val position : endpoint -> int
    val relocate : (int -> int) -> endpoint -> endpoint
    type tip = endpoint
    type tail = endpoint
    type ghost = endpoint
    type ('tail, 'tip, 'ghost) t =
      | Arrow of 'tail * 'tip
      | Ghost of 'ghost
    type free = (tail, tip, ghost) t
    type factor
    val free_to_string : free -> string
    val factor_to_string : factor -> string
    val map : (endpoint -> endpoint) -> free -> free
    val to_left_factor : (endpoint -> bool) -> free -> factor
    val to_right_factor : (endpoint -> bool) -> free -> factor
    val of_factor : factor -> free
    val negatives : free -> endpoint list
    val is_free : factor -> bool
    val is_ghost : free -> bool
    val single : endpoint -> endpoint -> free
    val double : endpoint -> endpoint -> free list
    val ghost : endpoint -> free
    val chain : int list -> free list
    val cycle : int list -> free list
    type merge =
      | Match of factor
      | Ghost_Match
      | Loop_Match
      | Mismatch
      | No_Match
    val merge : factor -> factor -> merge
    module BinOps : sig
      val (=>) : int -> int -> free
      val (==>) : int -> int -> free list
      val (<=>) : int -> int -> free list
      val (>=>) : int * int -> int -> free
      val (=>>) : int -> int * int -> free
      val (>=>>) : int * int -> int * int -> free
      val (??) : int -> free
    end
    module Test : Test
  end

module Arrow : Arrow

module type Propagator =
  sig
    type cf_in = int
    type cf_out = int
    type t = W | I of cf_in | O of cf_out | IO of cf_in * cf_out | G
    val to_string : t -> string
  end

module Propagator : Propagator

module type Birdtracks =
  sig
    type t
    val to_string : t -> string
    val pp : Format.formatter -> t -> unit
    val trivial : t -> bool
    val is_null : t -> bool
    val unit : t
    val null : t
    val two : t
    val half : t
    val third : t
    val minus : t
    val nc : t
    val imag : t
    val ints : (int * int) list -> t
    val const : Algebra.Laurent.t -> t
    val times : t -> t -> t
    val multiply : t list -> t
    val scale : Algebra.Q.t -> t -> t
    val sum : t list -> t
    val diff : t -> t -> t
    val f_of_rep : (int -> int -> int -> t) -> int -> int -> int -> t
    val d_of_rep : (int -> int -> int -> t) -> int -> int -> int -> t
    module BinOps : sig
      val ( +++ ) : t -> t -> t
      val ( --- ) : t -> t -> t
      val ( *** ) : t -> t -> t
    end
    val map : (int -> int) -> t -> t
    val fuse : int -> t -> Propagator.t list -> (Algebra.QC.t * Propagator.t) list
    module Test : Test
  end

module Birdtracks : Birdtracks

module type SU3 =
  sig
    include Birdtracks
    val delta3 : int -> int -> t
    val delta8 : int -> int -> t
    val delta8_loop : int -> int -> t
    val gluon : int -> int -> t
    val t : int -> int -> int -> t
    val f : int -> int -> int -> t
    val d : int -> int -> int -> t
    val epsilon : int -> int -> int -> t
    val epsilonbar : int -> int -> int -> t
    val t6 : int -> int -> int -> t
    val k6 : int -> int -> int -> t
    val k6bar : int -> int -> int -> t
  end

module SU3 : SU3
module U3 : SU3

module Vertex : SU3
