(* SU3.mli --

   Copyright (C) 2022-2023 by

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

(* We're computing with a general $N_C$, but [epsilon] and [epsilonbar]
   make only sense for $N_C=3$.  Also some of the terminology alludes
   to $N_C=3$: triplet, sextet, octet. *)

(* We can use all functions from [Birdtracks] that operate on
   [Birdtracks.t] transparently. *)
type t = Birdtracks.t

(* \thocwmodulesection{Constructors specific to $\mathrm{SU}(N_C)$} *)

(* Fundamental representation $N=3$ *)
val delta3 : int -> int -> t

(* ``Adjoint'' representation, but \emph{without} subtracting ghosts,
    i.\,e.~$N\otimes\bar N=9$.  Therefore, the ``8'' is a misnomer! *)
val delta8 : int -> int -> t

(* The trace $\tr(T_aT_b)$ contains additional ghosts *)
val delta8_loop : int -> int -> t

(* Gauge boson in the adjoint representation
   $N\otimes\bar N - N\cdot\text{ghost}$ *)
val gluon : int -> int -> t

(* Symmetric $N\otimes_{\mathrm{S}}N=6$ and
   $N\otimes_{\mathrm{S}}N\otimes_{\mathrm{S}}N=10$. *)
val delta6 : int -> int -> t
val delta10 : int -> int -> t

val t : int -> int -> int -> t
val f : int -> int -> int -> t
val d : int -> int -> int -> t

val epsilon : int list -> t
val epsilon_bar : int list -> t

val t8 : int -> int -> int -> t
val t6 : int -> int -> int -> t
val t10 : int -> int -> int -> t

val k6 : int -> int -> int -> t
val k6bar : int -> int -> int -> t

val delta_of_tableau : int Young.tableau -> int -> int -> t
val t_of_tableau : int Young.tableau -> int -> int -> int -> t

(* The Unit tests are in fact the largest part of this module. *)
module Test : sig val suite : OUnit.test val suite_long : OUnit.test end

