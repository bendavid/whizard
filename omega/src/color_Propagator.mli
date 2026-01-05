(* color_Propagator.mli --

   Copyright (C) 2022-2026 by

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

(* Possible color flows for a single propagator, as currently
   supported by WHIZARD. *)

(* In a model without $\epsilon$ or $\bar\epsilon$ couplings,
   the color flow can be represented by arrays of identifiers
   (integers) of color flow lines. One array for incoming lines
   and another one for outgoing lines.  In addition, the propagator
   can represent a ghost line.

   If there are only fundamental, conjugate and adjoint
   representations with $T_a$ and $f_{abc}$ couplings,
   there will be at most of incoming and at most one outgoing
   line.  In tensor product representations, there are more than
   one incoming or outgoing color flow line.

   Things become more involved, when there are $\epsilon$ or $\bar\epsilon$
   couplings.  Fortunately, it is not possible to contract two $\epsilon$
   or two $\bar\epsilon$, while pairs of $\epsilon$ and $\bar\epsilon$
   can always be replaced by a sum over color flows. *)

(* For typechecking, it might be beneficial to make these
   abstract or [private] eventually. *)
type cf_in = int
type cf_out = int

(* Note that these do not need to be not mutually recursive,
   since $\epsilon$ can not be nested beneath $\epsilon$ (analogously
   for $\bar\epsilon$) and a $\bar\epsilon$ beneath a $\epsilon$
   (and vice versa) can be expanded as a sum over permuted
   color flows. *)

(* Also note that the [list]s for [eps] and [eps_bar] have
   one element less than [s_eps] and [s_eps_bar].  The latter
   represent fully saturated $\epsilon$ and $\bar\epsilon$,
   while the former have one open index. *)

type eps = cf_out list
type s_eps = cf_out list
type cf_in_or_eps =
  | CF_in of cf_in
  | Epsilon of eps

type eps_bar = cf_in list
type s_eps_bar = cf_in list
type cf_out_or_eps_bar =
  | CF_out of cf_out
  | Epsilon_Bar of eps_bar

(* These types guarantee that there is never a pair
   of $\epsilon$ and $\bar\epsilon$ that has yet to be contracted. *)

type flow = cf_in PArray.t * cf_out PArray.t
type flow_eps = cf_in_or_eps PArray.t * cf_out PArray.t
type flow_eps_bar = cf_in PArray.t * cf_out_or_eps_bar PArray.t

(* Note that the ghosts might carry fully saturated
   $\epsilon$ and $\bar\epsilon$ originating from deeper
   in the DAG. *)

type t =
  | Flow of flow
  | Flow_with_Epsilons of flow_eps * s_eps list
  | Flow_with_Epsilon_Bars of flow_eps_bar * s_eps_bar list
  | Ghost
  | Ghost_with_Epsilons of s_eps list
  | Ghost_with_Epsilon_Bars of s_eps_bar list

(* Project onto [Flow], if possible. *)
val normalize : t -> t

(* Simple constructors. *)
val white : t
val of_lists : int list -> int list -> t

(* Simple predicates. *)
val is_white : t -> bool

(* Reverse arrows. *)
val conjugate : t -> t

(* Some ordering. *)
val compare : t -> t -> int
val equal : t -> t -> bool

(* Allowed as (a part of) an identifier in Fortran
   and other programming languages. *)
val to_symbol : t -> string

(* Pretty printer for the toplevel. *)
val to_string : t -> string
val pp : Format.formatter -> t -> unit
