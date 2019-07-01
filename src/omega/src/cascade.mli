(* $Id: cascade.mli 3670 2012-01-21 19:33:07Z jr_reuter $

   Copyright (C) 1999-2012 by

       Wolfgang Kilian <kilian@physik.uni-siegen.de>
       Thorsten Ohl <ohl@physik.uni-wuerzburg.de>
       Juergen Reuter <juergen.reuter@desy.de>
       Christian Speckner <christian.speckner@physik.uni-freiburg.de>

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

module type T =
  sig

    type flavor
    type p

    type t
    val of_string_list : int -> string list -> t
    val to_string : t -> string

(* An opaque type that describes the set of all constraints on an amplitude
   and how to construct it from a cascade description. *)
    type selectors
    val to_selectors : t -> selectors

(* Don't throw anything away: *)
    val no_cascades : selectors

(* [select_wf s is_timelike f p ps] returns [true] iff either the flavor [f] and
    momentum [p] match or \emph{all} combinations of the momenta in [ps]
    are compatible, i.\,e.~$\pm\sum p_i\leq q$ *)
    val select_wf : selectors -> (p -> bool) -> flavor -> p -> p list -> bool

(* [select_p s p ps] same as [select_wf s f p ps], but ignores the flavor [f] *)
    val select_p : selectors -> p -> p list -> bool

(* [on_shell s p] *)
    val on_shell : selectors -> flavor -> p -> bool

(* [is_gauss s p] *)
    val is_gauss : selectors -> flavor -> p -> bool

(* [partition s] returns a partition of the external particles that can not
   be reordered without violating the cascade constraints. *)
    val partition : selectors -> int list list

(* Diagnostics: *)
    val description : selectors -> string option

  end

module Make (M : Model.T) (P : Momentum.T) :
    T with type flavor = M.flavor and type p = P.t

(*i
 *  Local Variables:
 *  mode:caml
 *  indent-tabs-mode:nil
 *  page-delimiter:"^(\\* .*\n"
 *  End:
i*)

