(* $Id: powSet.mli 2468 2010-05-05 16:37:03Z kilian $

   Copyright (C) 1999-2010 by

       Wolfgang Kilian <kilian@hep.physik.uni-siegen.de>
       Thorsten Ohl <ohl@physik.uni-wuerzburg.de>
       Juergen Reuter <juergen.reuter@physik.uni-freiburg.de>

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

(* In the end, this should be generalized from \textit{power set} to
   \textit{lattice} with a notion of subtraction. *)

module type Ordered_Type =
  sig
    type t
    val compare : t -> t -> int

    (* Debugging \ldots *)
    val to_string : t -> string
  end

module type T =
  sig
    type elt
    type t

    val empty : t
    val is_empty : t -> bool
    val union : t list -> t

    val of_lists : elt list list -> t
    val to_lists : t -> elt list list

    (* The smallest set of disjoint subsets that generates the given subset. *)
    val basis : t -> t

    (* Debugging \ldots *)
    val to_string : t -> string
  end

module Make (E : Ordered_Type) : T with type elt = E.t


(*i
 *  Local Variables:
 *  mode:caml
 *  indent-tabs-mode:nil
 *  page-delimiter:"^(\\* .*\n"
 *  End:
i*)
