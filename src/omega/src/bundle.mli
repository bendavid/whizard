(* $Id: bundle.mli 2403 2010-04-23 20:28:27Z ohl $

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

module type Elt_Base =
  sig
    type elt
    type base
    val compare_elt : elt -> elt -> int
    val compare_base : base -> base -> int
  end

module type Projection =
  sig
    include Elt_Base

    (* $\pi: E \to B$ *)
    val pi : elt -> base

  end

module type T =
  sig

    type t

    type elt
    type fiber = elt list
    type base

    val add : elt -> t -> t
    val of_list : elt list -> t

    (* $\pi: E \to B$ *)
    val pi : elt -> base

    (* $\pi^{-1}: B \to E$ *)
    val inv_pi : base -> t -> fiber

    val base : t -> base list

    (* $\pi^{-1}\circ\pi$ *)
    val fiber : elt -> t -> fiber

    val fibers : t -> (base * fiber) list
  end

module Make (P : Projection) : T with type elt = P.elt and type base = P.base

(* The same thing again, but with a projection that is not hardcoded, but passed
   as an argument at runtime. *)

module type Dyn =
  sig
    type t
    type elt
    type fiber = elt list
    type base
    val add : (elt -> base) -> elt -> t -> t
    val of_list : (elt -> base) -> elt list -> t
    val inv_pi : base -> t -> fiber
    val base : t -> base list
    val fiber : (elt -> base) -> elt -> t -> fiber
    val fibers : t -> (base * fiber) list
  end

module Dyn (P : Elt_Base) : Dyn with type elt = P.elt and type base = P.base

(*i
 *  Local Variables:
 *  mode:caml
 *  indent-tabs-mode:nil
 *  page-delimiter:"^(\\* .*\n"
 *  End:
i*)
