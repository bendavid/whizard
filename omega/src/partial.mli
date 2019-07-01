(* $Id: partial.mli 4015 2013-01-03 16:04:18Z jr_reuter $

   Copyright (C) 1999-2013 by

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

(* Partial maps that are constructed from assoc lists. *)

module type T =
  sig
    type domain
    type 'a t
    val of_list : (domain * 'a) list -> 'a t
    val of_lists : domain list -> 'a list -> 'a t
    val apply : 'a t -> domain -> 'a
  end

module Make : functor (D : Map.OrderedType) -> T with type domain = D.t
module Test : sig val suite : OUnit.test end
