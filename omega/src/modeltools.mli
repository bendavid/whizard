(* $Id: modeltools.mli 5864 2014-06-02 13:12:47Z ohl $

   Copyright (C) 1999-2014 by

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

(* \thocwmodulesection{Compilation} *)

module type Flavor =
  sig
    type f
    type c
    val compare : f -> f -> int
    val conjugate : f -> f
  end

module type Fusions =
  sig
    type t
    type f
    type c
    val fuse2 : t -> f -> f -> (f * c Coupling.t) list
    val fuse3 : t -> f -> f -> f -> (f * c Coupling.t) list
    val fuse : t -> f list -> (f * c Coupling.t) list
    val of_vertices :
        (((f * f * f) * c Coupling.vertex3 * c) list
           * ((f * f * f * f) * c Coupling.vertex4 * c) list
           * (f list * c Coupling.vertexn * c) list) -> t
  end

module Fusions : functor (F : Flavor) ->
  Fusions with type f = F.f and type c = F.c

(* \thocwmodulesection{Mutable Models} *)

module Mutable : functor (FGC : sig type f and g and c end) ->
  Model.Mutable with type flavor = FGC.f and type gauge = FGC.g 
  and type constant = FGC.c

(*i
 *  Local Variables:
 *  mode:caml
 *  indent-tabs-mode:nil
 *  page-delimiter:"^(\\* .*\n"
 *  End:
i*)
