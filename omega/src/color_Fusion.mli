(* color_Fusion.mli --

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

(* This module uses a vertex color flow of type [Birdtracks.t]
   (which aliased to, e.\,g., [SU3.t]), to fuse a list of
   [Color_Propagator.t]. *)

(* [fuse nc vertex children] use the color flows in the [vertex]
   to combine the color flows in the incoming [children] and return
   the color flows for outgoing particle together with their weights. *)

val fuse : int -> Birdtracks.t -> Color_Propagator.t list -> (Algebra.Laurent.c * Color_Propagator.t) list

(* \begin{dubious}
     At the moment, [nc] is substituted for $N_C$.  It this necessary
     or the desired behavior?  Can we use
     [(Algebra.Laurent.t * Color_Propagator.t) list]
     as return type instead, in order to be able to write the symbolic
     expression to the amplitude?  This would necessitate changes in
     many places, however.
   \end{dubious} *)

(* Unit tests. *)
module Test : sig val suite : OUnit.test val suite_long : OUnit.test end
