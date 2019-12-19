(* UFO_targets.mli --

   Copyright (C) 1999-2017 by

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

(* \thocwmodulesection{Generating Code for UFO Lorentz Structures} *)

module type T =
  sig

    (* NB: The [spins : int list] argument is \emph{not} sufficient
       to determine the domain and codomain of the function.  We
       will need to inspect the flavors, where the Lorentz structure
       is referenced. *)
    val lorentz :
      Format_Fortran.formatter -> string -> Coupling.lorentz array ->
      UFO_Lorentz.t -> unit

    val propagator :
      Format_Fortran.formatter -> string ->
      Coupling.lorentz * Coupling.lorentz ->
      UFO_Lorentz.t -> UFO_Lorentz.t -> unit

    val fuse :
      Algebra.QC.t -> string -> Coupling.lorentzn ->
      string -> string list -> string list -> Coupling.fusen -> unit

    val eps4_g4_g44_decl : Format_Fortran.formatter -> unit -> unit
    val eps4_g4_g44_init : Format_Fortran.formatter -> unit -> unit

  end

module Fortran : T
