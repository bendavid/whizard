(* uFO_targets.mli --

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
      UFOx.Lorentz.t -> unit

    val fusion2 :
      Algebra.QC.t -> string -> Coupling.lorentz3 ->
      string -> string -> string -> string -> string -> Coupling.fuse2 -> unit
    val fusion3 :
      Algebra.QC.t -> string -> Coupling.lorentz4 ->
      string -> string -> string -> string -> string ->
      string -> string -> Coupling.fuse3 -> unit
    val fusionn :
      Algebra.QC.t -> string -> Coupling.lorentzn ->
      string -> string list -> string list -> Coupling.fusen -> unit

    val eps4_g4_g44_decl : Format_Fortran.formatter -> unit -> unit
    val eps4_g4_g44_init : Format_Fortran.formatter -> unit -> unit

  end

module Fortran : T

(* only for debugging: *)
module Lorentz_Fusion : sig
  type t
  val parse : Coupling.lorentz list -> UFOx.Lorentz.t -> t
  val to_string : t -> string
end

module type Dirac =
  sig
    type qc = Algebra.QC.t
    type t = qc array array
    val zero : qc
    val one : qc
    val minus_one : qc
    val i : qc
    val minus_i : qc
    val unit : t
    val null : t
    val gamma0 : t
    val gamma1 : t
    val gamma2 : t
    val gamma3 : t
    val gamma5 : t
    val gamma : t array
    val cc : t
    val neg : t -> t
    val add : t -> t -> t
    val sub : t -> t -> t
    val mul : t -> t -> t
    val times : qc -> t -> t
    val transpose : t -> t
    val adjoint : t -> t
    val conj : t -> t
    val product : t list -> t
    val test_suite : OUnit.test
  end

module Dirac : Dirac
