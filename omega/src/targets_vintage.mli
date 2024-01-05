(* targets_vintage.mli --

   Copyright (C) 1999-2024 by

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

(* This is the original implementation of [Target_Fortran().print_current] for
   hard coded models with [Coupling.V3] and [Coupling.V4] vertices only.
   It was adequate for the Standard Model and simple extensions upto the MSSM.
   The extension to higher dimensional operators became more and more baroque ---
   to the extent to be almost unmaintainable.  In order to make [Target_Fortran]
   maintainable, this code has been factored out. *)

(* Output routines for fermion couplings. *)
module type Fermions =
  sig
    open Coupling
    val print_current : int * fermionbar * boson * fermion ->
      string -> string -> string -> fuse2 -> unit
    val print_current_mom : int * fermionbar * boson * fermion ->
      string -> string -> string -> string -> string -> string -> fuse2 -> unit
    val print_current_p : int * fermion * boson * fermion ->
      string -> string -> string -> fuse2 -> unit
    val print_current_b : int * fermionbar * boson * fermionbar ->
      string -> string -> string -> fuse2 -> unit
    val print_current_g : int * fermionbar * boson * fermion ->
      string -> string -> string -> string -> string -> string -> fuse2 -> unit
    val print_current_g4 : int * fermionbar * boson2 * fermion ->
      string -> string -> string -> string -> fuse3 -> unit
    val reverse_braket : bool -> lorentz -> lorentz list -> bool
   end

(* We need to use the names of Fortran types, wave function variables and
   propagator functions consistently with \texttt{omegalib} and [Target_Fortran]. *)
module type Fermion_Maker = functor (N : Target_Fortran_Names.T) -> Fermions

module Fortran_Fermions : Fermion_Maker
module Fortran_Majorana_Fermions : Fermion_Maker

(* Output routines triple and quartic vertices. *)
module type T =
  sig

    type amplitude
    type constant
    type wf
    type rhs

    (* [print_current_V3 format_wf format_p amplitude dictionary
       amplitude dictionary rhs vertex fusion constant] writes code
       combining the children [rhs] into a current, using the vertex factor
       [vertex], coupling [constant] and the permutation [fusion] of its legs.
       [amplitude] is used with [dictionary] to disambiguate wavefunctions
       with the same flavor and momentum.  The formatting functions
       [format_wf] and [format_p] must be compatible with the remaining
       implementation of [Target]. *)
    (* \begin{dubious}
         The type is probably unnecessarily higher order.  It was natural
         in the monolithic implementation and has been kept in the first
         refactoring step.
       \end{dubious} *)
    val print_current_V3 :
      (amplitude -> (amplitude -> wf -> int) -> wf -> string) -> (wf -> string) ->
      amplitude -> (amplitude -> wf -> int) -> rhs ->
      constant Coupling.vertex3 -> Coupling.fuse2 -> constant -> unit

    val print_current_V4 :
      (amplitude -> (amplitude -> wf -> int) -> wf -> string) -> (wf -> string) ->
      amplitude -> (amplitude -> wf -> int) -> rhs ->
      constant Coupling.vertex4 -> Coupling.fuse3 -> constant -> unit

  end

module type Maker =
  functor (N : Target_Fortran_Names.T) -> functor (F : Fermion_Maker) ->
  functor (FM : Fusion.Maker) -> functor (P : Momentum.T) -> functor (M : Model.T) -> T
  with type amplitude = Fusion.Multi(FM)(P)(M).amplitude
   and type constant = Orders.Slice(Colorize.It(M)).constant
   and type wf = FM(P)(M).wf
   and type rhs = FM(P)(M).rhs

 module Make_Fortran : Maker


