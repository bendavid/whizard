(* keystones.mli --

   Copyright (C) 2019-2019 by

       Wolfgang Kilian <kilian@physik.uni-siegen.de>
       Thorsten Ohl <ohl@physik.uni-wuerzburg.de>
       Juergen Reuter <juergen.reuter@desy.de>

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

type field = Coupling.lorentz * int

type argument =
  | G of int (* complex coupling *)
  | M of int (* real mass (or width) *)
  | P of int (* momentum *)
  | F of field (* field *)
  | V of string (* verbatim *)

type keystone =
  { bra : field;
    name : string;
    args : argument list }

type vertex =
  { tag : string;
    keystones : keystone list }

val generate :
  ?reps:int -> ?threshold:float ->
  ?omega_module:string ->  ?modules:string list ->
  vertex list -> unit

type ufo_vertex =
  { v_tag : string;
    v_spins : Coupling.lorentz array;
    v_tensor : UFOx.Lorentz.t }

type ufo_propagator =
  { p_tag : string;
    p_omega : string;
    p_spins : Coupling.lorentz * Coupling.lorentz;
    p_propagator : UFO.Propagator.t }

val equivalent_tensors :
  Coupling.lorentz array -> (string * string) list -> ufo_vertex list

val transpose : ufo_propagator -> ufo_propagator

val generate_ufo :
  ?omega_module:string -> ?reps:int -> ?threshold:float ->
  string -> (ufo_vertex list * vertex) list -> ufo_propagator list -> unit
