(* omega_cli.mli --

   Copyright (C) 1999-2024 by

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

(* \begin{dubious}
     Next generation command line interface.

     Ideally, I would have liked to use \texttt{cmdliner}
     (\url{https://erratique.ch/software/cmdliner}),
     but more recent versions of this require ocaml 4.08.
     Also, building it without \texttt{dune} might be a challenge.

     Neverthess, I will take inspiration from \texttt{cmdliner}.
   \end{dubious} *)

module Models : sig
  type t
  val of_list : (string * string * (module Model.T)) list -> t
  val by_name_opt : t -> string -> (module Model.T) option
  val names : t -> (string * string) list
end

(* Since there are only very few implementations of [Target.Maker]
   that are actively maintained and only [Targets.Fortran_Majorana]
   can currently deal with Majorana fermions, we don't implement
   a lookup table but select them explicitey according to the command line
   options. *)

module type T =
  sig
    val main : ?current:int ref -> ?argv:string array -> unit -> unit
  end

module Make (F : Fusion.Maker) (P : Fusion.Maker) (T : Target.Maker) (M : Model.Mutable) : T
