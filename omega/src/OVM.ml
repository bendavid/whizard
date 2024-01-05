(* OVM.ml --

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

module Complex = Complex.Default

module Vector =
  struct
    
    type t = { t : Complex.t; x1 : Complex.t; x2 : Complex.t; x3 : Complex.t }

    let add v1 v2 =
      { t = Complex.add v1.t v2.t;
        x1 = Complex.add v1.x1 v2.x1;
        x2 = Complex.add v1.x2 v2.x2;
        x3 = Complex.add v1.x3 v2.x3 }

    let sub v1 v2 =
      { t = Complex.sub v1.t v2.t;
        x1 = Complex.sub v1.x1 v2.x1;
        x2 = Complex.sub v1.x2 v2.x2;
        x3 = Complex.sub v1.x3 v2.x3 }

  end

module type T =
  sig

    type amplitude
    type program
    type environment
          
    val compile : amplitude -> program
    val eval : program -> environment ->
      (float array * int) list -> float * float

  end

module Make (F : Fusion.T) =
  struct

    type amplitude = F.amplitude

    type instruction =
      | NOP

    type environment = (string, float) Hashtbl.t

    type program = (instruction * int * int * int) list

    let compile amplitude =
      failwith "OVM.compile: not available yet"

    let eval program environment momenta =
      failwith "OVM.eval: not available yet"

  end

(*i
 *  Local Variables:
 *  mode:caml
 *  indent-tabs-mode:nil
 *  page-delimiter:"^(\\* .*\n"
 *  End:
i*)
