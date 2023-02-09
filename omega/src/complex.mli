(* complex.mli --

   Copyright (C) 1999-2023 by

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

module type T =
    sig
      type t

      val null : t
      val one : t

      val real : t -> float
      val imag : t -> float

      val conj : t -> t
      val neg : t -> t
      val inv : t -> t

      val add : t -> t -> t
      val sub : t -> t -> t
      val mul : t -> t -> t
      val div : t -> t -> t

      val abs : t -> float
      val arg : t -> float

      val sqrt : t -> t
      val exp : t -> t
      val log : t -> t

      val of_float2 : float -> float -> t
      val of_int2 : int -> int -> t
      val to_float2 : t -> float * float
      val to_int2 : t -> int * int

      val of_float : float -> t
      val of_int : int -> t
      val to_float : t -> float
      val to_int : t -> int

      val to_string : t -> string
      val of_string : 'a -> 'b
    end

module Dense : T
module Default : T

(*i
 *  Local Variables:
 *  mode:caml
 *  indent-tabs-mode:nil
 *  page-delimiter:"^(\\* .*\n"
 *  End:
i*)
