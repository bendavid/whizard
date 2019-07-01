(* $Id: comphep_syntax.mli 3670 2012-01-21 19:33:07Z jr_reuter $

   Copyright (C) 1999-2012 by

       Wolfgang Kilian <kilian@physik.uni-siegen.de>
       Thorsten Ohl <ohl@physik.uni-wuerzburg.de>
       Juergen Reuter <juergen.reuter@desy.de>
       Christian Speckner <christian.speckner@physik.uni-freiburg.de>

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

type raw =
  | I | Integer of int | Symbol of string
  | Application of string * raw
  | Dotproduct of raw * raw
  | Product of (raw * int) list
  | Sum of (raw * int) list

val symbol : string -> raw
val integer : int -> raw
val imag : raw

val apply : string -> raw -> raw
val dot : raw -> raw -> raw
val multiply : raw -> raw -> raw
val divide : raw -> raw -> raw
val power : raw -> int -> raw
val add : raw -> raw -> raw
val subtract : raw -> raw -> raw
val neg : raw -> raw

(*i
 *  Local Variables:
 *  mode:caml
 *  indent-tabs-mode:nil
 *  page-delimiter:"^(\\* .*\n"
 *  End:
i*)

