(* feynmp.mli --

   Copyright (C) 1999-2023 by

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

module type T =
  sig
    type amplitudes

    val amplitudes_to_channel : bool -> amplitudes -> out_channel -> unit
    val amplitudes_sans_color_to_channel : bool -> amplitudes -> out_channel -> unit
    val amplitudes_color_only_to_channel : bool -> amplitudes -> out_channel -> unit

    (* Backward compatibility:
       \begin{dubious}
         These can only be retired, if Whizard can deal with
         ["\\jobname-fmf.mp"] as metapost files!
       \end{dubious} *)
    val amplitudes : bool -> string -> amplitudes -> unit
    val amplitudes_sans_color : bool -> string -> amplitudes -> unit
    val amplitudes_color_only : bool -> string -> amplitudes -> unit

  end

module Make (FM : Fusion.Maker) (P : Momentum.T) (M : Model.T) : T
       with type amplitudes = Fusion.Multi(FM)(P)(M).amplitudes

