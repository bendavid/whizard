(* targets.ml --

   Copyright (C) 1999-2024 by

       Wolfgang Kilian <kilian@physik.uni-siegen.de>
       Thorsten Ohl <ohl@physik.uni-wuerzburg.de>
       Juergen Reuter <juergen.reuter@desy.de>
       with contributions from
       Christian Speckner <cnspeckn@googlemail.com>
       Fabian Bach <fabian.bach@t-online.de> (only parts of this file)
       Marco Sekulla <marco.sekulla@kit.edu> (only parts of this file)
       Bijan Chokoufe Nejad <bijan.chokoufe@desy.de> (only parts of this file)
       So Young Shim <soyoung.shim@desy.de>

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

module Dummy (F : Fusion.Maker) (P : Momentum.T) (M : Model.T) =
  struct
    type amplitudes = Fusion.Multi(F)(P)(M).amplitudes
    type diagnostic = All | Arguments | Momenta | Gauge
    let options = Options.empty
    let amplitudes_to_channel _ _ _ = failwith "Targets.Dummy"
    let parameters_to_channel _ = failwith "Targets.Dummy"
  end

(* \thocwmodulesubsection{\texttt{FORTRAN\,77}} *)

module Fortran77 = Dummy

(* \thocwmodulesection{\texttt{C}} *)

module C = Dummy

(* \thocwmodulesubsection{\texttt{C++}} *)

module Cpp = Dummy

(* \thocwmodulesubsection{Java} *)

module Java = Dummy

(* \thocwmodulesection{O'Caml} *)

module Ocaml = Dummy

(* \thocwmodulesection{\LaTeX} *)

module LaTeX = Dummy
