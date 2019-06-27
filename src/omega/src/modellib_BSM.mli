(* $Id: modellib_BSM.mli 1513 2010-01-15 23:58:16Z cnspeckn $

   Copyright (C) 1999-2009 by

       Wolfgang Kilian <kilian@hep.physik.uni-siegen.de>
       Thorsten Ohl <ohl@physik.uni-wuerzburg.de>
       Juergen Reuter <juergen.reuter@physik.uni-freiburg.de>

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


(* \thocwmodulesection{More Hardcoded BSM Models} *)

module type BSM_flags = 
  sig 
    val u1_gauged         : bool
    val anom_ferm_ass     : bool
  end

module BSM_bsm : BSM_flags
module BSM_ungauged : BSM_flags
module BSM_anom : BSM_flags
module Littlest : functor (F: BSM_flags) -> Model.Gauge
module Littlest_Tpar : functor (F: BSM_flags) -> Model.T
module Simplest : functor (F: BSM_flags) -> Model.T
module Xdim : functor (F: BSM_flags) -> Model.Gauge
module UED : functor (F: BSM_flags) -> Model.Gauge
module GravTest : functor (F: BSM_flags) -> Model.Gauge
module Template : functor (F : BSM_flags) -> Model.Gauge

module type Threeshl_options =
	sig
		val include_ckm: bool
		val include_hf: bool
		val diet: bool
	end

module Threeshl_no_ckm: Threeshl_options
module Threeshl_ckm: Threeshl_options
module Threeshl_no_ckm_no_hf: Threeshl_options
module Threeshl_ckm_no_hf: Threeshl_options
module Threeshl_diet_no_hf: Threeshl_options 
module Threeshl_diet: Threeshl_options
module Threeshl: functor (Module_options: Threeshl_options) -> Model.T


(*i
 *  Local Variables:
 *  mode:caml
 *  indent-tabs-mode:nil
 *  page-delimiter:"^(\\* .*\n"
 *  End:
i*)
