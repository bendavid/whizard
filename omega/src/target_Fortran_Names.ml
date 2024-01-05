(* targets_vintage.ml --

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

module type T =
  sig
    val psi_type : string
    val psibar_type : string
    val chi_type : string
    val grav_type : string
    val psi_incoming : string
    val brs_psi_incoming : string
    val psibar_incoming : string
    val brs_psibar_incoming : string
    val chi_incoming : string
    val brs_chi_incoming : string
    val grav_incoming : string
    val psi_outgoing : string
    val brs_psi_outgoing : string
    val psibar_outgoing : string
    val brs_psibar_outgoing : string
    val chi_outgoing : string
    val brs_chi_outgoing : string
    val grav_outgoing : string
    val psi_propagator : string
    val psibar_propagator : string
    val chi_propagator : string
    val grav_propagator : string
    val psi_projector : string
    val psibar_projector : string
    val chi_projector : string
    val grav_projector : string
    val psi_gauss : string
    val psibar_gauss : string
    val chi_gauss : string
    val grav_gauss : string
    val use_module : string
    val require_library : string list
  end

module Dirac : T =
  struct

    let psi_type = "spinor"
    let psibar_type = "conjspinor"
    let chi_type = "???"
    let grav_type = "???"

    let psi_incoming = "u"
    let brs_psi_incoming = "brs_u"
    let psibar_incoming = "vbar"
    let brs_psibar_incoming = "brs_vbar"
    let chi_incoming = "???"
    let brs_chi_incoming = "???"
    let grav_incoming = "???"
    let psi_outgoing = "v"
    let brs_psi_outgoing = "brs_v"
    let psibar_outgoing = "ubar"
    let brs_psibar_outgoing = "brs_ubar"
    let chi_outgoing = "???"
    let brs_chi_outgoing = "???"
    let grav_outgoing = "???"

    let psi_propagator = "pr_psi"
    let psibar_propagator = "pr_psibar"
    let chi_propagator = "???"
    let grav_propagator = "???"

    let psi_projector = "pj_psi"
    let psibar_projector = "pj_psibar"
    let chi_projector = "???"
    let grav_projector = "???"

    let psi_gauss = "pg_psi"
    let psibar_gauss = "pg_psibar"
    let chi_gauss = "???"
    let grav_gauss = "???"

    let use_module = "omega95"
    let require_library =
      ["omega_spinors_2010_01_A"; "omega_spinor_cpls_2010_01_A"]
  end

module Majorana : T =
  struct

    let psi_type = "bispinor"
    let psibar_type = "bispinor"
    let chi_type = "bispinor"
    let grav_type = "vectorspinor"

(* \begin{JR}
   Because of our rules for fermions we are going to give all incoming fermions
   a [u] spinor and all outgoing fermions a [v] spinor, no matter whether they
   are Dirac fermions, antifermions or Majorana fermions.
   \end{JR} *)

    let psi_incoming = "u"
    let brs_psi_incoming = "brs_u"
    let psibar_incoming = "u"
    let brs_psibar_incoming = "brs_u"
    let chi_incoming = "u"
    let brs_chi_incoming = "brs_u"
    let grav_incoming = "ueps"

    let psi_outgoing = "v"
    let brs_psi_outgoing = "brs_v"
    let psibar_outgoing = "v"
    let brs_psibar_outgoing = "brs_v"
    let chi_outgoing = "v"
    let brs_chi_outgoing = "brs_v"
    let grav_outgoing = "veps"

    let psi_propagator = "pr_psi"
    let psibar_propagator = "pr_psi"
    let chi_propagator = "pr_psi"
    let grav_propagator = "pr_grav"

    let psi_projector = "pj_psi"
    let psibar_projector = "pj_psi"
    let chi_projector = "pj_psi"
    let grav_projector = "pj_grav"

    let psi_gauss = "pg_psi"
    let psibar_gauss = "pg_psi"
    let chi_gauss = "pg_psi"
    let grav_gauss = "pg_grav"

    let use_module = "omega95_bispinors"
    let require_library =
      ["omega_bispinors_2010_01_A"; "omega_bispinor_cpls_2010_01_A"]
  end
