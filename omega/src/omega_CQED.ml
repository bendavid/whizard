(* omega_CQED.ml --

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


(* QED with contact interactions. *)

module M : Model.T =
  struct
    open Coupling

    let options = Options.empty

    type flavor =
      | Electron | Positron
      | Muon | AntiMuon
      | Tau | AntiTau
      | Photon | XZ

    let external_flavors () =
      [ "Leptons", [Electron; Positron; Muon; AntiMuon; Tau; AntiTau];
        "Gauge Bosons", [Photon] ]
    let flavors () = ThoList.flatmap snd (external_flavors ())

    type gauge = unit
    type constant = Q

    let lorentz = function
      | Electron | Muon | Tau -> Spinor
      | Positron | AntiMuon | AntiTau -> ConjSpinor
      | Photon -> Vector
      | XZ -> Tensor_1

    let color _ = Color.Singlet

    let propagator = function
      | Electron | Muon | Tau -> Prop_Spinor
      | Positron | AntiMuon | AntiTau -> Prop_ConjSpinor
      | Photon -> Prop_Feynman
      | XZ -> Aux_Vector

    let width _ = Timelike

    let goldstone _ =
      None

    let conjugate = function
      | Electron -> Positron | Positron -> Electron
      | Muon -> AntiMuon | AntiMuon -> Muon
      | Tau -> AntiTau | AntiTau -> Tau
      | Photon -> Photon
      | XZ -> XZ

    let fermion = function
      | Electron | Muon | Tau -> 1
      | Positron | AntiMuon | AntiTau -> -1
      | Photon -> 0 | XZ -> 0

    module F = Modeltools.Fusions (struct
      type f = flavor
      type c = constant
      let compare = compare
      let conjugate = conjugate
    end)

    let vertices () = 
      ([(Positron, Photon, Electron), FBF (1, Psibar, V, Psi), Q;
        (AntiMuon, Photon, Muon), FBF (1, Psibar, V, Psi), Q;
        (AntiTau, Photon, Tau), FBF (1, Psibar, V, Psi), Q;
        (Positron, XZ, Electron), FBF (1, Psibar, VA, Psi), Q], [], [])

    let table = F.of_vertices (vertices ())
    let fuse2 = F.fuse2 table
    let fuse3 = F.fuse3 table
    let fuse = F.fuse table
    let max_degree () = 3

    let parameters () = { input = [Q, 1.0]; derived = []; derived_arrays = [] }

    let flavor_of_string = function
      | "e-" -> Electron | "e+" -> Positron
      | "m-" -> Muon | "m+" -> AntiMuon
      | "t-" -> Tau | "t+" -> AntiTau
      | "A" -> Photon
      | _ -> invalid_arg "Modellib_SM.QED.flavor_of_string"

    let flavor_to_string = function
      | Electron -> "e-" | Positron -> "e+"
      | Muon -> "m-" | AntiMuon -> "m+"
      | Tau -> "t-" | AntiTau -> "t+"
      | Photon -> "A" | XZ -> "xz"

    let flavor_symbol = function
      | Electron -> "ele" | Positron -> "pos"
      | Muon -> "muo" | AntiMuon -> "amu"
      | Tau -> "tau" | AntiTau -> "ata"
      | Photon -> "gam" | XZ -> "xz"

    let gauge_symbol () =
      failwith "Modellib_SM.QED.gauge_symbol: internal error"

    let pdg = function
      | Electron -> 11 | Positron -> -11
      | Muon -> 13 | AntiMuon -> -13
      | Tau -> 15 | AntiTau -> -15
      | Photon -> 22 | XZ -> 0

    let mass_symbol f = 
      "mass(" ^ string_of_int (abs (pdg f)) ^ ")"

    let width_symbol f =
      "width(" ^ string_of_int (abs (pdg f)) ^ ")"

    let constant_symbol = function
      | Q -> "qlep"
  end

module O = Omega.Make(Fusion.Binary)(Targets.Fortran)(M)
let _ = O.main ()


(*i
 *  Local Variables:
 *  mode:caml
 *  indent-tabs-mode:nil
 *  page-delimiter:"^(\\* .*\n"
 *  End:
i*)
