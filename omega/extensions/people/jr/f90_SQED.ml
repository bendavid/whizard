(* f90_SQED.ml --

   Copyright (C) 1999-2021 by

       Wolfgang Kilian <kilian@physik.uni-siegen.de>
       Thorsten Ohl <ohl@physik.uni-wuerzburg.de>
       Juergen Reuter <juergen.reuter@desy.de>
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

module SQED = 
  struct
    open Coupling
    let options = Options.empty

(* Originally [LOp] has been a local operator to generate the insertion
   of the derivative of the Faddeev-Popov ghost. This unaesthetic construction 
   can be avoided either by using physical polarization vectors for the 
   gauge bosons in which case this term cancels out (as was the case for
   the gauge identities) or by absorbing the contribution of this term
   coupled to the ghost-ghost-gaugino vertex into the external wavefunction
   of [BRST Photon]. 
*)

    type flavor = 
      | Elec | Pos | Ph | Phino 
      | SelecL | SelecR | SposL | SposR
      | C | Cbar | Xi     (*i | LOp  i*)
      | BRST of flavor

    let rec conjugate = function
      | Elec -> Pos | Pos -> Elec | Ph -> Ph | Phino -> Phino
      | SelecL -> SposL | SposL -> SelecL 
      | SelecR -> SposR | SposR -> SelecR 
      | Cbar -> C | C -> Cbar | Xi -> Xi    (*i | LOp -> LOp i*)
      | BRST f -> BRST (conjugate f) 


    let external_flavors () =
      [ "fields", [Elec; Pos; SelecL; SposL; SelecR; SposR; Ph; Phino];
        "ghosts", [C; Cbar; Xi];
        "BRST transformations (ghost sources)", [BRST Elec; BRST Pos; 
           BRST SelecL; BRST SposL; BRST SelecR; BRST SposR; BRST Ph;
                                              BRST Phino]]   
(*i        "Local Operator", [LOp]]  i*)

    let flavors () = ThoList.flatmap snd (external_flavors ())

    let flavor_of_string = function
      | "e-" -> Elec | "e+" -> Pos
      | "sel-" -> SelecL | "sel+" -> SposL
      | "ser-" -> SelecR | "ser+" -> SposR
      | "ph" -> Ph | "phino" -> Phino 
      | "c" -> C | "cbar" -> Cbar | "xi" -> Xi
      | "brs_e-" -> BRST Elec | "brs_e+" -> BRST Pos 
      | "brs_sel-" -> BRST SelecL | "brs_sel+" -> BRST SposL
      | "brs_ser-" -> BRST SelecR | "brs_ser+" -> BRST SposR
      | "brs_ph" -> BRST Ph | "brs_phino" -> BRST Phino 
(*i      | "lop" -> LOp   i*)
      | _ -> invalid_arg "SQED.flavor_of_string"

    let rec flavor_to_string = function
      | Elec -> "e-" | Pos -> "e+" 
      | SelecL -> "sel-" | SposL -> "sel+" 
      | SelecR -> "ser-" | SposR -> "ser+"
      | Ph -> "ph" | Phino -> "phino" 
      | C -> "c" | Cbar -> "cbar" | Xi -> "xi"
(*i      | LOp -> "lop"    i*)
      | BRST f -> "brs_" ^ flavor_to_string f 

    let rec flavor_symbol = function
      | Elec -> "ele" | Pos -> "pos" 
      | SelecL -> "sel" | SposL -> "spl"
      | SelecR -> "ser" | SposR -> "spr" 
      | Ph -> "ph" | Phino -> "phino" 
      | C -> "c" | Cbar -> "cbar" | Xi -> "xi"
(*i      | LOp -> "lop"     i*)
      | BRST f -> "brs_" ^ flavor_symbol f 

    let rec lorentz = function
      | SelecL | SposL | SelecR | SposR | C | Cbar -> Scalar
(*i     | LOp -> Scalar    i*)
      | Elec -> Spinor | Pos -> ConjSpinor
      | Phino -> Majorana
      | Xi -> Maj_Ghost 
      | Ph -> Vector
      | BRST f -> BRS (lorentz f)

    let propagator = function
      | SelecL | SposL | SelecR | SposR -> Prop_Scalar 
      | Elec -> Prop_Spinor | Pos -> Prop_ConjSpinor 
      | C | Cbar -> Prop_Ghost
      | Ph -> Prop_Feynman
      | Phino -> Prop_Majorana
(*i     | LOp -> Only_Insertion   i*)
      | Xi | BRST _ -> Only_Insertion 

    let width _ = Timelike
    let goldstone _ = None

    let fermion = function
      | SelecL | SposL | SelecR | SposR | Ph | C | Cbar 
      | BRST SelecL | BRST SposL | BRST SelecR 
      | BRST SposR | BRST Ph -> 0
(*i      | LOp -> 0     i*)
      | Elec | BRST Elec -> 1 
      | Pos | BRST Pos -> -1
      | Phino | Xi | BRST Phino -> 2
      | BRST _ -> 42

    let color _ = Color.Singlet
    type gauge = unit
    let gauge_symbol () = failwith "SQED.gauge_symbol: internal error"

    let colsymm _ = (0,false),(0,false)

(* Symbols [MM], [WM] for the matter fields, [MG], [WG] for the gauge 
    fields. *)

    type constant =
      | Unity | Im | Null | E | EC | I_E | E2 | ESQ | ISQ | ISQE
      | M | MM | MG | MXI | MC
      | WM | WG | WC
      | G_MOMA | G_MOMB | G_L | G_S2
    let constant_symbol = function
      | Unity -> "unity" | Im -> "im" | Null -> "null" | M -> "mass"
      | E -> "e" | EC -> "ec" | I_E -> "ie" | E2 -> "e2" 
      | ESQ -> "esq" | ISQ -> "isq" | ISQE -> "isqe"
      | MM -> "me" | MG -> "mp" | MXI -> "mxi" | MC -> "mc" 
      | WM -> "we" | WG -> "wp" | WC -> "wc"
      | G_MOMA -> "gmoma" | G_MOMB -> "gmomb"
      | G_L -> "gl" | G_S2 -> "gs2"

(* Compare the definitions in the [Coupling.mli]-module: The momenta there 
   are defined as outgoing. From this the sign must be (-1) for the 
   selectron-spositron-photon vertices. *)

    let vertices () =
      ([(Ph, SelecL, SposL), Vector_Scalar_Scalar (-1), EC;
        (Ph, SelecR, SposR), Vector_Scalar_Scalar (-1), EC;
        (Pos, Ph, Elec), FBF (1, Psibar, V, Psi), EC;
(* Is the sign of [SelecL] compared to [SelecR] fixed (just by gauge 
   invariance, without the use of supersymmetry? *) 
        (Pos, SelecL, Phino), FBF ((-1), Psibar, SR, Chi), ESQ;
        (Phino, SposL, Elec), FBF ((-1), Chibar, SL, Psi), ESQ;
        (Phino, SposR, Elec), FBF (1, Chibar, SR, Psi), ESQ;
        (Pos, SelecR, Phino), FBF (1, Psibar, SL, Chi), ESQ;  
(* Alternative signs.  *)
(*        (Pos, SelecL, Phino), FBF (1, Psibar, SR, Chi), ESQ;
        (Phino, SposL, Elec), FBF (1, Chibar, SL, Psi), ESQ;
        (Phino, SposR, Elec), FBF ((-1), Chibar, SR, Psi), ESQ;
        (Pos, SelecR, Phino), FBF ((-1), Psibar, SL, Chi), ESQ;  *)
(*  *)
        (BRST SposL, C, SelecL), Scalar_Scalar_Scalar 1, EC;
        (BRST SposR, C, SelecR), Scalar_Scalar_Scalar 1, EC;
        (BRST SelecL, C, SposL), Scalar_Scalar_Scalar (-1), EC;
        (BRST SelecR, C, SposR), Scalar_Scalar_Scalar (-1), EC;
(* These are the signs suitable for the gauge STIs. 
        (BRST Pos, C, Elec), FBF (1, Psibar, S, Psi), EC;
        (Pos, C, BRST Elec), FBF ((-1), Psibar, S, Psi), EC; *)
        (BRST Pos, C, Elec), FBF ((-1), Psibar, S, Psi), EC;
        (Pos, C, BRST Elec), FBF (1, Psibar, S, Psi), EC;
(*i         (BRST Ph, C, LOp), Vector_Scalar_Scalar 1, Unity;   i*)
        (Xi, BRST SposL, Elec), FBF (1, Chibar, SL, Psi), ISQ;
        (Pos, BRST SelecL, Xi), FBF ((-1), Psibar, SR, Chi), ISQ;
        (Xi, BRST SposR, Elec), FBF (1, Chibar, SR, Psi), ISQ;
        (Pos, BRST SelecR, Xi), FBF ((-1), Psibar, SL, Chi), ISQ;
(* Checked until here. *)
(* This is a first guess. Note that we have to switch the direction of the
   spinor structure for the terms containing [BRST Elec]. *) 
        (BRST Pos, SelecL, Xi), GBG (1, Psibar, MOMR, Chi), G_MOMA;
        (BRST Pos, SelecR, Xi), GBG ((-1), Psibar, MOML, Chi), G_MOMA;
        (Xi, SposL, BRST Elec), GBG (1, Chibar, LMOM, Chi), G_MOMA;  
        (Xi, SposR, BRST Elec), GBG ((-1), Chibar, RMOM, Chi), G_MOMA;  
        (BRST Phino, Ph, Xi), GBG (1, Chibar, VMOM, Chi), G_L;
        (Phino, BRST Ph, Xi), FBF (1, Chibar, V, Chi), Im;    
        (Phino, Cbar, Xi), GBG ((-1), Chibar, MOM, Chi), G_MOMB], 
 (*       (Phino, Cbar, Xi), GBG (1, Chibar, MOM, Chi), G_MOMB],   *)
       [(SelecL, SposL, SelecL, SelecR), Scalar4 (-2), E2;
        (SelecL, SposL, SelecL, SelecR), Scalar4 (-2), E2;
        (SelecL, SposL, SelecL, SelecR), Scalar4 1, E2;
        (SelecL, SposL, Ph, Ph), Scalar2_Vector2 2, E2;
        (SelecR, SposR, Ph, Ph), Scalar2_Vector2 2, E2;
        (BRST Pos, SelecL, Ph, Xi), GBBG ((-1), Psibar, SRV, Chi), ISQE;
        (BRST Pos, SelecR, Ph, Xi), GBBG (1, Psibar, SLV, Chi), ISQE;
        (Xi, SposL, Ph, BRST Elec), GBBG (1, Chibar, SRV, Psi), ISQE;
        (Xi, SposR, Ph, BRST Elec), GBBG ((-1), Chibar, SLV, Psi), ISQE; 
        (BRST Phino, SelecL, SposL, Xi), GBBG (1, Chibar, S2, Chi), G_S2;
	(BRST Phino, SelecR, SposR, Xi), GBBG (1, Chibar, S2, Chi), G_S2],
       [])

    let parameters () =
      { input = [E, 0.1; M, 10.00];
        derived =
        [ Complex Unity, Const 1;
          Complex Null, Const 0;
          Real MM, Atom M; Real MG, Const 0; Real MXI, Const 0; 
          Real MC, Const 0; 
          Real WM, Const 0; Real WG, Const 0; Real WC, Const 0;
          Complex EC, Atom E;
          Complex E2, Prod [Atom E; Atom E];
          Complex ESQ, Quot (Atom E, Sqrt(Const 2));
          Complex ISQ, Quot (I, Sqrt(Const 2)); 
          Complex ISQE, Quot (Prod [I; Atom E], Sqrt(Const 2));
          Complex I_E, Prod [I; Atom E];
          Complex Im, I;
          Complex G_L, Quot (I, Const 2);
          Complex G_S2, Neg (Prod [I; Atom E])];  
        derived_arrays = [Complex_Array G_MOMA, [Atom ISQ; 
                             Prod[Neg (Atom ISQ); Atom M]];
                          Complex_Array G_MOMB, [Const 1; Const 0]]}


(* Since the functions for the chiral couplings in omega_(bi)spinor_couplings
   are defined as $1 \pm \gamma^5$ instead of $(1 \pm \gamma^5)/2$ we have 
   to divide by two. *)

    module F = Models.Fusions (struct
      type f = flavor
      type c = constant
      let compare = compare
      let conjugate = conjugate
    end)
    let table = F.of_vertices (vertices ())
    let fuse2 = F.fuse2 table
    let fuse3 = F.fuse3 table
    let fuse = F.fuse table
    let max_degree () = 3

    let pdg = function
      | Elec -> 1 | Pos -> -1 | SelecL -> 2 | SposL -> -2 
      | SelecR -> 3 | SposR -> -3 | Ph -> 4 | Phino -> 5 
      | C -> 6 | Cbar -> -6 | Xi -> 7
      | BRST Elec -> 1001 | BRST Pos -> -1001 
      | BRST SelecL -> 1002 | BRST SposL -> -1002 
      | BRST SelecR -> 1003 | BRST SposR -> -10033 
      | BRST Ph -> 1004 | BRST Phino -> 1005 
      | BRST C -> 1006 | BRST Cbar -> -1006 
(*i       | LOp -> 42    i*)
      | BRST _ -> 1234567
    let mass_symbol = function
      | Elec | Pos | BRST Elec | BRST Pos -> "me" 
      | SelecL | SposL | BRST SelecL | BRST SposL -> "me"
      | SelecR | SposR | BRST SelecR | BRST SposR -> "me"
      | Ph | BRST Ph -> "mp" 
      | C | Cbar -> "mc" | Xi -> "mxi" (*i | LOp -> "mlop"   i*)
      | Phino | BRST Phino -> "mp" | BRST _ -> ""
    let width_symbol = function
      | Elec | Pos | BRST Elec | BRST Pos -> "we" 
      | SelecL | SposL | BRST SelecL | BRST SposL -> "we" 
      | SelecR | SposR | BRST SelecR | BRST SposR -> "we" 
      | Ph | BRST Ph -> "wp" 
      | C | Cbar -> "wc" | Xi -> "wxi" (*i | LOp -> "wlop"  i*)
      | Phino | BRST Phino -> "wp" | BRST _ -> ""
  end

module Main = Omega.Make(Fusion.Mixed23_Majorana)
    (Targets.Fortran_Majorana)(SQED)
let _ = Main.main ()

(*i
 *  Local Variables:
 *  mode:caml
 *  indent-tabs-mode:nil
 *  page-delimiter:"^(\\* .*\n"
 *  End:
i*)































































































