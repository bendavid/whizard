(* omega_Littlest_Zprime.ml --

   Copyright (C) 1999-2020 by

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

(* \thocwmodulesection{SM with Littlest Higgs Z'} *)

module type SM_flags =
  sig
    val include_gluons : bool
    val include_anomalous : bool
    val include_supp : bool
    val k_matrix : bool
  end

module SM_no_anomalous : SM_flags =
  struct
    let include_gluons = false
    let include_anomalous = false
    let include_supp = false
    let k_matrix = false
  end

module SM_anomalous : SM_flags =
  struct
    let include_gluons = false
    let include_anomalous = true
    let include_supp = false
    let k_matrix = false
  end

module SM_k_matrix : SM_flags =
  struct
    let include_gluons = false
    let include_anomalous = false
    let include_supp = false
    let k_matrix = true
  end

module SM_gluons : SM_flags =
  struct
    let include_gluons = true
    let include_anomalous = false
    let include_supp = false
    let k_matrix = false
  end

module SM_supp : SM_flags =
  struct
    let include_gluons = false
    let include_anomalous = false
    let include_supp = true 
    let k_matrix = false
  end 

module Zprime (Flags : SM_flags) =
  struct

    open Coupling

    let default_width = ref Timelike
    let use_fudged_width = ref false

    let options = Options.create
      [ "constant_width", Arg.Unit (fun () -> default_width := Constant),
        "use constant width (also in t-channel)";
        "fudged_width", Arg.Set use_fudged_width,
        "use fudge factor for charge particle width";
        "custom_width", Arg.String (fun f -> default_width := Custom f),
        "use custom width";
        "cancel_widths", Arg.Unit (fun () -> default_width := Vanishing),
        "use vanishing width";
        "running_width", Arg.Unit (fun () -> default_width := Running),
        "use running width" ]

(* We do not introduce the Goldstones for the heavy vectors here. *)

    type matter_field = L of int | N of int | U of int | D of int 
        | TopH | TopHq | DH | DHq
    type gauge_boson = Ga | Wp | Wm | Z | Gl | Gl_aux
        | Xp | Xm | X0 | Y0 | ZH  
    type other = Phip | Phim | Phi0 | H | Eta
    type flavor = M of matter_field | G of gauge_boson | O of other
    let matter_field f = M f
    let gauge_boson f = G f
    let other f = O f

    type field =
      | Matter of matter_field
      | Gauge of gauge_boson
      | Other of other

    let field = function
      | M f -> Matter f
      | G f -> Gauge f
      | O f -> Other f

    type gauge = unit

    let gauge_symbol () =
      failwith "Models.Zprime.gauge_symbol: internal error"

    let family n = List.map matter_field [ L n; N n; U n; D n ]

    let external_flavors () =
      [ "1st Generation", ThoList.flatmap family [1; -1];
        "2nd Generation", ThoList.flatmap family [2; -2];
        "3rd Generation", ThoList.flatmap family [3; -3];
        "Heavy Quarks", List.map matter_field [TopH; TopHq; DH; DHq];
        "Gauge Bosons", List.map gauge_boson [Ga; Z; Wp; Wm; Gl; Xp;
                                            Xm; X0; Y0; ZH];
        "Higgs", List.map other [H; Eta];
        "Goldstone Bosons", List.map other [Phip; Phim; Phi0] ]

    let flavors () = ThoList.flatmap snd (external_flavors ()) @ 
      [ G Gl_aux]

    let squ = function
      | x -> Pow (Atom x, 2)

    let spinor n =
      if n >= 0 then
        Spinor
      else
        ConjSpinor

    let lorentz = function
      | M f ->
          begin match f with
          | L n -> spinor n | N n -> spinor n
          | U n -> spinor n | D n -> spinor n
          | TopH -> Spinor | TopHq -> ConjSpinor 
          | DH -> Spinor | DHq -> ConjSpinor 
          end
      | G f ->
          begin match f with
          | Ga | Gl -> Vector
          | Wp | Wm | Z | Xp | Xm | X0 | Y0 | ZH -> Massive_Vector
          | Gl_aux -> Tensor_1
          end
      | O f -> 
          Scalar

    let color = function 
      | M (U n) -> Color.SUN (if n > 0 then 3 else -3)
      | M (D n) -> Color.SUN  (if n > 0 then 3 else -3)
      | M TopH -> Color.SUN 3 | M TopHq -> Color.SUN (-3)
      | M DH -> Color.SUN 3 | M DHq -> Color.SUN (-3)
      | G Gl | G Gl_aux -> Color.AdjSUN 3
      | _ -> Color.Singlet

    let prop_spinor n =
      if n >= 0 then
        Prop_Spinor
      else
        Prop_ConjSpinor

    let propagator = function
      | M f ->
          begin match f with
          | L n -> prop_spinor n | N n -> prop_spinor n
          | U n -> prop_spinor n | D n -> prop_spinor n
          | TopH -> Prop_Spinor | TopHq -> Prop_ConjSpinor 
          | DH -> Prop_Spinor | DHq -> Prop_ConjSpinor 
          end
      | G f ->
          begin match f with
          | Ga | Gl -> Prop_Feynman
          | Wp | Wm | Z | Xp | Xm | X0 | Y0 | ZH -> Prop_Unitarity
          | Gl_aux -> Aux_Tensor_1
          end
      | O f ->
          begin match f with
          | Phip | Phim | Phi0 -> Only_Insertion
          | H | Eta -> Prop_Scalar
          end

(* Optionally, ask for the fudge factor treatment for the widths of
   charged particles.  Currently, this only applies to $W^\pm$ and top. *)

    let width f =
      if !use_fudged_width then
        match f with
        | G Wp | G Wm | M (U 3) | M (U (-3)) 
        | M TopH | M TopHq | M DH | M DHq -> Fudged
        | _ -> !default_width
      else
        !default_width

    let goldstone = function
      | G f ->
          begin match f with
          | Wp -> Some (O Phip, Coupling.Const 1)
          | Wm -> Some (O Phim, Coupling.Const 1)
          | Z -> Some (O Phi0, Coupling.Const 1)
          | _ -> None
          end
      | _ -> None

    let conjugate = function
      | M f ->
          M (begin match f with
          | L n -> L (-n) | N n -> N (-n)
          | U n -> U (-n) | D n -> D (-n)
          | TopH -> TopHq | TopHq -> TopH
          | DH -> DHq | DHq -> DH
          end)
      | G f ->
          G (begin match f with
          | Gl -> Gl | Ga -> Ga | Z -> Z 
          | Wp -> Wm | Wm -> Wp 
          | Xp -> Xm | Xm -> Xp | X0 -> X0 | Y0 -> Y0 | ZH -> ZH
          | Gl_aux -> Gl_aux
          end)
      | O f ->
          O (begin match f with
          | Phip -> Phim | Phim -> Phip | Phi0 -> Phi0
          | H -> H | Eta -> Eta
          end)

    let fermion = function
      | M f ->
          begin match f with
          | L n -> if n > 0 then 1 else -1
          | N n -> if n > 0 then 1 else -1
          | U n -> if n > 0 then 1 else -1
          | D n -> if n > 0 then 1 else -1
          | TopH -> 1 | TopHq -> -1
          | DH -> 1 | DHq -> -1 
          end
      | G f ->
          begin match f with
          | Gl | Ga | Z | Wp | Wm | Gl_aux | Xp | Xm | X0 | Y0 | ZH -> 0
          end
      | O _ -> 0

    type constant =
      | Unit | Pi | Alpha_QED | Sin2thw
      | Sinthw | Costhw | E | G_weak | Vev | VHeavy
      | Supp | Supp2
      | Sinpsi | Cospsi | Atpsi | Sccs  (* Mixing angles of SU(2) *)
      | Q_lepton | Q_up | Q_down | Q_Z_up | G_CC 
      | G_NC_neutrino | G_NC_lepton | G_NC_up | G_NC_down
      | G_NC_h_neutrino | G_NC_h_lepton | G_NC_h_up | G_NC_h_down
      | G_CC_heavy | G_zhthth       
      | G_CC_supp1 | G_CC_supp2 
      | I_Q_W | I_G_ZWW | I_G_WWW
      | I_G_Z1 | I_G_Z2 | I_G_Z3 | I_G_Z4
      | I_Q_H | I_Q_ZH | G_over4 | G_over4_sup | G_CC_sup
      | G_WWWW | G_ZZWW | G_AZWW | G_AAWW
      | I_G1_AWW | I_G1_ZWW
      | I_G1_plus_kappa_AWW | I_G1_plus_kappa_ZWW
      | I_G1_minus_kappa_AWW | I_G1_minus_kappa_ZWW
      | I_kappa_minus_G1_AWW | I_kappa_minus_G1_ZWW
      | I_lambda_AWW | I_lambda_ZWW
      | Alpha_WWWW0 | Alpha_ZZWW1 | Alpha_WWWW2
      | Alpha_ZZWW0 | Alpha_ZZZZ
      | G_HWW | G_HHWW | G_HZZ | G_HHZZ
      | G_heavy_HVV | G_heavy_HWW | G_heavy_HZZ | G_heavy_HHVV
      | G_Htt | G_Hbb | G_Hcc | G_Htautau | G_H3 | G_H4
      | G_Hthth | G_Htht | G_Ethth | G_Etht | G_Ett
      | G_Ebb | G_ZEH | G_ZHEH | G_XEH
      | G_HGaGa | G_HGaZ | G_EGaGa | G_EGaZ | G_EGlGl
      | G_strong
      | Mass of flavor | Width of flavor
      | K_Matrix_Coeff of int | K_Matrix_Pole of int

(* \begin{dubious}
     The current abstract syntax for parameter dependencies is admittedly
     tedious. Later, there will be a parser for a convenient concrete syntax
     as a part of a concrete syntax for models.  But as these examples show,
     it should include simple functions.
   \end{dubious} *)


    let input_parameters =
      [ Alpha_QED, 1. /. 137.0359895;
        Sin2thw, 0.23124;
        VHeavy, 2000.0;
        Mass (G Z), 91.187;        
        Mass (M (N 1)), 0.0; Mass (M (L 1)), 0.51099907e-3;
        Mass (M (N 2)), 0.0; Mass (M (L 2)), 0.105658389;
        Mass (M (N 3)), 0.0; Mass (M (L 3)), 1.77705;
        Mass (M (U 1)), 5.0e-3; Mass (M (D 1)), 3.0e-3;
        Mass (M (U 2)), 1.2; Mass (M (D 2)), 0.1;
        Mass (M (U 3)), 174.0; Mass (M (D 3)), 4.2 ]


(* hier, Hier, hallo, hier Higgs couplings still missing. *)

    let derived_parameters =
      [ Real E, Sqrt (Prod [Const 4; Atom Pi; Atom Alpha_QED]);
        Real Sinthw, Sqrt (Atom Sin2thw);
        Real Costhw, Sqrt (Diff (Const 1, Atom Sin2thw));
        Real G_weak, Quot (Atom E, Atom Sinthw);
        Real (Mass (G Wp)), Prod [Atom Costhw; Atom (Mass (G Z))];
        Real Vev, Quot (Prod [Const 2; Atom (Mass (G Wp))], Atom G_weak);
        Real Supp, Quot (Atom Vev, Atom VHeavy);
        Real Supp2, squ Supp;
        Real Atpsi, Quot (Atom Cospsi, Atom Sinpsi);
        Real Sccs, Prod [Atom Sinpsi; Atom Cospsi; 
                         Diff (squ Cospsi, squ Sinpsi)]; 
        Real Q_lepton, Atom E;
        Real Q_up, Prod [Quot (Const (-2), Const 3); Atom E];
        Real Q_down, Prod [Quot (Const 1, Const 3); Atom E];
        Real G_CC, Neg (Quot (Atom G_weak, Prod [Const 2; Sqrt (Const 2)]));
        Real G_CC_heavy, Prod [Atom G_CC; Atom Atpsi];
(*        Real G_NC_heavy, Quot (Prod [Atom G_weak; Atom Atpsi], Const 4); *)
        Complex I_Q_W, Prod [I; Atom E];
        Complex I_G_ZWW, Prod [I; Atom G_weak; Atom Costhw];
        Complex I_G_WWW, Prod [I; Atom G_weak];
        Complex I_Q_ZH, Neg (Prod [I; Atom G_weak; Atom Supp2; Atom Sccs
                  ]);
        Complex I_Q_H, Quot (Atom I_Q_ZH, Atom Costhw) ]
             
(* \begin{equation}
      - \frac{g}{2\cos\theta_w}
   \end{equation} *)
    let g_over_2_costh =
      Quot (Neg (Atom G_weak), Prod [Const 2; Atom Costhw])

(* \begin{subequations}
     \begin{align}
           - \frac{g}{2\cos\theta_w} g_V
        &= - \frac{g}{2\cos\theta_w} (T_3 - 2 q \sin^2\theta_w) \\
           - \frac{g}{2\cos\theta_w} g_A
        &= - \frac{g}{2\cos\theta_w} T_3
     \end{align}
   \end{subequations} *)
    let nc_coupling c t3 q =
      (Real_Array c,
       [Prod [g_over_2_costh; Diff (t3, Prod [Const 2; q; Atom Sin2thw])];
        Prod [g_over_2_costh; t3]])

    let half = Quot (Const 1, Const 2)

    let derived_parameter_arrays =
      [ nc_coupling G_NC_neutrino half (Const 0);
        nc_coupling G_NC_lepton (Neg half) (Const (-1));
        nc_coupling G_NC_up half (Quot (Const 2, Const 3));
        nc_coupling G_NC_down (Neg half) (Quot (Const (-1), Const 3)) ]

    let parameters () =
      { input = input_parameters;
        derived = derived_parameters;
        derived_arrays = derived_parameter_arrays }

    module F = Modeltools.Fusions (struct
      type f = flavor
      type c = constant
      let compare = compare
      let conjugate = conjugate
    end)

(* \begin{equation}
     \mathcal{L}_{\textrm{EM}} =
        - e \sum_i q_i \bar\psi_i\fmslash{A}\psi_i
   \end{equation} *)

    let mgm ((m1, g, m2), fbf, c) = ((M m1, G g, M m2), fbf, c)
    let mom ((m1, o, m2), fbf, c) = ((M m1, O o, M m2), fbf, c)

    let electromagnetic_currents n =
      List.map mgm
        [ ((L (-n), Ga, L n), FBF (1, Psibar, V, Psi), Q_lepton);
          ((U (-n), Ga, U n), FBF (1, Psibar, V, Psi), Q_up);  
          ((D (-n), Ga, D n), FBF (1, Psibar, V, Psi), Q_down) ]
        
    let color_currents n =
      if Flags.include_gluons then
        List.map mgm
          [ ((U (-n), Gl, U n), FBF (1, Psibar, V, Psi), G_strong);
            ((D (-n), Gl, D n), FBF (1, Psibar, V, Psi), G_strong) ]
      else
        []

(* \begin{equation}
     \mathcal{L}_{\textrm{NC}} =
        - \frac{g}{2\cos\theta_W}
            \sum_i \bar\psi_i\fmslash{Z}(g_V^i-g_A^i\gamma_5)\psi_i
   \end{equation} *)

    let neutral_currents n =
      List.map mgm
        [ ((L (-n), Z, L n), FBF (1, Psibar, VA, Psi), G_NC_lepton);
          ((N (-n), Z, N n), FBF (1, Psibar, VA, Psi), G_NC_neutrino);
          ((U (-n), Z, U n), FBF (1, Psibar, VA, Psi), G_NC_up);
          ((D (-n), Z, D n), FBF (1, Psibar, VA, Psi), G_NC_down) ] 

(* The sign of this coupling is just the one of the T3, being -(1/2) for
   leptons and down quarks, and +(1/2) for neutrinos and up quarks. *)

(* This version is the canonical Little Higgs which is universal couplings
   of the heavy Z to the SM fermions.

    let neutral_heavy_currents n =
      List.map mgm
        [ ((L (-n), ZH, L n), FBF (1, Psibar, VL, Psi), G_NC_heavy);
          ((N (-n), ZH, N n), FBF ((-1), Psibar, VL, Psi), G_NC_heavy);
          ((U (-n), ZH, U n), FBF ((-1), Psibar, VL, Psi), G_NC_heavy);
          ((D (-n), ZH, D n), FBF (1, Psibar, VL, Psi), G_NC_heavy) ] 

   We want to allow for (almost) completely general couplings but maintain
   universality (generation independence). Maybe we should also separate the
   coupling to the top quark since the third generation is somewhat special.
 *)

    let neutral_heavy_currents n =
      List.map mgm
        [ ((L (-n), ZH, L n), FBF (1, Psibar, VLR, Psi), G_NC_h_lepton);
          ((N (-n), ZH, N n), FBF ((-1), Psibar, VLR, Psi), G_NC_h_neutrino);
          ((U (-n), ZH, U n), FBF ((-1), Psibar, VLR, Psi), G_NC_h_up);
          ((D (-n), ZH, D n), FBF (1, Psibar, VLR, Psi), G_NC_h_down);
         ] 

   let heavy_top_currents = 
     List.map mgm 
        [ ((TopHq, Ga, TopH), FBF (1, Psibar, V, Psi), Q_up);
          ((DHq, Ga, DH), FBF (1, Psibar, V, Psi), Q_down);
          ((TopHq, Z, TopH), FBF (4, Psibar, V, Psi), Q_Z_up);
          ((DHq, Z, DH), FBF (1, Psibar, V, Psi), Q_Z_up);
          ((DHq, X0, D 1), FBF (1, Psibar, VL, Psi), G_over4);
          ((D (-1), X0, DH), FBF (1, Psibar, VL, Psi), G_over4);
          ((DHq, Y0, D 1), FBF (1, Psibar, VL, Psi), G_over4);
          ((D (-1), Y0, DH), FBF ((-1), Psibar, VL, Psi), G_over4);
          ((DHq, Xm, U 1), FBF (1, Psibar, VL, Psi), G_CC);  
          ((U (-1), Xp, DH), FBF (1, Psibar, VL, Psi), G_CC);
          ((U (-3), X0, U 3), FBF (2, Psibar, VL, Psi), G_over4_sup); 
          ((U (-3), Y0, U 3), FBF (2, Psibar, VL, Psi), G_over4_sup); 
          ((U (-3), Xp, D 3), FBF (1, Psibar, VL, Psi), G_CC_sup); 
          ((D (-3), Xm, U 3), FBF (1, Psibar, VL, Psi), G_CC_sup)] 
          

   let neutral_supp_currents = 
     List.map mgm 
        [ ((TopHq, ZH, TopH), FBF (1, Psibar, VL, Psi), G_zhthth);
           ((DHq, ZH, DH), FBF (1, Psibar, VL, Psi), G_zhthth)]

(* \begin{equation}
     \mathcal{L}_{\textrm{CC}} =
        - \frac{g}{2\sqrt2} \sum_i \bar\psi_i
               (T^+\fmslash{W}^+ + T^-\fmslash{W}^-)(1-\gamma_5)\psi_i 
   \end{equation} *)

    let charged_currents n =
      List.map mgm
        [ ((L (-n), Wm, N n), FBF (1, Psibar, VL, Psi), G_CC);
          ((N (-n), Wp, L n), FBF (1, Psibar, VL, Psi), G_CC);
          ((D (-n), Wm, U n), FBF (1, Psibar, VL, Psi), G_CC);
          ((U (-n), Wp, D n), FBF (1, Psibar, VL, Psi), G_CC) ] 

    let charged_heavy_currents n =
      List.map mgm
        [ ((L (-n), Xm, N n), FBF (1, Psibar, VL, Psi), G_CC_heavy);
          ((N (-n), Xp, L n), FBF (1, Psibar, VL, Psi), G_CC_heavy);
          ((D (-n), Xm, U n), FBF (1, Psibar, VL, Psi), G_CC_heavy);
          ((U (-n), Xp, D n), FBF (1, Psibar, VL, Psi), G_CC_heavy) ] 

(*
    let charged_supp_currents = 
      List.map mgm 
        [ ((TopHq, WHp, D 3), FBF (1, Psibar, VL, Psi), G_CC_supp1);
          ((D (-3), WHm, TopH), FBF (1, Psibar, VL, Psi), G_CC_supp1);
          ((TopHq, Wp, D 3), FBF (1, Psibar, VL, Psi), G_CC_supp2);
          ((D (-3), Wm, TopH), FBF (1, Psibar, VL, Psi), G_CC_supp2)] 
*)

    let yukawa =
      [ ((M (U (-3)), O H, M (U 3)), FBF (1, Psibar, S, Psi), G_Htt);
        ((M (D (-3)), O H, M (D 3)), FBF (1, Psibar, S, Psi), G_Hbb);
        ((M (U (-2)), O H, M (U 2)), FBF (1, Psibar, S, Psi), G_Hcc);
        ((M (L (-3)), O H, M (L 3)), FBF (1, Psibar, S, Psi), G_Htautau) ]

    let yukawa_add = 
      [ ((M TopHq, O H, M TopH), FBF (1, Psibar, S, Psi), G_Hthth);
        ((M TopHq, O H, M (U 3)), FBF (1, Psibar, SLR, Psi), G_Htht);
        ((M (U (-3)), O H, M TopH), FBF (1, Psibar, SLR, Psi), G_Htht);
        ((M (U (-3)), O Eta, M (U 3)), FBF (1, Psibar, P, Psi), G_Ett);
        ((M TopHq, O Eta, M (U 3)), FBF (1, Psibar, SLR, Psi), G_Etht);
        ((M DHq, O Eta, M (D 1)), FBF (1, Psibar, SL, Psi), G_Ett);
        ((M (D (-3)), O Eta, M (D 3)), FBF (1, Psibar, P, Psi), G_Ebb);
        ((M (D (-1)), O Eta, M DH), FBF (1, Psibar, SR, Psi), G_Ett);
        ((M (U (-3)), O Eta, M TopH), FBF (1, Psibar, SLR, Psi), G_Etht)]
      
(* \begin{equation}
     \mathcal{L}_{\textrm{TGC}} =
        - e \partial_\mu A_\nu W_+^\mu W_-^\nu + \ldots
        - e \cot\theta_w  \partial_\mu Z_\nu W_+^\mu W_-^\nu + \ldots
   \end{equation} *)

    let tgc ((g1, g2, g3), t, c) = ((G g1, G g2, G g3), t, c)

    let standard_triple_gauge =
      List.map tgc
        [ ((Ga, Wm, Wp), Gauge_Gauge_Gauge 1, I_Q_W);
          ((Z, Wm, Wp), Gauge_Gauge_Gauge 1, I_G_ZWW) ]


    let heavy_triple_gauge =
      List.map tgc
        [ ((Ga, Xm, Xp), Gauge_Gauge_Gauge 1, I_Q_W);
          ((Z, Xm, Xp), Gauge_Gauge_Gauge 1,  I_Q_ZH);
          ((Z, X0, Y0), Gauge_Gauge_Gauge 1,  I_G_Z1);
          ((ZH, X0, Y0), Gauge_Gauge_Gauge 1, I_G_Z2);
          ((Y0, Wm, Xp), Gauge_Gauge_Gauge 1, I_G_Z3);
          ((Y0, Wp, Xm), Gauge_Gauge_Gauge (-1), I_G_Z3);
          ((X0, Wm, Xp), Gauge_Gauge_Gauge 1, I_G_Z4);
          ((X0, Wp, Xm), Gauge_Gauge_Gauge 1, I_G_Z4);
  ]


    let triple_gluon =
      if Flags.include_gluons then
        List.map tgc
          [ ((Gl, Gl, Gl), Gauge_Gauge_Gauge 1, G_strong);
            ((Gl_aux, Gl, Gl), Aux_Gauge_Gauge 1, G_strong) ]
      else
        []

(* \begin{multline}
     \mathcal{L}_{\textrm{TGC}}(g_1,\kappa)
        =   g_1 \mathcal{L}_T(V,W^+,W^-) \\
          + \frac{\kappa+g_1}{2} \Bigl(\mathcal{L}_T(W^-,V,W^+)
                                         - \mathcal{L}_T(W^+,V,W^-)\Bigr)\\
          + \frac{\kappa-g_1}{2} \Bigl(\mathcal{L}_L(W^-,V,W^+)
                                         - \mathcal{L}_T(W^+,V,W^-)\Bigr)
   \end{multline} *)

    let anomalous_triple_gauge =
      List.map tgc
        [ ((Ga, Wp, Wm), Dim4_Vector_Vector_Vector_T 1,
           I_G1_AWW);
          ((Z, Wp, Wm), Dim4_Vector_Vector_Vector_T 1,
           I_G1_ZWW);
          ((Wp, Wm, Ga), Dim4_Vector_Vector_Vector_T 1,
           I_G1_plus_kappa_AWW);
          ((Wp, Wm, Z), Dim4_Vector_Vector_Vector_T 1,
           I_G1_plus_kappa_ZWW);
          ((Wp, Wm, Ga), Dim4_Vector_Vector_Vector_L 1,
           I_G1_minus_kappa_AWW);
          ((Wp, Wm, Z), Dim4_Vector_Vector_Vector_L 1,
           I_G1_minus_kappa_ZWW);
          ((Wm, Ga, Wp), Dim4_Vector_Vector_Vector_T 1,
           I_G1_plus_kappa_AWW);
          ((Wm, Z, Wp), Dim4_Vector_Vector_Vector_T 1,
           I_G1_plus_kappa_ZWW);
          ((Wm, Ga, Wp), Dim4_Vector_Vector_Vector_L 1,
           I_kappa_minus_G1_AWW);
          ((Wm, Z, Wp), Dim4_Vector_Vector_Vector_L 1,
           I_kappa_minus_G1_ZWW);
          ((Ga, Wp, Wm), Dim6_Gauge_Gauge_Gauge 1,
           I_lambda_AWW);
          ((Z, Wp, Wm), Dim6_Gauge_Gauge_Gauge 1,
           I_lambda_ZWW) ]

    let triple_gauge =
      if Flags.include_anomalous then
        anomalous_triple_gauge
      else
        standard_triple_gauge @ heavy_triple_gauge

    let qgc ((g1, g2, g3, g4), t, c) = ((G g1, G g2, G g3, G g4), t, c)

    let gauge4 = Vector4 [(2, C_13_42); (-1, C_12_34); (-1, C_14_23)]
    let minus_gauge4 = Vector4 [(-2, C_13_42); (1, C_12_34); (1, C_14_23)]
    let standard_quartic_gauge =
      List.map qgc
        [ (Wm, Wp, Wm, Wp), gauge4, G_WWWW;
          (Wm, Z, Wp, Z), minus_gauge4, G_ZZWW;
          (Wm, Z, Wp, Ga), minus_gauge4, G_AZWW;
          (Wm, Ga, Wp, Ga), minus_gauge4, G_AAWW ]


    let anomalous_quartic_gauge =
      if Flags.include_anomalous then
        List.map qgc
          [ ((Wm, Wm, Wp, Wp),
             Vector4 [(1, C_13_42); (1, C_14_23)], Alpha_WWWW0);
            ((Wm, Wm, Wp, Wp),
             Vector4 [1, C_12_34], Alpha_WWWW2);
            ((Wm, Wp, Z, Z),
             Vector4 [1, C_12_34], Alpha_ZZWW0);
            ((Wm, Wp, Z, Z),
             Vector4 [(1, C_13_42); (1, C_14_23)], Alpha_ZZWW1);
            ((Z, Z, Z, Z),
             Vector4 [(1, C_12_34); (1, C_13_42); (1, C_14_23)], Alpha_ZZZZ) ]
      else
        []

(* In any diagonal channel~$\chi$, the scattering amplitude~$a_\chi(s)$ is
   unitary iff\footnote{%
     Trivial proof:
     \begin{equation}
       -1 = \textrm{Im}\left(\frac{1}{a_\chi(s)}\right)
          = \frac{\textrm{Im}(a_\chi^*(s))}{|a_\chi(s)|^2}
          = - \frac{\textrm{Im}(a_\chi(s))}{|a_\chi(s)|^2}
     \end{equation}
     i.\,e.~$\textrm{Im}(a_\chi(s)) = |a_\chi(s)|^2$.}
   \begin{equation}
     \textrm{Im}\left(\frac{1}{a_\chi(s)}\right) = -1
   \end{equation}
   For a real perturbative scattering amplitude~$r_\chi(s)$ this can be
   enforced easily--and arbitrarily--by
   \begin{equation}
     \frac{1}{a_\chi(s)} = \frac{1}{r_\chi(s)} - \mathrm{i}
   \end{equation} *)

    let k_matrix_quartic_gauge =
      if Flags.k_matrix then
        List.map qgc
          [ ((Wm, Wp, Wm, Wp),
             Vector4_K_Matrix_tho (0, [K_Matrix_Coeff 0, K_Matrix_Pole 0]), Alpha_WWWW0);
            ((Wm, Wm, Wp, Wp),    
             Vector4_K_Matrix_tho (0, [K_Matrix_Coeff 2, K_Matrix_Pole 2]), Alpha_WWWW0);
            ((Wm, Wp, Z, Z),      
             Vector4_K_Matrix_tho (0, [(K_Matrix_Coeff 0, K_Matrix_Pole 0);
                               (K_Matrix_Coeff 2, K_Matrix_Pole 2)]), Alpha_WWWW0);
            ((Wm, Z, Wp, Z),
             Vector4_K_Matrix_tho (0, [K_Matrix_Coeff 1, K_Matrix_Pole 1]), Alpha_WWWW0);
            ((Z, Z, Z, Z),
             Vector4_K_Matrix_tho (0, [K_Matrix_Coeff 0, K_Matrix_Pole 0]), Alpha_WWWW0) ]
      else
        []

    let heavy_quartic_gauge = 
      []


    let quartic_gauge =
      standard_quartic_gauge @ anomalous_quartic_gauge @ k_matrix_quartic_gauge
                             @ heavy_quartic_gauge

    let standard_gauge_higgs' =
      [ ((O H, G Wp, G Wm), Scalar_Vector_Vector 1, G_HWW);
        ((O H, G Z, G Z), Scalar_Vector_Vector 1, G_HZZ) ]

    let heavy_gauge_higgs = 
      [ ((O H, G Wp, G Xm), Scalar_Vector_Vector 1, G_heavy_HWW);
        ((O H, G Wm, G Xp), Scalar_Vector_Vector 1, G_heavy_HWW);
        ((O H, G Z, G X0),  Scalar_Vector_Vector 1,  G_heavy_HVV);
        ((O H, G ZH, G X0), Scalar_Vector_Vector 1, G_heavy_HVV)]

    let standard_gauge_higgs = 
      standard_gauge_higgs' @ heavy_gauge_higgs 

    let standard_gauge_higgs4 =
      [ (O H, O H, G Wp, G Wm), Scalar2_Vector2 1, G_HHWW;
        (O H, O H, G Z, G Z), Scalar2_Vector2 1, G_HHZZ ]

(*
    let standard_heavy_gauge_higgs4 =
      [ (O H, O H, G WHp, G Wm), Scalar2_Vector2 1, G_heavy_HHVV;
        (O H, O H, G Wp, G WHm), Scalar2_Vector2 1, G_heavy_HHVV;
        (O H, O H, G Z, G ZH), Scalar2_Vector2 1, G_heavy_HHVV ]
*)

    let standard_higgs =
      [ (O H, O H, O H), Scalar_Scalar_Scalar 1, G_H3 ]
        
   let anomaly_higgs = 
      [ (*
        (O H, G Ga, G Ga), Dim5_Scalar_Gauge2 1, G_HGaGa;
        (O H, G Ga, G Z), Dim5_Scalar_Gauge2 1, G_HGaZ;*)
        (O Eta, G Gl, G Gl), Dim5_Scalar_Gauge2_Skew 1, G_EGlGl;
        (O Eta, G Ga, G Ga), Dim5_Scalar_Gauge2_Skew 1, G_EGaGa; 
        (O Eta, G Ga, G Z), Dim5_Scalar_Gauge2_Skew 1, G_EGaZ]

    let standard_higgs4 =
      [ (O H, O H, O H, O H), Scalar4 1, G_H4 ]

    let anomalous_gauge_higgs =
      []

    let anomalous_gauge_higgs4 =
      []

    let anomalous_higgs =
      []

    let anomalous_higgs4 =
      []

    let gauge_higgs =
      if Flags.include_anomalous then
        standard_gauge_higgs @ anomalous_gauge_higgs
      else
        standard_gauge_higgs

    let gauge_higgs4 =
      if Flags.include_anomalous then
        standard_gauge_higgs4 @ anomalous_gauge_higgs4
      else
        standard_gauge_higgs4

    let higgs =
      if Flags.include_anomalous then
        standard_higgs @ anomalous_higgs
      else
        standard_higgs

    let eta_higgs_gauge =
      [ (G Z, O Eta, O H), Vector_Scalar_Scalar 1, G_ZEH;
        (G ZH, O Eta, O H), Vector_Scalar_Scalar 1, G_ZHEH;
        (G X0, O Eta, O H), Vector_Scalar_Scalar 1, G_XEH ]    


    let higgs4 =
      if Flags.include_anomalous then
        standard_higgs4 @ anomalous_higgs4
      else
        standard_higgs4

    let goldstone_vertices =
      [ ((O Phi0, G Wm, G Wp), Scalar_Vector_Vector 1, I_G_ZWW);
        ((O Phip, G Ga, G Wm), Scalar_Vector_Vector 1, I_Q_W);
        ((O Phip, G Z, G Wm), Scalar_Vector_Vector 1, I_G_ZWW);
        ((O Phim, G Wp, G Ga), Scalar_Vector_Vector 1, I_Q_W);
        ((O Phim, G Wp, G Z), Scalar_Vector_Vector 1, I_G_ZWW) ]

    let vertices3' =
      (ThoList.flatmap electromagnetic_currents [1;2;3] @ 
       ThoList.flatmap color_currents [1;2;3] @
       ThoList.flatmap neutral_currents [1;2;3] @
       ThoList.flatmap neutral_heavy_currents [1;2;3] @       
       ThoList.flatmap charged_currents [1;2;3] @
       anomaly_higgs @
(*       ThoList.flatmap charged_heavy_currents [1;2;3] @ *)
       heavy_top_currents @ eta_higgs_gauge @
       yukawa @ yukawa_add @ triple_gauge @ triple_gluon @
       gauge_higgs @ higgs @ goldstone_vertices)

    let vertices3 = 
      if Flags.include_supp then
        vertices3' @ neutral_supp_currents (* @ charged_supp_currents *)
      else
        vertices3' 

    let vertices4 =
      quartic_gauge @ gauge_higgs4 @ higgs4

    let vertices () = (vertices3, vertices4, [])

(* For efficiency, make sure that [F.of_vertices vertices] is
   evaluated only once. *)

    let table = F.of_vertices (vertices ())
    let fuse2 = F.fuse2 table
    let fuse3 = F.fuse3 table
    let fuse = F.fuse table
    let max_degree () = 4

    let flavor_of_string = function
      | "e-" -> M (L 1) | "e+" -> M (L (-1))
      | "mu-" -> M (L 2) | "mu+" -> M (L (-2))
      | "tau-" -> M (L 3) | "tau+" -> M (L (-3))
      | "nue" -> M (N 1) | "nuebar" -> M (N (-1))
      | "numu" -> M (N 2) | "numubar" -> M (N (-2))
      | "nutau" -> M (N 3) | "nutaubar" -> M (N (-3))
      | "u" -> M (U 1) | "ubar" -> M (U (-1))
      | "c" -> M (U 2) | "cbar" -> M (U (-2))
      | "t" -> M (U 3) | "tbar" -> M (U (-3))
      | "d" -> M (D 1) | "dbar" -> M (D (-1))
      | "s" -> M (D 2) | "sbar" -> M (D (-2))
      | "b" -> M (D 3) | "bbar" -> M (D (-3))
      | "th" -> M TopH  | "thbar" -> M TopHq
      | "dh" -> M DH  | "dhbar" -> M DHq
      | "eta" | "Eta" -> O Eta
      | "g" -> G Gl
      | "A" -> G Ga | "Z" | "Z0" -> G Z
      | "ZH" | "ZH0" | "Zh" | "Zh0" -> G ZH
      | "W+" -> G Wp | "W-" -> G Wm
      | "X+" -> G Xp | "X-" -> G Xm
      | "X0" -> G X0 | "Y0" -> G Y0
      | "H" -> O H
      | _ -> invalid_arg "Models.Zprime.flavor_of_string" 

    let flavor_to_string = function
      | M f ->
          begin match f with
          | L 1 -> "e-" | L (-1) -> "e+"
          | L 2 -> "mu-" | L (-2) -> "mu+"
          | L 3 -> "tau-" | L (-3) -> "tau+"
          | L _ -> invalid_arg
                "Models.Zprime.flavor_to_string: invalid lepton"
          | N 1 -> "nue" | N (-1) -> "nuebar"
          | N 2 -> "numu" | N (-2) -> "numubar"
          | N 3 -> "nutau" | N (-3) -> "nutaubar"
          | N _ -> invalid_arg
                "Models.Zprime.flavor_to_string: invalid neutrino"
          | U 1 -> "u" | U (-1) -> "ubar"
          | U 2 -> "c" | U (-2) -> "cbar"
          | U 3 -> "t" | U (-3) -> "tbar"
          | U _ -> invalid_arg
                "Models.Zprime.flavor_to_string: invalid up type quark"
          | D 1 -> "d" | D (-1) -> "dbar"
          | D 2 -> "s" | D (-2) -> "sbar"
          | D 3 -> "b" | D (-3) -> "bbar"
          | D _ -> invalid_arg
                "Models.Zprime.flavor_to_string: invalid down type quark"
          | TopH -> "th" | TopHq -> "thbar"
          | DH -> "dh" | DHq -> "dhbar"                
          end
      | G f ->
          begin match f with
          | Gl -> "g"
          | Ga -> "A" | Z -> "Z"
          | Wp -> "W+" | Wm -> "W-"
          | Xp -> "X+" | Xm -> "X-" | X0 -> "X0" | Y0 -> "Y0" | ZH -> "ZH" 
          | Gl_aux -> "gx"
          end
      | O f ->
          begin match f with
          | Phip -> "phi+" | Phim -> "phi-" | Phi0 -> "phi0" 
          | H -> "H" | Eta -> "Eta"
          end

    let flavor_symbol = function
      | M f ->
          begin match f with
          | L n when n > 0 -> "l" ^ string_of_int n
          | L n -> "l" ^ string_of_int (abs n) ^ "b"
          | N n when n > 0 -> "n" ^ string_of_int n
          | N n -> "n" ^ string_of_int (abs n) ^ "b"
          | U n when n > 0 -> "u" ^ string_of_int n
          | U n -> "u" ^ string_of_int (abs n) ^ "b"
          | D n when n > 0 ->  "d" ^ string_of_int n
          | D n -> "d" ^ string_of_int (abs n) ^ "b"
          | TopH -> "th" | TopHq -> "thb" 
          | DH -> "dh" | DHq -> "dhb" 
          end
      | G f ->
          begin match f with
          | Gl -> "gl"
          | Ga -> "a" | Z -> "z"
          | Wp -> "wp" | Wm -> "wm"
          | Xp -> "xp" | Xm -> "xm" | X0 -> "x0" | Y0 -> "y0" | ZH -> "zh"
          | Gl_aux -> "gx"
          end
      | O f ->
          begin match f with
          | Phip -> "pp" | Phim -> "pm" | Phi0 -> "p0" 
          | H -> "h" | Eta -> "eta"
          end

(* There are PDG numbers for Z', Z'', W', 32-34, respectively.
   We just introduce a number 38 for Y0 as a Z'''.
   As well, there is the number 8 for a t'. But we cheat a little bit and 
   take the number 35 which is reserved for a heavy scalar Higgs for the 
   Eta scalar.
*)  

    let pdg = function
      | M f ->
          begin match f with
          | L n when n > 0 -> 9 + 2*n
          | L n -> - 9 + 2*n
          | N n when n > 0 -> 10 + 2*n
          | N n -> - 10 + 2*n
          | U n when n > 0 -> 2*n
          | U n -> 2*n
          | D n when n > 0 -> - 1 + 2*n
          | D n -> 1 + 2*n
          | DH -> 7 | DHq -> (-7)
          | TopH -> 8 | TopHq -> (-8)
          end
      | G f ->
          begin match f with
          | Gl -> 21
          | Ga -> 22 | Z -> 23
          | Wp -> 24 | Wm -> (-24)
          | Xp -> 34 | Xm -> (-34) | ZH -> 32 | X0 -> 33 | Y0 -> 38
          | Gl_aux -> 21
          end
      | O f ->
          begin match f with
          | Phip | Phim -> 27 | Phi0 -> 26
          | H -> 25 | Eta -> 36
          end

    let mass_symbol f = 
      "mass(" ^ string_of_int (abs (pdg f)) ^ ")"

    let width_symbol f =
      "width(" ^ string_of_int (abs (pdg f)) ^ ")"

    let constant_symbol = function
      | Unit -> "unit" | Pi -> "PI" | VHeavy -> "vheavy"
      | Alpha_QED -> "alpha" | E -> "e" | G_weak -> "g" | Vev -> "vev"
      | Sin2thw -> "sin2thw" | Sinthw -> "sinthw" | Costhw -> "costhw"
      | Sinpsi -> "sinpsi" | Cospsi -> "cospsi" 
      | Atpsi -> "atpsi" | Sccs -> "sccs"
      | Supp -> "vF" | Supp2 -> "v2F2" 
      | Q_lepton -> "qlep" | Q_up -> "qup" | Q_down -> "qdwn"
      | Q_Z_up -> "qzup" 
      | G_over4 -> "gov4" | G_over4_sup -> "gov4sup" | G_CC_sup -> "gccsup"
      | G_zhthth -> "gzhthth" 
      | G_NC_lepton -> "gnclep" | G_NC_neutrino -> "gncneu"
      | G_NC_up -> "gncup" | G_NC_down -> "gncdwn"
      | G_CC -> "gcc" | G_CC_heavy -> "gcch" 
      | G_CC_supp1 -> "gsupp1" | G_CC_supp2 -> "gsupp2" 
      | G_NC_h_lepton -> "gnchlep" | G_NC_h_neutrino -> "gnchneu"
      | G_NC_h_up -> "gnchup" | G_NC_h_down -> "gnchdwn"   
(*      | G_NC_heavy -> "gnch"   *)
      | I_Q_W -> "iqw" | I_G_ZWW -> "igzww" | I_G_WWW -> "igwww"
      | I_Q_H -> "iqh" | I_Q_ZH -> "iqzh"
      | I_G_Z1 -> "igz1" | I_G_Z2 -> "igz2"
      | I_G_Z3 -> "igz3" | I_G_Z4 -> "igz4"
      | G_WWWW -> "gw4" | G_ZZWW -> "gzzww"
      | G_AZWW -> "gazww" | G_AAWW -> "gaaww"
      | I_G1_AWW -> "ig1a" | I_G1_ZWW -> "ig1z"
      | I_G1_plus_kappa_AWW -> "ig1pka"
      | I_G1_plus_kappa_ZWW -> "ig1pkz"
      | I_G1_minus_kappa_AWW -> "ig1mka"
      | I_G1_minus_kappa_ZWW -> "ig1mkz"
      | I_kappa_minus_G1_AWW -> "ikmg1a"
      | I_kappa_minus_G1_ZWW -> "ikmg1z"
      | I_lambda_AWW -> "ila" | I_lambda_ZWW -> "ilz"
      | Alpha_WWWW0 -> "alww0" | Alpha_WWWW2 -> "alww2"
      | Alpha_ZZWW0 -> "alzw0" | Alpha_ZZWW1 -> "alzw1"
      | Alpha_ZZZZ  -> "alzz"
      | G_HWW -> "ghww" | G_HZZ -> "ghzz"
      | G_heavy_HVV -> "ghyhvv"
      | G_heavy_HWW -> "ghyhww"
      | G_heavy_HZZ -> "ghyhzz"
      | G_HHWW -> "ghhww" | G_HHZZ -> "ghhzz"
      | G_heavy_HHVV -> "ghyhhvv"
      | G_Htt -> "ghtt" | G_Hbb -> "ghbb"
      | G_Htautau -> "ghtautau" | G_Hcc -> "ghcc"
      | G_Hthth -> "ghthth" | G_Htht -> "ghtht"
      | G_Ethth -> "gethth" | G_Etht -> "getht"
      | G_Ett -> "gett" | G_Ebb -> "gebb"
      | G_HGaGa -> "ghaa" | G_HGaZ -> "ghaz"
      | G_EGaGa -> "geaa" | G_EGaZ -> "geaz" | G_EGlGl -> "gegg"
      | G_ZEH -> "gzeh" | G_ZHEH -> "gzheh" | G_XEH -> "gxeh"
      | G_H3 -> "gh3" | G_H4 -> "gh4"
      | G_strong -> "gs"
      | Mass f -> "mass" ^ flavor_symbol f
      | Width f -> "width" ^ flavor_symbol f
      | K_Matrix_Coeff i -> "kc" ^ string_of_int i
      | K_Matrix_Pole i -> "kp" ^ string_of_int i
  end

module O = Omega.Make(Fusion.Mixed23)(Targets.Fortran)
    (Zprime(SM_no_anomalous))
let _ = O.main ()

(*i
 *  Local Variables:
 *  mode:caml
 *  indent-tabs-mode:nil
 *  page-delimiter:"^(\\* .*\n"
 *  End:
i*)
