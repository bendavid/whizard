(* f90_SAGT.ml --

   Copyright (C) 1999-2024 by

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

(* \subsection*{Lagrangian} *)

(* Simplest model available: 
   \begin{equation}
     \dfrac{1}{2} \begin{bmatrix} \hat{\Phi}^\dagger \exp(\mathcal{-V}) 
     \hat{\Phi} \end{bmatrix}_D + \dfrac{1}{2} \Re \begin{bmatrix} 
     \overline{W_R} W_L \end{bmatrix}_F 
   \end{equation}
   We discuss a SUSY-model with $U(1)$ gauge group 
   and only one superfield. Here the fermion is a Majorana-fermion and gets 
   a chiral charge, so this model is not SQED, the supersymmetric extension of
   QED. All particles are forced by gauge invariance to be massless.  *)

module SAGT = 
  struct
    open Coupling
    let options = Options.empty

    type flavor = 
      | A | B | F | Ph | Phino | J
      | C | Cbar | Xi | LOp
      | BRST of flavor

(* All particles are self-charge-conjugate. *)

    let rec conjugate = function
      | C -> Cbar 
      | Cbar -> C 
      | f -> f


    let external_flavors () =
      [ "fields", [A; B; F; Ph; Phino];
        "ghosts", [C; Cbar; Xi];
        "BRST transformations (ghost sources)", [BRST A; BRST B; 
                                      BRST F; BRST Ph; BRST Phino];
        "Local Operator", [LOp];
        "currents", [J] ]

    let flavors () = ThoList.flatmap snd (external_flavors ())

    let flavor_of_string = function
      | "a" -> A | "b" -> B 
      | "ph" -> Ph | "phino" -> Phino | "f" -> F
      | "c" -> C | "cbar" -> Cbar | "xi" -> Xi
      | "brs_a" -> BRST A | "brs_b" -> BRST B | "brs_f" -> BRST F
      | "brs_ph" -> BRST Ph | "brs_phino" -> BRST Phino 
      | "j" -> J | "lop" -> LOp
      | _ -> invalid_arg "SAGT.flavor_of_string"

    let rec flavor_to_string = function
      | A -> "a" | B -> "b" | F -> "f" 
      | Ph -> "ph" | Phino -> "phino" 
      | C -> "c" | Cbar -> "cbar" | Xi -> "xi"
      | J -> "j" | LOp -> "lop"
      | BRST f -> "brs_" ^ flavor_to_string f 

    let rec flavor_symbol = function
      | A -> "a" | B -> "b" | F -> "f" 
      | Ph -> "ph" | Phino -> "phino" 
      | C -> "c" | Cbar -> "cbar" | Xi -> "xi"
      | J -> "j" | LOp -> "lop"
      | BRST f -> "brs_" ^ flavor_symbol f 

    let rec lorentz = function
      | A | B | C | Cbar | LOp -> Scalar
      | F | Phino -> Majorana
      | Xi -> Maj_Ghost 
      | Ph -> Vector
      | J -> Vectorspinor
      | BRST f -> BRS (lorentz f)

    let propagator = function
      | A | B -> Prop_Scalar 
      | C | Cbar -> Prop_Ghost
      | Ph -> Prop_Feynman
      | F | Phino -> Prop_Majorana
      | J | Xi | LOp | BRST _ -> Only_Insertion 

    let width _ = Timelike
    let goldstone _ = None

    let fermion = function
      | A | B | Ph | C | Cbar | BRST A | BRST B | BRST Ph | LOp -> 0
      | F | Phino | J | Xi | BRST F | BRST Phino -> 2
      | BRST _ -> 42

    let color _ = Color.Singlet
    type gauge = unit
    let gauge_symbol () = failwith "SAGT.gauge_symbol: internal error"

    let colsymm _ = (0,false),(0,false)

(* \begin{multline}
     \mathcal{L} = \dfrac{1}{2} (\partial_\mu A) (\partial^\mu A) + 
       \dfrac{1}{2} (\partial_\mu B) (\partial^\mu B) + \dfrac{\ii}{2} 
       \overline{\Psi} \fmslash{\partial} \Psi - \dfrac{1}{4} F_{\mu\nu} 
       F^{\mu\nu} + \dfrac{\ii}{2} \overline{\lambda} \fmslash{\partial} 
       \lambda \\  
       + e G_\mu \left( B \partial^\mu A - A \partial^\mu B 
       \right) + \dfrac{e^2}{2} G_\mu G^\mu \left( A^2 + B^2 \right) - e 
       \left( \overline{\Psi} \lambda \right) A \\ 
       - \ii e \left( \overline{\Psi} \gamma^5 \lambda \right) B - 
       \dfrac{e}{2} \overline{\Psi} \fmslash{G} \gamma^5 \Psi - \dfrac{e^2}{8}
       \left( A^4 + B^4 + 2 A^2 B^2 \right) 
   \end{multline}
   Propagators
   \begin{subequations}
     \begin{align}
       \parbox{21mm}{%
         \begin{fmfgraph*}(20,5)
           \fmfleft{i}\fmfright{o}
           \fmflabel{$A(p)$}{i}
           \fmflabel{$A(p)$}{o}
           \fmf{dashes}{i,o}
           \fmfdot{i,o}
         \end{fmfgraph*}}\qquad\quad
           &= \frac{\mathrm{i}}{p^2+\mathrm{i}\epsilon} \\
       \parbox{21mm}{%
         \begin{fmfgraph*}(20,5)
           \fmfleft{i}\fmfright{o}
           \fmflabel{$B(p)$}{i}
           \fmflabel{$B(p)$}{o}
           \fmf{dbl_dashes}{i,o}
           \fmfdot{i,o}
         \end{fmfgraph*}}\qquad\quad
           &= \frac{\mathrm{i}}{p^2+\mathrm{i}\epsilon} \\
       \parbox{21mm}{%
         \begin{fmfgraph*}(20,5)
           \fmfleft{i}\fmfright{o}
           \fmflabel{$G_\mu(p)$}{i}
           \fmflabel{$G_\nu(p)$}{o}
           \fmf{photon}{i,o}
           \fmfdot{i,o}
         \end{fmfgraph*}}\qquad\quad
           &= \frac{- \mathrm{i} \eta_{\mu\nu}}{p^2+\mathrm{i}\epsilon} \\
       \parbox{21mm}{%
         \begin{fmfgraph*}(20,5)
           \fmfleft{i}\fmfright{o}
           \fmflabel{$\Psi(p)$}{i}
           \fmflabel{$\overline{\Psi}(p)$}{o}
           \fmf{plain}{i,o}
           \fmfdot{i,o}
         \end{fmfgraph*}}\qquad\quad
           &= \frac{\mathrm{i} \fmslash{p}}{p^2+\mathrm{i}\epsilon} \\
       \parbox{21mm}{%
         \begin{fmfgraph*}(20,5)
           \fmfleft{i}\fmfright{o}
           \fmflabel{$\lambda(p)$}{i}
           \fmflabel{$\overline{\lambda}(p)$}{o}
           \fmf{plain}{i,o} \fmf{photon,wiggly_len=1mm}{i,o}
           \fmfdot{i,o}
         \end{fmfgraph*}}\qquad\quad
           &= \frac{\mathrm{i} \fmslash{p}}{p^2+\mathrm{i}\epsilon}   
     \end{align}
   \end{subequations}
   Three point vertices (all momenta incoming)
   \begin{subequations}
     \begin{align}
       \parbox{21mm}{%
         \hfil\\\hfil\\
         \begin{fmfgraph*}(20,15)
           \fmfleft{p1}\fmfright{p2,p3}
           \fmflabel{$G_\mu(p_1)$}{p1}
           \fmflabel{$B(p_3)$}{p2}
           \fmflabel{$A(p_2)$}{p3}
           \fmf{photon}{p1,v}
           \fmf{dashes}{p3,v} \fmf{dbl_dashes}{p2,v}
           \fmfdot{v}
         \end{fmfgraph*}\\
         \hfil}\qquad\quad
           &= e (p_2 - p_3)_\mu \\
       \parbox{21mm}{%
         \hfil\\\hfil\\
         \begin{fmfgraph*}(20,15)
           \fmfleft{p1}\fmfright{p2,p3}
           \fmflabel{$G_\mu(p_1)$}{p1}
           \fmflabel{$\Psi(p_3)$}{p2}
           \fmflabel{$\Psi(p_2)$}{p3}
           \fmf{photon}{p1,v}
           \fmf{plain}{p2,v,p3} 
           \fmfdot{v}
         \end{fmfgraph*}\\
         \hfil}\qquad\quad
           &= \ii e \gamma^5 \gamma_\mu    \\
       \parbox{21mm}{%
         \hfil\\\hfil\\
         \begin{fmfgraph*}(20,15)
           \fmfleft{p1}\fmfright{p2,p3}
           \fmflabel{$A(p_1)$}{p1}
           \fmflabel{$\lambda(p_3)$}{p2}
           \fmflabel{$\Psi(p_2)$}{p3}
           \fmf{dashes}{p1,v}
           \fmf{plain}{p2,v,p3} \fmffreeze
           \fmf{photon}{v,p2}
           \fmfdot{v}
         \end{fmfgraph*}\\
         \hfil}\qquad\quad
           &= - \ii e \\
       \parbox{21mm}{%
         \hfil\\\hfil\\
         \begin{fmfgraph*}(20,15)
           \fmfleft{p1}\fmfright{p2,p3}
           \fmflabel{$B(p_1)$}{p1}
           \fmflabel{$\lambda(p_3)$}{p2}
           \fmflabel{$\Psi(p_2)$}{p3}
           \fmf{dbl_dashes}{p1,v}
           \fmf{plain}{p2,v,p3} \fmffreeze
           \fmf{photon}{v,p2}
           \fmfdot{v}
         \end{fmfgraph*}\\
         \hfil}\qquad\quad
           &= e \gamma^5
     \end{align}
   \end{subequations}
   Four point vertices
   \begin{subequations}
     \begin{align}
       \parbox{21mm}{%
         \hfil\\\hfil\\
         \begin{fmfgraph*}(20,15)
           \fmfleft{p1,p2}\fmfright{p3,p4}
           \fmflabel{$A(p_1)$}{p1}
           \fmflabel{$A(p_2)$}{p2}
           \fmflabel{$A(p_3)$}{p3}
           \fmflabel{$A(p_4)$}{p4}
           \fmf{dashes}{p1,v,p2}
           \fmf{dashes}{p3,v,p4}
           \fmfdot{v}
         \end{fmfgraph*}\\
         \hfil}\qquad\quad
           &= - 3 \ii e^2  \\
       \parbox{21mm}{%
         \hfil\\\hfil\\
         \begin{fmfgraph*}(20,15)
           \fmfleft{p1,p2}\fmfright{p3,p4}
           \fmflabel{$B(p_1)$}{p1}
           \fmflabel{$B(p_2)$}{p2}
           \fmflabel{$B(p_3)$}{p3}
           \fmflabel{$B(p_4)$}{p4}
           \fmf{dbl_dashes}{p1,v,p2}
           \fmf{dbl_dashes}{p3,v,p4}
           \fmfdot{v}
         \end{fmfgraph*}\\
         \hfil}\qquad\quad
           &= - 3 \ii e^2   \\
       \parbox{21mm}{%
         \hfil\\\hfil\\
         \begin{fmfgraph*}(20,15)
           \fmfleft{p1,p2}\fmfright{p3,p4}
           \fmflabel{$A(p_1)$}{p1}
           \fmflabel{$A(p_2)$}{p2}
           \fmflabel{$B(p_3)$}{p3}
           \fmflabel{$B(p_4)$}{p4}
           \fmf{dashes}{p1,v,p2}
           \fmf{dbl_dashes}{p3,v,p4}
           \fmfdot{v}
         \end{fmfgraph*}\\
         \hfil}\qquad\quad
           &= - \ii e^2 \\
       \parbox{21mm}{%
         \hfil\\\hfil\\
         \begin{fmfgraph*}(20,15)
           \fmfleft{p1,p2}\fmfright{p3,p4}
           \fmflabel{$A(p_1)$}{p1}
           \fmflabel{$A(p_2)$}{p2}
           \fmflabel{$G_\mu(p_3)$}{p3}
           \fmflabel{$G_\nu(p_4)$}{p4}
           \fmf{dashes}{p1,v,p2}
           \fmf{photon}{p3,v,p4}
           \fmfdot{v}
         \end{fmfgraph*}\\
         \hfil}\qquad\quad
           &= 2 \ii e^2 \eta_{\mu\nu} \\
       \parbox{21mm}{%
         \hfil\\\hfil\\
         \begin{fmfgraph*}(20,15)
           \fmfleft{p1,p2}\fmfright{p3,p4}
           \fmflabel{$B(p_1)$}{p1}
           \fmflabel{$B(p_2)$}{p2}
           \fmflabel{$G_\mu(p_3)$}{p3}
           \fmflabel{$G_\nu(p_4)$}{p4}
           \fmf{dbl_dashes}{p1,v,p2}
           \fmf{photon}{p3,v,p4}
           \fmfdot{v}
         \end{fmfgraph*}\\
         \hfil}\qquad\quad
           &= 2 \ii e^2 \eta_{\mu\nu}
     \end{align}
   \end{subequations} *)

(* \subsection*{Conserved Current} *)

(* \begin{multline}
     \mathcal{L}\lbrack J_{3/2} \rbrack =
       J_{3/2}^\mu \biggl\{ - (\fmslash{\partial} A) \gamma_\mu \Psi -  
     (\ii\fmslash{\partial} B) \gamma_\mu \gamma^5 \Psi + \ii e A \fmslash{G} 
     \gamma_\mu \gamma^5 \Psi - e B \fmslash{G}  \gamma_\mu \Psi \\   +
     \dfrac{1}{2} \lbrack \gamma^\alpha , \gamma^\beta \rbrack \gamma_\mu 
     \gamma^5 (\partial_\alpha G_\beta) \lambda - \dfrac{\ii e}{2} 
     \left( A^2 + B^2 \right) \gamma_\mu \lambda \biggr\}
   \end{multline} *)

    type constant =
      | Unity | Im | Null | E | EC | I_E | E2
      | MA | MB | MP | MPINO | MF | MJ | MXI | MC
      | WA | WB | WP | WPINO | WF | WJ | WC
      | G_MOMA | G_MOMB | G_L | G_S2
    let constant_symbol = function
      | Unity -> "unity" | Im -> "im" | Null -> "null" 
      | E -> "e" | EC -> "ec" | I_E -> "ie" | E2 -> "e2" 
      | MA -> "ma" | MB -> "mb" | MP -> "mp" | MXI -> "mxi" | MC -> "mc" 
      | MPINO -> "mpino" | MF -> "mf" | MJ -> "mj" 
      | WA -> "wa" | WB -> "wb" | WP -> "wp" | WC -> "wc"
      | WPINO -> "wpino" | WF -> "wf" | WJ -> "wj"
      | G_MOMA -> "gmoma" | G_MOMB -> "gmomb"
      | G_L -> "gl" | G_S2 -> "gs2"

    let vertices () =
      ([(Ph, A, B), Vector_Scalar_Scalar 1, I_E;
        (F, A, Phino), FBF ((-1), Chibar, S, Chi), EC;
        (F, B, Phino), FBF ((-1), Chibar, P, Chi), I_E;
        (F, Ph, F), FBF (1, Chibar, Coupling.A, Chi), EC;
        (BRST A, C, B), Scalar_Scalar_Scalar 1, I_E;
        (BRST B, C, A), Scalar_Scalar_Scalar (-1), I_E;
        (BRST F, C, F), FBF ((-1), Chibar, P, Chi), EC;
        (Xi, BRST A, F), FBF (1, Chibar, P, Chi), Im;
        (F, BRST B, Xi), FBF ((-1), Chibar, S, Chi), Unity;
        (BRST F, A, Xi), GBG (1, Chibar, MOM5, Chi), G_MOMA;
        (BRST F, B, Xi), GBG (1, Chibar, MOM, Chi), G_MOMB;
        (BRST Phino, Ph, Xi), GBG (1, Chibar, VMOM, Chi), G_L;
        (BRST Ph, C, LOp), Vector_Scalar_Scalar 1, Unity;
        (Phino, BRST Ph, Xi), FBF (1, Chibar, V, Chi), Im;
        (Phino, Cbar, Xi), GBG ((-1), Chibar, MOM, Chi), G_MOMB; 
        (J, A, F), GBG (1, Gravbar, S, Chi), Unity;
        (J, B, F), GBG (1, Gravbar, P, Chi), Unity;
        (J, Ph, Phino), GBG (1, Gravbar, V, Chi), Unity],
       [(A, A, A, A), Scalar4 (-3), E2;
        (B, B, B, B), Scalar4 (-3), E2;
        (A, A, B, B), Scalar4 (-1), E2;
        (A, A, Ph, Ph), Scalar2_Vector2 2, E2;
        (B, B, Ph, Ph), Scalar2_Vector2 2, E2;
        (BRST F, A, Ph, Xi), GBBG ((-1), Chibar, SV, Chi), I_E;
        (BRST F, B, Ph, Xi), GBBG (1, Chibar, PV, Chi), EC;
        (BRST Phino, A, A, Xi), GBBG (1, Chibar, S2, Chi), G_S2;
	(BRST Phino, B, B, Xi), GBBG (1, Chibar, S2, Chi), G_S2;   
        (J, A, Ph, F), GBBG (1, Gravbar, SV, Chi), Unity;
        (J, B, Ph, F), GBBG (1, Gravbar, PV, Chi), Unity;
        (J, A, A, Phino), GBBG (1, Gravbar, S2, Chi), Unity;
        (J, B, B, Phino), GBBG (1, Gravbar, S2, Chi), Unity],
       [])

    let parameters () =
      { input = [E, 0.1; MJ, 0.0; WJ, 0.0];
        derived =
        [ Complex Unity, Const 1;
          Complex Null, Const 0;
          Real MA, Const 0; Real MB, Const 0; Real MPINO, Const 0;
          Real MP, Const 0; Real MF, Const 0; Real MXI, Const 0; 
          Real MC, Const 0; 
          Real WA, Const 0; Real WB, Const 0; Real WPINO, Const 0;
          Real WP, Const 0; Real WF, Const 0; Real WC, Const 0;
          Complex EC, Atom E;
          Complex E2, Prod [Atom E; Atom E];
          Complex I_E, Prod [I; Atom E];
          Complex Im, I;
          Complex G_L, Quot (Neg I, Const 2);
          Complex G_S2, Neg (Quot (Prod [I; Atom E], Const 2))];  
        derived_arrays = [Complex_Array G_MOMA, [Neg I; Const 0];
                          Complex_Array G_MOMB, [Const 1; Const 0]]}

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
      | A -> 1 | B -> 2 | J -> 0
      | F -> 3 | Ph -> 4 | Phino -> 5 | C -> 6 | Cbar -> -6 | Xi -> 7
      | BRST A -> 1001 | BRST B -> 1002 | BRST F -> 1003
      | BRST Ph -> 1004 | BRST Phino -> 1005 | LOp -> 42 | BRST _ -> 1234567
    let mass_symbol = function
      | A | BRST A -> "ma" | B | BRST B -> "mb" | J -> "mj"
      | F | BRST F -> "mf" | Ph | BRST Ph -> "mp" 
      | C | Cbar -> "mc" | Xi -> "mxi" | LOp -> "mlop"
      | Phino | BRST Phino -> "mpino" | BRST _ -> ""
    let width_symbol = function
      | A | BRST A -> "wa" | B | BRST B -> "wb" | J -> "wj"
      | F | BRST F -> "wf" | Ph | BRST Ph -> "wp" 
      | C | Cbar -> "wc" | Xi -> "wxi" | LOp -> "wlop"
      | Phino | BRST Phino -> "wpino" | BRST _ -> ""
  end

(* \subsection*{Equations of Motion} *)

(* Equations of motion 
   \begin{subequations}
   \begin{align}
     \Box A &= - 2 e G_\mu \partial^\mu B - e B \partial_\mu G^\mu + 
     e^2 G_\mu G^\mu A - e \overline{\Psi} \lambda - \dfrac{e^2}{2} \left( 
     A^3 + A B^2 \right) \\
     \Box B &= 2 e G_\mu \partial^\mu A + e A \partial_\mu G^\mu 
     + e^2 G_\mu G^\mu B - \ii e \overline{\Psi} \gamma^5 \lambda - 
     \dfrac{e^2}{2} \left( B^3 + B A^2 \right) \\
     \ii \fmslash{\partial} \Psi &= e A \lambda + \ii e B \gamma^5 \lambda
     + e \fmslash{G} \gamma^5 \Psi \\
     \ii \fmslash{\partial} \lambda &= e A \Psi + \ii e B \gamma^5 \Psi \\
     \partial^\nu F_{\nu\mu} &= e \left( A \partial_\mu B - B 
     \partial_\mu A \right) - e^2 G_\mu \left( A^2 + B^2 \right) +
     \dfrac{e}{2} \overline{\Psi} \gamma_\mu \gamma^5 \Psi 
   \end{align}
   \end{subequations}
   Noether current
   \begin{multline}
     \mathcal{J}^\mu = - (\fmslash{\partial} A) \gamma^\mu \Psi - \ii 
     (\fmslash{\partial} B) \gamma^\mu \gamma^5 \Psi + \ii e A \fmslash{G} 
     \gamma^\mu \gamma^5 \Psi \\  - e B \fmslash{G}  \gamma^\mu \Psi +
     \dfrac{1}{2} \lbrack \gamma^\alpha , \gamma^\beta \rbrack \gamma^\mu 
     \gamma^5 (\partial_\alpha G_\beta) \lambda - \dfrac{\ii e}{2} \left( A^2 
     + B^2 \right) \gamma^\mu \lambda
   \end{multline}
   is conserved explicitely 
   \begin{equation}
     \label{conservedcurrent}
     \begin{aligned}
     \partial_\mu \mathcal{J}^\mu = & \; - (\Box A) \Psi - (\fmslash{\partial}
     A) (\fmslash{\partial} \Psi) - \ii (\Box B) \gamma^5 \Psi + \ii 
     (\fmslash{\partial} B) \gamma^5 (\fmslash{\partial} \Psi) \\ & \; + \ii e 
     \gamma^\alpha \gamma^\beta \gamma^5 A (\partial_\beta G_\alpha) \Psi + 
     \ii e \fmslash{G} (\fmslash{\partial} A) \gamma^5 \Psi - \ii e A 
     \fmslash{G} \gamma^5 (\fmslash{\partial} \Psi) \\ & \; - e \gamma^\alpha 
     \gamma^\beta B (\partial_\beta G_\alpha) \Psi  - e \fmslash{G} 
     (\fmslash{\partial} B) \Psi - e B \fmslash{G} (\fmslash{\partial} \Psi) 
     \\ & \; + \dfrac{1}{2} \gamma^\alpha \gamma^\beta \gamma^\mu \gamma^5 
     (\partial_\mu F_{\alpha\beta}) \lambda - \dfrac{1}{2} \gamma^\alpha 
     \gamma^\beta \gamma^5 F_{\alpha\beta} (\fmslash{\partial} \lambda) - 
     \ii e A (\fmslash{\partial} A) \lambda \\ & \; - \ii e B 
     (\fmslash{\partial} B) \lambda - \dfrac{e}{2} \left( A^2 + B^2 \right) 
     (\ii \fmslash{\partial} \lambda) \; \; = \; \; 0
     \end{aligned}
   \end{equation}
   We list all the terms separately after inserting several equations of 
   motion. First term of (\ref{conservedcurrent}):
   \begin{multline}
     \label{conserv1}
     - (\Box A) \Psi = 2 e G_\mu (\partial^\mu B) \Psi + e B (\partial_\mu 
     G^\mu) \Psi \\ - e^2 G_\mu G^\mu A \Psi + e (\overline{\Psi} \lambda) 
     \Psi - \dfrac{e^2}{2} \left( A^3 + A B^2 \right) \Psi 
   \end{multline}
   Third term of (\ref{conservedcurrent}): 
   \begin{multline}
     \label{conserv2}
     - \ii (\Box B) \gamma^5 \Psi = - 2 \ii e G_\mu (\partial^\mu A) \gamma^5 
     \Psi - \ii e A (\partial_\mu G^\mu) \gamma^5 \Psi - \ii e^2 G_\mu 
     G^\mu B \gamma^5 \Psi \\ - e (\overline{\Psi} \gamma^5 \lambda) \gamma^5 
     \Psi + \dfrac{\ii e^2}{2} \left( B^3 + B A^2 \right) \gamma^5 \Psi 
   \end{multline}
   Terms number 2, 6 and 13 of (\ref{conservedcurrent}):
   \begin{multline}
     \label{conserv3}
     - (\fmslash{\partial} A) (\fmslash{\partial} \Psi) + \ii e \fmslash{G} 
     (\fmslash{\partial} A) \gamma^5 \Psi - \ii e A (\fmslash{\partial} A) 
     \lambda = - e (\fmslash{\partial} A) B \gamma^5 \lambda + 2 \ii e G_\mu 
     (\partial^\mu A) \gamma^5 \Psi 
   \end{multline}
   The second term on the rhs cancels the first of rhs (\ref{conserv2}). 
   Terms number 4, 9 and 14 of (\ref{conservedcurrent}):
   \begin{multline}
     \label{conserv4}
     \ii (\fmslash{\partial} B) \gamma^5 (\fmslash{\partial} \Psi) - e 
     \fmslash{G} (\fmslash{\partial} B) \Psi - \ii e B (\fmslash{\partial} B)
     \lambda = e (\fmslash{\partial} B) A \gamma^5 \lambda - 2 e G_\mu 
     (\partial^\mu B) \Psi 
   \end{multline}
   On the rhs the second term cancels the first of (\ref{conserv1}). The 
   seventh term of (\ref{conservedcurrent}) reads 
   \begin{equation}
     \label{conserv5} 
     - e A \fmslash{G} \gamma^5 (\ii \fmslash{\partial} \Psi) = - e^2 A^2 
     \fmslash{G} \gamma^5 \lambda - \ii e^2 A B \fmslash{G} \lambda + e^2 
     A G_\mu G^\mu \Psi ,
   \end{equation}  
   while the tenth term of (\ref{conservedcurrent}) gives:
   \begin{equation}
     \label{conserv6}
     \ii e B \fmslash{G} (\ii \fmslash{\partial} \Psi) = \ii e^2 A B 
     \fmslash{G} \lambda - e^2 B^2 \fmslash{G} \gamma^5 \lambda + \ii e^2 
     B G_\mu G^\mu \gamma^5 \Psi 
   \end{equation}
   Second term rhs (\ref{conserv5}) cancels first of rhs (\ref{conserv6}), 
   third term rhs (\ref{conserv5}) cancels third term rhs (\ref{conserv1}),
   and third term rhs (\ref{conserv6}) eliminates third term rhs 
   (\ref{conserv2}). With the use of the identity 
   \begin{equation}
     \lbrack \gamma^\alpha , \gamma^\beta \rbrack \gamma^\mu = - 2 
     \eta^{\alpha\mu} \gamma^\beta + 2 \eta^{\beta\mu} \gamma^\alpha - 2 \ii
     \epsilon^{\alpha\beta\mu\sigma} \gamma_\sigma \gamma^5 
   \end{equation}  
   and the homogeneous Maxwell equations $\epsilon^{\alpha\beta\mu\sigma} 
   (\partial_\mu F_{\alpha\beta}) = 0$ term 11 of (\ref{conservedcurrent}) 
   yields 
   \begin{multline}
     \label{conserv7}
     \dfrac{1}{2} \gamma^\alpha \gamma^\beta \gamma^\mu \gamma^5 
     (\partial_\mu F_{\alpha\beta}) \lambda = - e \gamma^\beta \gamma^5 \left(
     A \partial_\beta B - B \partial_\alpha A \right) \lambda  \\ + e^2 
     \fmslash{G} \gamma^5 \left( A^2 + B^2 \right) \lambda - \dfrac{e}{2}  
     (\overline{\Psi} \gamma_\beta \gamma^5 \Psi) \gamma^\beta \gamma^5 
     \lambda .
   \end{multline}
   The first term on rhs cancels the first terms rhs of (\ref{conserv3}) and 
   (\ref{conserv4}), while the second one eliminates the first term rhs of
   (\ref{conserv5}) and the second one rhs of (\ref{conserv6}). Term number
   12 of (\ref{conservedcurrent}) is
   \begin{multline}
     \label{conserv8}
     \dfrac{\ii}{2} \lbrack \gamma^\alpha , \gamma^\beta \rbrack \gamma^5 
     (\partial_\alpha G_\beta) (\ii \fmslash{\partial} \lambda) = 
     \dfrac{\ii e}{2} \lbrack \gamma^\alpha , \gamma^\beta \rbrack \gamma^5 
     (\partial_\alpha G_\beta) A \Psi - \dfrac{e}{2} \lbrack \gamma^\alpha ,
     \gamma^\beta \rbrack (\partial_\alpha G_\beta) B \Psi 
   \end{multline}
   With the help of the Dirac algebra this cancels the fifth and eighth term
   of (\ref{conservedcurrent}) together with the second terms of the rhs of 
   (\ref{conserv1}) and (\ref{conserv2}). 
   The last term of (\ref{conservedcurrent}) gives
   \begin{equation}
     \label{conserv9}
     - \dfrac{e}{2} \left( A^2 + B^2 \right) (\ii \fmslash{\partial} \lambda) 
     = - \dfrac{e^2}{2} \left( A^3 + B^2 A \right) \Psi - \dfrac{\ii e^2}{2} 
     \left( B A^2 + B^3 \right) \gamma^5 \Psi ,
   \end{equation}
   which cancels the last terms on the rhs of (\ref{conserv1}) and 
   (\ref{conserv2}). The remaining terms, the fourth terms rhs of 
   (\ref{conserv1}) and (\ref{conserv2}) as well as the last term rhs of
   (\ref{conserv7}) add up to zero,
   \begin{equation}
     e (\overline{\Psi} \lambda) \Psi - e (\overline{\Psi} \gamma^5 \lambda) 
     \gamma^5 \Psi - \dfrac{e}{2} (\overline{\Psi} \gamma_\beta \gamma^5 \Psi)
     \gamma^\beta \gamma^5 \lambda = 0 ,
   \end{equation}
   which can be seen by Fierzing the first two terms. So the conservation of 
   the current is finally proven. 
   Conserved charge generates the SUSY transformations of the fields
   \begin{subequations}
   \begin{align}
     \lbrack \overline{\xi} Q, A \rbrack &=  \ii \left( \overline{\xi} \Psi 
     \right) \\ \lbrack \overline{\xi} Q,B \rbrack  &= \ii \left( \ii 
     \overline{\xi} \gamma^5 \Psi \right) 
   \end{align}
   \end{subequations}
   For the transformation of the fermions it is more comfortable to write 
   the charge in the following form
   \begin{multline}
   \overline{Q} \xi = \int d^3 \vec{x} \biggl\{ - \overline{\Psi} \gamma^0 
   (\fmslash{\partial} - \ii \overline{\Psi} \gamma^5 \gamma^0 
   (\fmslash{\partial} B) + \ii e \overline{\Psi} \gamma^5 \gamma^0 
   \fmslash{G} A - e \overline{\Psi} \gamma^0 \fmslash{G} B \\ + \dfrac{1}{2} 
   \overline{\lambda} (\partial_\alpha G_\beta) \gamma^5 \gamma^0 \lbrack 
   \gamma^\alpha , \gamma^\beta \rbrack + \dfrac{\ii e}{2} \overline{\lambda} 
   \gamma^0 \left( A^2 + B^2 \right) \biggr\} \xi 
   \end{multline}
   and to use the identity $\overline{\xi} Q = \overline{Q} \xi$ to show
   \begin{subequations}
   \begin{align}
     \lbrack \overline{\xi} Q , \Psi \rbrack &= \ii \left( - \ii 
     (\fmslash{\partial} - \ii e \fmslash{G} \gamma^5) (A + \ii \gamma^5 B) 
     \xi \right) \\
     \lbrack \overline{\xi} Q , \lambda \rbrack &= \ii \left( - \dfrac{\ii}{2}
     F_{\alpha\beta} \gamma^\alpha \gamma^\beta \gamma^5 \xi - \dfrac{e}{2} 
     \left( A^2 + B^2 \right) \xi \right) 
     \\
     \lbrack \overline{\xi} Q , G_\mu \rbrack &= \ii \left( - \overline{\xi} 
     \gamma_\mu \gamma^5 \lambda \right) 
   \end{align}
   \end{subequations}
   To show the covariance of the equations of motion is more complicated
   than in a simple gauge theory so we just show one example. 
   \begin{align}
     \lbrack \overline{\xi} Q , \Box A \rbrack = & \; 2 \ii e \left( 
     \overline{\xi} (\fmslash{\partial} B) \gamma^5 \lambda \right) + 2 e 
     \left( \overline{\xi} \gamma^5 G_\mu \partial^\mu \Psi \right) - \ii e 
     \left( \overline{\xi} B \gamma^5 \fmslash{\partial} \lambda \right) 
     \notag\\ & \;
     + e (\partial_\mu G^\mu) \left( \overline{\xi} \gamma^5 \Psi \right) - 
     2 \ii e^2 A \left( \overline{\xi} \fmslash{G} \gamma^5 \lambda \right) + 
     \ii e^2 G_\mu G^\mu \left( \overline{\xi} \Psi \right) \notag\\ & \; 
     + e \overline{\xi} \Bigl( \left( \fmslash{\partial} - \ii e \fmslash{G} 
     \gamma^5 \right) \left( A - \ii \gamma^5 B \right) \Bigr) \lambda + 
     \dfrac{e^2}{2} \left( \overline{\xi} \lbrack \gamma^\alpha , \gamma^\beta
     \rbrack \gamma^5 (\partial_\alpha G_\beta) \Psi \right) \notag \\ & \; + 
     \dfrac{\ii e^2}{2} \left( A^2 + B^2 \right) \left( \overline{\xi} \Psi 
     \right) - \dfrac{3 \ii e^2}{2} A^2 \left( \overline{\xi} \Psi \right) - 
     \dfrac{\ii e^2}{2} B^2 \left( \overline{\xi} \Psi \right) \notag \\ & \; 
     + e^2 A B \left( \overline{\xi} \gamma^5 \Psi \right) \notag 
     \\ = & \; 
     \ii e \left( 
     \overline{\xi} (\fmslash{\partial} B) \gamma^5 \lambda \right) + 2 e 
     \left( \overline{\xi} \gamma^5 G_\mu \partial^\mu \Psi \right) - \ii e 
     \left( \overline{\xi} B \gamma^5 \fmslash{\partial} \lambda \right) 
     \notag\\ & \;
     + e (\partial_\mu G^\mu) \left( \overline{\xi} \gamma^5 \Psi \right) - 
     \ii e^2 A \left( \overline{\xi} \fmslash{G} \gamma^5 \lambda \right) + 
     \ii e^2 G_\mu G^\mu \left( \overline{\xi} \Psi \right) \notag\\ & \; 
     + e \left( \overline{\xi} (\fmslash{\partial} A) \lambda \right)
     + e^2 B \left( \overline{\xi} \fmslash{G} \lambda \right) + 
     \dfrac{e^2}{2} \left( \overline{\xi} \lbrack \gamma^\alpha , \gamma^\beta
     \rbrack \gamma^5 (\partial_\alpha G_\beta) \Psi \right) \notag \\ & \;  
     - \ii e^2 A^2 \left( \overline{\xi} \Psi \right) + e^2 A B 
     \left( \overline{\xi} \gamma^5 \Psi \right)
   \end{align} 
   \begin{align}
     \Box \: \lbrack \overline{\xi} Q , A \rbrack = & \; \ii \left( 
     \overline{\xi} \fmslash{\partial} \fmslash{\partial} \Psi \right) \notag 
     \\ = & \; e A \left( \overline{\xi} \fmslash{\partial} \lambda \right)
     + e \left( \overline{\xi} (\fmslash{\partial} A) \lambda \right) - \ii e
     B \left( \overline{\xi} \gamma^5 \fmslash{\partial} \lambda \right) + 
     \ii e \left( \overline{\xi} (\fmslash{\partial} B) \gamma^5 \lambda 
     \right) \notag \\ & \; + e \left( \overline{\xi} (\fmslash{\partial} 
     \fmslash{G}) \gamma^5 \Psi \right) + e \left( \overline{\xi} \gamma^\mu 
     \fmslash{G} \gamma^5 \partial_\mu \Psi \right) \notag \\ = & \; 
     - \ii e^2 A^2 \left( \overline{\xi} \Psi \right) + e^2 A B \left( 
     \overline{\xi} \gamma^5 \Psi \right) + e \left( \overline{\xi} 
     (\fmslash{\partial} A) \lambda \right) - \ii e \left( \overline{\xi}
     B \gamma^5 \fmslash{\partial} \lambda \right) \notag \\ & \; + \ii e 
     \left( \overline{\xi} (\fmslash{\partial} B) \gamma^5 \lambda \right) + 
     \dfrac{e}{2} \left( \overline{\xi} \lbrack \gamma^\alpha , \gamma^\beta 
     \rbrack \gamma^5 (\partial_\alpha G_\beta) \Psi \right) + e (\partial_\mu
     G^\mu) \left( \overline{\xi} \gamma^5 \Psi \right) \notag \\ & \; + 2 e 
     \left( \overline{\xi} \gamma^5 G_\mu \partial^\mu \Psi \right)  
     - \ii e^2 A \left( \overline{\xi} \fmslash{G} \gamma^5 \lambda \right) + 
     e^2 B \left( \overline{\xi} \fmslash{G} \lambda \right) \notag \\ & \; + 
     \ii e^2 (G_\mu G^\mu) \left( \overline{\xi} \Psi \right)
   \end{align} 
   \begin{equation}
     \Longrightarrow \lbrack \overline{\xi} Q , \Box A \rbrack \, = \, 
     \Box \: \lbrack \overline{\xi} Q , A \rbrack 
   \end{equation} *)

(* \subsection*{Ward Identities} *)

(* On shell current matrix elements
   \begin{multline}
     J_\mu(p_1,p_2) = \Braket{0|\mathcal{J}_\mu(x)|A (p_1) \Psi(p_2)} \\ = 
          \Braket{0|\mathcal{J}_\mu(x)|A(p_1)\Psi(p_2)}_{(0)} + \mathcal{O} 
          (g) 
          \sim - \fmslash{p}_1 \gamma_\mu u(p_2) + \mathcal{O} (g) 
   \end{multline}
   \begin{equation}
     (p_1+p_2)^\mu J_\mu(p_1,p_2) = - \fmslash{p}_1 \left( \fmslash{p}_1 + 
     \fmslash{p}_2 \right) u(p_2) + \mathcal{O} (g) = \mathcal{O} (g) 
   \end{equation}
   Also for off-shell Green functions (from now on we take $\overline{\xi} 
   \mathcal{J}_\mu$ instead of $\mathcal{J}_\mu$ to deal with a bosonic
   operator)
   \begin{multline}
     \frac{\partial}{\partial x_\mu}
       \Braket{0|\mathrm{T}\overline{\xi}\mathcal{J}_\mu(x)A(y)\Psi(z)|0} =
         \delta(x_0-y_0) \Braket{0|\mathrm{T}\lbrack \overline{\xi} 
         \mathcal{J}_0(x),A(y) \rbrack \Psi(z)|0} \\
       + \delta(x_0-z_0) \Braket{0|\mathrm{T}A(y)\lbrack \overline{\xi} 
         \mathcal{J}_0(x), \Psi(z) \rbrack |0}
       + \Braket{0|\mathrm{T}\partial^\mu \overline{\xi} \mathcal{J}_\mu(x)
         A(y)\Psi(z)|0}
   \end{multline}
   where the last term vanishes for conserved supersymmetry or purely 
   spontaneous symmetry breaking (no explicit breaking). Assuming for all 
   fields~$\phi$ 
   \begin{equation}
     \lbrack \overline{\xi} \mathcal{J}_0(x),\phi(y) \rbrack 
     \Bigr\vert_{x_0=y_0}
       = \delta^3(\vec x - \vec y) \lbrack \overline{\xi}Q,\phi(y) \rbrack 
   \end{equation}
   this reads
   \begin{multline}
       \delta^4(x-y) \Braket{0|\mathrm{T}\lbrack \overline{\xi} Q, A(y) 
       \rbrack \Psi(z)|0}
     + \delta^4(x-z) \Braket{0|\mathrm{T}A(y)\lbrack \overline{\xi} Q,\Psi(z) 
       \rbrack |0} = \\
     \frac{\partial}{\partial x_\mu}
       \Braket{0|\mathrm{T}\overline{\xi} \mathcal{J}_\mu(x)A(y)\Psi(z)|0}
         - \Braket{0|\mathrm{T}\partial^\mu \overline{\xi} \mathcal{J}_\mu(x)
         A(y)\Psi(z)|0}
   \end{multline}
   Integrated (zero-momentum insertion, i.e. Fourier-transformation with zero
   momentum)
   \begin{multline}
       \Braket{0|\mathrm{T}\lbrack \overline{\xi}Q,A(y) \rbrack\Psi(z)|0}
     + \Braket{0|\mathrm{T}A(y)\lbrack \overline{\xi}Q,\Psi(z) \rbrack |0} =
      \Braket{0|\mathrm{T}\lbrack \overline{\xi}Q,A(y)\Psi(z) \rbrack |0} = \\
       \int\!\mathrm{d}^4x\, \frac{\partial}{\partial x_\mu}
       \Braket{0|\mathrm{T}\overline{\xi}\mathcal{J}_\mu(x)A(y)\Psi(z)|0}
         - \int\!\mathrm{d}^4x
              \Braket{0|\mathrm{T}\partial^\mu \overline{\xi}\mathcal{J}_\mu(x)A(y)\Psi(z)|0}
   \end{multline}
   where the first term does \emph{not} vanish in the case of zero momentum 
   for spontaneous symmetry breaking, because massless Goldstone boson states 
   give a contribution at infinity. We are here dealing with exact 
   supersymmetry, so the second term on the r.h.s. of the former equation is 
   zero, but we won't set the momentum of the current to zero at the moment.  
   E.\,g.:
   \begin{multline}
       \delta^4(y-x_1) \Braket{0|\mathrm{T}\lbrack \overline{\xi}Q,A(x_1) \rbrack \Psi(x_2)|0}
     + \delta^4(y-x_2) \Braket{0|\mathrm{T}A(x_1)\lbrack \overline{\xi}Q,\Psi(x_2) \rbrack |0} \\
     = \ii 
     \delta^4(y-x_1) \Braket{0|\mathrm{T}\Psi(x_2)\overline{\Psi}(x_1)\xi|0}
     + \ii \delta^4(y-x_2) \Braket{0|\mathrm{T}A(x_1) (-\ii \fmslash{\partial}A(x_2))\xi|0} \\
     = \frac{\partial}{\partial y_\mu}
       \Braket{0|\mathrm{T}\mathcal{J}_\mu(y)A(x_1)\Psi(x_2)|0} = - 
       \dfrac{\partial}{\partial y_\mu} \Braket{0|\mathrm{T} \Psi(x_2) 
       \overline{\Psi}(y) \gamma_\mu A(x_1) \fmslash{\partial}_y A(y) \xi |0}
   \end{multline}
   in tree approximation in configuration space
   \begin{multline}
    \ii \delta^4(y-x_1) S_F(x_2-x_1)\xi + \delta^4(y-x_2) 
    \fmslash{\partial}_{x_2} D_F (x_1-x_2)\xi \\ = - \partial^\mu_y \biggl\{
    S_F(x_2-y) \gamma_\mu \: \fmslash{\partial}_y D_F(x_1-y)\xi\biggr\}
   \end{multline}
   Inserting the expressions for the fermion and boson propagators (remember 
   that all particles here are massless)
   \begin{align}
   D_F (x-y) &= \; \int \dfrac{d^4 k_1}{(2\pi)^4} \dfrac{\ii e^{-\ii k_1 
                (x-y)}}{k_1^2 + \ii \epsilon} \\
   S_F (x-y) &= \; \int \dfrac{d^4 k_2}{(2\pi)^4} \dfrac{\ii e^{-\ii k_2 
                (x-y)}}{\fmslash{k}_2 + \ii \epsilon}
   \end{align}
   in tree approximation in momentum space
   \begin{multline}
     \mbox{} \left( \dfrac{\ii(-\ii)}{\fmslash{p}_2} + \dfrac{\ii(-\ii)
      \fmslash{p}_1}{p_1^2} \right) \xi \\ = + \ii \left( p_1 + p_2 
      \right)^\mu \biggl\{ S_F(x_2-y) \gamma_\mu \: \fmslash{\partial}_y 
      D_F(x_1-y)\xi\biggr\} \\ = 
      \ii^2 (-\ii)^2 \dfrac{1}{p_1^2} \dfrac{1}{\fmslash{p}_2} (\fmslash{p}_1 
      + \fmslash{p}_2)
      \fmslash{p}_1 \xi = + \left( \dfrac{1}{\fmslash{p}_2} + 
      \dfrac{\fmslash{p}_1}{p_1^2} \right) \xi  
   \end{multline}
   Some words about the signs: The momentum flux always goes from the right 
   spacetime event argument of the propagator to the left. In our case the two 
   propagators $S_F$ and $D_F$ have the exponentials $\exp(\ii p_2(x_2-y)$ and
   $\exp(\ii p_1 (x_1-y)$ respectively. The sign of the derivative of the 
   current can be understood as the derivative acts on a field operator 
   inserted in the amplitude and not on a field in an interaction vertex. 

   \vspace{.5cm}

   Similarly, the transformed $n$-point function can be related to the
   divergence of an $(n-1)$-point function with the insertion of one
   current. At this level we don't treat spontaneous symmetry breaking so
   we haven't any "mixing" of orders in perturbation theory. By this we mean 
   the masking of a diagram with one current insertion, taking the part with 
   the vacuum expectation value of the lower doublet component, combining with
   the coupling constant of what would normally be a higher order vertex in 
   perturbation theory to a mass term of the Higgs, as a diagram of lowest 
   order in perturbation theory.

   \vspace{5mm}

   Graphically denoting the influx of momentum by a dotted line, we have
   the \emph{exact} relation (for $k+k_1+k_2=0$ and all momenta incoming)
   \begin{equation}
       \parbox{21mm}{%
         \begin{fmfgraph*}(20,15)
           \fmfleft{i,di}\fmfright{o,do}
           \fmftop{k}
           \fmf{dashes,label=$k_1$,l.side=left}{i,o}
           \fmf{dots,label=$k$,l.side=left}{k,o}
         \end{fmfgraph*}} + 
       \parbox{21mm}{%
         \begin{fmfgraph*}(20,15)
           \fmfleft{i,di}\fmfright{o,do}
           \fmftop{k}
           \fmf{fermion,label=$k_2$,l.side=right}{o,i}
           \fmf{dots,label=$k$,l.side=left}{i,k}
         \end{fmfgraph*}} =
       \parbox{21mm}{%
         \begin{fmfgraph*}(20,15)
           \fmftop{t}
           \fmfbottom{b1,b2}
           \fmf{dashes}{b1,v}
           \fmf{fermion}{b2,v}
           \fmf{dbl_plain,label=\begin{math}k_\mu {\cal J}^\mu
           \end{math}}{v,t}
           \fmfdot{v}
           \fmfblob{.25w}{t}
          \end{fmfgraph*}}
   \end{equation}
   that can eventually be used to derive more complicated relations, if we
   manage to find the corresponding rules for vertices.

   \vspace{.5cm}

   We give another example of a 2-point-function with current insertion, but
   with a gauge boson and a gaugino. 
   \begin{align}
      &  \delta^4(y-x_1) \Braket{0|\mathrm{T}\lbrack \overline{\xi}Q,G_\nu
                                   (x_1) \rbrack \lambda(x_2)|0}
     + \delta^4(y-x_2) \Braket{0|\mathrm{T}G_\nu(x_1)\lbrack \overline{\xi}
       Q,\lambda(x_2) \rbrack |0} \notag \\
     = \; &  - \ii \delta^4(y-x_1) \Braket{0|\mathrm{T}\lambda(x_2)\overline{\lambda}(x_1)\gamma_\nu\gamma^5\xi|0} \notag \\ & \qquad \qquad 
     + \dfrac{1}{2} \delta^4(y-x_2) \Braket{0|\mathrm{T}G_\nu(x_1) 
       (\partial_\alpha^{x_2} G_\beta(x_2))\lbrack \gamma^\alpha , 
        \gamma^\beta \rbrack \gamma^5 \xi|0} \notag \\ 
     \stackrel{!}{=} \; & \frac{\partial}{\partial y_\mu}
       \Braket{0|\mathrm{T}\mathcal{J}_\mu(y)G_\nu(x_1)\lambda(x_2)|0} \notag 
       \\ = \; &   
       \dfrac{1}{2} \dfrac{\partial}{\partial y_\mu} \Braket{0|\mathrm{T} 
       \lambda(x_2) \overline{\lambda}(y) \gamma^5 \gamma_\mu \lbrack 
       \gamma^\alpha , \gamma^\beta \rbrack (\partial_\alpha^y G_\beta (y)) 
       G_\nu(x_1) \xi |0}
   \end{align}
   In configuration space:
   \begin{multline}
    - \ii \delta^4(y-x_1) S_F (x_2 - x_1) \gamma_\nu \gamma^5 \xi + 
     \dfrac{1}{2} \delta^4 (y-x_2) (-\eta_{\nu\beta}) \partial_\alpha^{x_2}
     D_F (x_1-x_2) \lbrack \gamma^\alpha , \gamma^\beta \rbrack \gamma^5 \xi \\
   \stackrel{!}{=} \dfrac{1}{2} \partial_\mu^y \biggl\{ S_F (x_2-y) \gamma^5 
     \gamma^\mu \lbrack \gamma^\alpha , \gamma^\beta \rbrack \partial_\alpha^y 
     (-\eta_{\beta\nu}) D_F (y-x_1) \xi \biggr\}
   \end{multline}
   In momentum space:
   \begin{multline}
    \dfrac{(-\ii)^2}{\fmslash{p}_2} \gamma_\nu \gamma^5 \xi - \dfrac{1}{2} 
    \dfrac{\ii}{p_1^2} \lbrack -\ii \fmslash{p}_1 , \gamma_\nu \rbrack 
    \gamma^5 \xi  \\ \stackrel{!}{=} 
     \dfrac{1}{2} (-\ii) (p_1^\mu + p_2^\mu) \dfrac{-\ii}{\fmslash{p}_2} 
     \gamma^5 \gamma_\mu \lbrack \gamma^\alpha , \gamma^\beta \rbrack (-
     \ii p_{1,\alpha}) \dfrac{-\ii \eta_{\beta\nu}}{p_1^2} \xi
   \end{multline}
   We better simplify this:
   \begin{multline}
    \dfrac{-1}{\fmslash{p}_2} \gamma_\nu \gamma^5 \xi - \dfrac{1}{2} 
    \dfrac{1}{p_1^2} \lbrack \fmslash{p}_1 , \gamma_\nu \rbrack 
    \gamma^5 \xi  \\ \stackrel{!}{=} 
     \dfrac{1}{2} \dfrac{1}{\fmslash{p}_2} \gamma^5 (\fmslash{p}_1 + 
     \fmslash{p}_2) \lbrack \fmslash{p}_1 , \gamma_\nu \rbrack 
     \dfrac{1}{p_1^2} \xi = - \dfrac{1}{2} \dfrac{1}{p_1^2} \lbrack 
     \fmslash{p}_1 , \gamma_\nu \rbrack \gamma^5 \xi - \dfrac{1}{2} \gamma^5 
     \dfrac{1}{\fmslash{p}_2} \fmslash{p}_1 \lbrack \fmslash{p}_1 , \gamma_\nu
     \rbrack \dfrac{1}{p_1^2} \xi \\
     = - \dfrac{1}{2} \dfrac{1}{p_1^2} \lbrack \fmslash{p}_1 , \gamma_\nu 
     \rbrack \gamma^5 \xi - \dfrac{1}{\fmslash{p}_2} \gamma_\nu \gamma^5 \xi 
     + \dfrac{1}{\fmslash{p}_2} \dfrac{\fmslash{p}_1}{p_1^2} p_{1,\nu} 
     \gamma^5 \xi 
   \end{multline}   
   The third term is proportional to the momentum of the gauge boson. Setting
   the gauge boson on-shell (multiplying with the inverse propagator and the
   polarization vector) or inserting this 3-point-function with current 
   insertion with the outer vector index into a gauge-invariant amplitude 
   result in a zero from that term. So we can eliminate it and the Ward 
   identity holds. 

   \vspace{.5cm}

   As long as the symmetry is exact (no spontaneous breaking) there is no 
   problem with disconnected diagrams which can be produced by {\em vev}s
   in the transformation rules (see [f90_O2.ml]). For better legibility we
   write $p_i$ instead of $k_i$ for the momenta of the particles now to 
   distinguish from the momentum coming from the current. 
   \begin{multline}
     k^{\mu}G_{\mu}^{j|\phi_2\phi_1\cdots,\text{amp.}} (k|p_1,p_2,\ldots)
       \Bigr|_{k+p_1+p_2+\ldots=0} = \\
     \frac{G^{\phi_1\phi_1}(p_1+k)}{G^{\phi_2\phi_2}(p_1)}
       G^{\phi_1\phi_1\cdots,\text{amp.}} (p_1+k,p_2,\ldots)
         \Bigr|_{k+p_1+p_2+\ldots=0} \\
       - \frac{G^{\phi_2\phi_2}(p_2+k)}{G^{\phi_1\phi_1}(p_2)}
           G^{\phi_2\phi_2\cdots,\text{amp.}} (p_1,p_2+k,\ldots)
             \Bigr|_{k+p_1+p_2+\ldots=0} - \ldots 
   \end{multline}
   For $k_\mu\to0$:
   \begin{multline}
     \lim_{k_\mu\to0}
         k^{\mu}G_{\mu}^{j|\phi_2\phi_1\cdots,\text{amp.}} (k|p_1,p_2,\ldots)
        \Bigr|_{k+p_1+p_2+\ldots=0} = \\
     \frac{G^{\phi_1\phi_1}(p_1)}{G^{\phi_2\phi_2}(p_1)}
       G^{\phi_1\phi_1\cdots,\text{amp.}} (p_1,p_2,\ldots)
         \Bigr|_{p_1+p_2+\ldots=0} \\
       - \frac{G^{\phi_2\phi_2}(p_2)}{G^{\phi_1\phi_1}(p_2)}
           G^{\phi_2\phi_2\cdots,\text{amp.}} (p_1,p_2,\ldots)
             \Bigr|_{p_1+p_2+\ldots=0} - \ldots 
   \end{multline}
   In case of spontaneous symmetry breaking there's one subtlety: the right 
   hand side of
   \begin{multline}
     G^{\phi_1\phi_1\cdots,\text{amp.}} (p_1,p_2,\ldots)
         \Bigr|_{p_1+p_2+\ldots=0} = \\
     \frac{G^{\phi_2\phi_2}(p_1)}{G^{\phi_1\phi_1}(p_1)}
     \lim_{k_\mu\to0}
         k^{\mu}G_{\mu}^{j|\phi_2\phi_1\cdots,\text{amp.}} (k|p_1,p_2,\ldots)
        \Bigr|_{k+p_1+p_2+\ldots=0} \\
     +  \frac{G^{\phi_2\phi_2}(p_1)}{G^{\phi_1\phi_1}(p_1)}
        \frac{G^{\phi_2\phi_2}(p_2)}{G^{\phi_1\phi_1}(p_2)}
           G^{\phi_2\phi_2\cdots,\text{amp.}} (p_1,p_2,\ldots)
             \Bigr|_{p_1+p_2+\ldots=0} + \ldots
   \end{multline}
   appears to vanish on the mass shell of the left hand side, but this
   must not mean that the corresponding scattering amplitude vanishes.
   What is going on, is that the insertion of a soft current or the
   emission or absorption of a soft Goldstone boson contributes another
   pole for $k_\mu\to0$, if momentum conservation is taken into account.
   Here we deal with exact supersymmetry so everything vanishes if we take
   the limit $k_\mu \rightarrow 0$. 

   Example:
   \begin{subequations}
   \begin{align}
     \mathrm{F.T.} \Braket{0|\mathrm{T}\ii \overline{\xi} \Psi(x_1) 
       G_\nu(x_2)\Psi(x_3)|0} &= 
       - e \frac{(-\mathrm{i})}{p_2^2} \frac{\mathrm{i}}{\fmslash{p}_3} 
       \gamma_\nu \gamma^5 \frac{\mathrm{i}}{\fmslash{p}_1 + \fmslash{k}} 
                 \xi \\
     \mathrm{F.T.} \Braket{0|\mathrm{T}A(x_1)\ii(-\overline{\xi} \gamma_\nu
        \gamma^5 \lambda (x_2))\Psi(x_3)|0}
        &= e \frac{\mathrm{i}}{p_1^2}
           \frac{\mathrm{i}}{\fmslash{p}_3} \frac{\mathrm{i}}{\fmslash{p}_2 + 
           \fmslash{k}} \gamma_\nu \gamma^5 \xi 
    \end{align}
    \begin{multline}
     \mathrm{F.T.} \Braket{0|\mathrm{T}A(x_1)G_\nu(x_2) \ii(-\gamma^5 
           \fmslash{\partial} B(x_3)\xi)|0} =  \\
            - e \frac{\mathrm{i}}{p_1^2} \frac{-\mathrm{i}}{p_2^2} 
            (k_\nu+p_{3,\nu}-p_{1,_\nu}) \frac{\mathrm{i}}{(p_3+k)^2} \gamma^5 
            (\fmslash{p}_3 + \fmslash{k}) \xi 
   \end{multline}
   \begin{equation}
     \mathrm{F.T.} \Braket{0|\mathrm{T}A(x_1)G_\nu(x_2) (-\ii\gamma^\mu 
           (G_\mu A)(x_3) \gamma^5 \xi)|0} = \dfrac{\ii}{p_1^2} \dfrac{-\ii 
           \eta_{\mu\nu}}{p_2^2} (-\ii e) \gamma^\mu \gamma^5 \xi        
   \end{equation}
   \end{subequations}
   For the last process there is a nonvanishing contribution from the 
   SUSY-transformation into an [A] scalar and a vectorboson; this seems to be 
   of higher order in perturbation theory but nonetheless must be considered 
   here. On-shell you have to take the one-particle-pole on the r.h.s. of the 
   transformation of the fields. The same is true for the appearing of 
   quadratic terms after inserting the equation of motion for the auxiliary 
   fields on the r.h.s. of the transformation rules. But off-shell this 
   becomes a local operator insertion. In the first two processes one has to 
   take account of the sign of the last fermion propagator which appears with 
   calculational direction opposite to the momentum flow. 

       \vspace{0.5cm}

   Now we must evaluate the 4-point-function with the current insertion. 
   We rewrite the current $\overline{\xi} \mathcal{J}_\mu$ as
   $\overline{\mathcal{J}_\mu} \xi$, which is identical due to the 
   Majorana properties of the current and the transformation parameter:
   \begin{align}
   \overline{\xi} \mathcal{J}_\mu = & \; \overline{\xi}
   \biggl\{ - (\fmslash{\partial} A) \gamma_\mu \Psi - \ii 
     (\fmslash{\partial} B) \gamma_\mu \gamma^5 \Psi + \ii e A \fmslash{G} 
     \gamma_\mu \gamma^5 \Psi - e B \fmslash{G} \gamma_\mu \Psi \notag \\ 
      & \qquad\qquad\qquad +
     \dfrac{1}{2} \lbrack \gamma^\alpha , \gamma^\beta \rbrack \gamma_\mu 
     \gamma^5 (\partial_\alpha G_\beta) \lambda - \dfrac{\ii e}{2} \left( A^2 
     + B^2 \right) \gamma_\mu \lambda
       \biggr\} \notag
     \\ = & \; \biggl\{ - \overline{\Psi} \gamma_\mu (\fmslash{\partial} A)
     + \ii \overline{\Psi} \gamma_\mu \gamma^5 (\fmslash{\partial} B) - \ii
     e \overline{\Psi} \gamma_\mu \gamma^5 \fmslash{G} A - e 
     \overline{\Psi} \gamma_\mu \fmslash{G} B \notag \\ & \qquad\qquad\qquad
     - \dfrac{1}{2} \overline{\lambda} (\partial_\alpha G_\beta) 
     \gamma_\mu \gamma^5 \lbrack \gamma^\alpha , \gamma^\beta \rbrack 
     + \dfrac{\ii e}{2} \overline{\lambda} \gamma_\mu \left( A^2 + B^2 \right) 
     \biggr\} \xi
   \end{align}
   This brings the propagator of the (matter) fermion to the farthest left.
   There are four diagrams contributing to the process which we will list now:
   \begin{equation}
       \parbox{21mm}{%
         \begin{fmfgraph*}(20,20)
           \fmfleft{i1,i2}\fmfright{o1,o2}
           \fmf{dashes}{o2,v2}
           \fmf{photon}{i2,v2}
           \fmf{dbl_dashes}{v1,v2}
           \fmf{dbl_plain}{i1,v1}
           \fmf{plain}{o1,v1}
           \fmfdot{v1,v2}
           \fmfblob{.25w}{i1}  
         \end{fmfgraph*}}\qquad + \quad   
       \parbox{21mm}{%
         \begin{fmfgraph*}(20,20)
           \fmfleft{i1,i2}\fmfright{o1,o2}
           \fmf{plain}{o2,v2}
           \fmf{photon}{i2,v2}
           \fmf{plain}{v1,v2}
           \fmf{dbl_plain}{i1,v1}
           \fmf{dashes}{o1,v1}
           \fmfdot{v1,v2}
           \fmfblob{.25w}{i1}  
         \end{fmfgraph*}}\qquad + \quad  
       \parbox{21mm}{%
         \begin{fmfgraph*}(20,20)
           \fmfleft{i1,i2}\fmfright{o1,o2}
           \fmf{plain}{o2,v2}
           \fmf{dashes}{i2,v2}
           \fmf{plain}{v1,v2}
           \fmf{dbl_plain}{i1,v1}
           \fmf{photon}{o1,v1}
           \fmffreeze
           \fmf{photon}{v1,v2}
           \fmfdot{v1,v2}
           \fmfblob{.25w}{i1}  
         \end{fmfgraph*}}\qquad + \quad           
       \parbox{21mm}{%
         \begin{fmfgraph*}(20,20)
           \fmfleft{i1,i2}\fmfright{o1,o2}
           \fmf{plain}{o2,v}
           \fmf{photon}{i2,v}
           \fmf{dbl_plain}{i1,v}
           \fmf{dashes}{o1,v}
           \fmfdot{v}
           \fmfblob{.25w}{i1}  
         \end{fmfgraph*}}\qquad\quad  
   \end{equation}
   
   For the sign of the fermion propagator one has to take care of the flow of
   momentum. 
   \begin{multline}
     \mathrm{F.T.} \Braket{0|\mathrm{T}\overline{\mathcal{J}_\mu}(y) \xi A(x_1)
            G_\nu(x_2)\Psi(x_3)|0} = \\
     \frac{\mathrm{i}}{p_1^2}\frac{-\mathrm{i}}{p_2^2} 
     \dfrac{-\ii}{\fmslash{p}_3} \left( \mathrm{F.T.} \Braket{0|\mathrm{T}
     \overline{\mathcal{J}_\mu} (y)A(x_1)G_\nu(x_2)\Psi(x_3)|0}_{\text{amp.}} 
     \right) \xi 
   \end{multline}
   \begin{multline}
     \mathrm{F.T.} \Braket{0|\mathrm{T}\overline{\mathcal{J}_\mu}(y)
           A(x_1)G_\nu(x_2)\Psi(x_3)|0}_{\text{amp.}} \xi = 
       - \ii e \gamma_\mu \gamma^5 \gamma_\nu \xi \\ 
      - \ii e \dfrac{\ii}{\fmslash{p}_2 + \fmslash{k}} \left(-\dfrac{1}{2}
      \right) (-\ii p_{2,\alpha}) \gamma_\mu \gamma^5 \lbrack \gamma^\alpha , 
               \gamma_\nu \rbrack 
      \xi + \ii e \gamma_\nu \gamma^5 \dfrac{\ii}{\fmslash{p}_1 + 
      \fmslash{k}} \gamma_\mu (-\ii \fmslash{p}_1) \xi \\
      + \dfrac{\ii}{(p_3 + k)^2} e \left( p_{1,\nu} - p_{3,\nu} - k_\nu \right)
        \ii \gamma_\mu \gamma^5 \ii \left( \fmslash{p}_3 + \fmslash{k} \right)
      \xi 
   \end{multline}
   with $\partial_\mu\to\mathrm{i}k_\mu=-\mathrm{i}(p_1+p_2+p_3)_\mu$ 
   \begin{align}
     & \; \dfrac{-\ii}{\fmslash{p}_3} \dfrac{1}{p_1^2 p_2^2} \;
       \mathrm{F.T.} \partial_y^\mu \Braket{0|\mathrm{T}
       \overline{\mathcal{J}_\mu}(y) A(x_1)G_\nu(x_2) 
        \Psi(x_3)|0}_{\text{amp.}} \xi \notag \\ = 
       & \; - \dfrac{\ii}{\fmslash{p}_3} \dfrac{1}{p_1^2 p_2^2} \biggl\{ 
       - e \left( \fmslash{p}_1 + \fmslash{p}_2 + \fmslash{p}_3 \right) 
       \gamma^5 \gamma_\nu - \dfrac{e}{2} \, \dfrac{1}{\fmslash{p}_1 + 
       \fmslash{p}_3} \left( \fmslash{p}_1 + \fmslash{p}_2 + \fmslash{p}_3 
       \right) \gamma^5 \lbrack \fmslash{p}_2 , \gamma_\nu \rbrack \notag
       \\ & \; \qquad + e \left( p_{1,\nu} - p_{3,\nu} - k_\nu \right) 
       \dfrac{1}{(p_3 + k)^2} \left( \fmslash{p}_1 + \fmslash{p}_2 + 
       \fmslash{p}_3 \right) \gamma^5 \left( \fmslash{p}_1 + \fmslash{p}_2 
       \right) \notag \\ & \; \qquad - e \gamma_\nu \gamma^5 
       \dfrac{1}{\fmslash{p}_2 + \fmslash{p}_3} \left( \fmslash{p}_1 + 
       \fmslash{p}_2 + \fmslash{p}_3 \right) \fmslash{p}_1 \biggr\} \xi
       \notag \\ = & \; 
       - \dfrac{\ii e}{p_1^2 p_2^2} \gamma_\nu \gamma^5 \xi - \dfrac{\ii e}
       {p_1^2 p_2^2} \dfrac{1}{\fmslash{p}_3} \left( \fmslash{p}_1 + 
       \fmslash{p}_2 \right) \gamma_\nu \gamma^5 \xi  
       \notag \\ & \;
       + \dfrac{\ii e}{2} \dfrac{1}{p_1^2 p_2^2} \dfrac{1}{\fmslash{p}_3} 
       \gamma^5 \lbrack \fmslash{p}_2 , \gamma_\nu \rbrack  \xi + \dfrac{\ii 
       e}{p_1^2} \dfrac{1}{\fmslash{p}_3} \dfrac{1}{\fmslash{p}_1 + 
       \fmslash{p}_3} \gamma_\nu \gamma^5 \xi - \dfrac{\ii e}{p_1^2 p_2^2}
       \dfrac{1}{\fmslash{p}_3} \dfrac{1}{\fmslash{p}_1 + \fmslash{p}_3}
       \fmslash{p}_2 p_{2,\nu} \gamma^5 \xi   
       \notag \\ & \;  
       + \dfrac{\ii e}{p_1^2 p_2^2} \dfrac{1}{\fmslash{p}_3} \left( 2 p_{1,\nu}
       + p_{2,\nu} \right) \gamma^5 \xi - \dfrac{\ii e}{p_1^2 p_2^2} \left(
       2 p_{1,\nu} + p_{2,\nu} \right) \dfrac{1}{(p_1 + p_2)^2} \gamma^5
       \left( \fmslash{p}_1 + \fmslash{p}_2 \right) \xi \notag 
       \\ & \; 
       + \ii e \dfrac{1}{p_1^2 p_2^2} \dfrac{1}{\fmslash{p}_3} \gamma_\nu 
       \gamma^5 \fmslash{p}_1 \xi + \ii e \dfrac{1}{p_2^2} 
       \dfrac{1}{\fmslash{p}_3} \gamma_\nu \gamma^5 \dfrac{1}{\fmslash{p}_2 +
       \fmslash{p}_3} \xi 
   \end{align}
   For the sign of the momentum it is important to know that the derivative 
   acting on the current really acts on a field operator insertion and 
   {\em not} on an operator belonging to an interaction vertex.  
   The first term in the first row, the second term in the second row, the 
   second term in the third row and the second term in the last row yield the 
   sum of the four amplitudes with one field SUSY-transformed which are given 
   by: 
   \begin{multline}
    \ii e \dfrac{1}{p_2^2} \dfrac{1}{\fmslash{p}_3} \gamma_\nu \gamma^5 
    \dfrac{1}{\fmslash{p}_2 + \fmslash{p}_3} \xi + \ii e \dfrac{1}{p_1^2}
    \dfrac{1}{\fmslash{p}_3} \dfrac{1}{\fmslash{p}_1 + \fmslash{p}_3} 
    \gamma_\nu \gamma^5 \xi - \dfrac{\ii e}{p_1^2 p_2^2} \gamma_\nu \gamma^5
    \xi \\ - \ii e \dfrac{1}{p_1^2} \dfrac{1}{p_2^2} 
    \left( 2 p_{1,\nu} + p_{2,\nu} \right) \dfrac{1}{(p_1 + p_2)^2} \gamma^5 
    (\fmslash{p}_1 + \fmslash{p}_2) \xi 
   \end{multline}
   So the remaining terms must cancel. But they don't. There still remains one
   term:
   \begin{equation}
   - \dfrac{\ii e}{p_1^2 p_2^2} \dfrac{1}{\fmslash{p}_3} \dfrac{1}{
   \fmslash{p}_1 + \fmslash{p}_3} \fmslash{p}_2 p_{2,\nu} \gamma^5 \xi  
   \end{equation}
   This again is a term proportional to the momentum of the gauge boson, so the
   same is true what we have said in the case of the testing of the 2-point 
   function $\Greensfunc{\lbrack Q(\xi) , G_\mu(x_1)\lambda(x_2)\rbrack}$. *)

(* The message from these examples is that the Ward-Takahashi identities are 
   only fulfilled between physical on-shell states, but not off-shell.  

   Caveat (T. Ohl): the Ward identities for on-shell amplitudes do \emph{not} 
   test the theory comprehensively, since only the coupling of Goldstone bosons
   and and currents to external lines. 

   The cause for this complication is that the SUSY charge is not conserved in
   the case of supersymmetric gauge theories, cf. Sibold, Scharf, Rupp: .... 
   There is a difference between the SUSY charge acting on the {\em in}-space 
   and the SUSY charge acting on the {\em out}-space given by the the 
   BRST-transformation of the derivative of the effective action with 
   respect to the SUSY ghost. This term of course vanishes between physical 
   states, so there the SUSY charge is a conserved operator. 

   For correctly derive off-shell relations between Green functions we have to
   turn to the BRST-formalism; we have to take into account the 
   BRST-transformations with ghosts instead of the simple "classical" 
   transformations. To achieve a closed algebra, we must include SUSY 
   transformations, gauge transformations and translations. 

*)

module Main = Omega.Make(Fusion.Mixed23_Majorana)
    (Targets.Fortran_Majorana)(SAGT)
let _ = Main.main ()

(*i
 *  Local Variables:
 *  mode:caml
 *  indent-tabs-mode:nil
 *  page-delimiter:"^(\\* .*\n"
 *  End:
i*)































































































