(* f90_WZ.ml --

   Copyright (C) 1999-2025 by

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
     \dfrac{1}{2} \begin{bmatrix} \hat{\Phi}^\dagger 
     \hat{\Phi} \end{bmatrix}_D + 2 \Re \, \begin{bmatrix} 
     \mu \hat{\Phi} + \dfrac{m}{2} \hat{\Phi}^2 + \dfrac{\lambda}{3!} 
     \hat{\Phi}^3 \end{bmatrix}_F
   \end{equation}
   The Wess-Zumino model is the simplest supersymmetric toy model (besides the
   possibility of a vanishing superpotential). The parameter $\mu$ can be 
   eliminated by a redefinition of the superfields. *) 

module WZ = 
  struct
    open Coupling
    let options = Options.empty

    type flavor = 
      | A | B | Psi | J 

(* All particles are self-charge-conjugate. *)

    let conjugate f = f

    let external_flavors () =
      [ "fields", [A; B; Psi];
        "currents", [J] ]

    let flavors () = ThoList.flatmap snd (external_flavors ())

    let flavor_of_string = function
      | "a" -> A | "b" -> B 
      | "psi" -> Psi
      | "j" -> J
      | _ -> invalid_arg "WZ.flavor_of_string"

    let flavor_to_string = function
      | A -> "a" | B -> "b" | Psi -> "psi" 
      | J -> "j"

    let flavor_symbol = function
      | A -> "a" | B -> "b" | Psi -> "psi" 
      | J -> "j"

    let lorentz = function
      | A | B -> Scalar
      | Psi -> Majorana
      | J -> Vectorspinor

    let propagator = function
      | A | B -> Prop_Scalar 
      | Psi -> Prop_Majorana
      | J -> Only_Insertion

    let width _ = Timelike
    let goldstone _ = None

    let fermion = function
      | A | B -> 0
      | Psi | J -> 2

    let color _ = Color.Singlet
    type gauge = unit
    let gauge_symbol () = failwith "WZ.gauge_symbol: internal error"

    let colsymm _ = (0,false),(0,false)

(* \begin{equation}
   \begin{aligned}
   {\cal L}_{WZ} = & \; \frac{1}{2} \left( \partial_\mu A \partial^\mu A - m^2
   A^2 \right) + \frac{1}{2} \left( \partial_\mu B \partial^\mu B - m^2 B^2
   \right) + \frac{1}{2} \overline{\Psi} \left( \ii \fmslash{\partial} - m
   \right) \Psi \\ & \; - \dfrac{\lambda}{2 \sqrt{2}} \overline{\Psi} \Psi A +
   \dfrac{\ii\lambda}{2 \sqrt{2}} \overline{\Psi} \gamma^5 \Psi B -
   \frac{\lambda^2}{16} A^4 - \frac{\lambda^2}{16} B^4 - \frac{\lambda^2}{8}
   A^2 B^2 \\ & \; - \frac{1}{2 \sqrt{2}} m \lambda A^3  - \frac{1}{2 \sqrt{2}}
   m \lambda A B^2
   \end{aligned}
   \end{equation}     
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
           &= \frac{\mathrm{i}}{p^2-m^2+\mathrm{i}\epsilon} \\
       \parbox{21mm}{%
         \begin{fmfgraph*}(20,5)
           \fmfleft{i}\fmfright{o}
           \fmflabel{$B(p)$}{i}
           \fmflabel{$B(p)$}{o}
           \fmf{dbl_dashes}{i,o}
           \fmfdot{i,o}
         \end{fmfgraph*}}\qquad\quad
           &= \frac{\mathrm{i}}{p^2-m^2+\mathrm{i}\epsilon} \\
       \parbox{21mm}{%
         \begin{fmfgraph*}(20,5)
           \fmfleft{i}\fmfright{o}
           \fmflabel{$\Psi(p)$}{i}
           \fmflabel{$\overline{\Psi}(p)$}{o}
           \fmf{plain}{i,o}
           \fmfdot{i,o}
         \end{fmfgraph*}}\qquad\quad
           &= \frac{\mathrm{i} \fmslash{p} + m}{p^2-m^2+\mathrm{i}\epsilon} 
     \end{align}
   \end{subequations}
   Three point vertices (no momenta necessary here)
   \begin{subequations}
     \begin{align}
       \parbox{21mm}{%
         \hfil\\\hfil\\
         \begin{fmfgraph*}(20,15)
           \fmfleft{p1}\fmfright{p2,p3}
           \fmflabel{$A$}{p1}
           \fmflabel{$A$}{p2}
           \fmflabel{$A$}{p3}
           \fmf{dashes}{p1,v}
           \fmf{dashes}{p2,v,p3}
           \fmfdot{v}
         \end{fmfgraph*}\\
         \hfil}\qquad\quad
           &= -\dfrac{3\ii}{\sqrt{2}} m \lambda \\
       \parbox{21mm}{%
         \hfil\\\hfil\\
         \begin{fmfgraph*}(20,15)
           \fmfleft{p1}\fmfright{p2,p3}
           \fmflabel{$A$}{p1}
           \fmflabel{$B$}{p2}
           \fmflabel{$B$}{p3}
           \fmf{dashes}{p1,v}
           \fmf{dbl_dashes}{p2,v,p3}
           \fmfdot{v}
         \end{fmfgraph*}\\
         \hfil}\qquad\quad
           &= -\dfrac{\ii}{\sqrt{2}} m \lambda \\
       \parbox{21mm}{%
         \hfil\\\hfil\\
         \begin{fmfgraph*}(20,15)
           \fmfleft{p1}\fmfright{p2,p3}
           \fmflabel{$A$}{p1}
           \fmflabel{$\Psi$}{p2}
           \fmflabel{$\Psi$}{p3}
           \fmf{dashes}{p1,v}
           \fmf{plain}{p2,v,p3}
           \fmfdot{v}
         \end{fmfgraph*}\\
         \hfil}\qquad\quad
           &= -\dfrac{\ii}{\sqrt{2}} \lambda \\
       \parbox{21mm}{%
         \hfil\\\hfil\\
         \begin{fmfgraph*}(20,15)
           \fmfleft{p1}\fmfright{p2,p3}
           \fmflabel{$B$}{p1}
           \fmflabel{$\Psi$}{p2}
           \fmflabel{$\Psi$}{p3}
           \fmf{dbl_dashes}{p1,v}
           \fmf{plain}{p2,v,p3}
           \fmfdot{v}
         \end{fmfgraph*}\\
         \hfil}\qquad\quad
           &= -\dfrac{1}{\sqrt{2}} \lambda \gamma^5
     \end{align}
   \end{subequations}     
   Four point vertices
   \begin{subequations}
     \begin{align}
       \parbox{21mm}{%
         \hfil\\\hfil\\
         \begin{fmfgraph*}(20,15)
           \fmfleft{p1,p2}\fmfright{p3,p4}
           \fmflabel{$A$}{p1}
           \fmflabel{$A$}{p2}
           \fmflabel{$A$}{p3}
           \fmflabel{$A$}{p4}
           \fmf{dashes}{p1,v,p2}
           \fmf{dashes}{p3,v,p4}
           \fmfdot{v}
         \end{fmfgraph*}\\
         \hfil}\qquad\quad
           &= - \dfrac{3\ii}{2} \lambda^2\\
       \parbox{21mm}{%
         \hfil\\\hfil\\
         \begin{fmfgraph*}(20,15)
           \fmfleft{p1,p2}\fmfright{p3,p4}
           \fmflabel{$B$}{p1}
           \fmflabel{$B$}{p2}
           \fmflabel{$B$}{p3}
           \fmflabel{$B$}{p4}
           \fmf{dbl_dashes}{p1,v,p2}
           \fmf{dbl_dashes}{p3,v,p4}
           \fmfdot{v}
         \end{fmfgraph*}\\
         \hfil}\qquad\quad
           &= - \dfrac{3\ii}{2} \lambda^2\\        
       \parbox{21mm}{%
         \hfil\\\hfil\\
         \begin{fmfgraph*}(20,15)
           \fmfleft{p1,p2}\fmfright{p3,p4}
           \fmflabel{$A$}{p1}
           \fmflabel{$A$}{p2}
           \fmflabel{$B$}{p3}
           \fmflabel{$B$}{p4}
           \fmf{dashes}{p1,v,p2}
           \fmf{dbl_dashes}{p3,v,p4}
           \fmfdot{v}
         \end{fmfgraph*}\\
         \hfil}\qquad\quad
           &= - \dfrac{\ii}{2} \lambda^2
     \end{align}
   \end{subequations} *)   

(* \subsection*{Conserved Current} *)

(* \begin{multline}
     \mathcal{L}\lbrack J_{3/2} \rbrack =
       J_{3/2}^\mu \biggl\{ \ii \left( (\ii \fmslash{\partial} - m) A \right) 
       \gamma^\mu \Psi + \left( (\ii \fmslash{\partial} + m) B \right) 
       \gamma^5 \gamma^\mu \Psi \\  - \dfrac{\ii \lambda}{2 \sqrt{2}} 
       \gamma^\mu \left( A^2 - B^2 \right) \Psi - \dfrac{\lambda}{\sqrt{2}} 
       \gamma^\mu \gamma^5 A B \Psi \biggr\}
   \end{multline} *)

    type constant =
      | Unity | Lambda | M | MJ
      | WA | WB | WP | WJ
      | G3_SSS | G3_APP | G3_BPP
      | G4_SSSS
    let constant_symbol = function
      | Unity -> "unity" | Lambda -> "l" 
      | M -> "m" | MJ -> "mj"
      | WA -> "wa" | WB -> "wb" | WP -> "wp" | WJ -> "wj"
      | G3_SSS -> "gsss" | G3_APP -> "gapp" | G3_BPP -> "g3_bpp"
      | G4_SSSS -> "gssss" 

    let vertices () =
      ([(A, A, A), Scalar_Scalar_Scalar 3, G3_SSS;
        (A, B, B), Scalar_Scalar_Scalar 1, G3_SSS;
        (Psi, A, Psi), FBF (1, Chibar, S, Chi), G3_APP;
        (Psi, B, Psi), FBF (1, Chibar, P, Chi), G3_BPP;
        (J, A, Psi), GBG (1, Gravbar, S, Chi), Unity;
        (J, B, Psi), GBG (1, Gravbar, P, Chi), Unity],
       [(A, A, A, A), Scalar4 3, G4_SSSS;
        (B, B, B, B), Scalar4 3, G4_SSSS;
        (A, A, B, B), Scalar4 1, G4_SSSS;
        (J, A, A, Psi), GBBG (1, Gravbar, S2, Chi), Unity;
        (J, B, B, Psi), GBBG (1, Gravbar, S2, Chi), Unity;
        (J, A, B, Psi), GBBG (1, Gravbar, S2, Chi), Unity],
       [])

    let parameters () =
      { input = [Lambda, 1.0; M, 1.0; MJ, 0.0; WJ, 0.0];
        derived =
        [ Complex Unity, Const 1;
          Real WA, Const 0; Real WB, Const 0; 
          Real G3_SSS, Neg (Quot (Prod [Atom M; Atom Lambda], 
                                     Sqrt (Const 2))); 
          Real G3_APP, Neg (Atom Lambda);  
          Complex G3_BPP, Prod [I; Atom Lambda];
          Real G4_SSSS, Neg (Quot (Prod [Atom Lambda; Atom Lambda], Const 2))];
        derived_arrays = [] }

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
      | A -> 1 | B -> 2 | J -> 0 | Psi -> 3 
    let mass_symbol = function
      | A -> "ma" | B -> "mb" | J -> "mj" | Psi -> "mp" 
    let width_symbol = function
      | A -> "wa" | B -> "wb" | J -> "wj" | Psi -> "wp" 
  end

(* \subsection*{Equations of Motion} *)

(* The equations of motion and the conservation of the Noether current have 
   been shown in J. Reuter: Supersymmetric Ward identities, unpublished. 
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
   \overline{Q} \xi = \int d^3 \vec{x} \biggl\{ \ii \overline{\Psi} \gamma^0 
   (\ii \fmslash{\partial} + m) A(x) - \overline{\Psi} \gamma^0 
   (\ii \fmslash{\partial} + m) B(x) \gamma^5 \\ + 
   \dfrac{\ii \lambda}{2\sqrt{2}} \overline{\Psi} \gamma^0 \left( A^2 (x) 
   - B^2 (x) \right) - \dfrac{\lambda}{\sqrt{2}} A(x) B(x) \gamma^0 \gamma^5 
   \biggr\} \xi 
   \end{multline}
   and to use the identity $\overline{\xi} Q = \overline{Q} \xi$ to show
   \begin{equation}
     \lbrack \overline{\xi} Q , \Psi \rbrack = - \ii \left( \ii 
     \fmslash{\partial} + m\right) (A + \ii \gamma^5 B) \xi - 
     \dfrac{\ii \lambda}{2\sqrt{2}} \left( A^2 - B^2 \right) \xi +
     \dfrac{\lambda}{\sqrt{2}} A B \gamma^5 \xi 
   \end{equation}
   Some remarks about that (nonlinear) transformation. On-shell only the 
   one-particle-pole contributes. But for off-shell Ward identities the 
   nonlinear terms give nonvanishing contributions in contact terms arising
   from the derivatives acting on the time ordering. The right method to 
   handle that difficulty is to define local operator insertions for every
   nonlinear term appearing in the transformations. 
*)

(* \subsection*{Ward Identities} *)

(* On shell current matrix elements
   \begin{multline}
     J_\mu(p_1,p_2) = \Braket{0|\mathcal{J}_\mu(x)|A (p_1) \Psi(p_2)} \\ = 
          \Braket{0|\mathcal{J}_\mu(x)|A(p_1)\Psi(p_2)}_{(0)} + \mathcal{O} 
          (g) \sim - \left(\fmslash{p}_1 - m\right) \gamma_\mu u(p_2) + 
          \mathcal{O} (g) 
   \end{multline}
   \begin{equation}
     (p_1+p_2)^\mu J_\mu(p_1,p_2) = - \left(\fmslash{p}_1 - m\right) 
     \left(\fmslash{p}_1 + \fmslash{p}_2 \right) u(p_2) + \mathcal{O} (g) = 
     \mathcal{O} (g) 
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
     -\ii \delta^4(y-x_2) \Braket{0|\mathrm{T}A(x_1) (\ii \fmslash{\partial} +
     m) A(x_2)\xi|0} \\
     = \partial^\mu_y
       \Braket{0|\mathrm{T}\mathcal{J}_\mu(y)A(x_1)\Psi(x_2)|0} = \ii 
       \partial^\mu_y \Braket{0|\mathrm{T} \Psi(x_2) 
       \overline{\Psi}(y) \gamma_\mu A(x_1) (\ii\fmslash{\partial}_y + m)A(y)
       \xi |0}
   \end{multline}
   in tree approximation in configuration space
   \begin{multline}
    \ii \delta^4(y-x_1) S_F(x_2-x_1)\xi -\ii \delta^4(y-x_2) 
    \left(\ii \fmslash{\partial}_{x_2} + m\right )D_F (x_1-x_2)\xi \\ 
   = \ii \partial^\mu_y \biggl\{
    S_F(x_2-y) \gamma_\mu \: (\ii \fmslash{\partial}_y + m) D_F(x_1-y)\xi
    \biggr\}
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
     \mbox{} \left( \dfrac{\ii(-\ii)}{\fmslash{p}_2+m} + \dfrac{\ii(-\ii)
      (\fmslash{p}_1+m)}{p_1^2-m^2} \right) \xi \\ = \left( p_1 + p_2 
      \right)^\mu \biggl\{ S_F(x_2-y) \gamma_\mu \: (\ii \fmslash{\partial}_y 
      + m) D_F(x_1-y)\xi\biggr\} \\ = 
      \ii (-\ii) \dfrac{1}{p_1^2-m^2} \dfrac{1}{\fmslash{p}_2+m} 
      (\fmslash{p}_1 + \fmslash{p}_2) (\fmslash{p}_1 + m) \xi = + \left( 
      \dfrac{1}{\fmslash{p}_2+m} + 
      \dfrac{\fmslash{p}_1+m}{p_1^2-m^2} \right) \xi  
   \end{multline}
   Some words about the signs: The momentum flux always goes from the right 
   spacetime event argument of the propagator to the left. In our case the two 
   propagators $S_F$ and $D_F$ have the exponentials $\exp(\ii p_2(x_2-y)$ and
   $\exp(\ii p_1 (x_1-y)$ respectively. The sign of the derivative of the 
   current can be understood as the derivative acts on a field operator 
   inserted in the amplitude and not on a field in an interaction vertex. *)

(* We now go to a more complex example:
   \begin{subequations}
   \begin{align}
     \mathrm{F.T.} \Braket{0|\mathrm{T}\ii \overline{\xi} \Psi(x_1)
       B(x_2)\Psi(x_3)|0} &= \dfrac{-\ii\lambda}{\sqrt{2}} 
       \dfrac{\ii}{p_2^2 - m^2} \dfrac{-\ii}{\fmslash{p}_3+m} \gamma^5 
       \dfrac{\ii}{\fmslash{p}_1 +\fmslash{k}-m} \xi 
   \end{align}
   \begin{align}
     \mathrm{F.T.} \Braket{0|\mathrm{T}A(x_1)(-\overline{\xi} \gamma^5
       \Psi (x_2))\Psi(x_3)|0}
        &= \dfrac{\ii\lambda}{\sqrt{2}} \dfrac{\ii}{p_1^2-m^2} \dfrac{
        -\ii}{\fmslash{p}_3+m} \dfrac{\ii}{\fmslash{p}_2 + \fmslash{k} - m}
        \gamma^5 \xi 
    \end{align}
    \begin{multline}
     \mathrm{F.T.} \Braket{0|\mathrm{T}A(x_1)B(x_2) \left( \ii 
     \fmslash{\partial}_{x_3} + m \right) B(x_3) \gamma^5 \xi|0} =  \\
     \dfrac{-\ii m \lambda}{\sqrt{2}} \dfrac{\ii}{p_1^2-m^2} \dfrac{\ii}{
     p_2^2-m^2} \dfrac{\ii}{(p_3+k)^2-m^2} \left( -\fmslash{p}_3 - 
     \fmslash{k} + m \right) \gamma^5 \xi 
   \end{multline}
   \begin{multline}
      \mathrm{F.T.} \Braket{0|\mathrm{T}A(x_1)B(x_2)  
      \dfrac{\lambda}{\sqrt{2}} (A B) (x_3) \gamma^5 \xi|0} =  \\  
      \dfrac{\lambda}{\sqrt{2}} \dfrac{\ii}{p_1^2-m^2} \dfrac{\ii}{p^2-m^2} 
      \gamma^5 \xi 
   \end{multline}
   \end{subequations}

       \vspace{0.5cm}                                                    

   Now we must evaluate the 4-point-function with the current insertion.
   We rewrite the current $\overline{\xi} \mathcal{J}_\mu$ as
   $\overline{\mathcal{J}_\mu} \xi$, which is identical due to the
   Majorana properties of the current and the transformation parameter:
   \begin{align}
   \overline{\xi} \mathcal{J}_\mu = & \; \overline{\xi}
   \biggl\{ \ii \left( (\ii \fmslash{\partial} - m) A \right) 
       \gamma_\mu \Psi + \left( (\ii \fmslash{\partial} + m) B \right) 
       \gamma^5 \gamma_\mu \Psi \\ &\qquad\qquad\qquad  
        - \dfrac{\ii \lambda}{2 \sqrt{2}} \gamma_\mu \left( A^2 - B^2 \right) 
       \Psi - \dfrac{\lambda}{\sqrt{2}} \gamma_\mu \gamma^5 A B \Psi
       \biggr\} \notag
     \\ = & \; \biggl\{ \overline{\Psi} \gamma_\mu \ii \left( \ii 
     \fmslash{\partial} + m\right) A  - \overline{\Psi} \gamma_\mu 
     \left( \ii \fmslash{\partial} + m\right) B \gamma^5  \\ & 
     \qquad\qquad\qquad + \dfrac{\ii\lambda}{2\sqrt{2}} \overline{\Psi}
     \gamma_\mu \left(
     A^2 - B^2 \right)  - \dfrac{\lambda}{\sqrt{2}} \overline{\Psi} 
     \gamma_\mu \gamma^5 A B \biggr\} \xi
   \end{align}
   This brings the propagator of the (matter) fermion to the farthest left.
   There are four diagrams contributing to the process which we will list now:       \begin{equation}
       \parbox{21mm}{%
         \begin{fmfgraph*}(20,20)
           \fmfleft{i1,i2}\fmfright{o1,o2}
           \fmf{plain}{o2,v2}
           \fmf{dbl_dashes}{i2,v2}
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
           \fmf{dbl_dashes}{o1,v1}
           \fmfdot{v1,v2}
           \fmfblob{.25w}{i1}
         \end{fmfgraph*}}\qquad + \quad
       \parbox{21mm}{%
         \begin{fmfgraph*}(20,20)
           \fmfleft{i1,i2}\fmfright{o1,o2}
           \fmf{dashes}{o2,v2}
           \fmf{dbl_dashes}{i2,v2}
           \fmf{dbl_dashes}{v1,v2}
           \fmf{dbl_plain}{i1,v1}
           \fmf{plain}{o1,v1}
           \fmfdot{v1,v2}
           \fmfblob{.25w}{i1}
         \end{fmfgraph*}}\qquad + \quad
       \parbox{21mm}{%
         \begin{fmfgraph*}(20,20)
           \fmfleft{i1,i2}\fmfright{o1,o2}
           \fmf{plain}{o2,v}
           \fmf{dashes}{i2,v}
           \fmf{dbl_plain}{i1,v}
           \fmf{dbl_dashes}{o1,v}
           \fmfdot{v}
           \fmfblob{.25w}{i1}
         \end{fmfgraph*}}\qquad\quad
   \end{equation}                               

   For the sign of the fermion propagator one has to take care of the flow of
   momentum.
   \begin{multline}
     \mathrm{F.T.} \Braket{0|\mathrm{T}\overline{\mathcal{J}_\mu}(y) \xi A(x_1)
            B(x_2)\Psi(x_3)|0} = \\
     \frac{\mathrm{i}}{p_1^2-m^2}\frac{\mathrm{i}}{p_2^2-m^2}
     \dfrac{-\ii}{\fmslash{p}_3+m} \left( \mathrm{F.T.} \Braket{0|\mathrm{T}
     \overline{\mathcal{J}_\mu} (y)A(x_1)B(x_2)\Psi(x_3)|0}_{\text{amp.}}
     \right) \xi
   \end{multline}
   \begin{multline}
     \mathrm{F.T.} \Braket{0|\mathrm{T}\overline{\mathcal{J}_\mu}(y)
           A(x_1)B(x_2)\Psi(x_3)|0}_{\text{amp.}} \xi =
       - \dfrac{\ii \lambda}{\sqrt{2}} \gamma^5 \dfrac{\ii}{\fmslash{p}_1 +
       \fmslash{k}-m} \gamma_\mu \left( \fmslash{p}_1 + m \right) \xi \\
       + \dfrac{\ii\lambda}{\sqrt{2}} \dfrac{\ii}{\fmslash{p}_2 + \fmslash{k}
       -m} \gamma_\mu \left( \fmslash{p}_2 + m\right) \gamma^5 \xi 
       - \dfrac{\lambda}{\sqrt{2}} \gamma_\mu \gamma^5 \xi 
       \\
       - \dfrac{\ii m \lambda}{\sqrt{2}} \dfrac{\ii}{(p_3 + k)^2 - m^2} 
       \gamma_\mu \left( \fmslash{p}_3 + \fmslash{k} - m \right) \gamma^5 \xi
   \end{multline}
   with $\partial_\mu\to\mathrm{i}k_\mu=-\mathrm{i}(p_1+p_2+p_3)_\mu$      
   \begin{align}
     & \; \dfrac{\ii}{\fmslash{p}_3+m} \dfrac{1}{(p_1^2-m^2)(p_2^2-m^2)} \;
       \mathrm{F.T.} \partial_y^\mu \Braket{0|\mathrm{T}
       \overline{\mathcal{J}_\mu}(y) A(x_1)B(x_2)
        \Psi(x_3)|0}_{\text{amp.}} \xi \notag \\ =
       & \; \dfrac{\ii}{\fmslash{p}_3+m} \dfrac{1}{(p_1^2-m^2)(p_2^2-m^2)} 
       \cdot \biggl\{
       \dfrac{\ii\lambda}{\sqrt{2}} \left( \fmslash{p}_1 + \fmslash{p}_2 +
       \fmslash{p}_3 \right) \gamma^5 \xi \notag \\ & \;
       - \dfrac{\ii\lambda}{\sqrt{2}} \gamma^5 \dfrac{1}{\fmslash{p}_1 + 
       \fmslash{k} - m} \left( \fmslash{p}_1 + \fmslash{p}_2 + \fmslash{p}_3
       \right) \left( \fmslash{p}_1 + m \right) \xi \notag \\ & \;
       + \dfrac{\ii\lambda}{\sqrt{2}} \dfrac{1}{\fmslash{p}_2 + \fmslash{k}
       -m} \left( \fmslash{p}_1 + \fmslash{p}_2 + \fmslash{p}_3 \right) \left(
       \fmslash{p}_2 + m\right) \gamma^5 \xi \notag \\ & \;
       - \dfrac{\ii m \lambda}{\sqrt{2}} \dfrac{1}{(p_3 + k)^2 - m^2} 
       \left( \fmslash{p}_1 + \fmslash{p}_2 + \fmslash{p}_3 \right) \left(
       \fmslash{p}_3 + \fmslash{k} - m \right) \gamma^5 \xi \biggr\} 
       \notag 
       \end{align}
       \begin{align}
       = & \; \dfrac{\lambda}{\sqrt{2}} 
       \dfrac{1}{\fmslash{p}_3+m} \dfrac{1}{(p_1^2-m^2)(p_2^2-m^2)} 
       \cdot \biggl\{
       - \left( \fmslash{p}_1 + \fmslash{p}_2 +
       \fmslash{p}_3 \right) \notag \\ & \;
       + \dfrac{1}{\fmslash{p}_2 + 
       \fmslash{p}_3 - m} \left( \fmslash{p}_1 + \fmslash{p}_2 + \fmslash{p}_3
       \right) \left( \fmslash{p}_1 - m \right) \notag \\ & \;
       + \dfrac{1}{\fmslash{p}_1 + \fmslash{p}_3
       +m} \left( \fmslash{p}_1 + \fmslash{p}_2 + \fmslash{p}_3 \right) \left(
       \fmslash{p}_2 + m\right) \notag \\ & \;
       - m \dfrac{1}{(p_1 + p_2)^2 - m^2} 
       \left( \fmslash{p}_1 + \fmslash{p}_2 + \fmslash{p}_3 \right) \left(
       \fmslash{p}_1 + \fmslash{p}_2 + m \right) \biggr\} \gamma^5 \xi 
       \notag \\
       = & \;
       - \dfrac{\lambda}{\sqrt{2}} \dfrac{1}{\fmslash{p}_3+m} \dfrac{1}{
       (p_1^2-m^2)(p_2^2-m^2)} \left( \fmslash{p}_1 + \fmslash{p}_2 + 
       \fmslash{p}_3 \right) \gamma^5 \xi \notag \\ & \; 
       + \dfrac{\lambda}{\sqrt{2}} \dfrac{1}{\fmslash{p}_3+m} \dfrac{1}{(p_1^2
       -m^2)(p_2^2-m^2)} \left( \fmslash{p}_1 - m \right) \gamma^5 \xi \notag
       \\ & \; + \dfrac{\lambda}{\sqrt{2}} \dfrac{1}{\fmslash{p}_3+m} 
       \dfrac{1}{p_2^2-m^2} \dfrac{1}{\fmslash{p}_2 + \fmslash{p}_3-m} 
       \gamma^5 \xi \notag \\ & \; 
       + \dfrac{\lambda}{\sqrt{2}} \dfrac{1}{\fmslash{p}_3+m} \dfrac{1}{(p_1^2
       -m^2)(p_2^2-m^2)} \left(\fmslash{p}_2+m\right) \gamma^5 \xi \notag \\
       & \; + \dfrac{\lambda}{\sqrt{2}} \dfrac{1}{\fmslash{p}_3+m} \dfrac{1}{
       p_1^2-m^2} \dfrac{1}{\fmslash{p}_1+\fmslash{p}_3+m} \gamma^5 \xi \notag 
       \\ & \;
       - \dfrac{m\lambda}{\sqrt{2}} \dfrac{1}{(p_1^2-m^2)(p_2^2-m^2)}
       \dfrac{1}{(p_1 + p_2)^2 - m^2} \left( \fmslash{p}_1 + \fmslash{p}_2 
       + m \right)\gamma^5 \xi \notag \\ & \; - \dfrac{m \lambda}{\sqrt{2}} 
       \dfrac{1}{\fmslash{p}_3+m} \dfrac{1}{(p_1^2-m^2)(p_2^2-m^2)} \gamma^5
       \xi
   \end{align}                                                          

  The third, fifth and sixth term equal the ones from the linearly transformed
  fields of the r.h.s.:
  \begin{multline}
    \dfrac{\lambda}{\sqrt{2}} \biggl\{ \dfrac{1}{p_2^2-m^2} \dfrac{1}{\fmslash{
     p}_3+m} \dfrac{1}{\fmslash{p}_2 + \fmslash{p}_3 - m} + \dfrac{1}{p_1^2-
     m^2} \dfrac{1}{\fmslash{p}_3+m} \dfrac{1}{\fmslash{p}_1+\fmslash{p}_3
     +m} \\ - m \dfrac{1}{p_1^2-m^2} \dfrac{1}{p_2^2-m^2} \dfrac{1}{(p_1+p_2)^2
     -m^2} \left( \fmslash{p}_1 + \fmslash{p}_2 + m \right) \biggr\} \gamma^5
     \xi 
  \end{multline}
  The remaining terms add up to:
  \begin{multline}
   \dfrac{\lambda}{\sqrt{2}} \dfrac{1}{(p_1^2-m^2)(p_2^2-m^2)} \dfrac{1}{
   \fmslash{p}_3 +m} \biggl\{ - \fmslash{p}_1 - \fmslash{p}_2 - \fmslash{p}_3
   + \fmslash{p}_1 - m + \fmslash{p}_2 + m - m 
   \biggr\} \gamma^5 \xi \\ = - \dfrac{\lambda}{\sqrt{2}} \dfrac{1}{(p_1^2-
   m^2)(p_2^2-m^2)} \gamma^5 \xi 
  \end{multline}  
  This is the one from the local operator insertion, and so the Ward identity
  is fulfilled.
 *)

(* Caveat: the Ward identities for on-shell amplitudes do \emph{not} test
   the theory comprehensively, since only the coupling of Goldstone bosons
   and and currents to external lines. *)

(* In the case with auxiliary fields:
   \begin{equation}
  \label{eq:kinvoll}
  \begin{aligned}
  {\cal L}_{WZ} = & \; \frac{1}{2} \partial_\mu A \partial^\mu A + \frac{1}{2}
  \partial_\mu B \partial^\mu B + \frac{1}{2} \overline{\Psi} \left( \ii 
  \fmslash{\partial} - m \right) \Psi + \dfrac{1}{2} F^2 + \dfrac{1}{2} G^2
  \\ & \; - \dfrac{\lambda}{2 \sqrt{2}} 
  \overline{\Psi} \Psi A + \dfrac{\ii\lambda}{2 \sqrt{2}} \overline{\Psi} 
  \gamma^5 \Psi B + m A F + m B G \\ & \; + \dfrac{\lambda}{2\sqrt{2}} A^2 F - 
  \dfrac{\lambda}{2\sqrt{2}} B^2 F + \dfrac{\lambda}{\sqrt{2}} A B G 
  \end{aligned}
\end{equation}      
The current is the same - with or without auxiliary fields - because the 
auxiliary fields cancel each other in the construction of the current. 
As one can easily see the current generates the SUSY transformations 
automatically with the equations of motion for the auxiliary fields inserted.
The real problem seems to be that the algebra implemented on the fields does
not close off-shell but needs insertion of the equations of motion of all 
fields. So using the formalism without the auxiliary fields integrated out in 
the path integral seems just to split the scalar component fields from their 
mass terms. The additional diagrams with the auxiliary fields only add the
masses for the scalar fields and make them equal the masses of the fermionic
component fields. 

The equations of motion of the scalar and pseudoscalar auxiliary fields are:
\begin{equation}
F = - m A - \dfrac{\lambda}{2\sqrt{2}} \left( A^2 - B^2 \right) , \qquad 
G = - m B - \dfrac{\lambda}{\sqrt{2}} A B 
\end{equation} *)

module Main = Omega.Make(Fusion.Mixed23_Majorana)
    (Targets.Fortran_Majorana)(WZ)
let _ = Main.main ()

(*i
 *  Local Variables:
 *  mode:caml
 *  indent-tabs-mode:nil
 *  page-delimiter:"^(\\* .*\n"
 *  End:
i*)




































































































































































