(* f90_O2.ml --

   Copyright (C) 1999-2023 by

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

(* Simplest model available: $\mathrm{SO}(2)$
   \begin{equation}
     \mathcal{L} = \frac{1}{2} \partial_\mu\Phi\partial^\mu\Phi
        - \frac{g}{4} (\Phi^2-v^2)^2
   \end{equation}
   equation of motion
   \begin{equation}
      \Box\Phi = g(v^2-\Phi^2)\Phi
   \end{equation} *)

module O2 = 
  struct
    open Coupling
    let options = Options.empty

(* Expand fields around a new minimum
   \begin{equation}
     \Phi = \begin{pmatrix} v + \phi_1 \\ \phi_2 \end{pmatrix}
   \end{equation}
   with $\Phi^2-v^2=\phi_1^2+\phi_2^2+2v\phi_1$. *)
    type flavor = Phi1 | Phi2 | J

    let conjugate f = f

    let external_flavors () =
      [ "fields", [Phi1; Phi2];
        "currents", [J] ]

    let flavors () = ThoList.flatmap snd (external_flavors ())

    let flavor_of_string = function
      | "1" -> Phi1 | "2" -> Phi2 | "j" -> J
      | _ -> invalid_arg "O2.flavor_of_string"

    let flavor_to_string = function
      | Phi1 -> "phi1" | Phi2 -> "phi2" | J -> "j"

    let flavor_symbol = function
      | Phi1 -> "p1" | Phi2 -> "p2" | J -> "j"

    let lorentz = function
      | Phi1 | Phi2 -> Scalar | J -> Vector

    let propagator = function
      | Phi1 | Phi2 -> Prop_Scalar | J -> Only_Insertion

    let width _ = Timelike
    let goldstone _ = None
    let fermion _ = 0
    let color _ = Color.Singlet
    type gauge = unit
    let gauge_symbol () = failwith "O2.gauge_symbol: internal error"

    let colsymm _ = (0,false), (0,false)

(* \begin{multline}
     \mathcal{L} =
         \frac{1}{2} \partial_\mu\phi_1\partial^\mu\phi_1
       - \frac{1}{2} 2gv^2\phi_1^2 - gv \phi_1^3  - \frac{g}{4} \phi_1^4 \\
       + \frac{1}{2} \partial_\mu\phi_2\partial^\mu\phi_2 - \frac{g}{4} \phi_2^4
       - gv \phi_1\phi_2^2
       - \frac{g}{2} \phi_1^2\phi_2^2
   \end{multline}
   Propagators
   \begin{subequations}
     \begin{align}
       \parbox{21mm}{%
         \begin{fmfgraph*}(20,5)
           \fmfleft{i}\fmfright{o}
           \fmflabel{$\phi_1(p)$}{i}
           \fmflabel{$\phi_1(p)$}{o}
           \fmf{plain}{i,o}
           \fmfdot{i,o}
         \end{fmfgraph*}}\qquad\quad
           &= \frac{\mathrm{i}}{p^2-2gv^2+\mathrm{i}\epsilon} \\
       \parbox{21mm}{%
         \begin{fmfgraph*}(20,5)
           \fmfleft{i}\fmfright{o}
           \fmflabel{$\phi_2(p)$}{i}
           \fmflabel{$\phi_2(p)$}{o}
           \fmf{dashes}{i,o}
           \fmfdot{i,o}
         \end{fmfgraph*}}\qquad\quad
           &= \frac{\mathrm{i}}{p^2+\mathrm{i}\epsilon}
     \end{align}
   \end{subequations}
   Three point vertices
   \begin{subequations}
     \begin{align}
       \parbox{21mm}{%
         \hfil\\\hfil\\
         \begin{fmfgraph*}(20,15)
           \fmfleft{p1}\fmfright{p2,p3}
           \fmflabel{$\phi_1(p_1)$}{p1}
           \fmflabel{$\phi_1(p_2)$}{p2}
           \fmflabel{$\phi_1(p_3)$}{p3}
           \fmf{plain}{p1,v}
           \fmf{plain}{p2,v,p3}
           \fmfdot{v}
         \end{fmfgraph*}\\
         \hfil}\qquad\quad
           &= -6\mathrm{i}gv\\
       \parbox{21mm}{%
         \hfil\\\hfil\\
         \begin{fmfgraph*}(20,15)
           \fmfleft{p1}\fmfright{p2,p3}
           \fmflabel{$\phi_1(p_1)$}{p1}
           \fmflabel{$\phi_2(p_2)$}{p2}
           \fmflabel{$\phi_2(p_3)$}{p3}
           \fmf{plain}{p1,v}
           \fmf{dashes}{p2,v,p3}
           \fmfdot{v}
         \end{fmfgraph*}\\
         \hfil}\qquad\quad
           &= -2\mathrm{i}gv
     \end{align}
   \end{subequations}
   Four point vertices
   \begin{subequations}
     \begin{align}
       \parbox{21mm}{%
         \hfil\\\hfil\\
         \begin{fmfgraph*}(20,15)
           \fmfleft{p1,p2}\fmfright{p3,p4}
           \fmflabel{$\phi_1(p_1)$}{p1}
           \fmflabel{$\phi_1(p_2)$}{p2}
           \fmflabel{$\phi_1(p_3)$}{p3}
           \fmflabel{$\phi_1(p_4)$}{p4}
           \fmf{plain}{p1,v,p2}
           \fmf{plain}{p3,v,p4}
           \fmfdot{v}
         \end{fmfgraph*}\\
         \hfil}\qquad\quad
           &= -6\mathrm{i}g\\
       \parbox{21mm}{%
         \hfil\\\hfil\\
         \begin{fmfgraph*}(20,15)
           \fmfleft{p1,p2}\fmfright{p3,p4}
           \fmflabel{$\phi_2(p_1)$}{p1}
           \fmflabel{$\phi_2(p_2)$}{p2}
           \fmflabel{$\phi_2(p_3)$}{p3}
           \fmflabel{$\phi_2(p_4)$}{p4}
           \fmf{dashes}{p1,v,p2}
           \fmf{dashes}{p3,v,p4}
           \fmfdot{v}
         \end{fmfgraph*}\\
         \hfil}\qquad\quad
           &= -6\mathrm{i}g\\
       \parbox{21mm}{%
         \hfil\\\hfil\\
         \begin{fmfgraph*}(20,15)
           \fmfleft{p1,p2}\fmfright{p3,p4}
           \fmflabel{$\phi_1(p_1)$}{p1}
           \fmflabel{$\phi_1(p_2)$}{p2}
           \fmflabel{$\phi_2(p_3)$}{p3}
           \fmflabel{$\phi_2(p_4)$}{p4}
           \fmf{plain}{p1,v,p2}
           \fmf{dashes}{p3,v,p4}
           \fmfdot{v}
         \end{fmfgraph*}\\
         \hfil}\qquad\quad
           &= -2\mathrm{i}g
     \end{align}
   \end{subequations} *)

(* \subsection*{Conserved Current} *)

(* \begin{equation}
     \mathcal{L}\lbrack j_V, j_S \rbrack =
       j_V^\mu\phi_1\mathrm{i}\overleftrightarrow{\partial_\mu}\phi_2
         + j_S v\mathrm{i}\partial_\mu\phi_2
   \end{equation} *)

    type constant =
      | Unity | G | Vev
      | M1 | M2 | MJ | W1 | W2 | WJ
      | G3_111 | G3_122
      | G4_1111 | G4_1122 | G4_2222
    let constant_symbol = function
      | Unity -> "unity" | G -> "g" | Vev -> "vev"
      | M1 -> "m1" | M2 -> "m2" | MJ -> "mj"
      | W1 -> "w1" | W2 -> "w2" | WJ -> "wj"
      | G3_111 -> "g111" | G3_122 -> "g122"
      | G4_1111 -> "g1111" | G4_2222 -> "g2222"
      | G4_1122 -> "g1122"

    let vertices () =
      ([(Phi1, Phi1, Phi1), Scalar_Scalar_Scalar 1, G3_111;
        (Phi1, Phi2, Phi2), Scalar_Scalar_Scalar 1, G3_122;
        (J, Phi1, Phi2), Vector_Scalar_Scalar 1, Unity],
       [(Phi1, Phi1, Phi1, Phi1), Scalar4 1, G4_1111;
        (Phi2, Phi2, Phi2, Phi2), Scalar4 1, G4_2222;
        (Phi1, Phi1, Phi2, Phi2), Scalar4 1, G4_1122],
       [])

    let parameters () =
      { input = [G, 1.0; Vev, 1.0; MJ, 0.0; WJ, 0.0];
        derived =
        [ Complex Unity, Const 1;
          Real M1, Sqrt (Prod [Const 2; Atom G; Atom Vev; Atom Vev]);
          Real M2, Const 0;
          Real W1, Const 0;
          Real W2, Const 0;
          Real G3_111, Prod [Const (-6); Atom G; Atom Vev];
          Real G4_1111, Prod [Const (-6); Atom G];
          Real G4_2222, Prod [Const (-6); Atom G];
          Real G3_122, Prod [Const (-2); Atom G; Atom Vev];
          Real G4_1122, Prod [Const (-2); Atom G] ];
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
      | Phi1 -> 1 | Phi2 -> 2 | J -> 0
    let mass_symbol = function
      | Phi1 -> "m1" | Phi2 -> "m2" | J -> "mj"
    let width_symbol = function
      | Phi1 -> "w1" | Phi2 -> "w2" | J -> "wj"
  end

(* \subsection*{Equations of Motion} *)

(* Equations of motion in the broken phase
   \begin{subequations}
   \begin{align}
     \Box\phi_1 &= - 2gv^2\phi_1 - 3gv\phi_1^2 - gv\phi_2^2
                   - g \phi_1^3 - g \phi_1\phi_2^2 \\
     \Box\phi_2 &= - 2gv \phi_1\phi_2 - g \phi_1^2\phi_2 - g\phi_2^3
   \end{align}
   \end{subequations}
   factoring invariants simplifies things below
   \begin{subequations}
   \begin{align}
     \Box\phi_1 &= - g (\phi_1 + v) (\phi_1^2 + \phi_2^2 + 2v\phi_1)\\
     \Box\phi_2 &= - g \phi_2(\phi_1^2 + \phi_2^2 + 2v\phi_1)
   \end{align}
   \end{subequations}
   Noether current
   \begin{equation}
     j_\mu = \phi_1\partial_\mu\phi_2 - \phi_2\partial_\mu\phi_1 + v\partial_\mu\phi_2
   \end{equation}
   is conserved explicitely
   \begin{multline}
     \partial^\mu j_\mu = (\phi_1+v)\Box\phi_2 - \phi_2\Box\phi_1 \\
       = - g(\phi_1+v)\phi_2(\phi_1^2 + \phi_2^2 + 2v\phi_1)
         + g\phi_2(\phi_1 + v) (\phi_1^2 + \phi_2^2 + 2v\phi_1) = 0
   \end{multline}
   conserved charge
   \begin{subequations}
   \begin{align}
     \lbrack Q,\phi_1 \rbrack &= - \phi_2 \\
     \lbrack Q,\phi_2 \rbrack  &= \phi_1 + v
   \end{align}
   \end{subequations}
   with
   \begin{equation}
     \lbrack Q , \phi_1^2 + \phi_2^2 + 2v\phi_1 \rbrack  = 0
   \end{equation}
   covariance of the equations of motion
   \begin{subequations}
   \begin{align}
     \Box\lbrack Q,\phi_1 \rbrack  &= \lbrack Q,\Box\phi_1 \rbrack  \\
     -\Box\phi_2 &= -g \lbrack Q, (\phi_1+v)(\phi_1^2 + \phi_2^2 + 2v\phi_1) \rbrack 
                 = g \phi_2 (\phi_1^2 + \phi_2^2 + 2v\phi_1)
   \end{align}
   \end{subequations}
   and
   \begin{subequations}
   \begin{align}
     \Box\lbrack Q,\phi_2 \rbrack  &= \lbrack Q,\Box\phi_2 \rbrack  \\
     \Box\phi_1 &= -g \lbrack Q, \phi_2(\phi_1^2 + \phi_2^2 + 2v\phi_1) \rbrack 
                 = -g (\phi_1+v) (\phi_1^2 + \phi_2^2 + 2v\phi_1)
   \end{align}
   \end{subequations} *)

(* \subsection*{Ward Identities} *)

(* On shell current matrix elements
   \begin{multline}
     J_\mu(k_1,k_2) = \Braket{0|j_\mu(x)|\phi_1(k_1)\phi_2(k_2)} = 
          \Braket{0|j_\mu(x)|\phi_1(k_1)\phi_2(k_2)}_{(0)} \\
        + \mathrm{i}\int\!\mathrm{d}^4y\;
          \Braket{0|j_\mu(x)\mathcal{L}(y)|\phi_1(k_1)\phi_2(k_2)}_{(0)}
        + O(g^2) \\
      \sim k_{2,\mu} - k_{1,\mu}
        + v (k_1+k_2)_\mu \frac{\mathrm{i}}{(k_1+k_2)^2} (-\mathrm{i} 2gv)
        + O(g^2)
   \end{multline}
   \begin{equation}
     (k_1+k_2)^\mu J_\mu(k_1,k_2) = k_2^2 - k_1^2 + 2gv^2 + O(g^2) = O(g^2)
   \end{equation}
   Also for off-shell Greensfunctions
   \begin{multline}
     \frac{\partial}{\partial x_\mu}
       \Braket{0|\mathrm{T}j_\mu(x)\phi(y)\phi(z)|0} =
         \delta(x_0-y_0) \Braket{0|\mathrm{T}\lbrack j_0(x),\phi(y) \rbrack \phi(z)|0} \\
       + \delta(x_0-z_0) \Braket{0|\mathrm{T}\phi(y)\lbrack j_0(x),\phi(z) \rbrack |0}
       + \Braket{0|\mathrm{T}\partial^\mu j_\mu(x)\phi(y)\phi(z)|0}
   \end{multline}
   where the last term vanishes for purely spontaneous symmetry
   breaking. Assuming
   \begin{equation}
     \lbrack j_0(x),\phi(y) \rbrack \Bigr\vert_{x_0=y_0}
       = \delta^3(\vec x - \vec y) \lbrack Q,\phi(y) \rbrack 
   \end{equation}
   this reads
   \begin{multline}
       \delta^4(x-y) \Braket{0|\mathrm{T}\lbrack Q,\phi(y) \rbrack \phi(z)|0}
     + \delta^4(x-z) \Braket{0|\mathrm{T}\phi(y)\lbrack Q,\phi(z) \rbrack |0} = \\
     \frac{\partial}{\partial x_\mu}
       \Braket{0|\mathrm{T}j_\mu(x)\phi(y)\phi(z)|0}
         - \Braket{0|\mathrm{T}\partial^\mu j_\mu(x)\phi(y)\phi(z)|0}
   \end{multline}
   Integrated (zero-momentum insertion)
   \begin{multline}
       \Braket{0|\mathrm{T}\lbrack Q,\phi(y) \rbrack \phi(z)|0}
     + \Braket{0|\mathrm{T}\phi(y)\lbrack Q,\phi(z) \rbrack |0} =
      \Braket{0|\mathrm{T}\lbrack Q,\phi(y)\phi(z) \rbrack |0} = \\
       \int\!\mathrm{d}^4x\, \frac{\partial}{\partial x_\mu}
       \Braket{0|\mathrm{T}j_\mu(x)\phi(y)\phi(z)|0}
         - \int\!\mathrm{d}^4x
              \Braket{0|\mathrm{T}\partial^\mu j_\mu(x)\phi(y)\phi(z)|0}
   \end{multline}
   where the first term does \emph{not} vanish for spontaneous symmetry
   breaking, because massless Goldstone boson states give a contribution
   at infinity.
   E.\,g.:
   \begin{multline}
       \delta^4(y-x_1) \Braket{0|\mathrm{T}\lbrack Q,\phi_1(x_1) \rbrack \phi_2(x_2)|0}
     + \delta^4(y-x_2) \Braket{0|\mathrm{T}\phi_1(x_1)\lbrack Q,\phi_2(x_2) \rbrack |0} \\
     =
     - \delta^4(y-x_1) \Braket{0|\mathrm{T}\phi_2(x_1)\phi_2(x_2)|0}
     + \delta^4(y-x_2) \Braket{0|\mathrm{T}\phi_1(x_1)\phi_1(x_2)|0} \\
     + v \delta^4(y-x_2) \Braket{0|\mathrm{T}\phi_1(x_1)|0}
     = \frac{\partial}{\partial y_\mu}
       \Braket{0|\mathrm{T}j_\mu(y)\phi_1(x_1)\phi_2(x_2)|0}
   \end{multline}
   in tree approximation in momentum space
   \begin{multline}
     \mbox{} - \frac{\mathrm{i}}{k_2^2} + \frac{\mathrm{i}}{k_1^2-2gv^2} =
        -\mathrm{i}(k_1+k_2)^\mu(k_{2,\mu}-k_{1,\mu})
           \frac{\mathrm{i}}{k_2^2}\frac{\mathrm{i}}{k_1^2-2gv^2} \\
       + v(-\mathrm{i}(k_1+k_2)^\mu) (k_1+k_2)_\mu
           \frac{\mathrm{i}}{(k_1+k_2)^2} (-\mathrm{i} 2gv)
           \frac{\mathrm{i}}{k_2^2}\frac{\mathrm{i}}{k_1^2-2gv^2} \\
      = -\mathrm{i} (k_2^2-k_1^2 + 2gv^2)
           \frac{\mathrm{i}}{k_2^2}\frac{\mathrm{i}}{k_1^2-2gv^2}
   \end{multline}
   similarly, the transformed $n$-point function can be related to the
   divergence of a $(n-1)$-point function with the insertion of one
   current.

   Graphically denoting the influx of momentum by a dotted line, we have
   the \emph{exact} relation (for $k+p_1+p_2=0$ and all momenta incoming)
   \begin{equation}
       \parbox{21mm}{%
         \begin{fmfgraph*}(20,15)
           \fmfleft{i,di}\fmfright{o,do}
           \fmftop{k}
           \fmf{plain,label=$p_1$,l.side=left}{i,o}
           \fmf{dots,label=$k$,l.side=left}{k,o}
         \end{fmfgraph*}} =
       \parbox{21mm}{%
         \begin{fmfgraph*}(20,15)
           \fmfleft{i,di}\fmfright{o,do}
           \fmftop{k}
           \fmf{dashes,label=$p_2$,l.side=left}{i,o}
           \fmf{dots,label=$k$,l.side=left}{i,k}
         \end{fmfgraph*}} +
       \parbox{21mm}{%
         \begin{fmfgraph*}(20,15)
           \fmfleft{i,di}\fmfright{o,do}
           \fmftop{k}
           \fmf{plain}{i,v}
           \fmf{dashes}{v,o}
           \fmf{dashes,label=$\phi_2$,l.side=left}{v,k}
           \fmfdot{v,k}
         \end{fmfgraph*}} +
       \parbox{21mm}{%
         \begin{fmfgraph*}(20,15)
           \fmfleft{i,di}\fmfright{o,do}
           \fmftop{k}
           \fmf{plain}{i,v}
           \fmf{dashes}{v,o}
           \fmf{dots,label=$\partial_\mu$,l.side=left,tension=2}{v,k}
           \fmfdot{v}
         \end{fmfgraph*}}
   \end{equation}
   that can eventually be used to derive more complicated relations, if we
   manage to find the corresponding rules for vertices.
   
   Caveat: in 
   \begin{multline}
     \frac{\partial}{\partial y_\mu}
       \Braket{0|\mathrm{T}j_\mu(y)\phi_2(x_1)\phi_1(x_2)\cdots|0} = \\
       \delta^4(y-x_1) \Braket{0|\mathrm{T}\lbrack Q,\phi_2(x_1) \rbrack \phi_1(x_2)\cdots|0} \\
     \mbox{} + \delta^4(y-x_2)
       \Braket{0|\mathrm{T}\phi_2(x_1)\lbrack Q,\phi_1(x_2) \rbrack \cdots|0} + \ldots = \\
               \delta^4(y-x_1)
       \Braket{0|\mathrm{T}\phi_1(x_1)\phi_1(x_2)\cdots|0}
         + v \delta^4(y-x_1)\Braket{0|\mathrm{T}\phi_1(x_2)\cdots|0} \\
     \mbox{} - \delta^4(y-x_2)
       \Braket{0|\mathrm{T}\phi_2(x_1)\phi_2(x_2)\cdots|0} - \ldots
   \end{multline}
   the $v$-term in the transformation of~$\phi_2$ does \emph{not}
   vanish.  However, a closer inspection of the fourier transform 
   \begin{multline}
     k^{\mu}G_{\mu}^{j|\phi_2\phi_1\cdots} (k|p_1,p_2,\ldots)
       \Bigr|_{k+p_1+p_2+\ldots=0} = \\
     G^{\phi_1\phi_1\cdots} (p_1+k,p_2,\ldots)
       + (2\pi)^4 \delta^4(k+p_1) v G^{\phi_1\cdots} (p_2,\ldots)\Bigr|_{p_2+\ldots=0}\\
       - G^{\phi_2\phi_2\cdots} (p_1,p_2+k,\ldots) - \ldots 
   \end{multline}
   reveals that the $v$-term corresponds to disconnected diagrams and
   can be dropped.
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
   Now we bring the second, third and all following terms of r.h.s.
   to the left and then exchange l.h.s. and r.h.s. We multiply everything 
   with the prefactor of the one remaining term on the l.h.s.
   There's one subtlety: the right hand side of
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
   need not mean that the corresponding scattering amplitude vanishes.
   What is going on, is that the insertion of a soft current or the
   emission or absorption of a soft Goldstone boson contributes another
   pole for $k_\mu\to0$, if momentum conservation is taken into account.
   \begin{subequations}
   \begin{multline}
     \mathrm{F.T.} \Braket{0|\mathrm{T}\phi_1(x_1)\phi_1(x_2)\phi_1(x_3)|0} = \\
        (-6\mathrm{i}gv) \frac{\mathrm{i}}{(p_1+k)^2-2gv^2}
           \frac{\mathrm{i}}{p_2^2-2gv^2} \frac{\mathrm{i}}{p_3^2-2gv^2}
   \end{multline}
   \begin{align}
     \mathrm{F.T.} \Braket{0|\mathrm{T}\phi_2(x_1)\phi_2(x_2)\phi_1(x_3)|0}
        &= (-2\mathrm{i}gv) \frac{\mathrm{i}}{p_1^2}
           \frac{\mathrm{i}}{(p_2+k)^2} \frac{\mathrm{i}}{p_3^2-2gv^2} \\
     \mathrm{F.T.} \Braket{0|\mathrm{T}\phi_2(x_1)\phi_1(x_2)\phi_2(x_3)|0}
        &= (-2\mathrm{i}gv) \frac{\mathrm{i}}{p_1^2}
           \frac{\mathrm{i}}{p_2^2-2gv^2} \frac{\mathrm{i}}{(p_3+k)^2}
   \end{align}
   \end{subequations}
   \begin{multline}
     \mathrm{F.T.} \Braket{0|\mathrm{T}j_\mu(y)\phi_2(x_1)\phi_1(x_2)\phi_1(x_3)|0} = \\
     \frac{\mathrm{i}}{p_1^2}\frac{\mathrm{i}}{p_2^2-2gv^2}\frac{\mathrm{i}}{p_3^2-2gv^2}
        \left( \mathrm{F.T.} \Braket{0|\mathrm{T}j_\mu(y)\phi_2(x_1)
                                       \phi_1(x_2)\phi_1(x_3)|0}_{\text{amp.}} \right)
   \end{multline}
   \begin{multline}
     \mathrm{F.T.} \Braket{0|\mathrm{T}j_\mu(y)\phi_2(x_1)
                             \phi_1(x_2)\phi_1(x_3)|0}_{\text{amp.}} =
         v k_\mu \frac{\mathrm{i}}{k^2} (-2\mathrm{i}g) \\
      + (p_{1,\mu}-p_{2,\mu}-p_{3,\mu}) 
          \frac{\mathrm{i}}{(p_2+p_3)^2-2gv^2} (-6\mathrm{i}gv) \\
      + v k_\mu \frac{\mathrm{i}}{k^2}
          (-2\mathrm{i}gv) \frac{\mathrm{i}}{(p_2+p_3)^2-2gv^2} (-6\mathrm{i}gv) \\
      + (p_{1,\mu}+p_{2,\mu}-p_{3,\mu}) \frac{\mathrm{i}}{(p_1+p_2)^2} (-2\mathrm{i}gv)
      + v k_\mu \frac{\mathrm{i}}{k^2} (-2\mathrm{i}gv)
            \frac{\mathrm{i}}{(p_1+p_2)^2} (-2\mathrm{i}gv) \\
      + (p_{1,\mu}+p_{3,\mu}-p_{2,\mu}) \frac{\mathrm{i}}{(p_1+p_3)^2} (-2\mathrm{i}gv)
      + v k_\mu \frac{\mathrm{i}}{k^2} (-2\mathrm{i}gv)
            \frac{\mathrm{i}}{(p_1+p_3)^2} (-2\mathrm{i}gv)
   \end{multline}
   with $\partial_\mu\to-\mathrm{i}k_\mu=\mathrm{i}(p_1+p_2+p_3)_\mu$:
   \begin{multline}
     \mathrm{F.T.} \partial_y^\mu\Braket{0|\mathrm{T}j_\mu(y)\phi_2(x_1)
                             \phi_1(x_2)\phi_1(x_3)|0}_{\text{amp.}} = \\
         -2\mathrm{i}gv
      -  \frac{p_1^2-(p_2+p_3)^2}{(p_2+p_3)^2-2gv^2} 6\mathrm{i}gv 
      - \frac{2gv^2}{(p_2+p_3)^2-2gv^2} 6\mathrm{i}gv \\
      - \frac{(p_1+p_2)^2-p_3^2}{(p_1+p_2)^2} 2\mathrm{i}gv
      - \frac{2gv^2}{(p_1+p_2)^2} 2\mathrm{i}gv
      - \frac{(p_1+p_3)^2-p_2^2}{(p_1+p_3)^2} 2\mathrm{i}gv
      - \frac{2gv^2}{(p_1+p_3)^2} 2\mathrm{i}gv \\
      = \mbox{} -2\mathrm{i}gv
      - \frac{p_1^2-(p_2+p_3)^2+2gv^2}{(p_2+p_3)^2-2gv^2} 6\mathrm{i}gv  \\
      - \frac{(p_1+p_2)^2-p_3^2+2gv^2}{(p_1+p_2)^2} 2\mathrm{i}gv
      - \frac{(p_1+p_3)^2-p_2^2+2gv^2}{(p_1+p_3)^2} 2\mathrm{i}gv \\
      = \mbox{}
      - \frac{p_1^2}{(p_2+p_3)^2-2gv^2} 6\mathrm{i}gv
      + \frac{p_3^2-2gv^2}{(p_1+p_2)^2} 2\mathrm{i}gv
      + \frac{p_2^2-2gv^2}{(p_1+p_3)^2} 2\mathrm{i}gv
   \end{multline}
   If the symmetry is unbroken, the 
   propagators cancel $G^{\phi_1\phi_1}(p_1)=G^{\phi_2\phi_2}(p_1)$:
   \begin{multline}
     \lim_{k_\mu\to0}
         k^{\mu}G_{\mu}^{j|\phi_2\phi_1\cdots,\text{amp.}} (k|p_1,p_2,\ldots)
        \Bigr|_{k+p_1+p_2+\ldots=0} = \\
         G^{\phi_1\phi_1\cdots,\text{amp.}} (p_1,p_2,\ldots)
           \Bigr|_{p_1+p_2+\ldots=0} 
       - G^{\phi_2\phi_2\cdots,\text{amp.}} (p_1,p_2,\ldots)
           \Bigr|_{p_1+p_2+\ldots=0} - \ldots 
   \end{multline} *)

(* Caveat: the Ward identities for on-shell amplitudes do \emph{not} test
   the theory comprehensively, since only the coupling of Goldstone bosons
   and and currents to external lines. *)

module Main = Omega.Make(Fusion.Mixed23)(Targets.Fortran)(O2)
let _ = Main.main ()

(*i
 *  Local Variables:
 *  mode:caml
 *  indent-tabs-mode:nil
 *  page-delimiter:"^(\\* .*\n"
 *  End:
i*)

