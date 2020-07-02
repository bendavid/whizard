(* UFO_Lorentz.ml --

   Copyright (C) 1999-2017 by

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

(* \thocwmodulesection{Processed UFO Lorentz Structures} *)

module Q = Algebra.Q
module QC = Algebra.QC
module A = UFOx.Lorentz_Atom
module D = Dirac.Chiral

(* Take a [A.t list] and return the corresponding pair
   [A.dirac list * A.vector list * A.scalar list * A.scalar list],
   without preserving the order (currently, the order is reversed). *)
let split_atoms atoms =
  List.fold_left
    (fun (d, v, s, i) -> function
      | A.Vector v' -> (d, v' :: v, s, i)
      | A.Dirac d' -> (d' :: d, v, s, i)
      | A.Scalar s' -> (d, v, s' :: s, i)
      | A.Inverse i' -> (d, v, s, i' :: i))
    ([], [], [], []) atoms

(* Just like [UFOx.Lorentz_Atom.dirac], but without the Dirac matrix indices. *)
type dirac =
  | Gamma5
  | ProjM
  | ProjP
  | Gamma of int
  | Sigma of int * int
  | C
  | Minus

let map_indices_gamma f = function
  | (Gamma5 | ProjM | ProjP | C | Minus as g) -> g
  | Gamma mu -> Gamma (f mu)
  | Sigma (mu, nu) -> Sigma (f mu, f nu)

(* A sandwich of a string of $\gamma$-matrices. [bra] and [ket] are
   positions of fields in the vertex. *)
type dirac_string =
  { bra : int;
    ket : int;
    gammas : dirac list }

let map_indices_dirac f d =
  { bra = f d.bra;
    ket = f d.ket;
    gammas = List.map (map_indices_gamma f) d.gammas }

(*
   Implementation of Dirac couplings using
   \texttt{conjspinor\_spinor}
   \begin{equation}
       \text{\texttt{psibar0 * psi1}}
     = \sum_\alpha \bar\psi_{0,\alpha} \psi_{1,\alpha}
     = \bar\psi_0\psi_1
   \end{equation}
   JRR's implementation of Majorana couplings using
   \texttt{spinor\_product}
   \begin{equation}
       \text{\texttt{chibar0 * chi1}}
     = \sum_{\alpha} \bar\chi_{0,\alpha} (C^T\chi_1)_\beta
     = \sum_{\alpha} (C\bar\chi_0^T)_\alpha \chi_{1,\alpha}
     = (C\bar\chi_0^T)^T \chi_1
     = \tilde\chi_0^T\chi_1
   \end{equation}
   with charge conjugation\footnote{%
     In detail, to make sure we understand all phases
     \begin{multline}
         \bar{\tilde\chi}
       = \tilde\chi^\dagger\gamma_0
       = \left(C\bar\chi^T\right)^\dagger\gamma_0
       = \left(C(\chi^\dagger\gamma_0)^T\right)^\dagger\gamma_0
       = \left(C\gamma_0^T{\chi^\dagger}^T\right)^\dagger\gamma_0
       = \left(C\gamma_0^T{\chi^T}^\dagger\right)^\dagger\gamma_0
       = {\chi^T} {\gamma_0^T}^\dagger C^\dagger\gamma_0 \\
       = {\chi^T} {\gamma_0^\dagger}^T C^{-1}\gamma_0
       = {\chi^T} {\gamma_0}^T C^{-1}\gamma_0
       = {\chi^T} C^{-1} C {\gamma_0}^T C^{-1}\gamma_0
       = - {\chi^T} C^{-1} \gamma_0 \gamma_0
       = - {\chi^T} C^{-1}\,.
     \end{multline}}
   \begin{subequations}
   \begin{align}
     \tilde\chi &= C\bar\chi^T \\
     \bar{\tilde\chi} &= -\chi^T C^{-1} \,.
   \end{align}
   \end{subequations}
   So we write in JRR's implementation
   \begin{equation}
     \bar\chi_0 \Gamma \chi_1\phi
       = \bar\chi_0 C^T C\Gamma \chi_1\phi
       = (C\bar\chi_0^T)^T C\Gamma \chi_1\phi
       = \tilde\chi_0^T C\Gamma \chi_1\phi
   \end{equation}
   using~$C^{-1}=C^\dagger$, $C^T=-C$ and the representation
   dependent~$C^2=-1$ that holds in all our representation(s).
   Analoguously
   \begin{multline}
     \bar\chi_0 \Gamma \chi_1\phi
       = \left(\bar\chi_0 \Gamma \chi_1\right)^T \phi
       = - \chi_1^T \Gamma^T \bar\chi_0^T \phi
       = \bar{\tilde\chi}_1 C \Gamma^T C^{-1}\tilde\chi_0 \phi
       = - \chi_1^T C^{-1} C \Gamma^T C^{-1}\tilde\chi_0 \phi \\
       = - \chi_1^T \Gamma^T C^{-1}\tilde\chi_0 \phi
       = - \chi_1^T \Gamma^T C^T \tilde\chi_0 \phi
       = - \chi_1^T (C\Gamma)^T \tilde\chi_0 \phi
   \end{multline}
 *)

(* In the following, we assume to be in a realization
   with~$C^{-1}=-C=C^T$: *)
let inv_C = [Minus; C]

(* In JRR's implementation of Majorana fermions
   (see~\pageref{pg:JRR-Fusions}),
   \emph{all} fermion-boson fusions are realized with the
   \texttt{f\_}$b$\texttt{f(g,phi,chi)} functions, where
   $b\in\{\text{\texttt{v}},\text{\texttt{a}},\ldots\}$.
   This is different from the original Dirac implementation, where
   \emph{both} \texttt{f\_}$b$\texttt{f(g,phi,psi)}
   and \texttt{f\_f}$b$\texttt{(g,psibar,phi)} are used. *)

(* However, the latter plays nicer with the permutations in the UFO
   version of [fuse].  Therefore, we want to automatically map
   \texttt{f\_}$b$\texttt{f(g,phi,chi)} to
   \texttt{f\_f}$b$\texttt{(g,chi,phi)} by an appropriate
   transformation of the $\gamma$-matrices involved. *)

(* Starting from
   \begin{equation}
     \text{\texttt{f\_}$b$\texttt{f(g,phi,chi)}}
        \cong
     \chi'_\alpha =
       \sum_{\mu,\beta} \phi_\mu \Gamma^\mu_{\alpha\beta}\chi_\beta
   \end{equation}
   with~$\Gamma$ an appropriate product of $\gamma$-matrices, we obtain
   \begin{equation}
     \text{\texttt{f\_f}$b$\texttt{(g,chi,phi)}}
        \cong
     \chi'_\alpha
       = \sum_{\mu,\beta} \phi_\mu \chi_\beta\tilde\Gamma^\mu_{\beta\alpha}
       = \sum_{\mu,\beta} \phi_\mu
           \left(\tilde\Gamma^\mu\right)^T_{\alpha\beta} \chi_\beta
   \end{equation}
   and we should require $\tilde\Gamma=\Gamma^T$. *)

(* We can now use the standard charge conjugation matrix relations
   \begin{subequations}
     \begin{align}
       \mathbf{1}^T           &= \mathbf{1} \\
       \gamma_\mu^T           &= - C\gamma_\mu C^{-1} \\
       \sigma_{\mu\nu}^T      &=   C\sigma_{\nu\mu} C^{-1}
                               = - C\sigma_{\mu\nu} C^{-1} \\
       (\gamma_5\gamma_\mu)^T &= \gamma_\mu^T \gamma_5^T
                               = - C\gamma_\mu\gamma_5 C^{-1}
                               = C\gamma_5\gamma_\mu C^{-1} \\
       \gamma_5^T             &= C\gamma_5 C^{-1}
     \end{align}
   \end{subequations}
   to perform the transpositions symbolically.
   For the chiral projectors
   \begin{equation}
     \gamma_\pm = \mathbf{1}\pm\gamma_5
   \end{equation}
   this means\footnote{The final two equations are two different ways
   to obtain the same result, of course.}

   \begin{subequations}
     \begin{align}
       \gamma_\pm^T
         &= (\mathbf{1}\pm\gamma_5)^T
          = C(\mathbf{1}\pm\gamma_5) C^{-1} = C\gamma_\pm C^{-1} \\
       (\gamma_\mu\gamma_\pm)^T
            &= \gamma_\pm^T \gamma_\mu^T
             = - C\gamma_\pm \gamma_\mu C^{-1}
             = - C\gamma_\mu\gamma_\mp C^{-1} \\
       (\gamma_\mu\pm\gamma_\mu\gamma_5)^T
            &= - C(\gamma_\mu\mp\gamma_\mu\gamma_5) C^{-1}
     \end{align}
   \end{subequations}
   and of course
   \begin{equation}
      C^T = - C\,.
   \end{equation} *)

let transpose1 = function
  | (Gamma5 | ProjM | ProjP as g) -> [C; g] @ inv_C
  | (Gamma _ | Sigma (_, _) as g) -> [Minus] @ [C; g] @ inv_C
  | C -> [Minus; C]
  | Minus -> [Minus]

let rec collect_signs_rev (negative, acc) = function
  | [] -> (negative, acc)
  | Minus :: g_list -> collect_signs_rev (not negative, acc) g_list
  | g :: g_list -> collect_signs_rev (negative, g :: acc) g_list

let rec compress_ccs_rev (negative, acc) = function
  | [] -> (negative, acc)
  | C :: C :: g_list -> compress_ccs_rev (not negative, acc) g_list
  | g :: g_list -> compress_ccs_rev (negative, g :: acc) g_list

let compress_signs g_list =
  let negative, g_list_rev = collect_signs_rev (false, []) g_list in
  match compress_ccs_rev (negative, []) g_list_rev with
  | true, g_list -> Minus :: g_list
  | false, g_list -> g_list

let transpose d =
  { d with
    gammas = compress_signs (ThoList.rev_flatmap transpose1 d.gammas) }

(* Regarding the tests in \texttt{keystones\_UFO\_bispinors}, we observe%
   \footnote{In components:
   \begin{subequations}
   \begin{align}
     \text{\texttt{chi0 * f\_}$b$\texttt{f(g,phi1,chi2)}}
        &\cong
     \sum_{\mu,\alpha,\alpha',\beta} \phi_{1,\mu}
        C_{\alpha\alpha'}\chi_{0,\alpha'} \Gamma^\mu_{\alpha\beta}\chi_{2,\beta}
     = \sum_{\mu,\alpha,\alpha',\beta} \phi_{1,\mu}
        \chi_{0,\alpha'} C^T_{\alpha'\alpha}
             \Gamma^\mu_{\alpha\beta}\chi_{2,\beta} \\
     \text{\texttt{f\_f}$b$\texttt{(g,chi0,phi1) * chi2}}
        &\cong
      \sum_{\mu,\alpha,\alpha',\beta} \phi_{1,\mu}
        C_{\alpha\alpha'}\chi_{0,\beta}
            \tilde\Gamma^\mu_{\beta\alpha'} \chi_{2,\alpha}
     =\sum_{\mu,\alpha,\alpha',\beta} \phi_{1,\mu}
         \chi_{0,\beta} \tilde\Gamma^\mu_{\beta\alpha'}
        C^T_{\alpha'\alpha}\chi_{2,\alpha} \\
     \text{\texttt{chi2 * f\_f}$b$\texttt{(g,chi0,phi1)}}
        &\cong
      \sum_{\mu,\alpha,\alpha',\beta} \phi_{1,\mu}
        C_{\alpha\alpha'} \chi_{2,\alpha'} \chi_{0,\beta}
            \tilde\Gamma^\mu_{\beta\alpha}
     =\sum_{\mu,\alpha,\alpha',\beta} \phi_{1,\mu}
        \chi_{0,\beta}
          \tilde\Gamma^\mu_{\beta\alpha} C_{\alpha\alpha'} \chi_{2,\alpha'}
   \end{align}
   \end{subequations}}
   \begin{subequations}
   \begin{align}
     \text{\texttt{chi0 * f\_}$b$\texttt{f(g,phi1,chi2)}}
       &\cong
     \phi_1^\mu (C\chi_0)^T \Gamma_\mu \chi_2
     = \phi_1^\mu \chi_0^T C^T\Gamma_\mu\chi_2 \\
     \text{\texttt{f\_f}$b$\texttt{(g,chi0,phi1) * chi2}}
       &\cong
       \phi_1^\mu (C(\chi_0^T\tilde\Gamma_\mu)^T)^T \chi_2
     = \phi_1 (C\tilde\Gamma_\mu^T\chi_0)^T \chi_2
     = \phi_1 \chi_0^T \tilde\Gamma_\mu C^T \chi_2 \\
     \text{\texttt{chi2 * f\_f}$b$\texttt{(g,chi0,phi1)}}
       &\cong
      \phi_1^\mu (C\chi_2)^T (\chi_0^T\tilde\Gamma_\mu)^T
     =\phi_1^\mu  \chi_0^T\tilde\Gamma_\mu C\chi_2
   \end{align}
   \end{subequations}
   The natural
   condition~$\text{\texttt{chi0 * f\_}$b$\texttt{f(g,phi1,chi2)}}
    = \text{\texttt{f\_f}$b$\texttt{(g,chi0,phi1) * chi2}}$
   can be satisfied with
   \begin{equation}
      C^T \Gamma_\mu = \tilde\Gamma_\mu C^T\,,
   \end{equation}
   i.\,e.
   \begin{equation}
      \tilde\Gamma_\mu = C \Gamma_\mu C^{-1}\,.
   \end{equation}
   This is \emph{not} compatible with with~$\tilde\Gamma_\mu=\Gamma^T_\mu$,
   but we can make this work for the \texttt{keystones\_UFO\_bispinors}
   tests with *)

let conjugate d =
  { d with gammas = compress_signs (C :: d.gammas @ inv_C) }

let conjugate_transpose d =
  conjugate (transpose d)

(* The alternative
   condition~$\text{\texttt{chi0 * f\_}$b$\texttt{f(g,phi1,chi2)}}
    = \text{\texttt{chi2 * f\_f}$b$\texttt{(g,chi0,phi1)}}$
   would require
   \begin{equation}
      C^T \Gamma_\mu = \tilde\Gamma_\mu C\,,
   \end{equation}
   i.\,e.
   \begin{equation}
      \tilde\Gamma_\mu = C \Gamma_\mu C = - C \Gamma_\mu C^{-1}\,.
   \end{equation}  *)

(* \begin{dubious}
     Now make also the fusions work in
     \texttt{fermi\_UFO} and \texttt{compare\_majorana\_UFO}?
   \end{dubious} *)

let minus d =
  { d with gammas = compress_signs (Minus :: d.gammas) }

let cc_times d =
  { d with gammas = compress_signs (C :: d.gammas) }

let times_cc_inv d =
  { d with gammas = compress_signs (d.gammas @ inv_C) }

(* [dirac_string bind ds] applies the mapping [bind] to the indices
   of $\gamma_\mu$ and~$\sigma_{\mu\nu}$ and multiplies the resulting
   matrices in order using complex rational arithmetic. *)
module type To_Matrix =
  sig
    val dirac_string : (int -> int) -> dirac_string -> D.t
  end

module To_Matrix : To_Matrix =
  struct

    let half = QC.make (Q.make 1 2) Q.null
    let half_i = QC.make Q.null (Q.make 1 2)

    let gamma_L = D.times half (D.sub D.unit D.gamma5)
    let gamma_R = D.times half (D.add D.unit D.gamma5)

    let sigma = Array.make_matrix 4 4 D.null
    let () =
      for mu = 0 to 3 do
        for nu = 0 to 3 do
          sigma.(mu).(nu) <-
            D.times
              half_i
              (D.sub
                 (D.mul D.gamma.(mu) D.gamma.(nu))
                 (D.mul D.gamma.(nu) D.gamma.(mu)))
        done
      done

    let dirac bind_indices = function
      | Gamma5 -> D.gamma5
      | ProjM -> gamma_L
      | ProjP -> gamma_R
      | Gamma (mu) -> D.gamma.(bind_indices mu)
      | Sigma (mu, nu) -> sigma.(bind_indices mu).(bind_indices nu)
      | C -> D.cc
      | Minus -> D.neg D.unit

    let dirac_string bind_indices ds =
      D.product (List.map (dirac bind_indices) ds.gammas)

  end
        
let dirac_string_to_matrix = To_Matrix.dirac_string

(* The Lorentz indices appearing in a term are either negative
   internal summation indices or positive external polarization
   indices.  Note that the external
   indices are not really indices, but denote the position
   of the particle in the vertex. *)
type 'a term =
  { indices : int list;
    atom : 'a }

let map_atom f term =
  { term with atom = f term.atom }

let map_term f_index f_atom term =
  { indices = List.map f_index term.indices;
    atom = f_atom term.atom }

(* Return a pair of lists: first the (negative) summation indices,
   second the (positive) external indices. *)
let classify_indices ilist =
  List.partition
    (fun i ->
      if i < 0 then
        true
      else if i > 0 then
        false
      else
        invalid_arg "classify_indices")
    ilist

type contraction =
  { coeff : QC.t;
    dirac : dirac_string term list;
    vector : A.vector term list;
    scalar : A.scalar list;
    inverse : A.scalar list }

let fermion_lines_of_contraction contraction =
  List.sort
    compare
    (List.map (fun term -> (term.atom.ket, term.atom.bra)) contraction.dirac)

let map_indices_contraction f c =
  { coeff = c.coeff;
    dirac = List.map (map_term f (map_indices_dirac f)) c.dirac;
    vector = List.map (map_term f (A.map_indices_vector f)) c.vector;
    scalar = c.scalar;
    inverse = c.inverse }

type t = contraction list

let rec charge_conjugate_dirac (bra, ket as fermion_line) = function
  | [] -> []
  | dirac :: dirac_list  ->
     if dirac.atom.bra = bra && dirac.atom.ket = ket then
       map_atom conjugate dirac :: dirac_list
     else
       dirac :: charge_conjugate_dirac fermion_line dirac_list

let charge_conjugate_contraction fermion_line c =
  { c with dirac = charge_conjugate_dirac fermion_line c.dirac }

let charge_conjugate fermion_line l =
  List.map (charge_conjugate_contraction fermion_line) l

let fermion_lines contractions =
  let pairs = List.map fermion_lines_of_contraction contractions in
  match ThoList.uniq (List.sort compare pairs) with
  | [] -> invalid_arg "UFO_Lorentz.fermion_lines: impossible"
  | [pairs] -> pairs
  | _ -> invalid_arg "UFO_Lorentz.fermion_lines: ambiguous"

let map_indices f contractions =
  List.map (map_indices_contraction f) contractions

let map_fermion_lines f pairs =
  List.map (fun (i, j) -> (f i, f j)) pairs

let dirac_of_atom = function
  | A.Identity (_, _) -> []
  | A.C (_, _) -> [C]
  | A.Gamma5 (_, _) -> [Gamma5]
  | A.ProjP (_, _) -> [ProjP]
  | A.ProjM (_, _) -> [ProjM]
  | A.Gamma (mu, _, _) -> [Gamma mu]
  | A.Sigma (mu, nu, _, _) -> [Sigma (mu, nu)]

let dirac_indices = function
  | A.Identity (i, j) | A.C (i, j)
  | A.Gamma5 (i, j) | A.ProjP (i, j) | A.ProjM (i, j)
  | A.Gamma (_, i, j) | A.Sigma (_, _, i, j) -> (i, j)

let rec scan_for_dirac_string stack = function

  | [] ->
     (* We're done with this pass.  There must be
        no leftover atoms on the [stack] of spinor atoms,
        but we'll check this in the calling function. *)
     (None, List.rev stack)

  | atom :: atoms ->
     let i, j = dirac_indices atom in
     if i > 0 then
       if j > 0 then
         (* That's an atomic Dirac string.  Collect
            all atoms for further processing.  *)
         (Some { bra = i; ket = j; gammas = dirac_of_atom atom},
          List.rev_append stack atoms)
       else
         (* That's the start of a new Dirac string.  Search
            for the remaining elements, not forgetting matrices
            that we might pushed on the [stack] earlier. *)
         collect_dirac_string
           i j (dirac_of_atom atom) [] (List.rev_append stack atoms)
     else
       (* The interior of a Dirac string.  Push it on the
          stack until we find the start.  *)
       scan_for_dirac_string (atom :: stack) atoms

(* Complete the string starting with [i] and the current summation
   index [j]. *)
and collect_dirac_string i j rev_ds stack = function

  | [] ->
     (* We have consumed all atoms without finding
        the end of the string. *)
     invalid_arg "collect_dirac_string: open string"

  | atom :: atoms ->
     let i', j' = dirac_indices atom in
     if i' = j then
       if j' > 0 then
         (* Found the conclusion.  Collect
            all atoms on the [stack] for further processing.  *)
         (Some { bra = i; ket = j';
                 gammas = List.rev_append rev_ds (dirac_of_atom atom)},
          List.rev_append stack atoms)
       else
         (* Found the continuation.  Pop the stack of open indices,
            since we're looking for a new one. *)
         collect_dirac_string
           i j' (dirac_of_atom atom @ rev_ds) [] (List.rev_append stack atoms)
     else
       (* Either the start of another Dirac string or a
          non-matching continuation.  Push it on the
          stack until we're done with the current one. *)
       collect_dirac_string i j rev_ds (atom :: stack) atoms

let dirac_string_of_dirac_atoms atoms =
  scan_for_dirac_string [] atoms

let rec dirac_strings_of_dirac_atoms' rev_ds atoms =
  match dirac_string_of_dirac_atoms atoms with
  | (None, []) -> List.rev rev_ds
  | (None, _) -> invalid_arg "dirac_string_of_dirac_atoms: leftover atoms"
  | (Some ds, atoms) -> dirac_strings_of_dirac_atoms' (ds :: rev_ds) atoms

let dirac_strings_of_dirac_atoms atoms =
  dirac_strings_of_dirac_atoms' [] atoms

let indices_of_vector = function
  | A.Epsilon (mu1, mu2, mu3, mu4) -> [mu1; mu2; mu3; mu4]
  | A.Metric (mu1, mu2) -> [mu1; mu2]
  | A.P (mu, n) ->
     if n > 0 then
       [mu]
     else
       invalid_arg "indices_of_vector: invalid momentum"

let classify_vector atom =
  { indices = indices_of_vector atom;
    atom }

let indices_of_dirac = function
  | Gamma5 | ProjM | ProjP | C | Minus -> []
  | Gamma (mu) -> [mu]
  | Sigma (mu, nu) -> [mu; nu]

let indices_of_dirac_string ds =
  ThoList.flatmap indices_of_dirac ds.gammas
                      
let classify_dirac atom =
  { indices = indices_of_dirac_string atom;
    atom }

let contraction_of_lorentz_atoms (atoms, coeff) =
  let dirac_atoms, vector_atoms, scalar, inverse = split_atoms atoms in
  let dirac =
    List.map classify_dirac (dirac_strings_of_dirac_atoms dirac_atoms)
  and vector =
    List.map classify_vector vector_atoms in
  { coeff; dirac; vector; scalar; inverse }

type redundancy =
  | Trace of int
  | Replace of int * int

let rec redundant_metric' rev_atoms = function
  | [] -> (None, List.rev rev_atoms)
  | { atom = A.Metric (mu, nu) } as atom :: atoms ->
     if mu < 1 then
       if nu = mu then
         (Some (Trace mu), List.rev_append rev_atoms atoms)
       else
         (Some (Replace (mu, nu)), List.rev_append rev_atoms atoms)
     else if nu < 0 then
       (Some (Replace (nu, mu)), List.rev_append rev_atoms atoms)
     else
       redundant_metric' (atom :: rev_atoms) atoms
  | { atom = (A.Epsilon (_, _, _, _ ) | A.P (_, _) ) } as atom :: atoms ->
     redundant_metric' (atom :: rev_atoms) atoms

let redundant_metric atoms =
  redundant_metric' [] atoms
                        
(* Substitude any occurance of the index [mu] by the index [nu]: *)
let substitute_index_vector1 mu nu = function
  | A.Epsilon (mu1, mu2, mu3, mu4) as eps ->
     if mu = mu1 then
       A.Epsilon (nu, mu2, mu3, mu4)
     else if mu = mu2 then
       A.Epsilon (mu1, nu, mu3, mu4)
     else if mu = mu3 then
       A.Epsilon (mu1, mu2, nu, mu4)
     else if mu = mu4 then
       A.Epsilon (mu1, mu2, mu3, nu)
     else
       eps
  | A.Metric (mu1, mu2) as g ->
     if mu = mu1 then
       A.Metric (nu, mu2)
     else if mu = mu2 then
       A.Metric (mu1, nu)
     else
       g
  | A.P (mu1, n) as p ->
     if mu = mu1 then
       A.P (nu, n)
     else
       p

let remove a alist =
  List.filter ((<>) a) alist

let substitute_index1 mu nu mu1 =
  if mu = mu1 then
    nu
  else
    mu1

let substitute_index mu nu indices =
  List.map (substitute_index1 mu nu) indices

(* This assumes that [mu] is a summation index and
   [nu] is a polarization index. *)
let substitute_index_vector mu nu vectors =
  List.map
    (fun v ->
      { indices = substitute_index mu nu v.indices;
        atom = substitute_index_vector1 mu nu v.atom })
    vectors

(* Substitude any occurance of the index [mu] by the index [nu]: *)
let substitute_index_dirac1 mu nu = function
  | (Gamma5 | ProjM | ProjP | C | Minus) as g -> g
  | Gamma (mu1) as g ->
     if mu = mu1 then
       Gamma (nu)
     else
       g
  | Sigma (mu1, mu2) as g ->
     if mu = mu1 then
       Sigma (nu, mu2)
     else if mu = mu2 then
       Sigma (mu1, nu)
     else
       g

(* This assumes that [mu] is a summation index and
   [nu] is a polarization index. *)
let substitute_index_dirac mu nu dirac_strings =
  List.map
    (fun ds ->
      { indices = substitute_index mu nu ds.indices;
        atom = { ds.atom with
                 gammas =
                   List.map
                     (substitute_index_dirac1 mu nu)
                     ds.atom.gammas } } )
    dirac_strings

let trace_metric = QC.make (Q.make 4 1) Q.null

(* FIXME: can this be made typesafe by mapping to a
   type that \emph{only} contains [P] and [Epsilon]? *)
let rec compress_metrics c =
  match redundant_metric c.vector with
  | None, _ -> c
  | Some (Trace mu), vector' ->
     compress_metrics
       { coeff = QC.mul trace_metric c.coeff;
         dirac = c.dirac;
         vector = vector';
         scalar = c.scalar;
         inverse = c.inverse }
  | Some (Replace (mu, nu)), vector' ->
     compress_metrics
       { coeff = c.coeff;
         dirac = substitute_index_dirac mu nu c.dirac;
         vector = substitute_index_vector mu nu vector';
         scalar = c.scalar;
         inverse = c.inverse }

let dummy =
  []

let parse1 spins atom =
  compress_metrics (contraction_of_lorentz_atoms atom)

let parse spins l =
  List.map (parse1 spins) l

let i2s = UFOx.Index.to_string

let vector_to_string = function
  | A.Epsilon (mu, nu, ka, la) ->
     Printf.sprintf "Epsilon(%s,%s,%s,%s)" (i2s mu) (i2s nu) (i2s ka) (i2s la)
  | A.Metric (mu, nu) ->
     Printf.sprintf "Metric(%s,%s)" (i2s mu) (i2s nu)
  | A.P (mu, n) ->
     Printf.sprintf "P(%s,%d)" (i2s mu) n

let dirac_to_string = function
  | Gamma5 -> "g5"
  | ProjM -> "(1-g5)/2"
  | ProjP -> "(1+g5)/2"
  | Gamma (mu) -> Printf.sprintf "g(%s)" (i2s mu)
  | Sigma (mu, nu) ->  Printf.sprintf "s(%s,%s)" (i2s mu) (i2s nu)
  | C -> "C"
  | Minus -> "-1"

let dirac_string_to_string ds =
  match ds.gammas with
  | [] -> Printf.sprintf "<%s|%s>" (i2s ds.bra) (i2s ds.ket)
  | gammas ->
     Printf.sprintf
       "<%s|%s|%s>"
       (i2s ds.bra)
       (String.concat "*" (List.map dirac_to_string gammas))
       (i2s ds.ket)

let scalar_to_string = function
  | A.Mass _ -> "m"
  | A.Width _ -> "w"

let contraction_to_string c =
  String.concat
    " * "
    (List.concat
       [if QC.is_unit c.coeff then
          []
        else
          [QC.to_string c.coeff];
        List.map (fun ds -> dirac_string_to_string ds.atom) c.dirac;
        List.map (fun v -> vector_to_string v.atom) c.vector;
        List.map scalar_to_string c.scalar]) ^
    (match c.inverse with
     | [] -> ""
     | inverse ->
        " / (" ^ String.concat "*" (List.map scalar_to_string inverse) ^ ")")

let fermion_lines_to_string fermion_lines =
  ThoList.to_string
    (fun (bra, ket) -> Printf.sprintf "%s->%s" (i2s bra) (i2s ket))
    fermion_lines

let to_string contractions =
  String.concat " + " (List.map contraction_to_string contractions)

module type Test =
  sig
    val suite : OUnit.test
  end

module Test : Test =
  struct

    open OUnit

    let braket gammas =
      { bra = 11; ket = 22; gammas }

    let assert_transpose gt g =
      assert_equal ~printer:dirac_string_to_string
        (braket gt) (transpose (braket g))

    let assert_conjugate_transpose gct g =
      assert_equal ~printer:dirac_string_to_string
        (braket gct) (conjugate_transpose (braket g))

    let suite_transpose =
      "transpose" >:::

        [ "identity" >::
            (fun () ->
              assert_transpose [] []);

          "gamma_mu" >::
            (fun () ->
              assert_transpose [C; Gamma 1; C] [Gamma 1]);

          "sigma_munu" >::
            (fun () ->
              assert_transpose [C; Sigma (1, 2); C] [Sigma (1, 2)]);

          "gamma_5*gamma_mu" >::
            (fun () ->
              assert_transpose
                [C; Gamma 1; Gamma5; C]
                [Gamma5; Gamma 1]);

          "gamma5" >::
            (fun () ->
              assert_transpose [Minus; C; Gamma5; C] [Gamma5]);

          "gamma+" >::
            (fun () ->
              assert_transpose [Minus; C; ProjP; C] [ProjP]);

          "gamma-" >::
            (fun () ->
              assert_transpose [Minus; C; ProjM; C] [ProjM]);

          "gamma_mu*gamma_nu" >::
            (fun () ->
              assert_transpose
                [Minus; C; Gamma 2; Gamma 1; C]
                [Gamma 1; Gamma 2]);

          "gamma_mu*gamma_nu*gamma_la" >::
            (fun () ->
              assert_transpose
                [C; Gamma 3; Gamma 2; Gamma 1; C]
                [Gamma 1; Gamma 2; Gamma 3]);

          "gamma_mu*gamma+" >::
            (fun () ->
              assert_transpose
                [C; ProjP; Gamma 1; C]
                [Gamma 1; ProjP]);

          "gamma_mu*gamma-" >::
            (fun () ->
              assert_transpose
                [C; ProjM; Gamma 1; C]
                [Gamma 1; ProjM]) ]

    let suite_conjugate_transpose =
      "conjugate_transpose" >:::

        [ "identity" >::
            (fun () ->
              assert_conjugate_transpose [] []);

          "gamma_mu" >::
            (fun () ->
              assert_conjugate_transpose [Minus; Gamma 1] [Gamma 1]);

          "sigma_munu" >::
            (fun () ->
              assert_conjugate_transpose [Minus; Sigma (1, 2)] [Sigma (1,2)]);

          "gamma_mu*gamma5" >::
            (fun () ->
              assert_conjugate_transpose
                [Minus; Gamma5; Gamma 1] [Gamma 1; Gamma5]);

          "gamma5" >::
            (fun () ->
              assert_conjugate_transpose [Gamma5] [Gamma5]) ]

    let suite =
      "UFO_Lorentz" >:::
        [suite_transpose;
         suite_conjugate_transpose]

  end
