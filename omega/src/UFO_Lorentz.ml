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
   [A.dirac list * A.vector list], without preserving the
   order (currently, the order is reversed). *)
let split_atoms atoms =
  List.fold_left
    (fun (d, v) -> function
      | A.Vector v' -> (d, v' :: v)
      | A.Dirac d' -> (d' :: d, v))
    ([], []) atoms

(* Just like [UFOx.Lorentz_Atom.dirac], but without the Dirac matrix indices. *)
type dirac =
  | Gamma5
  | ProjM
  | ProjP
  | Gamma of int
  | Sigma of int * int
  | C

let map_indices_gamma f = function
  | (Gamma5 | ProjM | ProjP | C as g) -> g
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
    vector : A.vector term list }

let fermion_lines_of_contraction contraction =
  List.sort
    compare
    (List.map (fun term -> (term.atom.ket, term.atom.bra)) contraction.dirac)

let map_indices_contraction f c =
  { coeff = c.coeff;
    dirac = List.map (map_term f (map_indices_dirac f)) c.dirac;
    vector = List.map (map_term f (A.map_indices_vector f)) c.vector }

type t = contraction list

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
  | Gamma5 | ProjM | ProjP | C -> []
  | Gamma (mu) -> [mu]
  | Sigma (mu, nu) -> [mu; nu]

let indices_of_dirac_string ds =
  ThoList.flatmap indices_of_dirac ds.gammas
                      
let classify_dirac atom =
  { indices = indices_of_dirac_string atom;
    atom }

let contraction_of_lorentz_atoms (atoms, coeff) =
  let dirac_atoms, vector_atoms = split_atoms atoms in
  let dirac =
    List.map classify_dirac (dirac_strings_of_dirac_atoms dirac_atoms)
  and vector =
    List.map classify_vector vector_atoms in
  { coeff; dirac; vector }

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
  | (Gamma5 | ProjM | ProjP | C) as g -> g
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
         vector = vector' }
  | Some (Replace (mu, nu)), vector' ->
     compress_metrics
       { coeff = c.coeff;
         dirac = substitute_index_dirac mu nu c.dirac;
         vector = substitute_index_vector mu nu vector' }

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

let dirac_string_to_string ds =
  match ds.gammas with
  | [] -> Printf.sprintf "<%d|%d>" ds.bra ds.ket
  | gammas ->
     Printf.sprintf
       "<%d|%s|%d>"
       ds.bra (String.concat "*" (List.map dirac_to_string gammas)) ds.ket

let contraction_to_string c =
  QC.to_string c.coeff ^ " * " ^
    String.concat
      " * " (List.map (fun ds -> dirac_string_to_string ds.atom) c.dirac) ^
      " * " ^
        String.concat
          " * " (List.map (fun v -> vector_to_string v.atom) c.vector)

let fermion_lines_to_string fermion_lines =
  ThoList.to_string
    (fun (bra, ket) -> Printf.sprintf "%d->%d" bra ket)
    fermion_lines

let to_string contractions =
  String.concat " + " (List.map contraction_to_string contractions)
