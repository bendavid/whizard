(* uFO_targets.ml --

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

let (@@) f g x =
  f (g x)

(* \thocwmodulesection{Dirac $\gamma$-matrices} *)

module type Dirac =
  sig

    (* Matrices with complex rational entries. *)
    type qc = Algebra.QC.t
    type t = qc array array

    (* Complex rational constants. *)
    val zero : qc
    val one : qc
    val minus_one : qc
    val i : qc
    val minus_i : qc

    (* Basic $\gamma$-matrices. *)
    val unit : t
    val null : t
    val gamma0 : t
    val gamma1 : t
    val gamma2 : t
    val gamma3 : t
    val gamma5 : t

    (* $(\gamma_0,\gamma_1,\gamma_2,\gamma_3)$ *)
    val gamma : t array

    (* Charge conjugation *)
    val cc : t

    (* Algebraic operations on $\gamma$-matrices *)
    val neg : t -> t
    val add : t -> t -> t
    val sub : t -> t -> t
    val mul : t -> t -> t
    val times : qc -> t -> t
    val transpose : t -> t
    val adjoint : t -> t
    val conj : t -> t
    val product : t list -> t

    (* Unit tests *)
    val test_suite : OUnit.test
  end

(* Chiral representation *)
module Dirac : Dirac =
  struct

    module Q = Algebra.Q
    module QC = Algebra.QC

    type qc = QC.t
    type t = qc array array

    let zero = QC.null
    let one = QC.one
    let minus_one = QC.neg one
    let i = QC.make Q.null Q.unit
    let minus_i = QC.conj i

    let null =
      [| [| zero; zero; zero; zero |];
         [| zero; zero; zero; zero |];
         [| zero; zero; zero; zero |];
         [| zero; zero; zero; zero |] |]

    let unit =
      [| [| one;  zero; zero; zero |];
         [| zero; one;  zero; zero |];
         [| zero; zero; one;  zero |];
         [| zero; zero; zero; one  |] |]

    let gamma0 =
      [| [| zero; zero; one;  zero |];
         [| zero; zero; zero; one  |];
         [| one;  zero; zero; zero |];
         [| zero; one;  zero; zero |] |]

    let gamma1 =
      [| [| zero;      zero;      zero; one  |];
         [| zero;      zero;      one;  zero |];
         [| zero;      minus_one; zero; zero |];
         [| minus_one; zero;      zero; zero |] |]

    let gamma2 =
      [| [| zero;    zero; zero; minus_i |];
         [| zero;    zero; i;    zero    |];
         [| zero;    i;    zero; zero    |];
         [| minus_i; zero; zero; zero    |] |]

    let gamma3 =
      [| [| zero;      zero; one;  zero      |];
         [| zero;      zero; zero; minus_one |];
         [| minus_one; zero; zero; zero      |];
         [| zero;      one;  zero; zero      |] |]

    let gamma5 =
      [| [| minus_one; zero;      zero; zero |];
         [| zero;      minus_one; zero; zero |];
         [| zero;      zero;      one;  zero |];
         [| zero;      zero;      zero; one  |] |]

    let gamma =
      [| gamma0; gamma1; gamma2; gamma3 |]

    let cc =
      [| [| zero; minus_one; zero;      zero |];
         [| one;  zero;      zero;      zero |];
         [| zero; zero;      zero;      one  |];
         [| zero; zero;      minus_one; zero |] |]

    let neg g =
      let g' = Array.make_matrix 4 4 zero in
      for i = 0 to 3 do
        for j = 0 to 3 do
          g'.(i).(j) <- QC.neg g.(i).(j)
        done
      done;
      g'

    let add g1 g2 =
      let g12 = Array.make_matrix 4 4 zero in
      for i = 0 to 3 do
        for j = 0 to 3 do
          g12.(i).(j) <- QC.add g1.(i).(j) g2.(i).(j)
        done
      done;
      g12

    let sub g1 g2 =
      let g12 = Array.make_matrix 4 4 zero in
      for i = 0 to 3 do
        for j = 0 to 3 do
          g12.(i).(j) <- QC.sub g1.(i).(j) g2.(i).(j)
        done
      done;
      g12

    let mul g1 g2 =
      let g12 = Array.make_matrix 4 4 zero in
      for i = 0 to 3 do
        for k = 0 to 3 do
          for j = 0 to 3 do
            g12.(i).(k) <- QC.add g12.(i).(k) (QC.mul g1.(i).(j) g2.(j).(k))
          done
        done
      done;
      g12

    let times q g =
      let g' = Array.make_matrix 4 4 zero in
      for i = 0 to 3 do
        for j = 0 to 3 do
          g'.(i).(j) <- QC.mul q g.(i).(j)
        done
      done;
      g'

    let transpose g =
      let g' = Array.make_matrix 4 4 zero in
      for i = 0 to 3 do
        for j = 0 to 3 do
          g'.(i).(j) <- g.(j).(i)
        done
      done;
      g'

    let adjoint g =
      let g' = Array.make_matrix 4 4 zero in
      for i = 0 to 3 do
        for j = 0 to 3 do
          g'.(i).(j) <- QC.conj g.(j).(i)
        done
      done;
      g'

    let conj g =
      let g' = Array.make_matrix 4 4 zero in
      for i = 0 to 3 do
        for j = 0 to 3 do
          g'.(i).(j) <- QC.conj g.(i).(j)
        done
      done;
      g'

    let product glist =
      List.fold_right mul glist unit

    open OUnit

    let two = QC.make (Q.make 2 1) Q.null
    let half = QC.make (Q.make 1 2) Q.null
    let two_unit = times two unit

    let ac_lhs mu nu =
      add (mul gamma.(mu) gamma.(nu)) (mul gamma.(nu) gamma.(mu))

    let ac_rhs mu nu =
      if mu = nu then
        if mu = 0 then
          two_unit
        else
          neg two_unit
      else
        null

    let test_ac mu nu =
      (ac_lhs mu nu) = (ac_rhs mu nu)

    let ac_lhs_all =
      let lhs = Array.make_matrix 4 4 null in
      for mu = 0 to 3 do
        for nu = 0 to 3 do
          lhs.(mu).(nu) <- ac_lhs mu nu
        done
      done;
      lhs
                                                                   
    let ac_rhs_all =
      let rhs = Array.make_matrix 4 4 null in
      for mu = 0 to 3 do
        for nu = 0 to 3 do
          rhs.(mu).(nu) <- ac_rhs mu nu
        done
      done;
      rhs

    let dump2 lhs rhs =
      for i = 0 to 3 do
        for j = 0 to 3 do
          Printf.printf
            "   i = %d, j =%d: %s + %s*I | %s + %s*I\n"
            i j
            (Q.to_string (QC.real lhs.(i).(j)))
            (Q.to_string (QC.imag lhs.(i).(j)))
            (Q.to_string (QC.real rhs.(i).(j)))
            (Q.to_string (QC.imag rhs.(i).(j)))
        done
      done

    let dump2_all lhs rhs =
      for mu = 0 to 3 do
        for nu = 0 to 3 do
          Printf.printf "mu = %d, nu =%d: \n" mu nu;
          dump2 lhs.(mu).(nu) rhs.(mu).(nu)
        done
      done

    let anticommute =
      "anticommutation relations" >::
        (fun () ->
          assert_bool
            ""
            (if ac_lhs_all = ac_rhs_all then
               true
             else
               begin
                 dump2_all ac_lhs_all ac_rhs_all;
                 false
               end))

    let equal_or_dump2 lhs rhs =
      if lhs = rhs then
        true
      else
        begin
          dump2 lhs rhs;
          false
        end

    let gamma5_def =
      "gamma5" >::
        (fun () ->
          assert_bool
            "definition"
            (equal_or_dump2
               gamma5
               (times i (product [gamma0; gamma1; gamma2; gamma3]))))

    let self_adjoint =
      "(anti)selfadjointness" >:::
        [ "gamma0" >::
            (fun () ->
              assert_bool "self" (equal_or_dump2 gamma0 (adjoint gamma0)));
          "gamma1" >::
            (fun () ->
              assert_bool "anti" (equal_or_dump2 gamma1 (neg (adjoint gamma1))));
          "gamma2" >::
            (fun () ->
              assert_bool "anti" (equal_or_dump2 gamma2 (neg (adjoint gamma2))));
          "gamma3" >::
            (fun () ->
              assert_bool "anti" (equal_or_dump2 gamma3 (neg (adjoint gamma3))));
          "gamma5" >::
            (fun () ->
              assert_bool "self" (equal_or_dump2 gamma5 (adjoint gamma5))) ]

    let cc_inv = neg cc

    let cc_gamma g =
      equal_or_dump2 (neg (transpose g)) (product [cc; g; cc_inv])

    let charge_conjugation =
      "charge conjugation" >:::
        [ "inverse" >::
            (fun () ->
              assert_bool "" (equal_or_dump2 (mul cc cc_inv) unit));
          "gamma0" >:: (fun () -> assert_bool "" (cc_gamma gamma0));
          "gamma1" >:: (fun () -> assert_bool "" (cc_gamma gamma1));
          "gamma2" >:: (fun () -> assert_bool "" (cc_gamma gamma2));
          "gamma3" >:: (fun () -> assert_bool "" (cc_gamma gamma3));
          "gamma5" >::
            (fun () ->
              assert_bool "" (equal_or_dump2 (transpose gamma5)
                                             (product [cc; gamma5; cc_inv])))
        ]

    let test_suite =
      "Dirac Matrices" >:::
        [anticommute;
         gamma5_def;
         self_adjoint;
         charge_conjugation]

  end

(* \thocwmodulesection{Generating Code for UFO Lorentz Structures} *)

(* O'Caml before 4.02 had a module typing bug that forces us to put this
   definition outside [Lorentz_Fusion]. *)
module Q = Algebra.Q
module QC = Algebra.QC
module A = UFOx.Lorentz_Atom
module D = Dirac

module type Lorentz_Fusion =
  sig

    (* Just like [UFOx.Lorentz_Atom.dirac], but without the Dirac matrix indices. *)
    type dirac = private
               | Gamma5
               | ProjM
               | ProjP
               | Gamma of int
               | Sigma of int * int
               | C

    (* A sandwich of a string of $\gamma$-matrices. [bra] and [ket] are
       positions of fields in the vertex, \emph{not} spinor indices. *)
    type dirac_string = private
      { bra : int;
        ket : int;
        gammas : dirac list }

    (* The Lorentz indices appearing in a term are either negative
       internal summation indices or positive external polarization
       indices.  Note that the external
       indices are not really indices, but denote the position
       of the particle in the vertex. *)
    type 'a term =  (* private *)
      { indices : int list;
        atom : 'a }

    (* Split the list of indices into summation and polarization indices. *)
    val classify_indices : int list -> int list * int list

    (* Replace the atom keeping the associated indices. *)
    val map_atom : ('a -> 'b) -> 'a term -> 'b term

    (* A contraction consists of a (possibly empty) product of
       Dirac strings and a (possibly empty) product of Lorentz
       tensors with a rational coefficient.  The summation
       indices could be recovered by scanning the [term]s, but
       we maintain a list for efficiency. *)
    type contraction = private
      { coeff : Q.t;
        dirac : dirac_string term list;
        vector : UFOx.Lorentz_Atom.vector term list }

    (* A sum. *)
    type t = contraction list

    (* [parse spins lorentz] uses the [spins] to parse the
       UFO [lorentz] structure as a list of [contraction]s. *)
    val parse : Coupling.lorentz list -> UFOx.Lorentz.t -> t

    (* Create a readable representation for debugging and
       documenting generated code. *)
    val to_string : t -> string

    (* Punting \ldots *)
    val dummy : t

    (* More debugging and documenting. *)
    val dirac_string_to_string : dirac_string -> string

    (* [dirac_string_to_matrix substitute ds] take a string
       of $\gamma$-matrices [ds], applies [substitute] to
       the indices and returns the product as a matrix. *)
    val dirac_string_to_matrix : (int -> int) -> dirac_string -> D.t

  end

module Lorentz_Fusion : Lorentz_Fusion =
  struct

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

    (* A sandwich of a string of $\gamma$-matrices. [bra] and [ket] are
       positions of fields in the vertex. *)
    type dirac_string =
      { bra : int;
        ket : int;
        gammas : dirac list }

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

    (* A contraction consists of a (possibly empty) product of
       Dirac strings and a (possibly empty) product of Lorentz
       tensors with a rational coefficient.  The summation
       indices could be recovered by scanning the [term]s, but
       we maintain a list for efficiency. *)
    type contraction =
      { coeff : Q.t;
        dirac : dirac_string term list;
        vector : A.vector term list }

    type t = contraction list

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

    let trace_metric = Q.make 4 1

    (* FIXME: can this be made typesafe by mapping to a
       type that \emph{only} contains [P] and [Epsilon]? *)
    let rec compress_metrics c =
      match redundant_metric c.vector with
      | None, _ -> c
      | Some (Trace mu), vector' ->
         compress_metrics
           { coeff = Q.mul trace_metric c.coeff;
             dirac = c.dirac;
             vector = vector' }
      | Some (Replace (mu, nu)), vector' ->
         compress_metrics
           { coeff = c.coeff;
             dirac = substitute_index_dirac mu nu c.dirac;
             vector = substitute_index_vector mu nu vector' }


    let dummy = []

    let parse1 spins atom =
      compress_metrics (contraction_of_lorentz_atoms atom)

    let parse spins l =
      List.map (parse1 spins) l

    let vector_to_string = function
      | A.Epsilon (mu, nu, ka, la) ->
	 Printf.sprintf "Epsilon(%d,%d,%d,%d)" mu nu ka la
      | A.Metric (mu, nu) ->
	 Printf.sprintf "Metric(%d,%d)" mu nu
      | A.P (mu, n) ->
	 Printf.sprintf "P(%d,%d)" mu n

    let dirac_to_string = function
      | Gamma5 -> "g5"
      | ProjM -> "(1-g5)/2"
      | ProjP -> "(1+g5)/2"
      | Gamma (mu) -> Printf.sprintf "g(%d)" mu
      | Sigma (mu, nu) ->  Printf.sprintf "s(%d,%d)" mu nu
      | C -> "C"

    let dirac_string_to_string ds =
      match ds.gammas with
      | [] -> Printf.sprintf "<%d|%d>" ds.bra ds.ket
      | gammas ->
         Printf.sprintf
           "<%d|%s|%d>"
           ds.bra (String.concat "*" (List.map dirac_to_string gammas)) ds.ket

    let contraction_to_string c =
      Q.to_string c.coeff ^ " * " ^
        String.concat
          " * " (List.map (fun ds -> dirac_string_to_string ds.atom) c.dirac) ^
          " * " ^
            String.concat
              " * " (List.map (fun v -> vector_to_string v.atom) c.vector)

    let to_string contractions =
      String.concat " + " (List.map contraction_to_string contractions)

  end

module type T =
  sig
    (* [lorentz formatter name spins v]
       writes a representation of the Lorentz structure [v] of
       particles with the Lorentz representations [spins] as a
       (Fortran) function [name] to [formatter]. *)
    val lorentz :
      Format_Fortran.formatter -> string -> Coupling.lorentz array ->
      UFOx.Lorentz.t -> unit

    val fusion2 :
      Algebra.QC.t -> string -> Coupling.lorentz3 ->
      string -> string -> string -> string -> string -> Coupling.fuse2 -> unit
    val fusion3 :
      Algebra.QC.t -> string -> Coupling.lorentz4 ->
      string -> string -> string -> string -> string ->
      string -> string -> Coupling.fuse3 -> unit
    val fusionn :
      Algebra.QC.t -> string -> Coupling.lorentzn ->
      string -> string list -> string list -> Coupling.fusen -> unit

    val eps4_g4_g44_decl : Format_Fortran.formatter -> unit -> unit
    val eps4_g4_g44_init : Format_Fortran.formatter -> unit -> unit

  end

module Fortran : T =
  struct

    open Format_Fortran

    let pp_divide ?(indent=0) ff () =
      fprintf ff "%*s! %s" indent "" (String.make (70 - indent) '-');
      pp_newline ff ()

    let conjugate = function
      | Coupling.Spinor -> Coupling.ConjSpinor
      | Coupling.ConjSpinor -> Coupling.Spinor
      | r -> r

    let spin_mnemonic = function
      | Coupling.Scalar -> "phi"
      | Coupling.Spinor -> "psi"
      | Coupling.ConjSpinor -> "psibar"
      | Coupling.Majorana -> "chi"
      | Coupling.Maj_Ghost -> "???"
      | Coupling.Vector -> "a"
      | Coupling.Massive_Vector -> "v"
      | Coupling.Vectorspinor -> "???"
      | Coupling.Tensor_1 -> "???"
      | Coupling.Tensor_2 -> "???"
      | Coupling.BRS l -> "???"

    let fortran_type = function
      | Coupling.Scalar -> "complex(kind=default)"
      | Coupling.Spinor -> "type(spinor)"
      | Coupling.ConjSpinor -> "type(conjspinor)"
      | Coupling.Majorana -> "type(bispinor)"
      | Coupling.Maj_Ghost -> "???"
      | Coupling.Vector -> "type(vector)"
      | Coupling.Massive_Vector -> "type(vector)"
      | Coupling.Vectorspinor -> "???"
      | Coupling.Tensor_1 -> "???"
      | Coupling.Tensor_2 -> "???"
      | Coupling.BRS l -> "???"

    (* The \texttt{omegalib} separates time from space.  Maybe
       not a good idea after all.  Mend it locally \ldots *)
    type wf =
      { pos : int;
        spin : Coupling.lorentz;
        name : string;
        local_array : string option;
        momentum : string;
        momentum_array : string;
        fortran_type : string }

    let wf_table spins =
      Array.mapi
        (fun i s ->
          let spin =
            if i = 0 then
              conjugate s
            else
              s in
          let pos = succ i in
          let i = string_of_int pos in
          let name = spin_mnemonic s ^ i in
          let local_array =
            begin match spin with
            | Coupling.Vector -> Some (name ^ "a")
            | _ -> None
            end in
          { pos;
            spin;
            name;
            local_array;
            momentum = "k" ^ i;
            momentum_array = "p" ^ i;
            fortran_type = fortran_type spin } )
        spins

    module F = Lorentz_Fusion

    let unparse_rational q =
      match Q.to_ratio q with
      |  0, _ -> printf "0"
      |  1, 1 -> printf "1"
      | -1, 1 -> printf "-1"
      |  n, 1 -> printf "%d" n
      |  1, d -> printf "(1/%d.0_default)" d
      | -1, d -> printf "(-1/%d.0_default)" d
      |  n, d -> printf "(%d.0_default/%d)" n d

    let unparse_error msg =
      printf " [[ERROR: %s]] " msg

    let unparse_list e o unparse_term = function
      | [] -> printf "%s" e
      | [t] -> unparse_term t;
      | t :: tl ->
         printf "(";
         unparse_term t;
         List.iter (fun t -> printf "%s" o; unparse_term t) tl;
         printf ")"

    let unparse_product unparse_term l =
      unparse_list "1" "*" unparse_term l

    let unparse_sum unparse_term l =
      unparse_list "0" "+" unparse_term l
                   
    let unparse fusion =
      Lorentz_Fusion.to_string fusion

    (* Format rational ([Q.t]) and complex rational ([QC.t])
       numbers as fortran values. *)
    let format_rational q =
      if Q.is_integer q then
        string_of_int (Q.to_integer q)
      else
        let n, d = Q.to_ratio q in
        Printf.sprintf "%d.0_default/%d" n d

    let format_complex_rational cq =
      let real = QC.real cq
      and imag = QC.imag cq in
      if Q.is_null imag then
        begin
          if Q.is_negative real then
            "(" ^ format_rational real ^ ")"
          else
            format_rational real
        end
      else if Q.is_integer real && Q.is_integer imag then
        Printf.sprintf "(%d, %d)" (Q.to_integer real) (Q.to_integer imag)
      else
        Printf.sprintf
          "cmplx (%s, %s, kind=default)"
          (format_rational real) (format_rational imag)

    (* Optimize the representation if used as a prefactor of
       a summand in a sum. *)
    let format_rational_factor q =
      if Q.is_unit q then
        "+"
      else if Q.is_unit (Q.neg q) then
        "-"
      else if Q.is_negative q then
        "- " ^ format_rational (Q.neg q) ^ " *"
      else
        "+ " ^ format_rational q ^ " *"

    let format_complex_rational_factor cq =
      let real = QC.real cq
      and imag = QC.imag cq in
      if Q.is_null imag then
        begin
          if Q.is_unit real then
            "+"
          else if Q.is_unit (Q.neg real) then
            "-"
          else if Q.is_negative real then
            "- " ^ format_rational (Q.neg real) ^ " *"
          else
            "+ " ^ format_rational real ^ " *"
        end
      else if Q.is_integer real && Q.is_integer imag then
        Printf.sprintf "+ (%d,%d) *" (Q.to_integer real) (Q.to_integer imag)
      else
        Printf.sprintf
          "+ cmplx (%s, %s, kind=default) *"
          (format_rational real) (format_rational imag)

    (* Append a formatted list of indices to [name]. *)
    let append_indices name = function
      | [] -> name
      | indices ->
         name ^ "(" ^ String.concat "," (List.map string_of_int indices) ^ ")"

    (* Dirac string variables and their names. *)
    type dsv =
      | Ket of int
      | Bra of int
      | Braket of int

    let dsv_name = function
      | Ket n -> Printf.sprintf "ket%02d" n
      | Bra n -> Printf.sprintf "bra%02d" n
      | Braket n -> Printf.sprintf "bkt%02d" n

    let dirac_dimension dsv indices =
      let tail ilist =
        String.concat "," (List.map (fun _ -> "0:3") ilist) ^ ")" in
      match dsv, indices with
      | Braket _, [] -> ""
      | (Ket _ | Bra _), [] -> ", dimension(1:4)"
      | Braket _, indices -> ", dimension(" ^ tail indices
      | (Ket _ | Bra _), indices -> ", dimension(1:4," ^ tail indices

    (* Write Fortran code to [decl] and [eval]: apply the Dirac matrix
       [gamma] with complex rational entries to the spinor [ket] from
       the left. [ket] must be the name of a scalar variable and cannot
       be an array element.  The result is stored in [dsv_name (Ket n)]
       which can have additional [indices].  Return [Ket n] for further
       processing. *)
    let dirac_ket_to_fortran_decl ff n indices =
      let printf fmt = fprintf ff fmt
      and nl = pp_newline ff in
      let dsv = Ket n in
      printf
        "    @[<2>complex(kind=default)%s ::@ %s@]"
        (dirac_dimension dsv indices) (dsv_name dsv);
      nl ()

    let dirac_ket_to_fortran_eval ff n indices gamma ket =
      let printf fmt = fprintf ff fmt
      and nl = pp_newline ff in
      let dsv = Ket n in
      for i = 0 to 3 do
        let name = append_indices (dsv_name dsv) (succ i :: indices) in
        printf "    @[<%d>%s = 0" (String.length name + 5) name;
        for j = 0 to 3 do
          if gamma.(i).(j) <> QC.null then
            printf
              "@ %s %s%%a(%d)"
              (format_complex_rational_factor gamma.(i).(j))
              ket.name (succ j)
        done;
        printf "@]";
        nl ()
      done;
      dsv

    (* The same as [dirac_bra_to_fortran], but apply the Dirac matrix
       [gamma] to [bra] from the right and return [Bra n]. *)
    let dirac_bra_to_fortran_decl ff n indices =
      let printf fmt = fprintf ff fmt
      and nl = pp_newline ff in
      let dsv = Bra n in
      printf
        "    @[<2>complex(kind=default)%s ::@ %s@]"
        (dirac_dimension dsv indices) (dsv_name dsv);
      nl ()

    let dirac_bra_to_fortran_eval ff n indices bra gamma =
      let printf fmt = fprintf ff fmt
      and nl = pp_newline ff in
      let dsv = Bra n in
      for j = 0 to 3 do
        let name = append_indices (dsv_name dsv) (succ j :: indices) in
        printf "    @[<%d>%s = 0" (String.length name + 5) name;
        for i = 0 to 3 do
          if gamma.(i).(j) <> QC.null then
            printf
              "@ %s %s%%a(%d)"
              (format_complex_rational_factor gamma.(i).(j))
              bra.name (succ i)
        done;
        printf "@]";
        nl ()
      done;
      dsv

    (* More of the same, but evaluating a spinor sandwich and
       returning [Braket n]. *)
    let dirac_braket_to_fortran_decl ff n indices =
      let printf fmt = fprintf ff fmt
      and nl = pp_newline ff in
      let dsv = Braket n in
      printf
        "    @[<2>complex(kind=default)%s ::@ %s@]"
        (dirac_dimension dsv indices) (dsv_name dsv);
      nl ()

    let dirac_braket_to_fortran_eval ff n indices bra gamma ket =
      let printf fmt = fprintf ff fmt
      and nl = pp_newline ff in
      let dsv = Braket n in
      let name = append_indices (dsv_name dsv) indices in
      printf "    @[<%d>%s = 0" (String.length name + 5) name;
      for i = 0 to 3 do
        for j = 0 to 3 do
          if gamma.(i).(j) <> QC.null then
            printf
              "@ %s %s%%a(%d)*%s%%a(%d)"
              (format_complex_rational_factor gamma.(i).(j))
              bra.name (succ i) ket.name (succ j)
        done
      done;
      printf "@]";
      nl ();
      dsv

    (* Choose among the previous functions according to the position
       of [bra] and [ket] among the wavefunctions.  If any is in the
       first position evaluate the spinor expression with the
       corresponding spinor removed, otherwise evaluate the
       spinir sandwich. *)
    let dirac_bra_or_ket_to_fortran_decl ff n indices bra ket =
      if bra = 1 then
        dirac_ket_to_fortran_decl ff n indices
      else if ket = 1 then
        dirac_bra_to_fortran_decl ff n indices
      else
        dirac_braket_to_fortran_decl ff n indices

    let dirac_bra_or_ket_to_fortran_eval ff n indices wfs bra gamma ket =
      if bra = 1 then
        dirac_ket_to_fortran_eval ff n indices gamma wfs.(pred ket)
      else if ket = 1 then
        dirac_bra_to_fortran_eval ff n indices wfs.(pred bra) gamma
      else
        dirac_braket_to_fortran_eval
          ff n indices wfs.(pred bra) gamma wfs.(pred ket)

    (* UFO summation indices are negative integers.  Derive a valid Fortran
       variable name. *)
    let prefix_summation = "mu"
    let prefix_polarization = "nu"
    let index_spinor = "alpha"

    let index_variable mu =
      if mu < 0 then
        Printf.sprintf "%s%d" prefix_summation (- mu)
      else if mu == 0 then
       prefix_polarization
      else
        Printf.sprintf "%s%d" prefix_polarization mu

    let format_indices indices =
      String.concat "," (List.map index_variable indices)

    module IntPM =
      Partial.Make (struct type t = int let compare = compare end)

    type tensor =
      | DS of dsv
      | V of string
      | T of UFOx.Lorentz_Atom.vector

    (* Write the [i]th Dirac string [ds] as Fortran code to [eval], including
       a shorthand representation as a comment.  Return [ds] with
       [ds.F.atom] replaced by the dirac string variable,
       i,\,e.~[DS dsv] annotated with the internal and external indices.
       In addition write the declaration to [decl].  *)
    let dirac_string_to_fortran ~decl ~eval i wfs ds =
      let printf fmt = fprintf eval fmt
      and nl = pp_newline eval in
      let bra = ds.F.atom.F.bra
      and ket = ds.F.atom.F.ket in
      pp_divide ~indent:4 eval ();
      begin match ds.F.indices with
      | [] ->
         printf "    ! %s" (F.dirac_string_to_string ds.F.atom); nl ();
         let gamma = F.dirac_string_to_matrix (fun _ -> 0) ds.F.atom in
         dirac_bra_or_ket_to_fortran_decl decl i [] bra ket;
         let dsv =
           dirac_bra_or_ket_to_fortran_eval eval i [] wfs bra gamma ket in
         F.map_atom (fun _ -> DS dsv) ds
      | indices ->
         printf
           "    ! %s"
           (F.dirac_string_to_string ds.F.atom); nl ();
         dirac_bra_or_ket_to_fortran_decl decl i indices bra ket;
         let combinations = Product.power (List.length indices) [0; 1; 2; 3] in
         let dsv =
           List.map
             (fun combination ->
               let substitution = IntPM.of_lists indices combination in
               let substitute = IntPM.apply substitution in
               let indices = List.map substitute indices in
               let gamma =
                 F.dirac_string_to_matrix substitute ds.F.atom in
               dirac_bra_or_ket_to_fortran_eval eval i indices wfs bra gamma ket)
             combinations in
         begin match ThoList.uniq (List.sort compare dsv) with
         | [dsv] -> F.map_atom (fun _ -> DS dsv) ds
         | _ -> failwith "dirac_string_to_fortran: impossible"
         end
      end

    (* Write the Dirac strings in the list [ds_list] as Fortran code to
       [eval], including shorthand representations as comments.
       Return the list of variables and corresponding indices to
       be contracted. *)
    let dirac_strings_to_fortran ~decl ~eval wfs last ds_list =
      List.fold_left
        (fun (i, acc) ds ->
          let i = succ i in
          (i, dirac_string_to_fortran ~decl ~eval i wfs ds :: acc))
        (last, []) ds_list

    (* Perform a nested sum of terms, as printed by [print_term]
       (which takes the number of spaces to indent as only argument)
       of the cartesian product of [indices] running from 0 to 3. *)
    let nested_sums ~decl ~eval initial_indent indices print_term =
      let rec nested_sums' indent = function
        | [] -> print_term indent
        | index :: indices ->
           let var = index_variable index in
           fprintf eval "%*s@[<2>do %s = 0, 3@]" indent "" var;
           pp_newline eval ();
           nested_sums' (indent + 2) indices; pp_newline eval ();
           fprintf eval "%*s@[<2>end do@]" indent "" in
      nested_sums' (initial_indent + 2) indices

    (* Polarization indices also need to be summed over, but they
       appear only once. *)
    let indices_of_contractions contractions =
      let index_pairs, polarizations =
        F.classify_indices
          (ThoList.flatmap (fun ds -> ds.F.indices) contractions) in
      try
        ThoList.pairs index_pairs @ ThoList.uniq (List.sort compare polarizations)
      with
      | Invalid_argument s ->
         invalid_arg
           ("indices_of_contractions: " ^
              ThoList.to_string string_of_int index_pairs)

    let format_dsv dsv indices =
      match dsv, indices with
      | Braket _, [] -> dsv_name dsv
      | Braket _, ilist ->
         Printf.sprintf "%s(%s)" (dsv_name dsv) (format_indices indices)
      | (Bra _ | Ket _), [] ->
         Printf.sprintf "%s(%s)" (dsv_name dsv) index_spinor
      | (Bra _ | Ket _), ilist ->
         Printf.sprintf
           "%s(%s,%s)" (dsv_name dsv) index_spinor (format_indices indices)

    let format_tensor t =
      let indices = t.F.indices in
      match t.F.atom with
      | DS dsv -> format_dsv dsv indices
      | V vector -> Printf.sprintf "%s(%s)" vector (format_indices indices)
      | T UFOx.Lorentz_Atom.P (mu, n) ->
         Printf.sprintf "p%d(%s)" n (index_variable mu)
      | T UFOx.Lorentz_Atom.Epsilon (mu1, mu2, mu3, mu4) ->
         Printf.sprintf "eps4_(%s)" (format_indices [mu1; mu2; mu3; mu4])
      | T UFOx.Lorentz_Atom.Metric (mu1, mu2) ->
         if mu1 > 0 && mu2 > 0 then
           Printf.sprintf "g44_(%s)" (format_indices [mu1; mu2])
         else
           failwith "format_tensor: compress_metrics has failed!"

    let rec multiply_tensors ~decl ~eval = function
      | [] -> fprintf eval "1";
      | [t] -> fprintf eval "%s" (format_tensor t)
      | t :: tensors ->
         fprintf eval "%s@ * " (format_tensor t);
         multiply_tensors ~decl ~eval tensors

    let contract_indices ~decl ~eval indent wf_index wfs (q, contractees) =
      let printf fmt = fprintf eval fmt
      and nl = pp_newline eval in
      let sum_var =
        begin match wf_index with
        | None -> wfs.(0).name
        | Some i ->
           begin match wfs.(0).local_array with
           | None -> Printf.sprintf "%s%%a(%s)" wfs.(0).name i
           | Some a -> Printf.sprintf "%s(%s)" a i
           end
        end in
      let indices =
        List.filter (fun i -> i <> 1) (indices_of_contractions contractees) in
      nested_sums
        ~decl ~eval
        indent indices
        (fun indent ->
          printf "%*s@[<2>%s = %s" indent "" sum_var sum_var;
          printf "@ %s" (format_rational_factor q);
          List.iter (fun i -> printf "@ g4_(%s) *" (index_variable i)) indices;
          printf "@ (";
          multiply_tensors ~decl ~eval contractees;
          printf ")@]");
      printf "@]";
      nl ()

    let external_wf_loop ~decl ~eval ~indent wfs contractees =
      pp_divide ~indent eval ();
      match wfs.(0).spin with
      | Coupling.Scalar ->
         contract_indices ~decl ~eval 2 None wfs contractees
      | Coupling.Spinor | Coupling.ConjSpinor ->
         let idx = index_spinor in
         fprintf eval "%*s@[<2>do %s = 1, 4@]" indent "" idx; pp_newline eval ();
         contract_indices ~decl ~eval 4 (Some idx) wfs contractees;
         fprintf eval "%*send do@]" indent ""; pp_newline eval ()
      | Coupling.Vector ->
         let idx = index_variable 1 in
         fprintf eval "%*s@[<2>do %s = 0, 3@]" indent "" idx; pp_newline eval ();
         contract_indices ~decl ~eval 4 (Some idx) wfs contractees;
         fprintf eval "%*send do@]" indent ""; pp_newline eval ()
      | _ -> failwith "external_wf_loop: incomplete"

    let local_vector_copies ~decl ~eval wfs =
      begin match wfs.(0).local_array with
      | None -> ()
      | Some a ->
         fprintf
           decl "    @[<2>complex(kind=default),@ dimension(0:3) ::@ %s@]" a;
         pp_newline decl ()
      end;
      let n = Array.length wfs in
      for i = 1 to n - 1 do
        match wfs.(i).local_array with
        | None -> ()
        | Some a ->
           fprintf
             decl "    @[<2>complex(kind=default),@ dimension(0:3) ::@ %s@]" a;
           pp_newline decl ();
           fprintf eval "    @[<2>%s(0) = %s%%t@]" a wfs.(i).name;
           pp_newline eval ();
           fprintf eval "    @[<2>%s(1:3) = %s%%x@]" a wfs.(i).name;
           pp_newline eval ()
      done

    let return_vector ff wfs =
      let printf fmt = fprintf ff fmt
      and nl = pp_newline ff in
      match wfs.(0).local_array with
      | None -> ()
      | Some a ->
         pp_divide ~indent:4 ff ();
         printf "    @[<2>%s%%t = %s(0)@]" wfs.(0).name a; nl ();
         printf "    @[<2>%s%%x = %s(1:3)@]" wfs.(0).name a; nl ()

    let multiply_coupling_and_scalars ff g wfs =
      let printf fmt = fprintf ff fmt
      and nl = pp_newline ff in
      pp_divide ~indent:4 ff ();
      printf "    @[<2>%s = %s * %s" wfs.(0).name g wfs.(0).name;
      for i = 1 to Array.length wfs - 1 do
        match wfs.(i).spin with
        | Coupling.Scalar -> printf "@ * %s" wfs.(i).name
        | _ -> ()
      done;
      printf "@]"; nl ()

    let local_momentum_copies ~decl ~eval wfs =
      let n = Array.length wfs in
      fprintf
        decl "    @[<2>real(kind=default),@ dimension(0:3) ::@ %s"
        wfs.(0).momentum_array;
      for i = 1 to n - 1 do
        fprintf decl ",@ %s" wfs.(i).momentum_array;
        fprintf
          eval "    @[<2>%s(0) = %s%%t@]"
          wfs.(i).momentum_array wfs.(i).momentum;
        pp_newline eval ();
        fprintf
          eval "    @[<2>%s(1:3) = %s%%x@]"
          wfs.(i).momentum_array wfs.(i).momentum;
        pp_newline eval ()
      done;
      fprintf eval "    @[<2>%s =" wfs.(0).momentum_array;
      for i = 1 to n - 1 do
        fprintf eval "@ - %s" wfs.(i).momentum_array
      done;
      fprintf decl "@]";
      pp_newline decl ();
      fprintf eval "@]";
      pp_newline eval ()

    (* FIXME: can be retired starting from O'Caml 4.02.0! *)
    let iset_of_list list =
      List.fold_right Sets.Int.add list Sets.Int.empty

    let contractees_of_fusion
          ~decl ~eval wfs (max_dsv, indices_seen, contractees) fusion =
      let max_dsv', dirac_strings =
        dirac_strings_to_fortran ~decl ~eval wfs max_dsv fusion.F.dirac
      and vectors =
        List.fold_left
          (fun acc wf ->
            match wf.local_array with
            | None -> acc
            | Some a -> { F.atom = V a; F.indices = [wf.pos] } :: acc)
          [] (List.tl (Array.to_list wfs))
      and tensors =
        List.map (F.map_atom (fun t -> T t)) fusion.F.vector in
      let contractees' = dirac_strings @ vectors @ tensors in
      let indices_seen' =
        iset_of_list (indices_of_contractions contractees') in
      (max_dsv',
       Sets.Int.union indices_seen indices_seen',
       (fusion.F.coeff, contractees') :: contractees)

    (* FIXME: add indices for vector wave functions and tensors. (???) *)
    let fusions_to_fortran ~decl ~eval wfs fusions =
      local_vector_copies ~decl ~eval wfs;
      local_momentum_copies ~decl ~eval wfs;
      let _, indices_used, contractions =
        List.fold_left
          (contractees_of_fusion ~decl ~eval wfs)
          (0, Sets.Int.empty, [])
          fusions in
      Sets.Int.iter
        (fun index ->
          fprintf decl "    @[<2>integer ::@ %s@]" (index_variable index);
          pp_newline decl ())
        indices_used;
      begin match wfs.(0).spin with
      | Coupling.Spinor | Coupling.ConjSpinor ->
         fprintf decl "    @[<2>integer ::@ %s@]" index_spinor;
         pp_newline decl ()
      | _ -> ()
      end;
      pp_divide ~indent:4 eval ();
      begin match wfs.(0).local_array with
      | Some a -> fprintf eval "    %s = 0" a
      | None ->
         match wfs.(0).spin with
         | Coupling.Spinor | Coupling.ConjSpinor ->
            fprintf eval "    %s%%a = 0" wfs.(0).name
         | Coupling.Scalar -> fprintf eval "    %s = 0" wfs.(0).name
         | _ -> failwith "fusions_to_fortran"
      end;
      pp_newline eval ();
      List.iter (external_wf_loop ~decl ~eval ~indent:4 wfs) contractions;
      return_vector eval wfs

    (* TODO: eventually, we should include the momentum among
       the arguments only if required.  But this can wait for
       another day. *)
    let lorentz ff name spins lorentz =
      let printf fmt = fprintf ff fmt
      and nl = pp_newline ff in
      let fusion =
        try
          Lorentz_Fusion.parse (Array.to_list spins) lorentz
        with
        | Failure msg ->
           begin
             prerr_endline msg;
             Lorentz_Fusion.dummy
           end in
      let wfs = wf_table spins in
      let n = Array.length wfs in
      printf "  @[<4>pure function %s@ (g,@ " name;
      for i = 1 to n - 2 do
        printf "%s,@ %s,@ " wfs.(i).name wfs.(i).momentum
      done;
      printf "%s,@ %s" wfs.(n - 1).name wfs.(n - 1).momentum;
      printf ")@ result (%s)@]" wfs.(0).name; nl ();
      printf "    @[<2>%s ::@ %s@]" wfs.(0).fortran_type wfs.(0).name; nl();
      printf "    @[<2>complex(kind=default),@ intent(in) ::@ g@]"; nl();
      for i = 1 to n - 1 do
        printf
          "    @[<2>%s, intent(in) :: %s@]"
          wfs.(i).fortran_type wfs.(i).name; nl();
      done;
      printf "    @[<2>type(momentum), intent(in) ::@ %s" wfs.(1).momentum;
      for i = 2 to n - 1 do
        printf ",@ %s" wfs.(i).momentum
      done;
      printf "@]";
      nl ();
      let width = 80 in (* get this from the default formatter instead! *)
      let decl_buf = Buffer.create 1024
      and eval_buf = Buffer.create 1024 in
      let decl = formatter_of_buffer ~width decl_buf
      and eval = formatter_of_buffer ~width eval_buf in
      fusions_to_fortran ~decl ~eval wfs fusion;
      multiply_coupling_and_scalars eval "g" wfs;
      pp_flush decl ();
      pp_flush eval ();
      pp_divide ~indent:4 ff ();
      printf "    ! %s" (unparse fusion); nl ();
      pp_divide ~indent:4 ff ();
      printf "%s" (Buffer.contents decl_buf);
      pp_divide ~indent:4 ff ();
      printf "%s" (Buffer.contents eval_buf);
      printf "  end function %s@]" name; nl ();
      Buffer.reset decl_buf;
      Buffer.reset eval_buf;
      ()

    let scale_coupling c g =
      if c = 1 then
        g
      else if c = -1 then
        "-" ^ g
      else
        Printf.sprintf "%d*%s" c g

    let scale_coupling z g =
      format_complex_rational_factor z ^ g

    (* As a prototypical example consider the vertex
       \begin{equation}
         \bar\psi\fmslash{A}\psi =
            \tr\left(\psi\otimes\bar\psi\fmslash{A}\right)
       \end{equation}
       encoded as \texttt{FFV} in the SM UFO file.  This example
       is useful, because all three fields have different type
       and we can use the Fortran compiler to check our
       implementation.

       In this case we need to generate the following function
       calls with the arguments in the following order
       \begin{center}
         \begin{tabular}{lcl}
           \texttt{F12}:&$\psi_1\bar\psi_2\to A$&
              \texttt{FFV\_p201(g,psi1,p1,psibar2,p2)} \\
           \texttt{F21}:&$\bar\psi_1\psi_2\to A$&
              \texttt{FFV\_p201(g,psi2,p2,psibar1,p1)} \\
           \texttt{F23}:&$\bar\psi_1 A_2 \to \bar\psi$&
              \texttt{FFV\_p012(g,psibar1,p1,A2,p2)} \\
           \texttt{F32}:&$A_1\bar\psi_2 \to \bar\psi$&
              \texttt{FFV\_p012(g,psibar2,p2,A1,p1)} \\
           \texttt{F31}:&$A_1\psi_2\to \psi$&
              \texttt{FFV\_p120(g,A1,p1,psi2,p2)} \\
           \texttt{F13}:&$\psi_1A_2\to \psi$&
              \texttt{FFV\_p120(g,A2,p2,psi1,p1)}
         \end{tabular}
       \end{center} *)

    (* Fortunately, all Fermi signs have been taken
       care of by [Fusions] and we can concentrate on
       injecting the wave functions into the correct slots. *)

    let fusion2 c v s g wf1 p1 wf2 p2 fuse2 =
      let g = scale_coupling c g in
      let open Coupling in
      let perm =
        begin match fuse2 with
        | F12 | F21 -> "201"
        | F23 | F32 -> "012"
        | F31 | F13 -> "120"
        end in
      match fuse2 with
      | F12 | F23 | F31 ->
         printf "%s_p%s(%s,%s,%s,%s,%s)" v perm g wf1 p1 wf2 p2
      | F21 | F32 | F13 ->
         printf "%s_p%s(%s,%s,%s,%s,%s)" v perm g wf2 p2 wf1 p1

    let fusion3 c v s g wf1 p1 wf2 p2 wf3 p3 fuse3 =
      let g = scale_coupling c g in
      let open Coupling in
      let perm =
        begin match fuse3 with
        | F234 | F243 | F432 | F342 | F324 | F423 -> "0123"
        | F134 | F341 | F413 | F143 | F431 | F314 -> "1230"
        | F124 | F241 | F412 | F142 | F421 | F214 -> "2301"
        | F123 | F231 | F312 | F132 | F321 | F213 -> "3012"
        end in
      match fuse3 with
      (* These are the obvious ones, b/c they're their own inverses. *)
      | F234 | F341 | F412 | F123 ->
         printf "%s_p%s(%s,%s,%s,%s,%s,%s,%s)" v perm g wf1 p1 wf2 p2 wf3 p3
      | F243 | F314 | F421 | F132 ->
         printf "%s_p%s(%s,%s,%s,%s,%s,%s,%s)" v perm g wf1 p1 wf3 p3 wf2 p2
      | F324 | F431 | F142 | F213 ->
         printf "%s_p%s(%s,%s,%s,%s,%s,%s,%s)" v perm g wf2 p2 wf1 p1 wf3 p3
      | F432 | F143 | F214 | F321 ->
         printf "%s_p%s(%s,%s,%s,%s,%s,%s,%s)" v perm g wf3 p3 wf2 p2 wf1 p1
      (* TODO: Explain why we need the inverses here \ldots *)
      | F342 | F413 | F124 | F231 ->
         printf "%s_p%s(%s,%s,%s,%s,%s,%s,%s)" v perm g wf3 p3 wf1 p1 wf2 p2
      | F423 | F134 | F241 | F312 ->
         printf "%s_p%s(%s,%s,%s,%s,%s,%s,%s)" v perm g wf2 p2 wf3 p3 wf1 p1


    (* \begin{dubious}
          FIXME: Implement the correct permutations also for
          higher order vertices!
       \end{dubious} *)

    let fusionn c v s g wfs ps fusion =
      let g = scale_coupling c g in
      printf
        "%s_p_(%s,%s)" v g
        (String.concat "," (List.map2 (fun wf p -> wf ^ "," ^ p) wfs ps))

    let eps4_g4_g44_decl ff () =
      let printf fmt = fprintf ff fmt
      and nl = pp_newline ff in
      printf "  @[<2>integer,@ dimension(0:3)";
      printf ",@ save,@ private ::@ g4_@]"; nl ();
      printf "  @[<2>integer,@ dimension(0:3,0:3)";
      printf ",@ save,@ private ::@ g44_@]"; nl ();
      printf "  @[<2>integer,@ dimension(0:3,0:3,0:3,0:3)";
      printf ",@ save,@ private ::@ eps4_@]"; nl ()

    let eps4_g4_g44_init ff () =
      let printf fmt = fprintf ff fmt
      and nl = pp_newline ff in
      printf "  @[<2>data g4_@            /@  1, -1, -1, -1 /@]"; nl ();
      printf "  @[<2>data g44_(0,:)@      /@  1,  0,  0,  0 /@]"; nl ();
      printf "  @[<2>data g44_(1,:)@      /@  0, -1,  0,  0 /@]"; nl ();
      printf "  @[<2>data g44_(2,:)@      /@  0,  0, -1,  0 /@]"; nl ();
      printf "  @[<2>data g44_(3,:)@      /@  0,  0,  0, -1 /@]"; nl ();
      for mu1 = 0 to 3 do
        for mu2 = 0 to 3 do
          for mu3 = 0 to 3 do
            printf "  @[<2>data eps4_(%d,%d,%d,:)@ /@ " mu1 mu2 mu3;
            for mu4 = 0 to 3 do
              if mu4 <> 0 then
                printf ",@ ";
              let mus = [mu1; mu2; mu3; mu4] in
              if List.sort compare mus = [0; 1; 2; 3] then
                printf "%2d" (Combinatorics.sign mus)
              else
                printf "%2d" 0;
            done;
            printf " /@]";
            nl ()
          done
        done
      done

  end
    
