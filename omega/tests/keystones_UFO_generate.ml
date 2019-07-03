(* keystones_UFO_generate.ml --

   Copyright (C) 2019-2019 by

       Wolfgang Kilian <kilian@physik.uni-siegen.de>
       Thorsten Ohl <ohl@physik.uni-wuerzburg.de>
       Juergen Reuter <juergen.reuter@desy.de>

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

open Coupling
open Keystones
open Format_Fortran

type ufo_vertex =
  { ufo_tag : string;
    spins : lorentz array;
    tensor : UFOx.Lorentz.t }

module P = Permutation.Default

let permute_spins p s = P.array p s

(* We must permute only the free indices, of course.
   Note that we apply the \emph{inverse} permutation to
   the indices in order to match the permutation of the
   particles/spins. *)
let permute_structure n p l =
  let permuted = P.array (P.inverse p) (Array.init n succ) in
  let permute_index i =
    if i > 0 then
      permuted.(pred i)
    else
      i in
  UFOx.Lorentz.map_indices permute_index l

let permute_vertex n v p =
  { ufo_tag = v.ufo_tag ^ "_p" ^ P.to_string p;
    spins = permute_spins p v.spins;
    tensor = permute_structure n p v.tensor }

let vertex_permutations v =
  let n = Array.length v.spins in
  List.map (permute_vertex n v) (P.cyclic n)

let keystones_of_ufo_vertex { ufo_tag; spins } =
  { tag = ufo_tag;
    keystones =
      let fields = Array.mapi (fun i s -> (s, i)) spins in
      let n = Array.length fields in
      List.map
        (fun p ->
          let permuted = P.array p fields in
          match Array.to_list permuted with
          | [] -> invalid_arg "keystones_of_ufo_vertex"
          | ket :: args ->
             { ket = ket;
               name = ufo_tag ^ "_p" ^ P.to_string p;
               args =
                 G (0) ::
                   (ThoList.flatmap (fun (s, i) -> [ F (s, i); P (i) ]) args) })
        (P.cyclic n) }

let merge (ufo_list, omegalib) =
  match ufo_list with
  | [] -> omegalib
  | ufo1 :: _ ->
     { tag = ufo1.ufo_tag;
       keystones =
         (ThoList.flatmap
            (fun ufo -> (keystones_of_ufo_vertex ufo).keystones)
            ufo_list)
         @ omegalib.keystones }

let fusions ff module_name vertices =
  let printf fmt = fprintf ff fmt
  and nl () = pp_newline ff () in
  printf "module %s" module_name; nl ();
  printf "  use kinds"; nl ();
  printf "  use omega95"; nl ();
  printf "  implicit none"; nl ();
  printf "  ! private"; nl ();
  UFO_targets.Fortran.eps4_g4_g44_decl std_formatter ();
  UFO_targets.Fortran.eps4_g4_g44_init std_formatter ();
  printf "contains"; nl ();
  List.iter
    (fun v ->
      List.iter
        (fun v' ->
          printf "  ! %s" (String.make 68 '='); nl ();
          printf "  ! %s" (UFOx.Lorentz.to_string v'.tensor); nl ();
          UFO_targets.Fortran.lorentz
            std_formatter v'.ufo_tag v'.spins v'.tensor)
        (vertex_permutations v))
    vertices;
  printf "end module %s" module_name; nl ()

let generate ?reps ?threshold module_name vertices =
  fusions std_formatter module_name (ThoList.flatmap fst vertices);
  Keystones.generate
    ?reps ?threshold ~modules:[module_name]
    (List.map merge vertices)

let equivalent_tensors spins alternatives =
  List.map
    (fun (ufo_tag, tensor) ->
      { ufo_tag; spins; tensor = UFOx.Lorentz.of_string tensor })
    alternatives

let qed =
  equivalent_tensors
    [| ConjSpinor; Vector; Spinor |]
    [ ("qed", "Gamma(2,1,3)") ]

let axial =
  equivalent_tensors
    [| ConjSpinor; Vector; Spinor |]
    [ ("axial1", "Gamma5(1,-1)*Gamma(2,-1,3)");
      ("axial2", "-Gamma(2,1,-3)*Gamma5(-3,3)") ]

let left =
  equivalent_tensors
    [| ConjSpinor; Vector; Spinor |]
    [ ("left1", "(Identity(1,-1)+Gamma5(1,-1))*Gamma(2,-1,3)");
      ("left2", "2*ProjP(1,-1)*Gamma(2,-1,3)");
      ("left3", "Gamma(2,1,-3)*(Identity(-3,3)-Gamma5(-3,3))");
      ("left4", "2*Gamma(2,1,-3)*ProjM(-3,3)") ]

let right =
  equivalent_tensors
    [| ConjSpinor; Vector; Spinor |]
    [ ("right1", "(Identity(1,-1)-Gamma5(1,-1))*Gamma(2,-1,3)");
      ("right2", "2*ProjM(1,-1)*Gamma(2,-1,3)");
      ("right3", "Gamma(2,1,-3)*(Identity(-3,3)+Gamma5(-3,3))");
      ("right4", "2*Gamma(2,1,-3)*ProjP(-3,3)") ]

let vector_spinor_current tag =
  { tag = Printf.sprintf "vector_spinor_current__%s_ff" tag;
    keystones =
      [ { ket = (ConjSpinor, 0);
          name = Printf.sprintf "f_%sf" tag;
          args = [G (0); F (Vector, 1); F (Spinor, 2)] };
        { ket = (Vector, 1);
          name = Printf.sprintf "%s_ff" tag;
          args = [G (0); F (ConjSpinor, 0); F (Spinor, 2)] };
        { ket = (Spinor, 2);
          name = Printf.sprintf "f_f%s" tag;
          args = [G (0); F (ConjSpinor, 0); F (Vector, 1)] } ] }

let fermi_ss =
  equivalent_tensors
    [| ConjSpinor; Spinor; ConjSpinor; Spinor |]
    [ ("fermi_ss", "Identity(1,2)*Identity(3,4)");
      ("fermi_ss_f",
       "   (1/4) * Identity(1,4)*Identity(3,2)" ^
       " + (1/4) * Gamma(-1,1,4)*Gamma(-1,3,2)" ^
       " + (1/8) * Sigma(-1,-2,1,4)*Sigma(-1,-2,3,2)" ^
       " - (1/4) * Gamma(-1,1,-4)*Gamma5(-4,4)*Gamma(-1,3,-2)*Gamma5(-2,2)" ^
       " + (1/4) * Gamma5(1,4)*Gamma5(3,2)") ]

let fermi_vv =
  equivalent_tensors
    [| ConjSpinor; Spinor; ConjSpinor; Spinor |]
    [ ("fermi_vv", "Gamma(-1,1,2)*Gamma(-1,3,4)");
      ("fermi_vv_f",
       "           Identity(1,4)*Identity(3,2)" ^
       " - (1/2) * Gamma(-1,1,4)*Gamma(-1,3,2)" ^
       " - (1/2) * Gamma(-1,1,-4)*Gamma5(-4,4)*Gamma(-1,3,-2)*Gamma5(-2,2)" ^
       " -         Gamma5(1,4)*Gamma5(3,2)") ]

let fermi_tt =
  equivalent_tensors
    [| ConjSpinor; Spinor; ConjSpinor; Spinor |]
    [ ("fermi_tt1", "   Sigma(-1,-2,1,2)*Sigma(-1,-2,3,4)");
      ("fermi_tt2", " - Sigma(-1,-2,1,2)*Sigma(-2,-1,3,4)");
      ("fermi_tt3", " - Sigma(-2,-1,1,2)*Sigma(-1,-2,3,4)");
      ("fermi_tt_f",
       "   3     * Identity(1,4)*Identity(3,2)" ^
       " - (1/2) * Sigma(-1,-2,1,4)*Sigma(-1,-2,3,2)" ^
       " + 3     * Gamma5(1,4)*Gamma5(3,2)") ]

let fermi_aa =
  equivalent_tensors
    [| ConjSpinor; Spinor; ConjSpinor; Spinor |]
    [ ("fermi_aa", "Gamma5(1,-2)*Gamma(-1,-2,2)*Gamma5(3,-3)*Gamma(-1,-3,4)");
      ("fermi_aa_f",
       " -         Identity(1,4)*Identity(3,2)" ^
       " - (1/2) * Gamma(-1,1,4)*Gamma(-1,3,2)" ^
       " - (1/2) * Gamma(-1,1,-4)*Gamma5(-4,4)*Gamma(-1,3,-2)*Gamma5(-2,2)" ^
       " +         Gamma5(1,4)*Gamma5(3,2)") ]

let fermi_pp =
  equivalent_tensors
    [| ConjSpinor; Spinor; ConjSpinor; Spinor |]
    [ ("fermi_pp", "Gamma5(1,2)*Gamma5(3,4)");
      ("fermi_pp_f",
       "   (1/4) * Identity(1,4)*Identity(3,2)" ^
       " - (1/4) * Gamma(-1,1,4)*Gamma(-1,3,2)" ^
       " + (1/8) * Sigma(-1,-2,1,4)*Sigma(-1,-2,3,2)" ^
       " + (1/4) * Gamma(-1,1,-4)*Gamma5(-4,4)*Gamma(-1,3,-2)*Gamma5(-2,2)" ^
       " + (1/4) * Gamma5(1,4)*Gamma5(3,2)") ]

let fermi_ll =
  equivalent_tensors
    [| ConjSpinor; Spinor; ConjSpinor; Spinor |]
    [ ("fermi_ll",   "   Gamma(-1,1,-2)*ProjM(-2,2)*Gamma(-1,3,-4)*ProjM(-4,4)");
      ("fermi_ll_f", " - Gamma(-1,1,-2)*ProjM(-2,4)*Gamma(-1,3,-4)*ProjM(-4,2)") ]

let fermi_va =
  equivalent_tensors
    [| ConjSpinor; Spinor; ConjSpinor; Spinor |]
    [ ("fermi_va", "Gamma(-1,1,2)*Gamma5(3,-3)*Gamma(-1,-3,4)") ]

let fermi_av =
  equivalent_tensors
    [| ConjSpinor; Spinor; ConjSpinor; Spinor |]
    [ ("fermi_av", "Gamma5(1,-2)*Gamma(-1,-2,2)*Gamma(-1,3,4)") ]

let sqed =
  equivalent_tensors
    [| Scalar; Vector; Scalar |]
    [ ("sqed1", "P(2,3)-P(2,1)");
      ("sqed2", "2*P(2,3)+P(2,2)");
      ("sqed3", "-P(2,2)-2*P(2,1)") ]

let vector_scalar_current =
  { tag = "vector_scalar_current__v_ss";
    keystones =
      [ { ket = (Vector, 1);
          name = "v_ss";
          args = [G (0); F (Scalar, 2); P (2); F (Scalar, 0); P (0)] };
        { ket = (Scalar, 0);
          name = "s_vs";
          args = [G (0); F (Vector, 1); P (1); F (Scalar, 2); P (2)] } ] }

let svv_t =
  equivalent_tensors
    [| Scalar; Vector; Vector |]
    [ ("svv_t", "P(-1,2)*P(-1,3)*Metric(2,3)-P(2,3)*P(3,2)") ]

let scalar_vector_current tag =
  { tag = Printf.sprintf "transversal_vector_current__s_vv_%s" tag;
    keystones = [ { ket = (Scalar, 0);
                    name = Printf.sprintf "s_vv_%s" tag;
                    args = [G (0); F (Vector, 1); P (1); F (Vector, 2); P (2)] };
                  { ket = (Vector, 1);
                    name = Printf.sprintf "v_sv_%s" tag;
                    args = [G (0); F (Scalar, 0); P (0); F (Vector, 2); P (2)] } ] }

let gauge =
  equivalent_tensors
    [| Vector; Vector; Vector |]
    [ ("gauge", "   Metric(1,2)*P(3,1) - Metric(1,2)*P(3,2) \
                  + Metric(3,1)*P(2,3) - Metric(3,1)*P(2,1) \
                  + Metric(2,3)*P(1,2) - Metric(2,3)*P(1,3)") ]

let gauge_omega =
  { tag = "g_gg";
    keystones =
      [ { ket = (Vector, 0);
          name = "(0,1)*g_gg";
          args = [G (0); F (Vector, 1); P (1); F (Vector, 2); P (2)] } ] }

let empty = { tag = "empty"; keystones = [ ] }

let vertices =
  [ (qed, vector_spinor_current "v");
    (axial, vector_spinor_current "a");
    (left, vector_spinor_current "vl");
    (right, vector_spinor_current "vr");
    (sqed, vector_scalar_current);
    (fermi_ss, empty);
    (fermi_vv, empty);
    (fermi_tt, empty);
    (fermi_aa, empty);
    (fermi_pp, empty);
    (fermi_ll, empty);
    (fermi_va, empty);
    (fermi_av, empty);
    (svv_t, scalar_vector_current "t");
    (gauge, gauge_omega) ]

let _ =
  generate ~reps:10000 ~threshold:0.70 "fusions" vertices;
  exit 0
