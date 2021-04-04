(* keystones_UFO_generate.ml --

   Copyright (C) 2019-2020 by

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

let qed =
  equivalent_tensors
    [| Majorana; Vector; Majorana |]
    [ ("qed", "Gamma(2,1,3)") ]

let axial =
  equivalent_tensors
    [| Majorana; Vector; Majorana |]
    [ ("axial1", "Gamma5(1,-1)*Gamma(2,-1,3)");
      ("axial2", "-Gamma(2,1,-3)*Gamma5(-3,3)") ]

let left =
  equivalent_tensors
    [| Majorana; Vector; Majorana |]
    [ ("left1", "(Identity(1,-1)+Gamma5(1,-1))*Gamma(2,-1,3)");
      ("left2", "2*ProjP(1,-1)*Gamma(2,-1,3)");
      ("left3", "Gamma(2,1,-3)*(Identity(-3,3)-Gamma5(-3,3))");
      ("left4", "2*Gamma(2,1,-3)*ProjM(-3,3)") ]

let right =
  equivalent_tensors
    [| Majorana; Vector; Majorana |]
    [ ("right1", "(Identity(1,-1)-Gamma5(1,-1))*Gamma(2,-1,3)");
      ("right2", "2*ProjM(1,-1)*Gamma(2,-1,3)");
      ("right3", "Gamma(2,1,-3)*(Identity(-3,3)+Gamma5(-3,3))");
      ("right4", "2*Gamma(2,1,-3)*ProjP(-3,3)") ]

let vector_spinor_current tag =
  { tag = Printf.sprintf "vector_spinor_current__%s_ff" tag;
    keystones =
      [ { bra = (Majorana, 0);
          name = Printf.sprintf "f_%sf" tag;
          args = [G (0); F (Vector, 1); F (Majorana, 2)] };
        { bra = (Vector, 1);
          name = Printf.sprintf "%s_ff" tag;
          args = [G (0); F (Majorana, 0); F (Majorana, 2)] } ] }

let scalar =
  equivalent_tensors
    [| Majorana; Scalar; Majorana |]
    [ ("scalar_current", "Identity(1,3)") ]

let pseudo =
  equivalent_tensors
    [| Majorana; Scalar; Majorana |]
    [ ("pseudo_current", "Gamma5(1,3)") ]

let left_scalar =
  equivalent_tensors
    [| Majorana; Scalar; Majorana |]
    [ ("left_scalar1", "Identity(1,3)-Gamma5(1,3)");
      ("left_scalar2", "2*ProjM(1,3)") ]

let right_scalar =
  equivalent_tensors
    [| Majorana; Scalar; Majorana |]
    [ ("right_scalar1", "Identity(1,3)+Gamma5(1,3)");
      ("right_scalar2", "2*ProjP(1,3)") ]

let scalar_spinor_current tag =
  { tag = Printf.sprintf "scalar_spinor_current__%s_ff" tag;
    keystones =
      [ { bra = (Majorana, 0);
          name = Printf.sprintf "f_%sf" tag;
          args = [G (0); F (Scalar, 1); F (Majorana, 2)] };
        { bra = (Scalar, 1);
          name = Printf.sprintf "%s_ff" tag;
          args = [G (0); F (Majorana, 0); F (Majorana, 2)] } ] }

let empty = { tag = "empty"; keystones = [ ] }

let vertices =
  [ (qed, vector_spinor_current "v");
    (axial, vector_spinor_current "a");
    (left, vector_spinor_current "vl");
    (right, vector_spinor_current "vr");
    (scalar, scalar_spinor_current "s");
    (pseudo, scalar_spinor_current "p");
    (left_scalar, scalar_spinor_current "sl");
    (right_scalar, scalar_spinor_current "sr");
  ]

let parse_propagator (p_tag, p_omega, p_spins, numerator, denominator) =
  let p =
    UFO.Propagator.of_propagator_UFO
      ~majorana:true
      { UFO.Propagator_UFO.name = p_tag;
        UFO.Propagator_UFO.numerator = UFOx.Lorentz.of_string numerator;
        UFO.Propagator_UFO.denominator = UFOx.Lorentz.of_string denominator } in
  { p_tag; p_omega; p_spins;
    p_propagator = p }

let default_denominator =
  "P('mu', id) * P('mu', id) - Mass(id) * Mass(id) \
   + complex(0,1) * Mass(id) * Width(id)"

let majorana_propagator =
  ( "majorana", "pr_psi", (Majorana, Majorana),
    "Gamma('mu', 1, 2) * P('mu', id) + Mass(id) * Identity(1, 2)",
    default_denominator )

let gravitino_propagator =
  ( "vectorspinor", "pr_grav", (Vectorspinor, Vectorspinor),
    "(Gamma(-1,1,2)*P(-1,id) - Mass(id)*Identity(1,2)) \
      * (Metric(1,2) - P(1,id)*P(2,id)/Mass(id)**2) \
     + 1/3 * (Gamma(1,1,-1) - P(1,id)/Mass(id)*Identity(1,-1)) \
           * (Gamma(-3,-1,-2)*P(-3,id) + Mass(id)*Identity(-1,-2)) \
           * (Gamma(2,-2,2) - P(2,id)/Mass(id)*Identity(-2,2)) ",
    default_denominator )

let gravitino_propagator =
  ( "vectorspinor", "pr_grav", (Vectorspinor, Vectorspinor),
    "(Gamma(-1,2001,2002)*P(-1,id) - Mass(id)*Identity(2001,2002)) \
      * (Metric(1001,1002) - P(1001,id)*P(1002,id)/Mass(id)**2) \
     + 1/3 * (Gamma(1001,2001,-1) - P(1001,id)/Mass(id)*Identity(2001,-1)) \
           * (Gamma(-3,-1,-2)*P(-3,id) + Mass(id)*Identity(-1,-2)) \
           * (Gamma(1002,-2,2002) - P(1002,id)/Mass(id)*Identity(-2,2002)) ",
    default_denominator )

let propagators =
  List.map
    parse_propagator
    [ majorana_propagator;
      (* [gravitino_propagator] *) ]

let _ =
  generate_ufo
    ~reps:10000 ~threshold:0.70 ~omega_module:"omega95_bispinors"
    "fusions_UFO_bispinors" vertices propagators;
  exit 0
