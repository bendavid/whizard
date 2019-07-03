(* test_linalg.ml --

   Copyright (C) 1999-2019 by

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

let random_vector n =
  Array.init n (fun _ -> Random.float 1.0)

let random_matrix n =
  Array.init n (fun _ -> random_vector n)

let infty_metric a b : float =
  let d = ref (abs_float (a.(0) -. b.(0))) in
  for i = 1 to Array.length a - 1 do
    d := max !d (abs_float (a.(i) -. b.(i)))
  done;
  !d

let infty_metric2 a b : float =
  let d = ref (infty_metric a.(0) b.(0)) in
  for i = 1 to Array.length a - 1 do
    d := max !d (infty_metric a.(i) b.(i))
  done;
  !d

let test_lu_decompostion n =
  let a = random_matrix n in
  let l, u = Linalg.lu_decompose a in
  infty_metric2 (Linalg.matmul l u) a

let test_solve n =
  let a = random_matrix n
  and b = random_vector n in
  let x = Linalg.solve a b in
  infty_metric (Linalg.matmulv a x) b

let _ =
  let usage = "usage: " ^ Sys.argv.(0) ^ " [options]" in
  Arg.parse
    [ "-lu", Arg.Int (fun n ->
      Printf.printf "|L*U-A|_infty = %g\n" (test_lu_decompostion n)),
      "test LU decomposition";
      "-s", Arg.Int (fun n ->
      Printf.printf "|A*x-b|_infty = %g\n" (test_solve n)),
      "test solve" ]
    (fun _ -> print_endline usage; exit 1)
    usage;
  exit 0

(*i
 *  Local Variables:
 *  mode:caml
 *  indent-tabs-mode:nil
 *  page-delimiter:"^(\\* .*\n"
 *  End:
i*)
