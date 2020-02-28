(* count.ml --

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

open Num

(* Factorial and double factorial for big integers.  *)

let rec factorial' fn n =
  if sign_num n <= 0 then
    fn
  else
    factorial' (n */ fn) (pred_num n)
          
let factorial n =
      factorial' (Int 1) n

let rec dfactorial' fn n =
  if sign_num n <= 0 then
    fn
  else
    dfactorial' (n */ fn) (n -/ (Int 2))

let dfactorial n =
  dfactorial' (Int 1) n

(* \thocwmodulesection{[Binary]: $\lambda\phi^3$} *)

module B =
  struct

    module T = Topology.Binary

    let partition_to_string p =
      "(" ^ String.concat ","
              (List.map string_of_int (T.inspect_partition p)) ^ ")"

    let print_partitions n =
      for i = 4 to n do
        Printf.printf "%d -> %s\n"
          i (String.concat ", "
               (List.map partition_to_string (T.partitions i)))
      done

(* See equation~(\ref{eq:S(1,2,3)}): *)

    let symmetry n1 n2 n3 =
      if n1 = n2 && n2 = n3 then
        Int 6
      else if n1 = n2 && n3 = 2 * n1 then
        Int 4
      else if n1 = n2 || n2 = n3 then
        Int 2
      else if n3 = n1 + n2 then
        Int 2
      else
        Int 1

    let trees n =
      dfactorial (n +/ n -/ (Int 5))

    let number p =
      match T.inspect_partition p with
      | [n1'; n2'; n3'] ->
          let n1 = Int n1' and n2 = Int n2' and n3 = Int n3' in
          factorial (n1 +/ n2 +/ n3)
            */ trees (succ_num n1) */ trees (succ_num n2) */ trees (succ_num n3)
            // factorial n1 // factorial n2 // factorial n3 // symmetry n1' n2' n3'
      | _ -> invalid_arg "B.number"
        
    let partition_sum n =
      List.fold_left (fun sum n' -> number n' +/ sum) (Int 0) (T.partitions n)

    let partition_count n =
      Printf.sprintf "%s*%s" (string_of_num (number n)) (partition_to_string n)

    let print_symmetry n =
      for i = 4 to n do
        let p = partition_sum i in
        Printf.printf "%d -> %s %s = %s\n" i (string_of_num p)
          (if compare_num p (trees (Int i)) = 0 then "(OK)" else "???")
          (String.concat " + " (List.map partition_count (T.partitions i)))
      done

    let print_diagrams n =
      for i = 4 to n do
        Printf.printf "  %d & %s & %s \\\\\n" i
          (string_of_num (power_num (Int 2) (pred_num (Int i)) -/ Int (i + 1)))
          (string_of_num (trees (Int i)))
      done

  end

(* \thocwmodulesection{[Nary]: $\sum_n\lambda_n\phi^n$} *)

module N =
  struct

    module I =
      struct
        type t = num
        let zero = num_of_int 0
        let one = num_of_int 1
        let ( + ) = add_num
        let ( - ) = sub_num
        let ( * ) = mult_num
        let ( / ) = quo_num
        let pred = pred_num
        let succ = succ_num
        let ( = ) = ( =/ )
        let ( <> ) = ( <>/ )
        let ( < ) = ( </ )
        let ( <= ) = ( <=/ )
        let ( > ) = ( >/ )
        let ( >= ) = ( >=/ )
        let of_int = num_of_int
        let to_int = int_of_num
        let to_string = string_of_num
        let compare = compare_num
        let factorial = factorial
      end

    let max_degree = 6

    module C = Topology.Count(I)
    module T = Topology.Nary(struct let max_arity = fun () -> pred max_degree end)

    let partition_to_string p =
      "(" ^ String.concat ","
              (List.map string_of_int (T.inspect_partition p)) ^ ")"

    let print_partitions n =
      for i = 4 to n do
        Printf.printf "%d -> %s\n"
          i (String.concat ", "
               (List.map partition_to_string (T.partitions i)))
      done

    let partition_count p0 =
      let p = List.map I.of_int (T.inspect_partition p0)
      and d = I.of_int max_degree in
      I.to_string ((C.diagrams_per_keystone d p) */ (C.keystones p)) ^ "*" ^
      partition_to_string p0

    let print_symmetry n =
      let d = I.of_int max_degree in
      for i = 4 to n do
        let i' = I.of_int i in
        let count = C.diagrams d i' in
        Printf.printf "%d -> %s %s = %s\n" i (I.to_string count)
          (if count =/ C.diagrams_via_keystones d i' then
            "(OK)"
          else
            "???")
          (String.concat " + " (List.map partition_count (T.partitions i)))
      done

    let print_symmetries n =
      let l = ThoList.range 1 n in
      List.iter (fun p ->
        let p = T.inspect_partition p in
        let n = List.length (Combinatorics.keystones p l)
        and n' = I.to_int (C.keystones (List.map I.of_int p))
        and name = String.concat "," (List.map string_of_int p) in
        if n = n' then
          Printf.printf "(%s): %d (OK)\n" name n
        else
          Printf.printf "(%s): %d != %d\n" name n n')
        (T.partitions n)

  end

(* \thocwmodulesection{Main Program} *)

let _ =
  let usage = "usage: " ^ Sys.argv.(0) ^ " [options]" in
  Arg.parse
    ["-d", Arg.Int B.print_diagrams, "diagrams";
     "-p", Arg.Int B.print_partitions, "partitions";
     "-P", Arg.Int N.print_partitions, "partitions";
     "-s", Arg.Int B.print_symmetry, "symmetry";
     "-S", Arg.Int N.print_symmetry, "symmetry";
     "-X", Arg.Int N.print_symmetries, "symmetry"]
    (fun _ -> print_endline usage; exit 1)
    usage;
  exit 0

(*i

(* \begin{dubious}
     [Numerix.Slong] appears to be \emph{slower} here \ldots
   \end{dubious} *)

module BI =
  struct
    open Numerix.Slong
    type t = Numerix.Slong.t
    let zero = of_int 0
    let one = of_int 1
    let ( + ) = add
    let ( - ) = sub
    let ( * ) = mul
    let ( / ) = quo
    let pred n = sub_1 n 1
    let succ n = add_1 n 1
    let ( = ) = eq
    let ( <> ) = neq
    let ( < ) = inf
    let ( <= ) = infeq
    let ( > ) = sup
    let ( >= ) = supeq
    let of_int = of_int
    let to_int = int_of
    let to_string = string_of
    let compare = cmp
    let rec factorial' fn n =
      if infeq_1 n 0 then
        fn
      else
        factorial' (n * fn) (pred n)
    let factorial n =
      factorial' (of_int 1) n
  end
i*)

(*i
 *  Local Variables:
 *  mode:caml
 *  indent-tabs-mode:nil
 *  page-delimiter:"^(\\* .*\n"
 *  End:
i*)
