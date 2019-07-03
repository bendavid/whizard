(* keystones.ml --

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

type field = lorentz * int

type argument =
  | G of int (* coupling *)
  | P of int (* momentum *)
  | F of field (* field *)

type keystone =
  { ket : field;
    name : string;
    args : argument list }

type vertex =
  { tag : string;
    keystones : keystone list }

let order_fields (_, i) (_, j) =
  compare i j

let extract_fields { ket; args } =
  List.sort
    order_fields
    (List.fold_left
       (fun acc arg ->
         match arg with
         | F f -> f :: acc
         | _ -> acc)
       [ket] args)

let extract_momenta { args } =
  List.sort
    compare
    (List.fold_left
       (fun acc arg ->
         match arg with
         | P i -> i :: acc
         | _ -> acc)
       [] args)

let extract_couplings { args } =
  List.sort
    compare
    (List.fold_left
       (fun acc arg ->
         match arg with
         | G i -> i :: acc
         | _ -> acc)
       [] args)

let check_indices field_list =
  if List.exists
       (fun (n, _) -> n > 1)
       (ThoList.classify (List.map snd field_list)) then
    invalid_arg "check_indices";
  ()

let spin_to_string = function
  | Scalar -> "Scalar"
  | Spinor -> "Spinor"
  | ConjSpinor -> "ConjSpinor"
  | Majorana -> "Majorana"
  | Vector | Massive_Vector -> "Vector"
  | _ -> failwith "spin_to_string"

let fields_to_string fields =
  "[" ^
    String.concat
      "; " (List.map
              (fun (s, i) -> Printf.sprintf "%s(%d)" (spin_to_string s) i)
              fields) ^ "]"

let check_fields ks_list =
  let fields = List.map extract_fields ks_list in
  if not (ThoList.homogeneous fields) then
    begin
      let spins =
        "[" ^ String.concat "; " (List.map fields_to_string fields) ^ "]" in
      invalid_arg ("check_spins: " ^ spins)
    end;
  check_indices (List.hd fields)

open Format_Fortran

let spin_type = function
  | Scalar -> "complex(kind=default)"
  | Spinor -> "type(spinor)"
  | ConjSpinor -> "type(conjspinor)"
  | Majorana -> "type(bispinor)"
  | Vector | Massive_Vector -> "type(vector)"
  | _ -> failwith "spin_type"

let type_arg = function
  | G _ -> "complex(kind=default)"
  | P _ -> "type(momentum)"
  | F (s, _) -> spin_type s

let spin_mnemonic = function
  | Scalar -> "phi"
  | Spinor -> "psi"
  | ConjSpinor -> "psibar"
  | Majorana -> "chi"
  | Maj_Ghost -> "???"
  | Vector -> "a"
  | Massive_Vector -> "v"
  | _ -> failwith "spin_mnemonic"

let format_coupling i =
  Printf.sprintf "g%d" i

let format_momentum i =
  Printf.sprintf "p%d" i

let format_field (s, i) =
  Printf.sprintf "%s%d" (spin_mnemonic s) i

let format_arg = function
  | G i -> format_coupling i
  | P i -> format_momentum i
  | F f -> format_field f

let fusion_to_fortran ff name args =
  let printf fmt = fprintf ff fmt in
  match args with
  | [] -> invalid_arg "fusion_to_fortran"
  | arg1 :: arg2n ->
     printf "%s (%s" name (format_arg arg1);
     List.iter (fun arg -> printf ",@ %s" (format_arg arg)) arg2n;
     printf ")"

let keystone_to_fortran ff (ksv, { ket; name; args }) =
  let printf fmt = fprintf ff fmt
  and nl = pp_newline ff in
  printf "      @[<2>%s =@ " ksv;
  begin match ket with
  | Spinor, _ ->
     fusion_to_fortran ff name args;
     printf "@ * %s" (format_field ket)
  | _, _ -> 
     printf "%s@ * " (format_field ket);
     fusion_to_fortran ff name args
  end;
  printf "@]"; nl()

let keystones_to_subroutine ff { tag; keystones } =
  check_fields keystones;
  let printf fmt = fprintf ff fmt
  and nl = pp_newline ff in
  printf "  @[<4>subroutine@ testks_%s@ (repetitions," tag;
  printf "@ passed,@ threshold,@ quiet,@ abs_threshold)@]"; nl ();
  printf "    integer, intent(in) :: repetitions"; nl ();
  printf "    logical, intent(inout) :: passed"; nl ();
  printf "    logical, intent(in), optional :: quiet"; nl ();
  printf "    @[<2>real(kind=default),@ intent(in),@ optional ::";
  printf "@ threshold,@ abs_threshold@]"; nl ();
  printf "    integer :: i"; nl ();
  let ks1 = List.hd keystones in
  let all_momenta =
    List.map
      (fun i -> P i)
      (ThoList.range 0 (List.length (extract_fields ks1) - 1)) in
  let variables =
    ThoList.uniq (List.sort compare (F (ks1.ket) :: ks1.args @ all_momenta)) in
  List.iter
    (fun a ->
      printf "    @[<2>%s :: %s@]" (type_arg a) (format_arg a); nl ())
    variables;
  let ks_list =
    List.map
      (fun (n, ks) -> (Printf.sprintf "ks%d" n, ks))
      (ThoList.enumerate 0 keystones) in
  begin match ks_list with
  | [] -> failwith "keystones_to_fortran"
  | (ksv1, _) :: ks2n ->
     printf "    @[<2>complex(kind=default) ::@ %s" ksv1;
     List.iter (fun (ksv, _) -> printf ",@ %s" ksv) ks2n;
     printf "@]"; nl ()
  end;
  printf "    do i = 1, repetitions"; nl ();
  List.iter
    (fun a ->
      match a with
      | P 0 -> () (* this will be determined by momentum conservation! *)
      | a ->
         printf "      @[<2>call@ make_random@ (%s)@]" (format_arg a); nl ())
    variables;
  begin match all_momenta with
  | [] -> failwith "keystones_to_fortran"
  | p1 :: p2n ->
     printf "      @[<2>%s =" (format_arg p1);
     List.iter (fun p -> printf "@ - %s" (format_arg p)) p2n;
     printf "@]"; nl ()
  end;
  List.iter (keystone_to_fortran ff) ks_list;
  begin match ks_list with
  | [] -> failwith "keystones_to_fortran"
  | (ksv1, ks1) :: ks2n ->
     List.iter
       (fun (ksv, ks) ->
         printf "      @[<8>call@ expect@ (%s,@ %s," ksv ksv1;
         printf "@ '%s: %s <> %s'," tag ks.name ks1.name;
         printf "@ passed,@ threshold, quiet, abs_threshold)@]";
         nl ())
       ks2n
  end;
  printf "    end do"; nl ();
  printf "  @[<2>end@ subroutine@ testks_%s@]" tag; nl ()

let keystones_to_fortran
      ff ?(reps=1000) ?(threshold=0.85)
      ?(modules=[]) vertices =
  let printf fmt = fprintf ff fmt
  and nl = pp_newline ff in
  printf "program keystones_omegalib_demo"; nl ();
  List.iter
    (fun m -> printf "  use %s" m; nl ())
    ("kinds" :: "constants" :: "omega95" ::
       "omega_testtools" :: "keystones_tools" :: modules);
  printf "  implicit none"; nl ();
  printf "  logical :: passed"; nl ();
  printf "  logical, parameter :: quiet = .false."; nl ();
  printf "  integer, parameter :: reps = %d" reps; nl ();
  printf "  real(kind=default), parameter :: threshold = %f" threshold; nl ();
  printf "  real(kind=default), parameter :: abs_threshold = 1E-17"; nl ();
  printf "  integer, dimension(8) :: date_time"; nl ();
  printf "  integer :: rsize"; nl ();
  printf "  call date_and_time (values = date_time)"; nl ();
  printf "  call random_seed (size = rsize)"; nl ();
  printf "  @[<8>call random_seed@ (put = spread (product (date_time),";
  printf "@ dim = 1,@ ncopies = rsize))@]"; nl ();
  printf "  passed = .true."; nl ();
  List.iter
    (fun v ->
      printf "  @[<8>call testks_%s@ (reps,@ passed," v.tag;
      printf "@ threshold, quiet, abs_threshold)@]"; nl ())
    vertices;
  printf "  if (passed) then"; nl ();
  printf "    stop 0"; nl ();
  printf "  else"; nl ();
  printf "    stop 1"; nl ();
  printf "  end if"; nl ();
  printf "contains"; nl ();
  List.iter (keystones_to_subroutine ff) vertices;
  printf "end program keystones_omegalib_demo"; nl ()

let vector_spinor_current tag =
  { tag = Printf.sprintf "vector_spinor_current__%s_ff" tag;
    keystones = [ { ket = (ConjSpinor, 0);
                    name = Printf.sprintf "f_%sf" tag;
                    args = [G (0); F (Vector, 1); F (Spinor, 2)] };
                  { ket = (Vector, 1);
                    name = Printf.sprintf "%s_ff" tag;
                    args = [G (0); F (ConjSpinor, 0); F (Spinor, 2)] };
                  { ket = (Spinor, 2);
                    name = Printf.sprintf "f_f%s" tag;
                    args = [G (0); F (ConjSpinor, 0); F (Vector, 1)] } ] }

let scalar_spinor_current tag =
  { tag = Printf.sprintf "scalar_spinor_current__%s_ff" tag;
    keystones = [ { ket = (ConjSpinor, 0);
                    name = Printf.sprintf "f_%sf" tag;
                    args = [G (0); F (Scalar, 1); F (Spinor, 2)] };
                  { ket = (Scalar, 1);
                    name = Printf.sprintf "%s_ff" tag;
                    args = [G (0); F (ConjSpinor, 0); F (Spinor, 2)] };
                  { ket = (Spinor, 2);
                    name = Printf.sprintf "f_f%s" tag;
                    args = [G (0); F (ConjSpinor, 0); F (Scalar, 1)] } ] }

(* NB: the vertex is anti-symmetric in the scalars and we need to
   use a cyclic permutation. *)
let vector_scalar_current =
  { tag = "vector_scalar_current__v_ss";
    keystones = [ { ket = (Vector, 0);
                    name = "v_ss";
                    args = [G (0); F (Scalar, 1); P (1); F (Scalar, 2); P (2)] };
                  { ket = (Scalar, 2);
                    name = "s_vs";
                    args = [G (0); F (Vector, 0); P (0); F (Scalar, 1); P (1)] } ] }

let scalar_vector_current tag =
  { tag = Printf.sprintf "transversal_vector_current__s_vv_%s" tag;
    keystones = [ { ket = (Scalar, 0);
                    name = Printf.sprintf "s_vv_%s" tag;
                    args = [G (0); F (Vector, 1); P (1); F (Vector, 2); P (2)] };
                  { ket = (Vector, 1);
                    name = Printf.sprintf "v_sv_%s" tag;
                    args = [G (0); F (Scalar, 0); P (0); F (Vector, 2); P (2)] } ] }

let vertices =
  List.concat
    [ List.map vector_spinor_current ["v"; "a"; "vl"; "vr"];
      List.map scalar_spinor_current ["s"; "p"; "sl"; "sr"];
      [ vector_scalar_current ];
      List.map scalar_vector_current ["t"; "6D"; "6DP"] ]

let generate ?(reps=1000) ?(threshold=0.85) ?modules vertices =
  let my_name = Sys.argv.(0) in
  let verbose = ref false
  and cat = ref false
  and usage = "usage: " ^ my_name ^ " ..." in
  Arg.parse
    (Arg.align 
       [ ("-cat", Arg.Set cat, " print test snippets");
	 ("-v", Arg.Set verbose, " be more verbose");
	 ("-verbose", Arg.Set verbose, " be more verbose") ])
    (fun s -> raise (Arg.Bad s))
    usage;
  if !cat then
    keystones_to_fortran std_formatter ~reps ~threshold ?modules vertices
