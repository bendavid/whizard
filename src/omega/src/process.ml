(* $Id: process.ml 2468 2010-05-05 16:37:03Z kilian $

   Copyright (C) 1999-2010 by

       Wolfgang Kilian <kilian@hep.physik.uni-siegen.de>
       Thorsten Ohl <ohl@physik.uni-wuerzburg.de>
       Juergen Reuter <juergen.reuter@physik.uni-freiburg.de>

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

module type T =
  sig
    type flavor
    type t = flavor list * flavor list
    val incoming : t -> flavor list
    val outgoing : t -> flavor list
    type decay
    val parse_decay : string -> decay
    val expand_decays : decay list -> t list
    type scattering
    val parse_scattering : string -> scattering
    val expand_scatterings : scattering list -> t list
    type any
    type process = Any of any | Decay of decay | Scattering of scattering
    val parse_process : string -> process
    val remove_duplicate_final_states : int list list -> t list -> t list
    val diff : t list -> t list -> t list
  end

module Make (M : Model.T) =
  struct

    type flavor = M.flavor

    type t = flavor list * flavor list

    let incoming (fin, _ ) = fin
    let outgoing (_, fout) = fout

(* \thocwmodulesection{Parsing Process Descriptions} *)

    type 'a bag = 'a list

    type any = flavor bag list
    type decay = flavor bag * flavor bag list
    type scattering = flavor bag * flavor bag * flavor bag list

    type process =
      | Any of any
      | Decay of decay
      | Scattering of scattering

    let unique_flavors f_bags =
      List.for_all (function [f] -> true | _ -> false) f_bags

    let unique_final_state = function 
      | Any fs -> unique_flavors fs
      | Decay (_, fs) -> unique_flavors fs
      | Scattering (_, _, fs) -> unique_flavors fs

    let parse_process process =
      let last = String.length process - 1
      and flavor off len = M.flavor_of_string (String.sub process off len) in

      let add_flavors flavors = function
        | Any l ->  Any (List.rev flavors :: l)
        | Decay (i, f) -> Decay (i, List.rev flavors :: f)
        | Scattering (i1, i2, f) -> Scattering (i1, i2, List.rev flavors :: f) in

      let rec scan_list so_far n =
        if n > last then
          so_far
        else
          let n' = succ n in
          match process.[n] with
          | ' ' | '\n' -> scan_list so_far n'
          | '-' -> scan_gtr so_far n'
          | c -> scan_flavors so_far [] n n'

      and scan_flavors so_far flavors w n =
        if n > last then
          add_flavors (flavor w (last - w + 1) :: flavors) so_far
        else
          let n' = succ n in
          match process.[n] with
          | ' ' | '\n' ->
              scan_list (add_flavors (flavor w (n - w) :: flavors) so_far) n'
          | ':' -> scan_flavors so_far (flavor w (n - w) :: flavors) n' n'
          | _ -> scan_flavors so_far flavors w n'

      and scan_gtr so_far n =
        if n > last then
          invalid_arg "expecting `>'"
        else
          let n' = succ n in
          match process.[n] with
          | '>' ->
	      begin match so_far with
	      | Any [i] ->  scan_list (Decay (i, [])) n'
	      | Any [i2; i1] ->  scan_list (Scattering (i1, i2, [])) n'
	      | Any _ -> invalid_arg "only 1 or 2 particles in |in>"
	      | _ -> invalid_arg "too many `->'s"
	      end
          | _ -> invalid_arg "expecting `>'" in

      match scan_list (Any []) 0 with
      | Any l -> Any (List.rev l)
      | Decay (i, f) -> Decay (i, List.rev f)
      | Scattering (i1, i2, f) -> Scattering (i1, i2, List.rev f)

    let parse_decay process =
      match parse_process process with
      | Any (i :: f) ->
          prerr_endline "missing `->' in process description, assuming decay.";
          (i, f)
      | Decay (i, f) -> (i, f)
      | _ -> invalid_arg "expecting decay description: got scattering"

    let parse_scattering process =
      match parse_process process with
      | Any (i1 :: i2 :: f) ->
          prerr_endline "missing `->' in process description, assuming scattering.";
          (i1, i2, f)
      | Scattering (i1, i2, f) -> (i1, i2, f)
      | _ -> invalid_arg "expecting scattering description: got decay"

    let expand_scatterings scatterings =
      ThoList.flatmap
        (function (fin1, fin2, fout) ->
          Product.list
            (function
              | fin1' :: fin2' :: fout' -> ([fin1'; fin2'], fout')
              | [_] | [] -> failwith "Omega.expand_scatterings: can't happen")
            (fin1 :: fin2 :: fout)) scatterings
    
    let expand_decays decays =
      ThoList.flatmap
        (function (fin, fout) ->
          Product.list
            (function
              | fin' :: fout' -> ([fin'], fout')
              | [] -> failwith "Omega.expand_decays: can't happen")
            (fin :: fout)) decays

(* \thocwmodulesection{Remove Duplicate Final States} *)

    let rec homogeneous_final_state = function
      | [] | [_] -> true
      | (_, fs1) :: ((_, fs2) :: _ as rest) ->
          if fs1 <> fs2 then
            false
          else
            homogeneous_final_state rest

    let by_color f1 f2 =
      let c = Color.compare (M.color f1) (M.color f2) in
      if c <> 0 then
        c
      else
        compare f1 f2
          
    module Pre_Bundle =
      struct

        type elt = t
        type base = elt
              
        let compare_elt (fin1, fout1) (fin2, fout2) =
          let c = ThoList.compare ~cmp:by_color fin1 fin2 in
          if c <> 0 then
            c
          else
            ThoList.compare ~cmp:by_color fout1 fout2

        let compare_base b1 b2 = compare_elt b2 b1

      end

    module Process_Bundle = Bundle.Dyn (Pre_Bundle)

    let to_string (fin, fout) =
      String.concat " " (List.map M.flavor_to_string fin)
      ^ " -> " ^ String.concat " " (List.map M.flavor_to_string fout)

    let fiber_to_string (base, fiber) =
      (to_string base) ^ " -> [" ^
      (String.concat ", " (List.map to_string fiber)) ^ "]"
                                                            
    let bundle_to_strings list =
      List.map fiber_to_string list

    let shift_left_pred' n index_set =
      List.fold_right
        (fun i acc -> let i' = i - n - 1 in if i' < 0 then acc else i' :: acc)
        index_set []
        
    let shift_left_pred fin index_sets =
      let n = match fin with [_] -> 1 | [_;_] -> 2 | _ -> 0 in
      List.fold_right
        (fun iset acc ->
          match shift_left_pred' n iset with
          | [] -> acc
          | iset' -> iset' :: acc)
        index_sets []
        
    let remove_duplicate_final_states partition = function 
      | [] -> []
      | [process] -> [process]
      | list ->
          if homogeneous_final_state list then
            list
          else
            let pi (fin, fout) =
              (fin,
               ThoList.partitioned_sort by_color (shift_left_pred fin partition) fout) in
            Process_Bundle.base (Process_Bundle.of_list pi list)

(*i
    let remove_duplicate_final_states list =
      List.iter (fun (fin, fout) -> 
        Printf.eprintf ">>> %s\n" (process_to_string fin fout)) list;
      let result = remove_duplicate_final_states list in
      List.iter (fun (fin, fout) -> 
        Printf.eprintf "<<< %s\n" (process_to_string fin fout)) result;
      result
i*)

    type t' = t
    module PSet = Set.Make (struct type t = t' let compare = compare end)

    let set list =
      List.fold_right PSet.add list PSet.empty

    let diff list1 list2 =
      PSet.elements (PSet.diff (set list1) (set list2))

  end

(*i
 *  Local Variables:
 *  mode:caml
 *  indent-tabs-mode:nil
 *  page-delimiter:"^(\\* .*\n"
 *  End:
i*)
