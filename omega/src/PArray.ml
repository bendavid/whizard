(* PArray.ml --

   Copyright (C) 2022-2024 by

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


(* \begin{dubious}
     The [Map] based implementation has the drawback that the polymorphic
     [compare] and [(=)] will occasionally report two [PArray.t] as
     different even if they describe the same array.  Options
     \begin{enumerate}
       \item Replace [compare] by specific functions everywhere.  This is
         the preferred approach, but can become very tedious.
       \item Replace [Map] by sorted association lists.
     \end{enumerate}
   \end{dubious} *) 

(* \thocwmodulesection{Maps} *)
module Maps =
  struct

    module IMap = Map.Make(Int)

    type 'a t = 'a IMap.t

    let _empty = IMap.empty
    let is_empty = IMap.is_empty
    let _map = IMap.map
    let _add = IMap.add
    let remove = IMap.remove
    let get_opt = IMap.find_opt

    let min_key map = fst (IMap.min_binding map)
    let max_key map = fst (IMap.max_binding map)

    let index_base = 0

    let to_option_list map =
      if IMap.is_empty map then
        []
      else if min_key map < index_base then
        invalid_arg "PArray.Maps.to_option_list"
      else
        let rec to_option_list' acc n =
          if n < index_base then
            acc
          else
            to_option_list' (get_opt n map :: acc) (pred n) in
        to_option_list' [] (max_key map)

    let _to_string a2s map =
      match to_option_list map with
      | [] -> "[]"
      | [None] -> "?"
      | [Some a] -> a2s a
      | pairs -> ThoList.to_string (function None -> "?" | Some a -> a2s a) pairs

    let _of_pairs pairs =
      List.fold_right
        (fun (k, v) map ->
          if k < index_base then
            invalid_arg "PArray.Maps.of_pairs"
          else
            IMap.add k v map)
        pairs IMap.empty

    let _to_pairs = IMap.bindings

    let _compare = IMap.compare
    let _equal = IMap.equal

    type ('a, 'b) taken =
      | Nothing of 'b t
      | Single of int * 'a * 'b t
      | Multiple of int * 'a * 'a t

    let _take_one project_opt parray =
      let select k v =
        match project_opt k v with
        | Some _ -> false
        | None -> true
      and project k v =
        match project_opt k v with
        | Some v' -> v'
        | None -> failwith "PArray.Maps.take_one: impossible" in
      let matches, other = IMap.partition select parray in
      match IMap.choose_opt matches with
      | None -> Nothing (IMap.mapi project parray)
      | Some (k, v) ->
         let more_matches = remove k matches in
         if is_empty more_matches then
           Single (k, v, IMap.mapi project other)
         else
           Multiple (k, v, IMap.fold IMap.add more_matches other)

  end

(* \thocwmodulesection{Association Lists} *)

(* We assume that the lists are short and use non tail recursive implementations
   if they are faster. *)

module Alists =
  struct

    type 'a t = (int * 'a) list

    let empty = []

    let is_empty = function
      | [] -> true
      | _ -> false

    let map f parray =
      List.map (fun (i, a) -> (i, f a)) parray

    let rec add i a = function
      | [] -> [(i, a)]
      | (i', _ as ia') :: tail as alist ->
         if i' = i then
           (i, a) :: tail
         else if  i' > i then
           (i, a) :: alist
         else
           ia' :: add i a tail

    let rec remove i = function
      | [] -> []
      | (i', _ as ia') :: tail as alist ->
         if i' = i then
           tail
         else if  i' > i then
           alist
         else
           ia' :: remove i tail

    let rec get_opt i = function
      | [] -> None
      | (i', a') :: tail ->
         if i' = i then
           Some a'
         else
           get_opt i tail

    let _min_key = function
      | [] -> invalid_arg "PArray.Alists.min_key"
      | (i, _) :: _ -> i

    let rec _max_key = function
      | [] -> invalid_arg "PArray.Alists.max_key"
      | [(i, _)] -> i
      | _ :: tail -> _max_key tail

    let index_base = 0

    let to_option_list parray =
      let rec to_option_list' i = function
        | [] -> []
        | (i', a') :: tail ->
           (if i' = i then Some a' else None) :: to_option_list' (succ i) tail in
      to_option_list' index_base parray

    let to_string a2s map =
      match to_option_list map with
      | [] -> "[]"
      | [None] -> "?"
      | [Some a] -> a2s a
      | pairs -> ThoList.to_string (function None -> "?" | Some a -> a2s a) pairs

    let of_pairs pairs =
      List.fold_right
        (fun (i, a) acc ->
          if i < index_base then
            invalid_arg "PArray.Alists.of_pairs"
          else
            add i a acc)
        pairs empty

    let to_pairs parray = parray

    let compare _ = compare
    let equal _ = (=)

    type ('a, 'b) taken =
      | Nothing of 'b t
      | Single of int * 'a * 'b t
      | Multiple of int * 'a * 'a t

    let take_one project_opt parray =
      let select (k, v) =
        match project_opt k v with
        | Some _ -> false
        | None -> true
      and project (k, v) =
        match project_opt k v with
        | Some v' -> (k, v')
        | None -> failwith "PArray.Alists.take_one: impossible" in
      match List.partition select parray with
      | [], other -> Nothing (List.map project other)
      | [(k, v)], other -> Single (k, v, List.map project other)
      | (k, v) :: _, _ -> Multiple (k, v, remove k parray)

  end

include Alists

module Test =
  struct

    open OUnit

    let project_single _ = function
      | [v] -> Some v
      | _ -> None

    let suite_take_one =
      "take_one" >:::
        [ "Nothing" >::
            (fun () ->
              assert_equal
                (Nothing (of_pairs [(1, "1"); (3, "3")]))
                (take_one project_single (of_pairs [(1, ["1"]); (3, ["3"])])));

          "Single" >::
            (fun () ->
              assert_equal
                (Single (2, ["2"; "2"], of_pairs [(1, "1"); (3, "3")]))
                (take_one project_single (of_pairs [(1, ["1"]); (3, ["3"]); (2, ["2"; "2"])])));

          "Multiple" >::
            (fun () ->
              assert_equal
                (Multiple (2, ["2"; "2"], of_pairs [(1, ["1"]); (3, ["3"]); (4, [])]))
                (take_one project_single (of_pairs [(1, ["1"]); (3, ["3"]); (2, ["2"; "2"]); (4, [])]))) ]

    let suite =
      "PArray" >:::
	[ suite_take_one ]

  end
