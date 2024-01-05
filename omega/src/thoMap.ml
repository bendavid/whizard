(* thoMap.ml --

   Copyright (C) 2023-2024 by

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

(* \thocwmodulesection{Maps to Sets} *)

module type Buckets =
  sig

    type t
    type key
    type element

    val empty : t
    val add : key -> element -> t -> t
    val to_lists : t -> (key * element list) list
    val factorize : (key * element) list -> (key * element list) list
    val factorize_batches : (key * element) list list -> (key * element list) list

  end

module Buckets (Key : Map.OrderedType) (Element : Set.OrderedType) : Buckets
       with type key = Key.t and type element = Element.t =
  struct

    module Keys = Map.Make(Key)
    module Elements = Set.Make(Element)
    type t = Elements.t Keys.t
    type key = Key.t
    type element = Element.t

    let empty = Keys.empty

    let lookup key map =
      match Keys.find_opt key map with
      | None -> Elements.empty
      | Some set -> set

    let add key element map =
      Keys.add key (Elements.add element (lookup key map)) map

    let to_lists map =
      List.map (fun (key, set) -> (key, Elements.elements set)) (Keys.bindings map)

    let add_pairs initial pairs =
      List.fold_left (fun acc (key, elt) -> add key elt acc) initial pairs

    let of_pairs = add_pairs empty

    let factorize pairs =
      to_lists (of_pairs pairs)

    let factorize_batches pairs_list =
      to_lists (List.fold_left add_pairs empty pairs_list)

  end

let random_int_list imax n =
  let imax = succ imax in
  let rec random_int_list' acc i =
    if i = 0 then
      List.rev acc
    else
      random_int_list' (Random.int imax :: acc) (pred i) in
  random_int_list' [] n

let shuffle l =
  let a = Array.of_list l in
  ThoArray.shuffle a;
  Array.to_list a

module Test =
  struct

    open OUnit

    module Integers = struct type t = int let compare = compare end
    module II = Buckets(Integers)(Integers)

    let compare_pair (a1, b1) (a2, b2) =
      let c = compare a1 a2 in
      if c <> 0 then
        c
      else
        compare b1 b2

    let ilist = ThoList.range 1 42
    let mod7 i = (i mod 7, i)
    let mod7_ilist = List.map mod7 ilist
    let mod7_ilist_batched = ThoList.chopn 10 mod7_ilist
    let mod7_factorized = List.sort compare_pair (ThoList.factorize mod7_ilist)

    let factorized_to_string l =
      ThoList.to_string
        (fun (i, ilist) -> "(" ^ string_of_int i ^ ", " ^ ThoList.to_string string_of_int ilist ^ ")" )
        l

    let suite_factorize =
      "factorize" >:::

	[ "int list" >::
	    (fun () ->
              assert_equal ~printer:factorized_to_string
                mod7_factorized (II.factorize mod7_ilist));

          "reversed int list" >::
	    (fun () ->
              assert_equal ~printer:factorized_to_string
                mod7_factorized (II.factorize (List.rev mod7_ilist)));

          "shuffled int list" >::
	    (fun () ->
              assert_equal ~printer:factorized_to_string
                mod7_factorized (II.factorize (shuffle mod7_ilist))) ]

    let suite_factorize_batches =
      "factorize_batches" >:::

	[ "int list" >::
	    (fun () ->
              assert_equal ~printer:factorized_to_string
                mod7_factorized (II.factorize_batches mod7_ilist_batched));

          "reversed int list" >::
	    (fun () ->
              assert_equal ~printer:factorized_to_string
                mod7_factorized (II.factorize_batches (List.rev mod7_ilist_batched)));

          "shuffled int list" >::
	    (fun () ->
              assert_equal ~printer:factorized_to_string
                mod7_factorized (II.factorize_batches (shuffle mod7_ilist_batched))) ]

    let suite_buckets =
      "Buckets" >:::

	[ suite_factorize;
          suite_factorize ]

    let suite =
      "ThoMap" >:::
	[ suite_buckets ]

  end
