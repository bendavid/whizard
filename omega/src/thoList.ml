(* $Id: thoList.ml 6465 2015-01-10 15:22:31Z jr_reuter $

   Copyright (C) 1999-2015 by

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

let rec hdn n l =
  if n <= 0 then
    []
  else
    match l with
    | x :: rest -> x :: hdn (pred n) rest
    | [] -> invalid_arg "ThoList.hdn"

let rec tln n l =
  if n <= 0 then
    l
  else
    match l with
    | _ :: rest -> tln (pred n) rest
    | [] -> invalid_arg "ThoList.tln"

let rec splitn' n l1_rev l2 =
  if n <= 0 then
    (List.rev l1_rev, l2)
  else
    match l2 with
    | x :: l2' -> splitn' (pred n) (x :: l1_rev) l2'
    | [] -> invalid_arg "ThoList.splitn n > len"

let splitn n l =
  if n < 0 then
    invalid_arg "ThoList.splitn n < 0"
  else
    splitn' n [] l

(* This is [splitn'] all over again, but without the exception. *)
let rec chopn'' n l1_rev l2 =
  if n <= 0 then
    (List.rev l1_rev, l2)
  else
    match l2 with
    | x :: l2' -> chopn'' (pred n) (x :: l1_rev) l2'
    | [] -> (List.rev l1_rev, [])
  
let rec chopn' n ll_rev = function
  | [] -> List.rev ll_rev
  | l ->
      begin match chopn'' n [] l with
      | [], [] -> List.rev ll_rev
      | l1, [] -> List.rev (l1 :: ll_rev)
      | l1, l2 -> chopn' n (l1 :: ll_rev) l2
      end

let chopn n l =
  if n <= 0 then
    invalid_arg "ThoList.chopn n <= 0"
  else
    chopn' n [] l

let of_subarray n1 n2 a =
  let rec of_subarray' n1 n2 =
    if n1 > n2 then
      []
    else
      a.(n1) :: of_subarray' (succ n1) n2 in
  of_subarray' (max 0 n1) (min n2 (pred (Array.length a)))

let range ?(stride=1) n1 n2 =
  if stride <= 0 then
    invalid_arg "ThoList.range: stride <= 0"
  else
    let rec range' n =
      if n > n2 then
        []
      else
        n :: range' (n + stride) in
    range' n1

(* Tail recursive: *)
let enumerate ?(stride=1) n l =
  let _, l_rev =
    List.fold_left
      (fun (i, acc) a -> (i + stride, (i, a) :: acc))
      (n, []) l in
  List.rev l_rev

(* This is \emph{not} tail recursive! *)
let rec flatmap f = function
  | [] -> []
  | x :: rest -> f x @ flatmap f rest

(* This is! *)
let rev_flatmap f l =
  let rec rev_flatmap' acc f = function
    | [] -> acc
    | x :: rest -> rev_flatmap' (List.rev_append (f x) acc) f rest in
  rev_flatmap' [] f l

let fold_left2 f acc lists =
  List.fold_left (List.fold_left f) acc lists

let fold_right2 f lists acc =
  List.fold_right (List.fold_right f) lists acc

let iteri f start list =
  ignore (List.fold_left (fun i a -> f i a; succ i) start list)

let iteri2 f start_outer star_inner lists =
  iteri (fun j -> iteri (f j) star_inner) start_outer lists

let mapi f start list =
  let next, list' =
    List.fold_left (fun (i, acc) a -> (succ i, f i a :: acc)) (start, []) list in
  List.rev list'

(* Is there a more efficient implementation? *)
let transpose lists =
  let rec transpose' rest =
    if List.for_all ((=) []) rest then
      []
    else
      List.map List.hd rest :: transpose' (List.map List.tl rest) in
  try
    transpose' lists
  with
  | Failure "tl" -> invalid_arg "ThoList.transpose: not rectangular"

let compare ?(cmp=Pervasives.compare) l1 l2 =
  let rec compare' l1' l2' =
    match l1', l2' with
    | [], [] -> 0
    | [], _ -> -1
    | _, [] -> 1
    | n1 :: r1, n2 :: r2 ->
        let c = cmp n1 n2 in
        if c <> 0 then
          c
        else
          compare' r1 r2
  in
  compare' l1 l2

let rec uniq' x = function
  | [] -> []
  | x' :: rest ->
      if x' = x then
        uniq' x rest
      else
        x' :: uniq' x' rest

let uniq = function
  | [] -> []
  | x :: rest -> x :: uniq' x rest

let rec homogeneous = function
  | [] | [_] -> true
  | a1 :: (a2 :: _ as rest) ->
      if a1 <> a2 then
        false
      else
        homogeneous rest
          
(* If we needed it, we could use a polymorphic version of [Set] to
   speed things up from~$O(n^2)$ to~$O(n\ln n)$.  But not before it
   matters somewhere \ldots *)
let classify l =
  let rec add_to_class a = function
    | [] -> [1, a]
    | (n, a') :: rest ->
        if a = a' then
          (succ n, a) :: rest
        else
          (n, a') :: add_to_class a rest
  in
  let rec classify' cl = function
    | [] -> cl
    | a :: rest -> classify' (add_to_class a cl) rest
  in
  classify' [] l

let rec factorize l =
  let rec add_to_class x y = function
    | [] -> [(x, [y])]
    | (x', ys) :: rest ->
        if x = x' then
          (x, y :: ys) :: rest
        else
          (x', ys) :: add_to_class x y rest
  in
  let rec factorize' fl = function
    | [] -> fl
    | (x, y) :: rest -> factorize' (add_to_class x y fl) rest
  in
  List.map (fun (x, ys) -> (x, List.rev ys)) (factorize' [] l)
    
let rec clone n x =
  if n < 0 then
    invalid_arg "ThoList.clone"
  else if n = 0 then
    []
  else
    x :: clone (pred n) x

let interleave f list =
  let rec interleave' rev_head tail =
    let rev_head' = List.rev_append (f rev_head tail) rev_head in
    match tail with
    | [] -> List.rev rev_head'
    | x :: tail' -> interleave' (x :: rev_head') tail'
  in
  interleave' [] list

let interleave_nearest f list =
  interleave
    (fun head tail ->
      match head, tail with
      | h :: _, t :: _ -> f h t
      | _ -> [])
    list

let rec rev_multiply n rl l =
  if n < 0 then
    invalid_arg "ThoList.multiply"
  else if n = 0 then
    []
  else
    List.rev_append rl (rev_multiply (pred n) rl l)

let multiply n l = rev_multiply n (List.rev l) l

module ISet = Set.Make (struct type t = int let compare = Pervasives.compare end)

exception Overlapping_indices
exception Out_of_bounds

let iset_of_list list =
  List.fold_right ISet.add list ISet.empty

let iset_list_union list =
  List.fold_right ISet.union list ISet.empty

let complement_index_sets n index_set_lists =
  let index_sets = List.map iset_of_list index_set_lists in
  let index_set = iset_list_union index_sets in
  let size_index_sets =
    List.fold_left (fun acc s -> ISet.cardinal s + acc) 0 index_sets in
  if size_index_sets <> ISet.cardinal index_set then
    raise Overlapping_indices
  else if ISet.exists (fun i -> i < 0 || i >= n) index_set then
    raise Overlapping_indices
  else
    match ISet.elements (ISet.diff (iset_of_list (range 0 (pred n))) index_set) with
    | [] -> index_set_lists
    | complement -> complement :: index_set_lists

let sort_section cmp array index_set =
  List.iter2
    (Array.set array)
    index_set (List.sort cmp (List.map (Array.get array) index_set))

let partitioned_sort cmp index_sets list =
  let array = Array.of_list list in
  List.fold_left
    (fun () -> sort_section cmp array)
    () (complement_index_sets (List.length list) index_sets);
  Array.to_list array

let ariadne_sort ?(cmp=Pervasives.compare) list =
  let sorted =
    List.sort (fun (n1, a1) (n2, a2) -> cmp a1 a2) (enumerate 0 list) in
  (List.map snd sorted, List.map fst sorted)

let ariadne_unsort (sorted, indices) =
  List.map snd
    (List.sort
       (fun (n1, a1) (n2, a2) -> Pervasives.compare n1 n2)
       (List.map2 (fun n a -> (n, a)) indices sorted))
