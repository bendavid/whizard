(* NList.ml --

   Copyright (C) 2022-2026 by

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

(* The constructor [Zero] appears to be not needed,
   but the constructor [Successor] is required. *)    

type z = Zero
type 'a s = Successor

type (_, _) t =
  | Nil  : (z, 'a) t
  | Cons : 'a * ('n, 'a) t -> ('n s, 'a) t

let empty = Nil

let cons : type n. 'a -> (n, 'a) t -> (n s, 'a) t =
  fun x xs ->
  Cons (x, xs)

let hd : type n. (n s, 'a) t -> 'a = function
  | Cons (x, _) -> x

let tl : type n. (n s, 'a) t -> (n, 'a) t = function
  | Cons (_, xs) -> xs

let rec fold_right : type n. ('a -> 'b -> 'b) -> (n, 'a) t -> 'b -> 'b=
  fun f alist b ->
  match alist with
  | Nil -> b
  | Cons (a, rest) -> f a (fold_right f rest b)

let rec map : type n. ('a -> 'b) -> (n, 'a) t -> (n, 'b) t =
  fun f ->
  function
  | Nil -> Nil
  | Cons (x, xs) -> Cons (f x, map f xs)

let rec to_list : type n. (n, 'a) t -> 'a list = function
  | Nil -> []
  | Cons (a, a_list) -> a :: to_list a_list

let rec map2 : type n. ('a -> 'b -> 'c) -> (n, 'a) t -> (n, 'b) t -> (n, 'c) t =
  fun f a_list b_list ->
  match a_list, b_list with
  | Nil, Nil -> Nil
  | Cons (x, xs), Cons (y, ys) -> Cons (f x y, map2 f xs ys)

(* This corresponds to a bubble sort. Don't use this for long lists!
   However, we expect the lists to be very short anyway and type safe
   reversing or concatenating two lists as required by the better performing
   algorithms requires to much effort for our applications. *)

(* Inner step: find an element that is out of order and push it past
   the adjacent lesser elements.  Report whether a transposition was made. *)

let rec cycle : type n. ('a -> 'a -> int) -> (n, 'a) t -> bool * (n, 'a) t =
  fun cmp ->
  function
  | Nil -> (false, Nil)
  | Cons (_, Nil) as a -> (false, a)
  | Cons (a1, (Cons (a2, alist2) as alist1)) ->
     if cmp a1 a2 <= 0 then
       let flipped, alist = cycle cmp alist1 in
       (flipped, Cons (a1, alist))
     else
       let flipped, alist = cycle cmp (Cons (a1, alist2)) in
       (true, Cons (a2, alist))

(* Repeat the inner step until no more elements are out of order. *)

let rec sort : type n. ('a -> 'a -> int) -> (n, 'a) t -> (n, 'a) t =
  fun cmp alist ->
  let flipped, cycled = cycle cmp alist in
  if flipped then
    sort cmp cycled
  else
    cycled
