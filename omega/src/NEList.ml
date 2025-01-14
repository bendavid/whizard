(* NEList.ml --

   Copyright (C) 2022-2025 by

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

(* The implementation is now trivial, except for the few cases
   where we need to avoid incomplete pattern match warnings: *)

let impossible f = failwith ("NList." ^ f ^ ": impossible []")

type 'a t = 'a list

let make a alist = a :: alist
let singleton a = make a []
let cons = make

let to_list l = l [@@inline]

let hd = List.hd
let tl = List.tl

let tl_opt = function
  | [] -> impossible "tl_opt"
  | [_] -> None
  | _ :: tail -> Some tail

let snoc = function
  | [] -> impossible "snoc"
  | head :: tail -> (head, tail)

let snoc_opt = function
  | [] -> impossible "snoc_opt"
  | [head] -> (head, None)
  | head :: tail -> (head, Some tail)

let map = List.map
let fold_right = List.fold_right
let sort = List.sort


