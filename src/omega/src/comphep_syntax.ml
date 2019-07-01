(* $Id: comphep_syntax.ml 3670 2012-01-21 19:33:07Z jr_reuter $

   Copyright (C) 1999-2012 by

       Wolfgang Kilian <kilian@physik.uni-siegen.de>
       Thorsten Ohl <ohl@physik.uni-wuerzburg.de>
       Juergen Reuter <juergen.reuter@desy.de>
       Christian Speckner <christian.speckner@physik.uni-freiburg.de>

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

type raw =
  | I | Integer of int | Symbol of string
  | Application of string * raw
  | Dotproduct of raw * raw
  | Product of (raw * int) list
  | Sum of (raw * int) list

let symbol name = Symbol name
let integer n = Integer n
let imag = I

let apply f x = Application (f, x)
let dot x y = Dotproduct (x, y)

let negate = List.map (fun (x, c) -> (x, -c))
let scale n = List.map (fun (x, c) -> (x, n*c))

let add1 (x, c) y =
  if c = 0 then
    y
  else
    try
      let c' = List.assoc x y + c in
      if c' = 0 then
        List.remove_assoc x y
      else
        (x, c') :: (List.remove_assoc x y)
    with
    | Not_found -> (x, c) :: y

let addn = List.fold_right add1

let multiply x y =
  match x, y with
  | Product x', Product y' -> Product (addn x' y')
  | Integer n, Product y' -> Product (scale n y')
  | Product x', Integer n -> Product (scale n x')
  | _, Product y' -> Product (add1 (x, 1) y')
  | Product x', _ -> Product (add1 (y, 1) x')
  | _ when x = y -> Product ([(x, 2)])
  | _ -> Product ([(x, 1); (y, 1)])

let divide x y =
  match y with
  | Product y' -> multiply x (Product (negate y'))
  | _ when x = y -> Product ([])
  | _ -> Product ([(x, 1); (y, -1)])

let power x n =
  match x with
  | Product x' -> Product (scale n x')
  | x -> Product ([(x, n)])

let add x y =
  match x, y with
  | Sum x', Sum y' -> Sum (addn x' y')
  | _, Sum y' -> Sum (add1 (x, 1) y')
  | Sum x', _ -> Sum (add1 (y, 1) x')
  | _ when x = y -> Sum ([(x, 2)])
  | _ -> Sum ([(x, 1); (y, 1)])

let subtract x y =
  match y with
  | Sum y' -> add x (Sum (negate y'))
  | _ when x = y -> Sum ([])
  | _ -> Sum ([(x, 1); (y, -1)])

let neg = function
  | Sum x -> Sum (negate x)
  | x -> Sum ([(x, -1)])

type vector =
  | Momentum of int
  | Index of int
  | Index' of int

let vector_keyword = function
  | "p1" -> Some (Momentum 1)
  | "p2" -> Some (Momentum 2)
  | "p3" -> Some (Momentum 3)
  | "p4" -> Some (Momentum 4)
  | "m1" -> Some (Index 1)
  | "m2" -> Some (Index 2)
  | "m3" -> Some (Index 3)
  | "m4" -> Some (Index 4)
  | "M1" -> Some (Index' 1)
  | "M2" -> Some (Index' 2)
  | "M3" -> Some (Index' 3)
  | "M4" -> Some (Index' 4)
  | _ -> None

(*i
 *  Local Variables:
 *  mode:caml
 *  indent-tabs-mode:nil
 *  page-delimiter:"^(\\* .*\n"
 *  End:
i*)

