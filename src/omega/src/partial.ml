(* $Id: partial.ml 4015 2013-01-03 16:04:18Z jr_reuter $

   Copyright (C) 1999-2013 by

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

module type T =
  sig
    type domain
    type 'a t
    val of_list : (domain * 'a) list -> 'a t
    val of_lists : domain list -> 'a list -> 'a t
    val apply : 'a t -> domain -> 'a
  end

module Make (D : Map.OrderedType) : T with type domain = D.t =
  struct

    module M = Map.Make (D)

    type domain = D.t
    type 'a t = 'a M.t

    let of_list l =
      List.fold_left (fun m (d, v) -> M.add d v m) M.empty l

    let of_lists domain values =
      of_list
	(try
	   List.rev_map2 (fun d v -> (d, v)) domain values
	 with
	    | Invalid_argument "List.rev_map2" ->
	      invalid_arg "Partial.of_lists: length mismatch")

    let apply partial d = M.find d partial

  end

module Test : sig val suite : OUnit.test end =
  struct

    open OUnit

    module P = Make (struct type t = int let compare = compare end)

    let apply_ok =
      "apply/ok" >::
	(fun () ->
	  let p = P.of_list [ (0,"a"); (1,"b"); (2,"c") ]
	  and l = [ 0; 1; 2 ] in
	  assert_equal [ "a"; "b"; "c" ] (List.map (P.apply p) l))
	
    let suite_apply =
      "apply" >:::
	[apply_ok]

    let suite =
      "Partial" >:::
	[suite_apply]

    let time () =
      ()

  end

