(* $Id: cascade_syntax.ml 4926 2013-12-04 12:35:06Z jr_reuter $

   Copyright (C) 1999-2014 by

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

(* Concerning the Gaussian propagators, we admit the following: In
   principle, they would allow for flavor sums like the off-shell 
   lines, but for all practical purposes they are used only for
   determining the significance of a specified intermediate state. 
   So we select them in the same manner as on-shell states. *)

type ('flavor, 'p) t =
  | True
  | False
  | On_shell of 'flavor list * 'p
  | On_shell_not of 'flavor list * 'p
  | Off_shell of 'flavor list * 'p
  | Off_shell_not of 'flavor list * 'p
  | Gauss of 'flavor list * 'p
  | Gauss_not of 'flavor list * 'p
  | Any_flavor of 'p
  | Or of ('flavor, 'p) t list
  | And of ('flavor, 'p) t list

let mk_true () = True
let mk_false () = False
let mk_on_shell f p = On_shell (f, p)
let mk_on_shell_not f p = On_shell_not (f, p)
let mk_off_shell f p = Off_shell (f, p)
let mk_off_shell_not f p = Off_shell_not (f, p)
let mk_gauss f p = Gauss (f, p)
let mk_gauss_not f p = Gauss_not (f, p)
let mk_any_flavor p = Any_flavor p

let mk_or c1 c2 =
  match c1, c2 with
  | _, True | True, _ -> True
  | c, False | False, c -> c
  | Or cs, Or cs' -> Or (cs @ cs')
  | Or cs, c | c, Or cs -> Or (c::cs)
  | c, c' -> Or [c; c']

let mk_and c1 c2 =
  match c1, c2 with
  | c, True | True, c -> c
  | c, False | False, c -> False
  | And cs, And cs' -> And (cs @ cs')
  | And cs, c | c, And cs -> And (c::cs)
  | c, c' -> And [c; c']

let to_string flavor_to_string momentum_to_string cascades =
  let rec to_string' = function
    | True -> "true"
    | False -> "false"
    | On_shell (fs, p) ->
        momentum_to_string p ^ " = " ^ (String.concat ":" (List.map flavor_to_string fs))
    | On_shell_not (fs, p) ->
        momentum_to_string p ^ " = !" ^ (String.concat ":" (List.map flavor_to_string fs))
    | Off_shell (fs, p) ->
        momentum_to_string p  ^ " ~ " ^
        (String.concat ":" (List.map flavor_to_string fs))
    | Off_shell_not (fs, p) ->
        momentum_to_string p  ^ " ~ !" ^
        (String.concat ":" (List.map flavor_to_string fs))
    | Gauss (fs, p) ->
        momentum_to_string p ^ " # " ^ (String.concat ":" (List.map flavor_to_string fs))
    | Gauss_not (fs, p) ->
        momentum_to_string p ^ " # !" ^ (String.concat ":" (List.map flavor_to_string fs))
    | Any_flavor p ->
        momentum_to_string p ^ " ~ ?"
    | Or cs ->
        String.concat " || " (List.map (fun c -> "(" ^ to_string' c ^ ")") cs)
    | And cs ->
        String.concat " && " (List.map (fun c -> "(" ^ to_string' c ^ ")") cs) in
  to_string' cascades

let int_list_to_string p =
  String.concat "+" (List.map string_of_int (Sort.list (<) p))

exception Syntax_Error of string * int * int

(*i
 *  Local Variables:
 *  mode:caml
 *  indent-tabs-mode:nil
 *  page-delimiter:"^(\\* .*\n"
 *  End:
i*)

