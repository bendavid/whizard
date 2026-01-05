(* color_Propagator.ml --

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

type cf_in = int
type cf_out = int

type eps = cf_out list
type s_eps = cf_out list
type cf_in_or_eps =
  | CF_in of cf_in
  | Epsilon of eps

type eps_bar = cf_in list
type s_eps_bar = cf_in list
type cf_out_or_eps_bar =
  | CF_out of cf_out
  | Epsilon_Bar of eps_bar

type flow = cf_in PArray.t * cf_out PArray.t
type flow_eps = cf_in_or_eps PArray.t * cf_out PArray.t
type flow_eps_bar = cf_in PArray.t * cf_out_or_eps_bar PArray.t
type t =
  | Flow of flow
  | Flow_with_Epsilons of flow_eps * s_eps list
  | Flow_with_Epsilon_Bars of flow_eps_bar * s_eps_bar list
  | Ghost
  | Ghost_with_Epsilons of s_eps_bar list
  | Ghost_with_Epsilon_Bars of s_eps_bar list

(* For partial maps of ['a Map.t], an exception is the right
   choice, since we would have to use ['a Map.fold] to
   reconstruct resulting map completele. *)
exception Fail

let to_cf_in_opt cfi =
  let project = function
    | CF_in cf -> cf
    | Epsilon _ -> raise Fail in
  try Some (PArray.map project cfi) with Fail -> None

let to_cf_out_opt cfo =
  let project = function
    | CF_out cf -> cf
    | Epsilon_Bar _ -> raise Fail in
  try Some (PArray.map project cfo) with Fail -> None

let normalize = function
  | (Ghost | Ghost_with_Epsilons _ | Ghost_with_Epsilon_Bars _ | Flow _) as flow -> flow
  | Flow_with_Epsilons ((cfi, cfo), []) as flow ->
     begin match to_cf_in_opt cfi with
     | None -> flow
     | Some cfi -> Flow (cfi, cfo)
     end
  | Flow_with_Epsilons (_, _ :: _) as flow -> flow
  | Flow_with_Epsilon_Bars ((cfi, cfo), []) as flow ->
     begin match to_cf_out_opt cfo with
     | None -> flow
     | Some cfo -> Flow (cfi, cfo)
     end
  | Flow_with_Epsilon_Bars (_, _ :: _) as flow -> flow

let white = Flow (PArray.empty, PArray.empty)

let of_lists cfi cfo =
  let cfi = ThoList.mapi (fun n cf -> (n, cf)) 0 cfi
  and cfo = ThoList.mapi (fun n cf -> (n, cf)) 0 cfo in
  Flow (PArray.of_pairs cfi, PArray.of_pairs cfo)
      
let is_white = function
  | Flow (incoming, outgoing) -> PArray.is_empty incoming && PArray.is_empty outgoing
  | Flow_with_Epsilons (_, _) | Flow_with_Epsilon_Bars (_, _) -> false
  | Ghost | Ghost_with_Epsilons _ | Ghost_with_Epsilon_Bars _ -> false

let cfi_or_eps_to_cfo_or_eps_bar = function
  | CF_in cf -> CF_out cf
  | Epsilon eps -> Epsilon_Bar eps

let cfo_or_eps_bar_to_cfi_or_eps = function
  | CF_out cf -> CF_in cf
  | Epsilon_Bar eps -> Epsilon eps

let conjugate = function
  | Flow (cfi, cfo) -> Flow (cfo, cfi)
  | Flow_with_Epsilons ((cfi, cfo), eps) ->
     Flow_with_Epsilon_Bars ((cfo, PArray.map cfi_or_eps_to_cfo_or_eps_bar cfi), eps)
  | Flow_with_Epsilon_Bars ((cfi, cfo), eps) ->
     Flow_with_Epsilons ((PArray.map cfo_or_eps_bar_to_cfi_or_eps cfo, cfi), eps)
  | Ghost -> Ghost
  | Ghost_with_Epsilons eps -> Ghost_with_Epsilon_Bars eps
  | Ghost_with_Epsilon_Bars eps -> Ghost_with_Epsilons eps

let _cf_in_or_eps_to_string = function
  | CF_in i -> string_of_int i
  | Epsilon cfos -> Printf.sprintf "E(%s)" (ThoList.to_string string_of_int cfos)

let _cf_out_or_eps_bar_to_string = function
  | CF_out i -> string_of_int i
  | Epsilon_Bar cfis -> Printf.sprintf "B(%s)" (ThoList.to_string string_of_int cfis)

let cf_in_out_to_string cfi cfo =
  match PArray.is_empty cfi, PArray.is_empty cfo with
  | true, true -> "W"
  | false, true -> Printf.sprintf "I(%s)" (PArray.to_string string_of_int cfi)
  | true, false -> Printf.sprintf "O(%s)" (PArray.to_string string_of_int cfo)
  | false, false ->
     Printf.sprintf "IO(%s,%s)"
       (PArray.to_string string_of_int cfi)
       (PArray.to_string string_of_int cfo)
      
let to_string = function
  | Ghost -> "G"
  | Flow (cfi, cfo) -> cf_in_out_to_string cfi cfo
  | Ghost_with_Epsilons _epsilons ->
     failwith "Color_Propagator.to_string: incomplete"
  | Ghost_with_Epsilon_Bars _epsilon_bars ->
     failwith "Color_Propagator.to_string: incomplete"
  | Flow_with_Epsilons ((_cfi, _cfo), _epsilons) ->
     failwith "Color_Propagator.to_string: incomplete"
  | Flow_with_Epsilon_Bars ((_cfi, _cfo), _epsilon_bars) ->
     failwith "Color_Propagator.to_string: incomplete"

let digit_option_to_symbol = function
  | None -> "_"
  | Some i ->
     if i < 0 then
       invalid_arg "Color_Propagator.digit_option_to_symbol: negative"
     else
       if i < 10 then
         string_of_int i
       else if i < 36 then
         String.make 1 (Char.chr (Char.code 'A' + i - 10))
       else
         invalid_arg "Color_Propagator.digit_option_to_symbol: too large"

let cf_in_cf_out_to_symbol cfi cfo =
  match PArray.to_option_list cfi, PArray.to_option_list cfo with
  | [], [] -> "w"
  | cfi, [] -> "i" ^ String.concat "" (List.map digit_option_to_symbol cfi)
  | [], cfo -> "o" ^ String.concat "" (List.map digit_option_to_symbol cfo)
  | cfi, cfo ->
     "i" ^ String.concat "" (List.map digit_option_to_symbol cfi) ^
       "_o" ^ String.concat "" (List.map digit_option_to_symbol cfo)

let to_symbol = function
  | Ghost -> "g"
  | Flow (cfi, cfo) -> cf_in_cf_out_to_symbol cfi cfo
  | Ghost_with_Epsilons _epsilons ->
     failwith "Color_Propagator.to_string: incomplete"
  | Ghost_with_Epsilon_Bars _epsilon_bars ->
     failwith "Color_Propagator.to_string: incomplete"
  | Flow_with_Epsilons ((_cfi, _cfo), _epsilons) ->
     failwith "Color_Propagator.to_string: incomplete"
  | Flow_with_Epsilon_Bars ((_cfi, _cfo), _epsilon_bars) ->
     failwith "Color_Propagator.to_string: incomplete"

let pp fmt p =
  Format.fprintf fmt "%s" (to_string p)

let compare_pairs compare_x compare_y (x1, y1) (x2, y2) =
  let c = compare_x x1 x2 in
  if c <> 0 then
    c
  else
    compare_y y1 y2

let compare_flows p1 p2 =
  compare_pairs (PArray.compare compare) (PArray.compare compare) p1 p2

let compare_eps e1 e2 =
  compare_pairs (compare_pairs (PArray.compare compare) (PArray.compare compare)) compare e1 e2

let compare p1 p2 =
  match normalize p1, normalize p2 with
  | Flow f1, Flow f2 -> compare_flows f1 f2
  | Flow_with_Epsilons (f1, e1), Flow_with_Epsilons (f2, e2) -> compare_eps (f1, e1) (f2, e2)
  | Flow_with_Epsilon_Bars (f1, e1), Flow_with_Epsilon_Bars (f2, e2) -> compare_eps (f1, e1) (f2, e2)
  | Ghost, Ghost -> 0
  | Ghost_with_Epsilons e1, Ghost_with_Epsilons e2 -> compare e1 e2
  | Ghost_with_Epsilon_Bars e1, Ghost_with_Epsilon_Bars e2 -> compare e1 e2

  | Flow _, (Flow_with_Epsilons _ | Flow_with_Epsilon_Bars _ | Ghost
             | Ghost_with_Epsilons _ | Ghost_with_Epsilon_Bars _)
  | Flow_with_Epsilons _, (Flow_with_Epsilon_Bars _ | Ghost
                          | Ghost_with_Epsilons _ | Ghost_with_Epsilon_Bars _)
  | Flow_with_Epsilon_Bars _ , (Ghost | Ghost_with_Epsilons _ | Ghost_with_Epsilon_Bars _)
  | Ghost, (Ghost_with_Epsilons _ | Ghost_with_Epsilon_Bars _)
  | Ghost_with_Epsilons _, Ghost_with_Epsilon_Bars _ -> -1

  | (Flow_with_Epsilons _ | Flow_with_Epsilon_Bars _ | Ghost
     | Ghost_with_Epsilons _ | Ghost_with_Epsilon_Bars _), Flow _
  | (Flow_with_Epsilon_Bars _ | Ghost
    | Ghost_with_Epsilons _ | Ghost_with_Epsilon_Bars _), Flow_with_Epsilons _
  | (Ghost | Ghost_with_Epsilons _ | Ghost_with_Epsilon_Bars _), Flow_with_Epsilon_Bars _
  | (Ghost_with_Epsilons _ | Ghost_with_Epsilon_Bars _), Ghost
  | Ghost_with_Epsilon_Bars _, Ghost_with_Epsilons _ -> 1

let _equal p1 p2 =
  compare p1 p2 = 0

(* Since [PArray.Alist.t] has a unique physical representation, we can fall back
   on the polymorphic [compare] again. *)

let compare = compare
let equal = (=)
