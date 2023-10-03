(* color_Fusion.ml --

   Copyright (C) 2022-2023 by

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

(* \label{sec:colorflow-fusions} *)

(* Here we will use the color flow described by a [Arrow.free list]
   to determine the possible outgoing color flows for the incoming
   color flows in a fusion.  This translates from vertices described
   by connections among integers describing factors in the tensor product
   to color flows with integers describing individual color flow lines.
   For the treatment of $\epsilon$ and $\bar\epsilon$, see the discussion
   on page~\pageref{sec:epsilon-evaluation-strategy}. *)

(* \begin{dubious}
     At the moment both the factors in the tensor product and
     the color flow lines are [int]s.  This could be made clearer
     by abstract types.
   \end{dubious} *)

(* \begin{dubious}
     This still needs to be extended to $\epsilon$ and $\bar\epsilon$,
     i.\,e.~[Arrow.free_eps] and [Arrow.free_eps_bar].
   \end{dubious} *)

module A = Arrow
open A.Infix
module CP = Color_Propagator
module L = Algebra.Laurent
module QC = Algebra.QC

(* Take a [Color_Propagator.t list], ignore the uncolored ([Color_Propagator.W])
   ones and construct a map into the colored ones indexed by
   the offset into the original list.
   Actually, one could use a [Color_Propagator.t option array] instead,
   but the elements of ['a array] are updated in place, making
   it harder to keep track. *)
let line_map lines =
  let _, map =
    List.fold_left
      (fun (i, acc) line ->
        (succ i,
         if CP.is_white line then
           acc
         else
           PArray.add i line acc))
      (1, PArray.empty)
      lines in
  map

(* [clear i lines] removes the [Color_Propagator.t] at position [i]
   from the map [lines]. *)
let clear = PArray.remove

(* Return $+1$ if the list [l1] is an even permutation of
   the list [l2], $-1$ if [l1] is an odd permutation of [l2]
   and $0$ otherwise. *)
let relative_permutation l1 l2 =
  let eps1, l1 = Combinatorics.sort_signed l1
  and eps2, l2 = Combinatorics.sort_signed l2 in
  if l1 = l2 then
    eps1 * eps2
  else
    0

(* Return the integers in the list [elements] that are not in
   the list [universe]. *)
let not_in elements universe =
  let universe = Sets.Int.of_list universe in
  let rec collect missing = function
    | [] -> missing
    | x :: tail ->
       if Sets.Int.mem x universe then
         collect missing tail
       else
         collect (x :: missing) tail in
  collect [] elements

(* [open_epsilon] is an $\epsilon_{ii_2\cdots i_n}$
   (or~$\bar\epsilon^{ii_2\cdots i_n}$)
   with one index~$i$ open and [epsilon_bar] a matching
   $\bar\epsilon^{j_1j_2\cdots j_n}$
   (or $\epsilon_{j_1j_2\cdots j_n}$).  Replace~$i$
   by the single $j\in\{j_m\}_{m=1,\ldots,n}$ with
   $j\not\in\{i_m\}_{m=2,\ldots,n}$ and compute
   \begin{equation}
           \epsilon_{ii_2\cdots i_n}
           \bar\epsilon^{j_1j_2\cdots j_n}
         = \delta_{ii_2\cdots i_n}^{j_1j_2\cdots j_n}
         = \sum_{\sigma\in S_n} (-1)^{\varepsilon(\sigma)}
            \delta_{i}^{\sigma(j_1)} 
            \delta_{i_2}^{\sigma(j_2)} 
            \cdots
            \delta_{i_n}^{\sigma(j_n)}\,.
   \end{equation}
   Return [None] if the two index sets are not permutations
   of one another and [Some (sign, i)] if they are. *)

let open_contract open_epsilon epsilon_bar =
  match not_in epsilon_bar open_epsilon with
  | [] -> None
  | [i] ->
     let sign = relative_permutation (i :: open_epsilon) epsilon_bar in
     if sign = 0 then
       None
     else
       Some (sign, i)
  | _ -> None

(* [connect n (sign, flow_n, lines) arrow] tries to form a new connection in the
   map [lines] using a single [arrow].  The outgoing line in the fusion
   is represented by [flow_n] and corresponds to [n] in the [arrow]. *)

(* If the arrow is a ghost and is connected to the outgoing line,
   just add it.  If it is connected to an incoming line, remove
   this propagator, as it is saturated. *)

let connect_ghost_opt n g (sign, flow_n, lines) =
  let g' = A.position_ghost g in
  if g' = n then
    Some (sign, CP.Ghost, lines)
  else
    match PArray.get_opt g' lines with
    | Some CP.Ghost -> Some (sign, flow_n, clear g' lines)
    | Some CP.Ghost_with_Epsilons _ ->
       failwith "connect_ghost_opt: incomplete"
    | Some CP.Ghost_with_Epsilon_Bars _ ->
       failwith "connect_ghost_opt: incomplete"
    | _ -> None

(* Add the normalized propagator [p] to the map [lines] at position
   [i], unless it contains no color flows.  Remove it in this case. *)

let add_or_remove_if_white i p lines =
  let p = CP.normalize p in
  if CP.is_white p then
    PArray.remove i lines
  else
    PArray.add i p lines

(* If the arrow is a connection and is connected on one side
   to the outgoing line, find the matching incoming line.
   If it is connected to two incoming lines, merge them,
   which amounts to throwing them away. *)

(* \begin{dubious}
     Here's where the $\epsilon$-$\bar\epsilon$ pairs will be consumed.
     We should move this to a preprocessing step, so that the
     repeated application of arrows does not have to take care of it.
     Or do it in a postprocessing step, which has the advantage that
     the contractions have been processed and a possible new $\epsilon$
     or $\bar\epsilon$ is available.
   \end{dubious} *)

(* Try to extract
   an $\epsilon$ (or $\bar\epsilon$) from the color flow
   given as the argument. *)

let take_epsilon cfi =
  let project_opt _ = function
    | CP.CF_in cf -> Some cf
    | CP.Epsilon _ -> None in
  PArray.take_one project_opt cfi

let take_epsilon_bar cfo =
  let project_opt _ = function
    | CP.CF_out cf -> Some cf
    | CP.Epsilon_Bar _ -> None in
  PArray.take_one project_opt cfo

(* This is a part of [connect_in_opt] below that requires recursion and
   therefore needs to be its own function. *)

(* Keeping track of the overall [sign], connect the
   incoming [CP.Flow_with_Epsilons] at index [i'] at position
   [i] in [lines] with
   the outgoing [CP.Flow_with_Epsilon_Bars] at index [n'].
   Return the updated propagator and [lines] if the color flows
   match. *)

let rec connect_in_contract_epsilons_opt sign :
          int -> CP.flow_eps_bar -> CP.eps_bar list ->
          int -> CP.flow_eps -> CP.eps list ->
          int -> CP.t PArray.t -> (int * CP.t * CP.t PArray.t) option =
  fun n' (cfi_n, cfo_n as cf_n) epsilon_bars_n
      i' (cfi_i, cfo_i as cf_i) epsilons_i i lines ->
  let open PArray in
  match epsilon_bars_n, epsilons_i with
  | epsilon_bar :: epsilon_bars_n, epsilon :: epsilons_i ->
     let relative_sign = relative_permutation epsilon epsilon_bar  in
     if relative_sign = 0 then
       None
     else
       connect_in_contract_epsilons_opt (relative_sign * sign)
         n' cf_n epsilon_bars_n i' cf_i epsilons_i i lines
  | epsilon_bar :: _, [] ->
     begin match take_epsilon cfi_i with
     | Nothing cfi ->
        let flow_n = CP.Flow_with_Epsilon_Bars (cf_n, epsilon_bars_n)
        and pi = CP.Flow (cfi, cfo_i) in
        Some (sign, flow_n, add_or_remove_if_white i pi lines)
     | Single (_, _, cfi_i) ->
        failwith "Color_Fusion.connect_in_contract_epsilons_opt: incomplete"
     | Multiple (_, _, cfi_i) ->
        failwith "Color_Fusion.connect_in_contract_epsilons_opt: incomplete"
     end
  | [], epsilon :: _ ->
     begin match take_epsilon_bar cfo_n with
     | Nothing cfo ->
        let flow_n = CP.Flow (cfi_n, cfo)
        and pi = CP.Flow_with_Epsilons (cf_i, epsilons_i) in
        Some (sign, flow_n, add_or_remove_if_white i pi lines)
     | Single (_, _, cfo_n) ->
        failwith "Color_Fusion.connect_in_contract_epsilons_opt: incomplete"
     | Multiple (_, _, cfo_n) ->
        failwith "Color_Fusion.connect_in_contract_epsilons_opt: incomplete"
     end
  | [], [] ->
     begin match take_epsilon_bar cfo_n, take_epsilon cfi_i with
     | Nothing cfo, Nothing cfi ->
        let flow_n = CP.Flow (cfi_n, cfo)
        and pi = CP.Flow (cfi, cfo_i) in
        Some (sign, flow_n, add_or_remove_if_white i pi lines)
     | _ ->
        failwith "Color_Fusion.connect_in_contract_epsilons_opt: incomplete"
     end

let connect_in_opt n' (i, i') (sign, flow_n, lines) =
  let open PArray in
  match get_opt i lines with
  | None -> None
  | Some flow_i ->
     begin match flow_i with
     | CP.Ghost | CP.Ghost_with_Epsilons _ | CP.Ghost_with_Epsilon_Bars _ -> None
     | CP.Flow (cfi_i, cfo_i) ->
        begin match get_opt i' cfi_i with
        | None -> None
        | Some cfi ->
           begin match flow_n with
           | CP.Ghost -> None
           | CP.Ghost_with_Epsilons _ ->
              failwith "connect_in_opt: incomplete"
           | CP.Ghost_with_Epsilon_Bars _ ->
              failwith "connect_in_opt: incomplete"
           | CP.Flow (cfi_n, cfo_n) ->
              let flow_n = CP.Flow (add n' cfi cfi_n, cfo_n)
              and pi = CP.Flow (remove i' cfi_i, cfo_i) in
              Some (sign, flow_n, add_or_remove_if_white i pi lines)
           | CP.Flow_with_Epsilons ((cfi_n, cfo_n), epsilons_n) ->
              let cfi = CP.CF_in cfi in
              let flow_n = CP.Flow_with_Epsilons ((add n' cfi cfi_n, cfo_n), epsilons_n)
              and pi = CP.Flow (remove i' cfi_i, cfo_i) in
              Some (sign, flow_n, add_or_remove_if_white i pi lines)
           | CP.Flow_with_Epsilon_Bars ((cfi_n, cfo_n), epsilon_bars_n) ->
              let flow_n = CP.Flow_with_Epsilon_Bars ((add n' cfi cfi_n, cfo_n), epsilon_bars_n)
              and pi = CP.Flow (remove i' cfi_i, cfo_i) in
              Some (sign, flow_n, add_or_remove_if_white i pi lines)
           end
        end
     | CP.Flow_with_Epsilons ((cfi_i, cfo_i), epsilons_i) ->
        begin match get_opt i' cfi_i with
        | None -> None
        | Some cfi ->
           begin match flow_n with
           | CP.Ghost -> None
           | CP.Ghost_with_Epsilons _ ->
              failwith "connect_in_opt: incomplete"
           | CP.Ghost_with_Epsilon_Bars _ ->
              failwith "connect_in_opt: incomplete"
           | CP.Flow (cfi_n, cfo_n) ->
              let cfi_n = map (fun cf -> CP.CF_in cf) cfi_n in
              let flow_n = CP.Flow_with_Epsilons ((add n' cfi cfi_n, cfo_n), epsilons_i)
              and pi = CP.Flow_with_Epsilons ((remove i' cfi_i, cfo_i), []) in
              Some (sign, flow_n, add_or_remove_if_white i pi lines)
           | CP.Flow_with_Epsilons ((cfi_n, cfo_n), epsilons_n) ->
              let flow_n = CP.Flow_with_Epsilons ((add n' cfi cfi_n, cfo_n), epsilons_i @ epsilons_n)
              and pi = CP.Flow_with_Epsilons ((remove i' cfi_i, cfo_i), []) in
              Some (sign, flow_n, add_or_remove_if_white i pi lines)
           | CP.Flow_with_Epsilon_Bars ((cfi_n, cfo_n), epsilon_bars_n) ->
              connect_in_contract_epsilons_opt sign
                n' (cfi_n, cfo_n) epsilon_bars_n
                i' (cfi_i, cfo_i) epsilons_i
                i lines
           end
        end
     | CP.Flow_with_Epsilon_Bars ((cfi_i, cfo_i), epsilon_bars_i) ->
        begin match get_opt i' cfi_i with
        | None -> None
        | Some cfi ->
           begin match flow_n with
           | CP.Ghost -> None
           | CP.Ghost_with_Epsilons _ ->
              failwith "connect_in_opt: incomplete"
           | CP.Ghost_with_Epsilon_Bars _ ->
              failwith "connect_in_opt: incomplete"
           | CP.Flow (cfi_n, cfo_n) ->
              let cfo_n = map (fun cf -> CP.CF_out cf) cfo_n in
              let flow_n = CP.Flow_with_Epsilon_Bars ((add n' cfi cfi_n, cfo_n), epsilon_bars_i)
              and pi = CP.Flow_with_Epsilon_Bars ((remove i' cfi_i, cfo_i), []) in
              Some (sign, flow_n, add_or_remove_if_white i pi lines)
           | CP.Flow_with_Epsilon_Bars ((cfi_n, cfo_n), epsilon_bars_n) ->
              let flow_n = CP.Flow_with_Epsilon_Bars ((add n' cfi cfi_n, cfo_n), epsilon_bars_i @ epsilon_bars_n)
              and pi = CP.Flow_with_Epsilon_Bars ((remove i' cfi_i, cfo_i), []) in
              Some (sign, flow_n, add_or_remove_if_white i pi lines)

           | CP.Flow_with_Epsilons ((cfi_n, cfo_n), epsilons_n) ->
              failwith "Color_Fusion.connect_in_opt: no epsilon contractions yet"
           end
        end
     end

let connect_out_opt n' (o, o') (sign, flow_n, lines) =
  let open PArray in
  match get_opt o lines with
  | None -> None
  | Some flow ->
     begin match flow with
     | CP.Ghost | CP.Ghost_with_Epsilons _ | CP.Ghost_with_Epsilon_Bars _ -> None
     | CP.Flow (cfi_o, cfo_o) ->
        begin match get_opt o' cfo_o with
        | None -> None
        | Some cfo ->
           begin match flow_n with
           | CP.Ghost -> None
           | CP.Ghost_with_Epsilons _ ->
              failwith "connect_out_opt: incomplete"
           | CP.Ghost_with_Epsilon_Bars _ ->
              failwith "connect_out_opt: incomplete"
           | CP.Flow (cfi_n, cfo_n) ->
              let flow_n = CP.Flow (cfi_n, add n' cfo cfo_n)
              and po = CP.Flow (cfi_o, remove o' cfo_o) in
              Some (sign, flow_n, add_or_remove_if_white o po lines)
           | CP.Flow_with_Epsilons ((cfi_n, cfo_n), epsilons_n) ->
              let flow_n = CP.Flow_with_Epsilons ((cfi_n, add n' cfo cfo_n), epsilons_n)
              and po = CP.Flow (cfi_o, remove o' cfo_o) in
              Some (sign, flow_n, add_or_remove_if_white o po lines)
           | CP.Flow_with_Epsilon_Bars ((cfi_n, cfo_n), epsilon_bars_n) ->
              let cfo = CP.CF_out cfo in
              let flow_n = CP.Flow_with_Epsilon_Bars ((cfi_n, add n' cfo cfo_n), epsilon_bars_n)
              and po = CP.Flow (cfi_o, remove o' cfo_o) in
              Some (sign, flow_n, add_or_remove_if_white o po lines)
           end
        end
     | CP.Flow_with_Epsilons ((cfi_o, cfo_o), epsilons_o) ->
        begin match get_opt o' cfo_o with
        | None -> None
        | Some cfo ->
           begin match flow_n with
           | CP.Ghost -> None
           | CP.Ghost_with_Epsilons _ ->
              failwith "connect_out_opt: incomplete"
           | CP.Ghost_with_Epsilon_Bars _ ->
              failwith "connect_out_opt: incomplete"
           | CP.Flow (cfi_n, cfo_n) ->
              let cfi_n = map (fun cf -> CP.CF_in cf) cfi_n in
              let flow_n = CP.Flow_with_Epsilons ((cfi_n, add n' cfo cfo_n), epsilons_o)
              and po = CP.Flow_with_Epsilons ((cfi_o, remove o' cfo_o), []) in
              Some (sign, flow_n, add_or_remove_if_white o po lines)
           | CP.Flow_with_Epsilons ((cfi_n, cfo_n), epsilons_n) ->
              let flow_n = CP.Flow_with_Epsilons ((cfi_n, add n' cfo cfo_n), epsilons_o @ epsilons_n)
              and po = CP.Flow_with_Epsilons ((cfi_o, remove o' cfo_o), []) in
              Some (sign, flow_n, add_or_remove_if_white o po lines)
           | CP.Flow_with_Epsilon_Bars ((cfi_n, cfo_n), epsilon_bars_n) ->
              failwith "Color_Fusion.connect_out_opt: no epsilon contractions yet"
           end
        end
     | CP.Flow_with_Epsilon_Bars ((cfi_o, cfo_o), epsilon_bars_o) ->
        begin match get_opt o' cfo_o with
        | None -> None
        | Some cfo ->
           begin match flow_n with
           | CP.Ghost -> None
           | CP.Ghost_with_Epsilons _ ->
              failwith "connect_out_opt: incomplete"
           | CP.Ghost_with_Epsilon_Bars _ ->
              failwith "connect_out_opt: incomplete"
           | CP.Flow (cfi_n, cfo_n) ->
              let cfo_n = map (fun cf -> CP.CF_out cf) cfo_n in
              let flow_n = CP.Flow_with_Epsilon_Bars ((cfi_n, add n' cfo cfo_n), epsilon_bars_o)
              and po = CP.Flow_with_Epsilon_Bars ((cfi_o, remove o' cfo_o), []) in
              Some (sign, flow_n, add_or_remove_if_white o po lines)
           | CP.Flow_with_Epsilon_Bars ((cfi_n, cfo_n), epsilon_bars_n) ->
              let flow_n = CP.Flow_with_Epsilon_Bars ((cfi_n, add n' cfo cfo_n), epsilon_bars_o @ epsilon_bars_n)
              and po = CP.Flow_with_Epsilon_Bars ((cfi_o, remove o' cfo_o), []) in
              Some (sign, flow_n, add_or_remove_if_white o po lines)

           | CP.Flow_with_Epsilons ((cfi_n, cfo_n), epsilons_n) ->
              failwith "Color_Fusion.connect_out_opt: no epsilon contractions yet"
           end
        end
     end

let connect_in_out_opt (i, i') (o, o') (sign, flow_n, lines) =
  let open PArray in
  match get_opt i lines, get_opt o lines with
  | None, _ | _, None -> None
  | Some flow_i, Some flow_o ->
     begin match flow_i, flow_o with
     | (CP.Ghost | CP.Ghost_with_Epsilons _ | CP.Ghost_with_Epsilon_Bars _), _
       | _, (CP.Ghost | CP.Ghost_with_Epsilons _ | CP.Ghost_with_Epsilon_Bars _) -> None
     | CP.Flow (cfi_i, cfo_i), CP.Flow (cfi_o, cfo_o) ->
        begin match get_opt i' cfi_i, get_opt o' cfo_o with
        | Some cfi, Some cfo when cfi = cfo ->
           let pi = CP.Flow (remove i' cfi_i, cfo_i)
           and po = CP.Flow (cfi_o, remove o' cfo_o) in
           Some (sign, flow_n, add_or_remove_if_white i pi (add_or_remove_if_white o po lines))
        | _, _ -> None
        end
     | CP.Flow (cfi_i, cfo_i), CP.Flow_with_Epsilons ((cfi_o, cfo_o), epsilons_o) ->
        begin match get_opt i' cfi_i, get_opt o' cfo_o with
        | Some cfi, Some cfo when cfi = cfo ->
           let pi = CP.Flow (remove i' cfi_i, cfo_i)
           and po = CP.Flow_with_Epsilons ((cfi_o, remove o' cfo_o), epsilons_o) in
           Some (sign, flow_n, add_or_remove_if_white i pi (add_or_remove_if_white o po lines))
        | _, _ -> None
        end
     | CP.Flow_with_Epsilons ((_, _), _), CP.Flow_with_Epsilons ((_, _), _) ->
        failwith "Color_Fusion.connect_in_out_opt: incomplete"
     | CP.Flow_with_Epsilon_Bars ((cfi_i, cfo_i), epsilon_bars_i), CP.Flow (cfi_o, cfo_o) ->
        begin match get_opt i' cfi_i, get_opt o' cfo_o with
        | Some cfi, Some cfo when cfi = cfo ->
           let pi = CP.Flow_with_Epsilon_Bars ((remove i' cfi_i, cfo_i), epsilon_bars_i)
           and po = CP.Flow ((cfi_o, remove o' cfo_o)) in
           Some (sign, flow_n, add_or_remove_if_white i pi (add_or_remove_if_white o po lines))
        | _, _ -> None
        end
     | CP.Flow_with_Epsilon_Bars ((_, _), _), CP.Flow_with_Epsilon_Bars ((_, _), _) ->
        failwith "Color_Fusion.connect_in_out_opt: incomplete"
     | CP.Flow_with_Epsilons ((cfi_i, cfo_i), epsilons_i), CP.Flow (cfi_o, cfo_o) ->
        begin match get_opt i' cfi_i, get_opt o' cfo_o with
        | Some (CP.CF_in cfi), Some cfo when cfi = cfo ->
           let pi = CP.Flow_with_Epsilons ((remove i' cfi_i, cfo_i), epsilons_i)
           and po = CP.Flow (cfi_o, remove o' cfo_o) in
           Some (sign, flow_n, add_or_remove_if_white i pi (add_or_remove_if_white o po lines))
        | Some (CP.Epsilon epsilon_i), Some cfo ->
           let epsilon_n = cfo :: epsilon_i in
           let flow_n =
             match flow_n with
             | CP.Ghost -> CP.Ghost
             | CP.Ghost_with_Epsilons _ ->
                failwith "connect_in_out_opt: incomplete"
             | CP.Ghost_with_Epsilon_Bars _ ->
                failwith "connect_in_out_opt: incomplete"
             | CP.Flow (cfo, cfi) ->
                let cfi = map (fun cf -> CP.CF_in cf) cfi in
                CP.Flow_with_Epsilons ((cfi, cfo), [epsilon_n])
             | CP.Flow_with_Epsilons (flow, epsilons_n) ->
                CP.Flow_with_Epsilons (flow, epsilon_n :: epsilons_n)
             | CP.Flow_with_Epsilon_Bars (flow, epsilon_bars_n) ->
                failwith "Color_Fusion.connect_in_out_opt: no epsilon contractions yet" in
           let pi = CP.Flow_with_Epsilons ((remove i' cfi_i, cfo_i), epsilons_i)
           and po = CP.Flow (cfi_o, remove o' cfo_o) in
           Some (sign, flow_n, add_or_remove_if_white i pi (add_or_remove_if_white o po lines))
        | _, _ -> None
        end
     | CP.Flow (cfi_i, cfo_i), CP.Flow_with_Epsilon_Bars ((cfi_o, cfo_o), epsilon_bars_o) ->
        begin match get_opt i' cfi_i, get_opt o' cfo_o with
        | Some cfi, Some (CP.CF_out cfo) when cfi = cfo ->
           let pi = CP.Flow (remove i' cfi_i, cfo_i)
           and po = CP.Flow_with_Epsilon_Bars ((cfi_o, remove o' cfo_o), epsilon_bars_o) in
           Some (sign, flow_n, add_or_remove_if_white i pi (add_or_remove_if_white o po lines))
        | Some cfi, Some (CP.Epsilon_Bar epsilon_bar_o) ->
           let epsilon_bar_n = cfi :: epsilon_bar_o in
           let flow_n =
             match flow_n with
             | CP.Ghost -> CP.Ghost
             | CP.Ghost_with_Epsilons _ ->
                failwith "connect_in_out_opt: incomplete"
             | CP.Ghost_with_Epsilon_Bars _ ->
                failwith "connect_in_out_opt: incomplete"
             | CP.Flow (cfo, cfi) ->
                let cfo = map (fun cf -> CP.CF_out cf) cfo in
                CP.Flow_with_Epsilon_Bars ((cfi, cfo), [epsilon_bar_n])
             | CP.Flow_with_Epsilon_Bars (flow, epsilon_bars_n) ->
                CP.Flow_with_Epsilon_Bars (flow, epsilon_bar_n :: epsilon_bars_n)
             | CP.Flow_with_Epsilons (flow, epsilons_n) ->
                failwith "Color_Fusion.connect_in_out_opt: no epsilon contractions yet" in
           let pi = CP.Flow (remove i' cfi_i, cfo_i)
           and po = CP.Flow_with_Epsilon_Bars ((cfi_o, remove o' cfo_o), epsilon_bars_o) in
           Some (sign, flow_n, add_or_remove_if_white i pi (add_or_remove_if_white o po lines))
        | _, _ -> None
        end
     | CP.Flow_with_Epsilons ((_, _), _), CP.Flow_with_Epsilon_Bars ((_, _), _) ->
        failwith "Color_Fusion.connect_in_out_opt: no epsilon contractions yet"
     | CP.Flow_with_Epsilon_Bars ((_, _), _), CP.Flow_with_Epsilons ((_, _), _) ->
        failwith "Color_Fusion.connect_in_out_opt: no epsilon contractions yet"
     end

(* \thocwmodulesection{Putting Everything Together} *)
let decode_endpoint = function
  | A.I n -> (n, 0)
  | A.M (n, m) -> (n, m)

let decode_tail t = decode_endpoint (t : A.tail :> A.endpoint)
let decode_tip t = decode_endpoint (t : A.tip :> A.endpoint)
let decode_ghost g = decode_endpoint (g : A.ghost :> A.endpoint)

let endpoint_to_string = function
  | A.I n -> string_of_int n
  | A.M (n, m) -> string_of_int n ^ "." ^ string_of_int m

let tail_to_string t = endpoint_to_string (t : A.tail :> A.endpoint)
let tip_to_string t = endpoint_to_string (t : A.tip :> A.endpoint)
let ghost_to_string g = endpoint_to_string (g : A.ghost :> A.endpoint)

let connect_arrow_opt n i o lines =
  let i, i' as ii' = decode_tail i
  and o, o' as oo' = decode_tip o in
  if o = n then
    connect_in_opt o' ii' lines
  else if i = n then
    connect_out_opt i' oo' lines
  else
    connect_in_out_opt ii' oo' lines

let lines_to_string (sign, flow_n, lines) =
  Printf.sprintf
    "%d*%s<%s"
    sign (CP.to_string flow_n)
    (ThoList.to_string
       (fun (i, p) -> Printf.sprintf "%s@%d" (CP.to_string p) i)
       (PArray.to_pairs lines))

let connect_arrow_opt_logging n i o lines =
  let result = connect_arrow_opt n i o lines in
  Printf.eprintf
    "  (%s,%s) %s >>> %s\n"
    (tail_to_string i) (tip_to_string o)
    (lines_to_string lines)
    (match result with
     | None -> "None"
     | Some lines -> lines_to_string lines);
  result

(*i
let connect_arrow_opt = connect_arrow_opt_logging
i*)

(* Performan a single connection of the [lines] as described by
   [arrow_or_ghost].  Use [n] as the index of the outgoing line.
   Return the updated outgoing and incoming lines. *)

let connect_arrow_or_ghost_opt :
      int -> A.free -> int * CP.t * CP.t PArray.t -> (int * CP.t * CP.t PArray.t) option =
  fun n arrow_or_ghost lines ->
  match arrow_or_ghost with
  | A.Ghost g -> connect_ghost_opt n g lines
  | A.Arrow (i, o) -> connect_arrow_opt n i o lines

(* Return the signed color [flow] iff all color flows in [lines]
   have been consumed. *)
let all_lines_consumed_opt (sign, flow, lines) =
  if PArray.is_empty lines then
    Some (sign, flow)
  else
    None

(* Try to use the ghosts and arrows in [connections] to combine the
   color flows in [lines].  *)
let connect_arrows_opt : A.free list -> CP.t list -> (int * CP.t) option =
  fun connections lines ->
  let n = List.length lines + 1 in
  let rec connect' acc = function
    | arrow :: arrows ->
       begin match connect_arrow_or_ghost_opt n arrow acc with
       | None -> None
       | Some acc -> connect' acc arrows
       end
    | [] -> Some acc in
  match connect' (1, CP.white, line_map lines) connections with
  | Some acc -> all_lines_consumed_opt acc
  | None -> None

let extract_lines_opt endpoints lines =
  let rec extract_lines' acc lines = function
    | [] -> Some (List.rev acc, lines)
    | A.I i :: rest ->
       begin match PArray.get_opt i lines with
       | None -> None
       | Some (CP.Flow (_, cfo)) ->
          begin match PArray.to_option_list cfo with
          | [Some cf] ->
             extract_lines' (cf :: acc) (PArray.remove i lines) rest
          | _ -> failwith "extract_lines_opt: incomplete"
          end
       | Some (CP.Flow_with_Epsilons ((_, _), _)) ->
          failwith "extract_lines_opt: incomplete"
       | Some (CP.Flow_with_Epsilon_Bars ((_, _), _)) ->
          failwith "extract_lines_opt: incomplete"
       | Some CP.Ghost ->
          failwith "extract_lines_opt: incomplete"
       | Some (CP.Ghost_with_Epsilons _)->
          failwith "extract_lines_opt: incomplete"
       | Some (CP.Ghost_with_Epsilon_Bars _) ->
          failwith "extract_lines_opt: incomplete"
       end
    | A.M (_, _) :: _ -> failwith "extract_lines_opt: incomplete" in
  extract_lines' [] endpoints lines

(*i
    let connect_epsilon_saturated_opt n epsilon (flow_n, lines) =
      match extract_lines_opt epsilon lines with
      | None -> None
      | Some (flow_n, lines) -> Some (CP.Epsilon flow_n, lines)

    let connect_epsilon_opt n epsilon (flow_n, lines) =
      match extract_lines_opt epsilon lines with
      | None -> None
      | Some (flow_n, lines) -> Some (CP.Epsilon flow_n, lines)
i*)

let fuse1 n_c lines arrow =
  let open Birdtracks in
  match arrow with
  | Arrows { coeff; arrows } ->
     begin match connect_arrows_opt arrows lines with
     | None -> []
     | Some (sign, flow) ->
        [(QC.mul (QC.int sign) (L.eval (QC.int n_c) coeff), flow)]
     end
  | Epsilons _ -> failwith "Birdtracks.fuse1: Epsilons"
  | Epsilon_Bars _ -> failwith "Birdtracks.fuse1: Epsilon_Bars"

let fuse n_c vertex lines =
  match vertex with
  | [] ->
     if List.for_all CP.is_white lines then
       [(QC.unit, CP.white)]
     else
       []
  | vertex ->
     ThoList.flatmap (fuse1 n_c lines) vertex

let flow_to_string flow =
  ThoList.to_string
    (fun (c, p) ->
      let p = CP.to_string p in
      if QC.is_unit c then
        p
      else
        Printf.sprintf "%s*%s" (QC.to_string c) p)
    flow

let fuse_logging n_c vertex lines =
  let flow_n = fuse n_c vertex lines in
  Printf.eprintf
    "%s >>> %s\n"
    (ThoList.to_string CP.to_string lines)
    (flow_to_string flow_n);
  flow_n

(*i
let fuse = fuse_logging
i*)

(* \thocwmodulesection{Unit Tests} *)

module Test =
  struct
    open OUnit

    let vertices_equal v1 v2 =
      (Birdtracks.canonicalize v1) = (Birdtracks.canonicalize v2)

    let eq v1 v2 =
      assert_equal ~printer:Birdtracks.to_string_raw ~cmp:vertices_equal v1 v2

    let suite_open_contract =
      "open_contract" >:::

        [ "[2;3] [1;2;4]" >::
	    (fun () -> assert_equal None (open_contract [2;3] [1;2;4]));

          "[2;3] [1;2;3;4]" >::
	    (fun () -> assert_equal None (open_contract [2;3] [1;2;3;4]));

          "[2;3] [1;2;3]" >::
	    (fun () -> assert_equal (Some ( 1,1)) (open_contract [2;3] [1;2;3]));

          "[1;3] [1;2;3]" >::
	    (fun () -> assert_equal (Some (-1, 2)) (open_contract [1;3] [1;2;3])) ]

    let signed_flow_option_to_string = function
      | Some (sign, flow) ->
         let flow = CP.to_string flow in
         if sign = 1 then
           flow
         else
           Printf.sprintf "%d*%s" sign flow
      | None -> "None"

    let test_connect_arrows_msg vertex formatter (expected, result) =
      Format.fprintf
        formatter
        "[%s]: expected %s, got %s"
        (ThoList.to_string A.free_to_string vertex)
        (signed_flow_option_to_string expected)
        (signed_flow_option_to_string result)

    let test_connect_arrows expected lines vertex =
      assert_equal ~printer:signed_flow_option_to_string
        expected (connect_arrows_opt vertex lines)

    let test_connect_arrows_permutations expected lines vertex =
      List.iter
        (fun v ->
	  assert_equal ~pp_diff:(test_connect_arrows_msg v)
            expected (connect_arrows_opt v lines))
        (Combinatorics.permute vertex)

    let suite_connect_arrows =
      "connect_arrows" >:::

        [ "delta" >::
	    (fun () ->
              test_connect_arrows_permutations
                (Some (1, CP.of_lists [1] []))
                [ CP.of_lists [1] []; CP.white]
                ( 1 ==> 3 ));

          "f: 1->3->2->1" >::
            (fun () ->
              test_connect_arrows_permutations
                (Some (1, CP.of_lists [1] [3]))
                [CP.of_lists [1] [2]; CP.of_lists [2] [3]]
                (A.cycle [1; 3; 2]));

          "f: 1->2->3->1" >::
            (fun () ->
              test_connect_arrows_permutations
                (Some (1, CP.of_lists [1] [2]))
                [CP.of_lists [3] [2]; CP.of_lists [1] [3]]
                (A.cycle [1; 2; 3])) ]

    let test_fuse_msg vertex lines formatter (expected, result) =
      Format.fprintf
        formatter
        "%s // %s => %s failed, got %s instead"
        (Birdtracks.to_string vertex)
        (ThoList.to_string CP.to_string lines)
        (flow_to_string expected)
        (flow_to_string result)

    let compare_fusion (c1, p1) (c2, p2) =
      let c = Algebra.QC.compare c1 c2 in
      if c <> 0 then
        c
      else
        CP.compare p1 p2

    let equal_fusion f1 f2 =
      compare_fusion f1 f2 = 0

    let cmp_fusions f1 f2 =
      let f1 = List.sort compare_fusion f1
      and f2 = List.sort compare_fusion f2 in
      try
        List.for_all2 equal_fusion f1 f2
      with
      | Invalid_argument _ -> false

    let test_fuse expected vertex lines =
      let nc = 3 in
      assert_equal
        ~cmp:cmp_fusions
        ~pp_diff:(test_fuse_msg vertex lines)
        expected (fuse nc vertex lines)

    (* This way, we can write [vertex // lines => expected] in the
       tests. *)
    let (//) vertex lines = (vertex, lines)
    let (=>) (vertex, lines) expected = test_fuse expected vertex lines

    (* Abbreviations *)
    let tf = test_fuse
    let e = QC.unit
    let half = QC.fraction 2
    let w = CP.white

    (* Quarks and anti quarks: *)
    let q i = CP.of_lists [i] []
    let aq i = CP.of_lists [] [i]

    (* Diquarks and anti diquarks: *)
    let dq i j = CP.of_lists [i; j] []
    let adq i j = CP.of_lists [] [i; j]

    (* Gluons without ghosts *)
    let g i j = CP.of_lists [i] [j]

    (* Couplings *)
    let d = SU3.delta3
    let d6 = SU3.delta6
    let t = SU3.t
    let t6 = SU3.t6
    let k6 = SU3.k6
    let k6b = SU3.k6bar

    let suite_binary_qed3 =
      "triplet" >:::
        [ "1 2 " >:: (fun () -> d 2 1 // [q 1;  aq 1] => [(e, w)]);
          "1 2'" >:: (fun () -> d 2 1 // [aq 1; q 1 ] => []);
          "2 1 " >:: (fun () -> d 1 2 // [aq 1; q 1 ] => [(e, w)]);
          "2 1'" >:: (fun () -> d 1 2 // [q 1;  aq 1] => []);
          "1 3 " >:: (fun () -> d 3 1 // [q 1;  w   ] => [(e, q 1)]);
          "2 3 " >:: (fun () -> d 3 2 // [w;    q 1 ] => [(e, q 1)]);
          "3 1 " >:: (fun () -> d 1 3 // [aq 1; w   ] => [(e, aq 1)]);
          "3 2 " >:: (fun () -> d 2 3 // [w;    aq 1] => [(e, aq 1)]) ]

    let suite_binary_qed6 =
      "sextet" >:::
        [ "1 2  " >:: (fun () -> d6 2 1 // [dq 1 2; adq 1 2] => [(half, w)]);
          "1 2' " >:: (fun () -> d6 2 1 // [dq 1 2; adq 2 1] => [(half, w)]);
          "1 2''" >:: (fun () -> d6 2 1 // [dq 1 2; adq 1 3] => []) ]

    let suite_binary_qcd3 =
      "triplet" >:::
        [ "1 2 " >:: (fun () -> t 3 2 1 // [q 1; aq 2] => [(e, g 1 2)]);
          "1 2'" >:: (fun () -> t 3 2 1 // [aq 1; q 2] => []) ]

    let suite_binary_qcd6 =
      "sextet" >:::
        [ "1 2" >:: (fun () -> t6 3 2 1 // [dq 1 2; adq 2 3] => [(half, g 1 3)]) ]

    let suite_binary_k6 =
      "k6(bar)" >:::
        [ "321  " >:: (fun () -> k6b 3 2 1 // [q 1;  q 2 ] => [(e, dq 2 1); (e, dq 1 2)]);
          "321* " >:: (fun () -> k6  3 2 1 // [aq 1; aq 2] => [(e, adq 2 1); (e, adq 1 2)]);
          "123  " >:: (fun () -> k6b 1 2 3 // [adq 1 2; q 1] => [(e, aq 2)]);
          "132  " >:: (fun () -> k6b 1 3 2 // [adq 1 2; q 1] => [(e, aq 2)]);
          "123' " >:: (fun () -> k6b 1 2 3 // [adq 1 2; q 2] => [(e, aq 1)]);
          "132' " >:: (fun () -> k6b 1 3 2 // [adq 1 2; q 2] => [(e, aq 1)]);
          "213  " >:: (fun () -> k6b 2 1 3 // [q 1; adq 1 2] => [(e, aq 2)]);
          "231  " >:: (fun () -> k6b 2 3 1 // [q 1; adq 1 2] => [(e, aq 2)]);
          "213' " >:: (fun () -> k6b 2 1 3 // [q 2; adq 1 2] => [(e, aq 1)]);
          "231' " >:: (fun () -> k6b 2 3 1 // [q 2; adq 1 2] => [(e, aq 1)]);
          "123 *" >:: (fun () -> k6  1 2 3 // [dq 1 2; aq 1] => [(e, q 2)]);
          "132 *" >:: (fun () -> k6  1 3 2 // [dq 1 2; aq 1] => [(e, q 2)]);
          "123'*" >:: (fun () -> k6  1 2 3 // [dq 1 2; aq 2] => [(e, q 1)]);
          "132'*" >:: (fun () -> k6  1 3 2 // [dq 1 2; aq 2] => [(e, q 1)]);
          "213 *" >:: (fun () -> k6  2 1 3 // [aq 1; dq 1 2] => [(e, q 2)]);
          "231 *" >:: (fun () -> k6  2 3 1 // [aq 1; dq 1 2] => [(e, q 2)]);
          "213'*" >:: (fun () -> k6  2 1 3 // [aq 2; dq 1 2] => [(e, q 1)]);
          "231'*" >:: (fun () -> k6  2 3 1 // [aq 2; dq 1 2] => [(e, q 1)]) ]

    let suite_binary =
      "binary" >:::
        [ "colorless" >:: (fun () -> [] // [w; w] => [(e, w)]);
          "qed" >::: [ suite_binary_qed3; suite_binary_qed6; suite_binary_k6 ];
          "qcd" >::: [ suite_binary_qcd3; suite_binary_qcd6 ] ]

    let suite_tertiary =
      "tertiary" >:::
        [ "colorless" >:: (fun () -> [] // [w; w; w] => [(e, w)]);
          "qed 1 2" >:: (fun () -> d 2 1 // [q 1; aq 1; w] => [(e, w)]);
          "qed 1 3" >:: (fun () -> d 3 1 // [q 1; w; aq 1] => [(e, w)]);
          "qed 2 3" >:: (fun () -> d 3 2 // [w; q 1; aq 1] => [(e, w)]) ]


    let suite_nary =
      "n-ary" >:::
        [ "colorless" >:: (fun () -> [] // [w; w; w; w; w] => [(e, w)]) ]


    let suite_fuse =
      "fuse" >:::
        [ suite_binary;
          suite_tertiary;
          suite_nary ]

    let suite =
      "Color_Fusion" >:::
	[suite_open_contract;
         suite_connect_arrows;
         suite_fuse]

    let suite_long =
      "Color_Fusion long" >:::
	[]
  end

