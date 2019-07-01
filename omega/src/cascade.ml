(* $Id: cascade.ml 6222 2014-10-13 08:40:53Z ohl $

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

module type T =
  sig

    type flavor
    type p

    type t
    val of_string_list : int -> string list -> t
    val to_string : t -> string

    type selectors
    val to_selectors : t -> selectors
    val no_cascades : selectors

    val select_wf : selectors -> (p -> bool) -> flavor -> p -> p list -> bool
    val select_p : selectors -> p -> p list -> bool
    val on_shell : selectors -> flavor -> p -> bool
    val is_gauss : selectors -> flavor -> p -> bool

    val partition : selectors -> int list list
    val description : selectors -> string option

  end

module Make (M : Model.T) (P : Momentum.T) :
    (T with type flavor = M.flavor and type p = P.t) =
  struct

    module CS = Cascade_syntax

    type flavor = M.flavor
    type p = P.t

(* Since we have
   \begin{equation}
      p \le q \Longleftrightarrow (-q) \le (-p)
   \end{equation}
   also for $\le$ as set inclusion [lesseq], only four of the eight
   combinations are independent
   \begin{equation}
     \begin{aligned}
        p &\le q    &&\Longleftrightarrow & (-q) &\le (-p) \\
        q &\le p    &&\Longleftrightarrow & (-p) &\le (-q) \\
        p &\le (-q) &&\Longleftrightarrow & q    &\le (-p) \\
     (-q) &\le p    &&\Longleftrightarrow & (-p) &\le q 
     \end{aligned}
   \end{equation}  *)

    let one_compatible p q =
      let neg_q = P.neg q in
      P.lesseq p q ||
      P.lesseq q p ||
      P.lesseq p neg_q ||
      P.lesseq neg_q p

(* 'tis wasteful \ldots (at least by a factor of two, because every momentum
    combination is generated, including the negative ones. *)

    let all_compatible p p_list q =
      let l = List.length p_list in
      if l <= 2 then
        one_compatible p q
      else
        let tuple_lengths = ThoList.range 2 (succ l / 2) in
        let tuples = ThoList.flatmap (fun n -> Combinatorics.choose n p_list) tuple_lengths in
        let momenta = List.map (List.fold_left P.add (P.zero (P.dim q))) tuples in
        List.for_all (one_compatible q) momenta

(* The following assumes that the [flavor list] is always very short.  Otherwise
   one should use an efficient set implementation. *)

    type t =
      | True
      | False
      | On_shell of flavor list * P.t
      | On_shell_not of flavor list * P.t
      | Off_shell of flavor list * P.t
      | Off_shell_not of flavor list * P.t
      | Gauss of flavor list * P.t
      | Gauss_not of flavor list * P.t
      | Any_flavor of P.t
      | And of t list

    let of_string s = 
      Cascade_parser.main Cascade_lexer.token (Lexing.from_string s)

(* \begin{dubious}
     If we knew that we're dealing with a scattering, we could apply
     [P.flip_s_channel_in] to all momenta, so that $1+2$ accepts the particle
     and not the antiparticle.  Right now, we don't have this information.
   \end{dubious} *)

    let import dim cascades =
      let rec import' = function
        | CS.True ->
            True
        | CS.False ->
            False
        | CS.On_shell (f, p) ->
            On_shell (List.map M.flavor_of_string f, P.of_ints dim p)
        | CS.On_shell_not (f, p) ->
            On_shell_not (List.map M.flavor_of_string f, P.of_ints dim p)
        | CS.Off_shell (fs, p) ->
            Off_shell (List.map M.flavor_of_string fs, P.of_ints dim p)
        | CS.Off_shell_not (fs, p) ->
            Off_shell_not (List.map M.flavor_of_string fs, P.of_ints dim p)
        | CS.Gauss (f, p) ->
            Gauss (List.map M.flavor_of_string f, P.of_ints dim p)
        | CS.Gauss_not (f, p) ->
            Gauss (List.map M.flavor_of_string f, P.of_ints dim p)
        | CS.Any_flavor p ->
            Any_flavor (P.of_ints dim p)
        | CS.Or cs ->
            invalid_arg "Cascade: OR patterns (||) not supported in this version!"
        | CS.And cs -> And (List.map import' cs) in
      import' cascades

    let of_string_list dim strings =
      match List.map of_string strings with
      | [] -> True
      | first :: next ->
          import dim (List.fold_right CS.mk_and next first)

    let flavors_to_string fs =
      (String.concat ":" (List.map M.flavor_to_string fs))

    let momentum_to_string p =
      String.concat "+" (List.map string_of_int (P.to_ints p))

    let rec to_string = function
      | True ->
          "true"
      | False ->
          "false"
      | On_shell (fs, p) ->
          momentum_to_string p ^ " = " ^ flavors_to_string fs
      | On_shell_not (fs, p) ->
          momentum_to_string p ^ " = !" ^ flavors_to_string fs
      | Off_shell (fs, p) ->
          momentum_to_string p  ^ " ~ " ^ flavors_to_string fs
      | Off_shell_not (fs, p) ->
          momentum_to_string p  ^ " ~ !" ^ flavors_to_string fs
      | Gauss (fs, p) ->
          momentum_to_string p ^ " # " ^ flavors_to_string fs
      | Gauss_not (fs, p) ->
          momentum_to_string p ^ " # !" ^ flavors_to_string fs
      | Any_flavor p ->
          momentum_to_string p ^ " ~ ?"
      | And cs ->
          String.concat " && " (List.map (fun c -> "(" ^ to_string c ^ ")") cs)

    type selectors =
        { select_p : p -> p list -> bool;
          select_wf : (p -> bool) -> flavor -> p -> p list -> bool;
          on_shell : flavor -> p -> bool;
          is_gauss : flavor -> p -> bool;
          partition : int list list;
          description : string option }

    let no_cascades =
      { select_p = (fun _ _ -> true);
        select_wf = (fun _ _ _ _ -> true);
        on_shell = (fun _ _ -> false);
        is_gauss = (fun _ _ -> false);
        partition = [];
        description = None }

    let select_p s = s.select_p
    let select_wf s = s.select_wf
    let on_shell s = s.on_shell
    let is_gauss s = s.is_gauss
    let partition s = s.partition
    let description s = s.description

    let to_select_p cascades p p_in =
      let rec to_select_p' = function
        | True -> true
        | False -> false
        | On_shell (_, momentum) | On_shell_not (_, momentum)
        | Off_shell (_, momentum) | Off_shell_not (_, momentum)
        | Gauss (_, momentum) | Gauss_not (_, momentum)
        | Any_flavor momentum -> all_compatible p p_in momentum
        | And [] -> false
        | And cs -> List.for_all to_select_p' cs in
      to_select_p' cascades

    let to_select_wf cascades is_timelike f p p_in =
      let f' = M.conjugate f in
      let rec to_select_wf' = function
        | True -> true
        | False -> false
        | Off_shell (flavors, momentum) ->
            if p = momentum then
              List.mem f' flavors || (if is_timelike p then false else List.mem f flavors)
            else if p = P.neg momentum then
              List.mem f flavors || (if is_timelike p then false else List.mem f' flavors)
            else
              one_compatible p momentum && all_compatible p p_in momentum
        | On_shell (flavors, momentum) | Gauss (flavors, momentum) ->
             if is_timelike p then begin
               if p = momentum then
                 List.mem f' flavors
               else if p = P.neg momentum then
                 List.mem f flavors
               else
                 one_compatible p momentum && all_compatible p p_in momentum
             end else
               false
        | Off_shell_not (flavors, momentum) ->
            if p = momentum then
              not (List.mem f' flavors || (if is_timelike p then false else List.mem f flavors))
            else if p = P.neg momentum then
              not (List.mem f flavors || (if is_timelike p then false else List.mem f' flavors))
            else
              one_compatible p momentum && all_compatible p p_in momentum
        | On_shell_not (flavors, momentum) | Gauss_not (flavors, momentum) ->
            if is_timelike p then begin
              if p = momentum then
                not (List.mem f' flavors)
              else if p = P.neg momentum then
                not (List.mem f flavors)
              else
                one_compatible p momentum && all_compatible p p_in momentum
            end else
              false
        | Any_flavor momentum ->
            one_compatible p momentum && all_compatible p p_in momentum
        | And [] -> false
        | And cs -> List.for_all to_select_wf' cs in
      to_select_wf' cascades


(* In case you're wondering: [to_on_shell f p] and [is_gauss f p] only search
   for on shell conditions and are to be used in a target, not in [Fusion]! *)

    let to_on_shell cascades f p =
      let f' = M.conjugate f in
      let rec to_on_shell' = function
        | True | False | Any_flavor _ 
        | Off_shell (_, _) | Off_shell_not (_, _)
        | Gauss (_, _) | Gauss_not (_, _) -> false
        | On_shell (flavors, momentum) ->
            (p = momentum || p = P.neg momentum) && (List.mem f flavors || List.mem f' flavors)
        | On_shell_not (flavors, momentum) ->
            (p = momentum || p = P.neg momentum) && not (List.mem f flavors || List.mem f' flavors)
        | And [] -> false
        | And cs -> List.for_all to_on_shell' cs in
      to_on_shell' cascades


    let to_gauss cascades f p =
      let f' = M.conjugate f in
      let rec to_gauss' = function
        | True | False | Any_flavor _ 
        | Off_shell (_, _) | Off_shell_not (_, _)
        | On_shell (_, _) | On_shell_not (_, _) -> false
        | Gauss (flavors, momentum) ->
            (p = momentum || p = P.neg momentum) && (List.mem f flavors || List.mem f' flavors)
        | Gauss_not (flavors, momentum) ->
            (p = momentum || p = P.neg momentum) && not (List.mem f flavors || List.mem f' flavors)
        | And [] -> false
        | And cs -> List.for_all to_gauss' cs in
      to_gauss' cascades

(* \begin{dubious}
     Not a working implementation yet, but it isn't used either \ldots 
   \end{dubious} *)

    module IPowSet =
      PowSet.Make (struct type t = int let compare = compare let to_string = string_of_int end)
      
    let rec coarsest_partition' = function
        | True | False -> IPowSet.empty
        | On_shell (_, momentum) | On_shell_not (_, momentum)
        | Off_shell (_, momentum) | Off_shell_not (_, momentum)
        | Gauss (_, momentum) | Gauss_not (_, momentum)
        | Any_flavor momentum -> IPowSet.of_lists [P.to_ints momentum]
        | And [] -> IPowSet.empty
        | And cs -> IPowSet.basis (IPowSet.union (List.map coarsest_partition' cs))

    let coarsest_partition cascades =
      let p = coarsest_partition' cascades in
      if IPowSet.is_empty p then
        []
      else
        IPowSet.to_lists p

    let part_to_string part =
      "{" ^ String.concat "," (List.map string_of_int part) ^ "}"

    let partition_to_string = function
      | [] -> ""
      | parts ->
          "  grouping {" ^ String.concat "," (List.map part_to_string parts) ^ "}"

    let to_selectors = function
      | True -> no_cascades
      | c ->
          let partition = coarsest_partition c in
          { select_p = to_select_p c;
            select_wf = to_select_wf c;
            on_shell = to_on_shell c;
            is_gauss = to_gauss c;
            partition = partition;
            description = Some (to_string c ^ partition_to_string partition) }

  end

(*i
 *  Local Variables:
 *  mode:caml
 *  indent-tabs-mode:nil
 *  page-delimiter:"^(\\* .*\n"
 *  End:
i*)

