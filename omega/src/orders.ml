(* orders.ml --

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

(* \thocwmodulesection{Conditions} *)
module type Conditions =
  sig
    type coupling_order
    type orders = (coupling_order * int) list
    type t
    val trivial : t
    val of_strings : string list -> t
    val to_strings : t -> string list
    val constant : t -> orders -> bool
    val fusion : t -> orders -> bool
    val braket : t -> orders -> orders option
    val exclusive_fusion : t -> coupling_order list
    val exclusive_braket : t -> coupling_order list
    val square_root : t -> t

    val to_string : t -> string
    val pp : Format.formatter -> t -> unit
  end

(* A projection of [Model.T] containing only coupling constants
   and coupling orders.  This is useful for testing without having
   to link real models. *)
module type Model_CO =
  sig
    type constant 
    type coupling_order
    val all_coupling_orders : unit -> coupling_order list
    val coupling_order_to_string : coupling_order -> string
    val coupling_orders : constant -> (coupling_order * int) list
  end

module Conditions (M : Model_CO (* $\subset$ [Model.T] *)) : Conditions
       with type coupling_order = M.coupling_order =
  struct

    type coupling_order = M.coupling_order
    type orders = (coupling_order * int) list

    module CO = struct type t = coupling_order let compare = Stdlib.compare end
    module COSet = Set.Make(CO)
    module COMap = Map.Make(CO)
    module COSMap = Partial.Make(String)

    (* Add a [unit] argument to support [Model.Mutable]: *)
    let co_set () =
      COSet.of_list (M.all_coupling_orders ())

    let co_map () =
      COSMap.of_list (List.map (fun co -> (M.coupling_order_to_string co, co)) (M.all_coupling_orders ()))

    let co_set_of_strings pmap co_list =
      List.fold_left
        (fun acc s ->
          match COSMap.apply_opt pmap s with
          | None ->
             Printf.eprintf "omega: ignoring unknown coupling_order `%s'!\n" s;
             acc
          | Some co ->
             COSet.add co acc)
      COSet.empty co_list

    let complement = COSet.diff

    (* All the integers are non negative.  We don't need a [LE] constructor,
       because $i \le n$ is equivalent to $0\le i \le n$ in this case. This saves
       us redundant match cases below. *)
    type range =
      | GE of int
      | IN of int * int
      | EQ of int

    type mode = Slice | Sum
      
    (* The lists of type [orders] must be very short to allow encoding of the
       counted coupling orders in Fortran variable names!  That's why we keep the potentially
       much larger set of couplings that are set to zero separate.

       One could think of supporting a union of non overlapping ranges, but this adds a lot
       of complexity for little practical value. *)

    (* \begin{dubious}
          The correct semantics for \textit{OR}-ing conditions on \emph{different} coupling orders
          can not be implemented with the following data type.  One would need a set or list
          of [(range * mode) COMap.t] for [orders].  It is not clear if this is worth the effort.
       \end{dubious} *)

    (* [fusion] is the union of [braket] and [only_fusion].  One of the three is therefore
       redundant, but we maintain all three for convenience.  Similarly,
       [exclusive_braket] and [exclusive_fusion] are simply the result of applying
       [List.map fst] to [braket] and [fusion].  They are here just for convenience. *)
    type t =
      { braket : (coupling_order * range) list;
        fusion : (coupling_order * range) list;
        only_fusion : (coupling_order * range) list;
        exclusive_braket : coupling_order list;
        exclusive_fusion : coupling_order list;
        is_null : COSet.t }

    let trivial =
      { braket = [];
        fusion = [];
        only_fusion = [];
        exclusive_braket = [];
        exclusive_fusion = [];
        is_null = COSet.empty }

    type t_intermediate =
      { orders_map : (range * mode) COMap.t;
        null_set : COSet.t }

    let range_to_string l r = function
      | IN (i, j) -> Printf.sprintf "%c%d..%d%c" l i j r
      | GE i -> Printf.sprintf "%c%d..%c" l i r
      | EQ i -> Printf.sprintf "%d" i

    let interval_to_string = range_to_string '[' ']'
    let slice_to_string = range_to_string '{' '}'

    let co_and_interval_to_string (co, r) =
      M.coupling_order_to_string co ^ " = " ^ interval_to_string r

    let co_and_slice_to_string (co, r) =
      M.coupling_order_to_string co ^ " = " ^ slice_to_string r

    let to_string c =
      let is_null =
        match COSet.elements c.is_null with
        | [] -> []
        | [co] -> [M.coupling_order_to_string co ^ " = 0"]
        | is_null -> ["{" ^ String.concat ", " (List.map M.coupling_order_to_string is_null) ^ "} = 0"]
      and intervals = List.map co_and_interval_to_string c.only_fusion
      and slices = List.map co_and_slice_to_string c.braket in
      String.concat "; " (is_null @ intervals @ slices)
      
    let to_string_raw c =
      let is_null = String.concat ", " (List.map M.coupling_order_to_string (COSet.elements c.is_null))
      and braket = List.map co_and_slice_to_string c.braket
      and fusion = List.map co_and_interval_to_string c.fusion
      and only_fusion = List.map co_and_interval_to_string c.only_fusion in
      Printf.sprintf
        "is_null = {%s}; braket = (%s); fusion = (%s); only_fusion = (%s)"
        is_null (String.concat ", " braket) (String.concat ", " fusion) (String.concat ", " only_fusion)
      
    let to_strings c =
      let intervals = List.map co_and_interval_to_string c.only_fusion
      and slices = List.map co_and_slice_to_string c.braket in
      match COSet.elements c.is_null with
      | [] -> List.concat [intervals; slices]
      | is_null ->
         List.concat
           [intervals;
            slices;
            List.map
              (fun co_list ->
                "disabled: " ^ String.concat ", " (List.map M.coupling_order_to_string co_list))
              (ThoList.chopn 5 is_null)]

    let accept_all =
      { orders_map = COMap.empty;
        null_set = COSet.empty }

    module S = Orders_syntax

    let rec compile_set all_co pmap = function
      | S.Set co_list -> co_set_of_strings pmap co_list
      | S.Diff (set, set') -> COSet.diff (compile_set all_co pmap set) (compile_set all_co pmap set')
      | S.Complement (S.Complement set) -> compile_set all_co pmap set
      | S.Complement set -> complement all_co (compile_set all_co pmap set)

    let compile_range = function
      | S.Range (i, j) ->
         if i = j then
           EQ i
         else if i < j then
           IN (i, j)
         else
           EQ 0
      | S.Min i ->
         GE (max i 0)
      | S.Max j ->
         if j > 0 then
           IN (0, j)
         else
           EQ 0

    let make_interval_or_slice mode all_co pmap co_set range =
      let co_set = compile_set all_co pmap co_set in
      let orders_map =
        COSet.fold (fun co map -> COMap.add co (compile_range range, mode) map) co_set COMap.empty in
      { accept_all with orders_map }

    let compile_atom all_co pmap = function
      | S.Null co_set | S.Exact (co_set, 0)
      | S.Interval (co_set, (S.Max 0 | S.Range (_, 0)))
      | S.Slices (co_set, (S.Max 0 | S.Range (_, 0))) ->
         { accept_all with null_set = compile_set all_co pmap co_set }
      | S.Exact (co_set, n) ->
         let co_set = compile_set all_co pmap co_set in
         let orders_map = COSet.fold (fun co map -> COMap.add co (EQ n, Slice) map) co_set COMap.empty in
         { accept_all with orders_map }
      | S.Interval (co_set, range) ->
         make_interval_or_slice Sum all_co pmap co_set range
      | S.Slices (co_set, range) ->
         make_interval_or_slice Slice all_co pmap co_set range

    let in_or_eq i j =
      if i = j then
        Some (EQ i)
      else if i <= j then
        Some (IN (i, j))
      else
        None

    let and_range_opt r1 r2 =
      match r1, r2 with
      | GE i1, GE i2 ->
         Some (GE (max i1 i2))
      | EQ i1, EQ i2 ->
         if i1 = i2 then Some (EQ i1) else None
      | IN (i1, j1), IN (i2, j2) ->
         in_or_eq (max i1 i2) (min j1 j2)
      | IN (i, j), GE k | GE k, IN (i, j) ->
         in_or_eq (max i k) j
      | GE i, EQ j | EQ j, GE i ->
         if i <= j then Some (EQ i) else None
      | IN (i, j), EQ k | EQ k, IN (i, j) ->
         if i <= k && k <= j then Some (EQ k) else None

    let prefer_slice m1 m2 =
      match m1, m2 with
      | Sum, Sum -> Sum
      | Slice, Sum | Sum, Slice | Slice, Slice -> Slice

    let and_range co (r1, m1) (r2, m2) =
      match and_range_opt r1 r2 with
      | None -> None
      | Some r -> Some (r, prefer_slice m1 m2)

    let and_pair c1 c2 =
      { null_set = COSet.union c1.null_set c2.null_set;
        orders_map = COMap.union and_range c1.orders_map c2.orders_map }

    let gap co =
      let co = M.coupling_order_to_string co in
      invalid_arg (Printf.sprintf "or_range: %s: ranges with gaps not supported!" co)

    let or_range_opt co r1 r2 =
      match r1, r2 with
      | GE i1, GE i2 ->
         Some (GE (max 0 (min i1 i2)))
      | EQ i1, EQ i2 ->
         if i1 = i2 then
           Some (EQ i1)
         else if i1 = pred i2  then
           Some (IN (i1, i2))
         else if i1 = succ i2  then
           Some (IN (i2, i1))
         else
           gap co
      | IN (i1, j1), IN (i2, j2) ->
         if i2 <= succ j1 then
           Some (IN (i1, j2))
         else if i1 <= succ j2 then
           Some (IN (i2, j1))
         else
           gap co
      | IN (i, j), GE k | GE k, IN (i, j) ->
         if k <= succ j then Some (GE i) else gap co
      | GE i, EQ j | EQ j, GE i ->
         if j >= pred j then Some (GE j) else gap co
      | IN (i, j), EQ k | EQ k, IN (i, j) ->
         if i <= k && k <= j then
           Some (IN (i, j))
         else if k = pred i then
           Some (IN (k, j))
         else if k = succ j then
           Some (IN (i, k))
         else
           gap co

    let or_range co (r1, m1) (r2, m2) =
      match or_range_opt co r1 r2 with
      | None -> None
      | Some r -> Some (r, prefer_slice m1 m2)

    (* This will be used with [COMap.merge] and fails if the coupling
       order [co] appears as key in only one of the maps. *)
    let merge_or_range co r1 r2 =
      match r1, r2 with
      | None, None -> None
      | Some r1, Some r2 -> or_range co r1 r2
      | None, Some _ | Some _, None ->
         let co = M.coupling_order_to_string co in
         invalid_arg (Printf.sprintf "or_range: %s: OR of different coupling_orders not supported!" co)

    let or_pair c1 c2 =
      { null_set = COSet.inter c1.null_set c2.null_set;
        orders_map = COMap.merge merge_or_range c1.orders_map c2.orders_map }

    let cleanup_condition c =
      let null_set =
        COMap.fold
          (fun co (r, _) set ->
            match r with
            | EQ 0 | IN (_, 0) -> COSet.add co set
            | _ -> COSet.remove co set)
          c.orders_map c.null_set in
      let orders_map = COMap.filter (fun co _ -> not (COSet.mem co null_set)) c.orders_map in
      { null_set; orders_map }

    let combine_conditions combine_pairs = function
      | [] -> accept_all
      | c0 :: clist -> cleanup_condition (List.fold_left combine_pairs c0 clist)
        
    let compile expr =
      let all_co = co_set ()
      and pmap = co_map () in
      let rec compile' = function
        | S.Atom atom -> compile_atom all_co pmap atom
        | S.And clist -> combine_conditions and_pair (List.map compile' clist)
        | S.Or clist -> combine_conditions or_pair (List.map compile' clist) in
      let c = cleanup_condition (compile' expr) in
      let braket_rev, fusion_rev, only_fusion_rev =
        COMap.fold
          (fun co (range, mode) (braket, fusion, only_fusion) ->
            let co_range = (co, range) in
            match mode with
            | Slice -> (co_range :: braket, co_range :: fusion, only_fusion)
            | Sum -> (braket, co_range :: fusion, co_range :: only_fusion))
        c.orders_map ([], [], []) in
      { braket = List.rev braket_rev;
        fusion = List.rev fusion_rev;
        only_fusion = List.rev only_fusion_rev;
        exclusive_braket = List.rev_map fst braket_rev;
        exclusive_fusion = List.rev_map fst fusion_rev;
        is_null = c.null_set}

    (* An empty list of ranges is interpreted as no constraint.
       This is used for brakets. *)
    let in_range n = function
      | GE i -> n >= i
      | IN (i, j) -> n >= i && n <= j
      | EQ i -> n = i

    (* In fusions, the coupling orders may still be below the final range. *)
    let beneath_range n = function
      | IN (_, i) | EQ i -> n <= i
      | GE _ -> true

    (* Test whether to include a vertex at all. *)
    let test_condition range_tester is_null condition co_list =
      let rec test_condition' acc = function
        | [], [] -> (* we're done *)
           Some (List.rev acc)
        | (co, r) :: rest, [] -> (* conditions on some orders remain, add them with power 0 *)
           if range_tester 0 r then
             test_condition' ((co, 0) :: acc) (rest, [])
           else
             None
        | [], (co', n') :: rest' -> (* no further conditions, check that the remaining couplings are allowed *)
           if n' > 0 && COSet.mem co' is_null then
             None
           else
             test_condition' acc ([], rest')
        | ((co, r) :: rest as orders), ((co', n') :: rest' as orders') ->
           if n' > 0 && COSet.mem co' is_null then (* bail if the coupling is forbidden *)
             None
           else if co = co' then  (* condition and coupling line up *)
             begin
               if range_tester n' r then
                 test_condition' ((co', n') :: acc) (rest, rest')
               else
                 None
             end
           else if co < co' then (* condition missing from the couplings *)
             begin
               if range_tester 0 r then
                 test_condition' ((co, 0) :: acc) (rest, orders')
               else
                 None
             end
           else (* coupling not in the conditions, skip it *)
             test_condition' acc (orders, rest') in
      test_condition' [] (condition, co_list)

    (* Check that a the sum of coupling orders in a fusion does not exceed
       the limits. *)
    let fusion condition co_list =
      match test_condition beneath_range condition.is_null condition.fusion co_list with
      | None -> false
      | Some _ -> true

    (* Check both the intervals in [only_fusion] and the slices in [braket], but
       return only the matches of the latter: *)
    let braket condition co_list =
      match test_condition in_range condition.is_null condition.only_fusion co_list with
      | None -> None
      | Some _ -> test_condition in_range condition.is_null condition.braket co_list

    let constant condition co_list =
      not (List.exists (fun (co, n) -> n > 0 && COSet.mem co condition.is_null) co_list)

    let exclusive_fusion c = c.exclusive_fusion
    let exclusive_braket c = c.exclusive_braket

    (* Turn all intervals into slices, since we need to sum products.
       Include \emph{all} lower orders. *)
    let square_root_range = function
      | GE _ -> GE 0
      | IN (_, j) | EQ j -> IN (0, j)

    let square_root_ranges ranges =
      List.map (fun (co, range) -> (co, square_root_range range)) ranges

    let square_root c =
      let fusion =
        square_root_ranges
          (List.sort
             (fun (co1, _) (co2, _) -> Stdlib.compare co1 co2)
             (List.rev_append c.only_fusion c.braket))
      and exclusive_fusion =
        List.sort Stdlib.compare (List.rev_append c.exclusive_fusion c.exclusive_braket) in
      { fusion;
        braket = fusion;
        only_fusion = [];
        exclusive_fusion;
        exclusive_braket = exclusive_fusion;
        is_null = c.is_null }

    let parse_string s =
      Orders_parser.main Orders_lexer.token (Lexing.from_string s)

    let parse_strings slist =
      parse_string (String.concat "; " slist)

    let of_strings slist =
      compile (parse_strings slist)

    let pp fmt c =
      Format.fprintf fmt "%s" (to_string_raw c)
      
  end

(* \thocwmodulesection{Decorate Flavors with Coupling Constant Orders} *)

module type Coupling_Orders =
  sig
    type coupling_order

    (* The list is ordered wrt.~[order] and there must be no duplicate
       entry.
       Note that we're using lists instead of [Map.S.t], because we want
       to be able to use the polymorphic [compare] as long as possible.
       The lists are assumed to be short and we don't care about
       tail recursion. *)
    (* \begin{dubious}
         Eventually, we want to make this type abstract!
       \end{dubious} *)
    type orders = (coupling_order * int) list

    (* Simple constructors. *)
    val null : orders

    (* Sort the list and test it for duplicates. *)
    val of_list : (coupling_order * int) list -> orders
    val to_list : orders -> (coupling_order * int) list

    (* Add the matching powers of the coupling orders.  The coupling orders
       in both operands \emph{must} be identical and the \emph{must} appear
       in the same order.   If the coupling orders would be known at compile
       time, we could implement this in a type safe way as tuples, but the
       coupling orders can be selected on the command line and in UFO models
       not even the set of possible coupling orders is known at compile time. *)
    val add : orders -> orders -> orders

    (* Increment the powers of the coupling orders in the second operand by
       the powers of matching coupling orders in the first operand.  Ignore
       the other coupling orders in the first operand. The coupling orders in
       the operands \emph{must} be ordered according to the same ordering
       relation. *)
    val incr : orders -> orders -> orders

    (* [square_root condition orders_list] returns a triple [(used, squares, interferences)]
       where [used] is a list of are all combinations of powers of coupling orders that appear
       at least once in [squares] or [interferences].  [squares] are the terms
       that satisfy [condition] when multiplied with themselves and the pairs in
       [interferences] satisfy [condition] when  multiplied. *)
    val square_root : (orders -> bool) -> orders list ->
                      orders list * orders list * (orders * orders) list

    (* Debugging: *)
    val to_string : orders -> string

  end


module Coupling_Orders (M : sig type coupling_order val coupling_order_to_string : coupling_order -> string end) : Coupling_Orders
       with type coupling_order = M.coupling_order =
  struct

    type coupling_order = M.coupling_order
    type orders = (coupling_order * int) list

    let to_string ol =
      "{" ^ ThoList.to_string (fun (co, n) -> M.coupling_order_to_string co ^ ":" ^ string_of_int n) ol ^ "}"

    let null = []

    let rec duplicates = function
      | [] | [_] -> false
      | (o1, _) :: ((o2, _) :: _ as tail) ->
         if o1 = o2 then
           true
         else
           duplicates tail

    let of_list o =
      let o = List.sort (fun (o1, _) (o2, _) -> Stdlib.compare o1 o2) o in
      if duplicates o then
        invalid_arg "Orders.Flavor.of_list: duplicates"
      else
        o

    let to_list o = o

    (* Here's a dedicated version, but \ldots *)
    let rec add ol1 ol2 =
      match ol1, ol2 with
      | [], [] -> []
      | [], tail | tail, [] -> invalid_arg "Orders.Coupling_Orders.add: length mismatch"
      | (o1, n1) :: tail1, (o2, n2) :: tail2 ->
         if o1 = o2 then
           (o1, n1 + n2) :: add tail1 tail2
         else
           invalid_arg
             (Printf.sprintf "Orders.Coupling_Orders.add: mismatch '%s' <> '%s'"
                (M.coupling_order_to_string o1) (M.coupling_order_to_string o2))

    (* Here's a tail recursive version.  Once we can use a modern compiler
       with the tail-mod-cons optimization, we can go back to the first version. *)
    let add ol1 ol2 =
      let rec add' acc ol1 ol2 =
        match ol1, ol2 with
        | [], [] -> List.rev acc
        | [], tail | tail, [] -> invalid_arg "Orders.Coupling_Orders.add: length mismatch"
        | (o1, n1) :: tail1, (o2, n2) :: tail2 ->
           if o1 = o2 then
             add' ((o1, n1 + n2) :: acc) tail1 tail2
           else
             invalid_arg
               (Printf.sprintf "Orders.Coupling_Orders.add: mismatch '%s' <> '%s'"
                  (M.coupling_order_to_string o1) (M.coupling_order_to_string o2)) in
      add' [] ol1 ol2

    (* This is very similar to [add], but coupling orders that appear only in
       the first, but not the second argument are ignored. *)
    let rec incr ol1 ol2 =
      match ol1, ol2 with
      | _, [] -> (* we're done with the second argument, ignore the rest of the first *)
         []
      | [], tail -> (* we're done with the first argument, keep the rest of the second *)
         tail
      | (o1, n1) :: tail1, (o2, n2 as on2) :: tail2 ->
         if o1 = o2 then (* coupling orders match, add the powers *)
           (o1, n1 + n2) :: incr tail1 tail2
         else if o1 < o2 then (* [o1] does not appear in the second argument, ignore it *)
           incr tail1 ol2
         else  (* [o2] does not appear in the first argument, keep it unchanged *)
           on2 :: incr ol1 tail2

    (* Here's again a tail recursive version. *)
    let incr ol1 ol2 =
      let rec incr' acc ol1 ol2 =
        match ol1, ol2 with
        | _, [] -> (* we're done with the second argument, ignore the rest of the first *)
           List.rev acc
        | [], tail -> (* we're done with the first argument, keep the rest of the second *)
           List.rev_append acc tail
        | (o1, n1) :: tail1, (o2, n2 as on2) :: tail2 ->
           if o1 = o2 then (* coupling orders match, add the powers *)
             incr' ((o1, n1 + n2) :: acc) tail1 tail2
           else if o1 < o2 then (* [o1] does not appear in the second argument, ignore it *)
             incr' acc tail1 ol2
           else  (* [o2] does not appear in the first argument, keep it unchanged *)
             incr' (on2 :: acc) ol1 tail2 in
      incr' [] ol1 ol2

    let _add ol1 ol2 =
      let ol = add ol1 ol2 in
      Printf.eprintf "add %s %s -> %s\n" (to_string ol1) (to_string ol2) (to_string ol);
      ol

    let _incr ol1 ol2 =
      let ol = incr ol1 ol2 in
      Printf.eprintf "incr %s %s -> %s\n" (to_string ol1) (to_string ol2) (to_string ol);
      ol

    (* Resist the temptation to implement this as
       [List.fold_left add null olist],
       because then [add] would need to accept orders
       of different lengths. *)
    let sum = function
      | [] -> null
      | o :: rest -> List.fold_left add o rest

    (* We use the polymorphic compare, because we don't need a particular ordering
       to test of equality in a [Set]. *)
    module OSet = Set.Make(struct type t = orders let compare = Stdlib.compare end)

    (* Return the list of all pairs of elements of a list, where the first element
       appears before the second in the list.
       E.\,g.~[ ordered_pairs [1; 2; 3] = [(1, 2); (1, 3); (2, 3)] ] *)

    (* For longer lists for which the result will be passed to [List.fold],
       an implementation of the corresponding [fold] would be more efficient,
       but the lists will always be short. *)
    let rec ordered_pairs = function
      | [] -> []
      | a1 :: a2_list -> List.map (fun a2 -> (a1, a2)) a2_list @ ordered_pairs a2_list

    let square_root condition orders =
      let used = OSet.empty in
      let squares, used =
        List.fold_right
          (fun o (squares, used as acc) ->
            if condition (add o o) then
              (o :: squares, OSet.add o used)
            else
              acc)
        orders ([], used) in
      let interferences, used =
        List.fold_right
          (fun (o1, o2 as o12) (interferences, used as acc) ->
            if condition (add o1 o2) then
              (o12 :: interferences, OSet.add o1 (OSet.add o2 used))
            else
              acc)
          (ordered_pairs orders) ([], used) in
      (OSet.elements used, squares, interferences)

  end

(* \begin{dubious}
     Conceptually, there is no need to demand a [Colorized] model as
     a functor argument.  Nevertheless, we should first implement a
     working example for the common use case, before embarking on
     a generalization that is mostly of academic interest.
   \end{dubious} *)

module Flavor (M : Model.Colorized) =
  struct

    module CO = Coupling_Orders(M)

    type orders = CO.orders

    let add_orders = CO.add
    let incr_orders = CO.incr
    let null = CO.null
    let orders_of_list = CO.of_list

    type t = { all_orders : M.flavor; orders : orders }
    let all_orders f = f.all_orders
    let pullback f a = f (all_orders a)
    let make all_orders orders = { all_orders; orders }
    let trivial f = make f null

    (* Resist the temptation to implement this as
       [List.fold_right (fun f -> add_orders f.orders) f_list null],
       because then [add_orders] would need to accept orders
       of different lengths. *)
    let fuse_orders = function
      | [] -> null
      | f :: rest -> List.fold_right (fun f -> add_orders f.orders) rest f.orders

    let orders_to_string = CO.to_string
        
    let digit_to_symbol i =
      if i < 0 then
        invalid_arg "Orders.Flavor.digit_to_symbol: negative"
      else
        if i < 10 then
          string_of_int i
        else if i < 36 then
          String.make 1 (Char.chr (Char.code 'A' + i - 10))
        else
          invalid_arg "Orders.Flavor.digit_to_symbol: too large"

    let orders_symbol orders =
      match CO.to_list orders with
      | [] -> ""
      | orders ->
         if List.for_all (fun (_, n) -> n = 0) orders then
           ""
         else
           "_c" ^ String.concat "" (List.map (fun (_, n) -> digit_to_symbol n) orders)

    let to_string f =
      M.flavor_to_string f.all_orders ^ orders_to_string f.orders

    let to_symbol f =
      M.flavor_symbol f.all_orders ^ orders_symbol f.orders

  end

(* \thocwmodulesection{Slice Amplitudes According to Coupling Constant Orders} *)

let incomplete s =
  failwith ("Orders.Slice()." ^ s ^ " not done yet!")

module Slice (CM : Model.Colorized) =
  struct

    module OCF = Flavor(CM)

    type flavor = OCF.t
    type flavor_sans_color = CM.flavor_sans_color
    type flavor_all_orders = CM.flavor
    type gauge = CM.gauge
    type constant = CM.constant
    type coupling_order = CM.coupling_order
    type orders = OCF.orders
    module Ch = CM.Ch
    let charges = OCF.pullback CM.charges
    let flavor_sans_color = OCF.pullback CM.flavor_sans_color
    let flavor_all_orders = OCF.all_orders
    let trivial = OCF.trivial
    let orders f = f.OCF.orders
    let add_orders = OCF.add_orders
    let incr_orders = OCF.incr_orders
    let orders_to_string = OCF.orders_to_string
    let orders_symbol = OCF.orders_symbol
    let flavor_equal f1 f2 =
      CM.flavor_equal (flavor_all_orders f1) (flavor_all_orders f2) && f1.orders = f2.orders
    let color = OCF.pullback CM.color
    let pdg = OCF.pullback CM.pdg
    let lorentz = OCF.pullback CM.lorentz
    let propagator = OCF.pullback CM.propagator
    let width = OCF.pullback CM.width
    let conjugate f = { f with OCF.all_orders = CM.conjugate f.OCF.all_orders } 
    let conjugate_sans_color = CM.conjugate_sans_color
    let conjugate_all_orders = CM.conjugate
    let fermion = OCF.pullback CM.fermion
    let max_degree = CM.max_degree
    let max_degree = CM.max_degree

    let vertices () =
      incomplete "vertices"

    let coupling = function
      | Coupling.V3 (_, _, c) | Coupling.V4 (_, _, c) | Coupling.Vn (_, _, c) -> c

    let incr_coupling_orders orders (f, c) =
      let coupling_orders = CM.coupling_orders (coupling c) in
      let orders = OCF.incr_orders (OCF.orders_of_list coupling_orders) orders in
      (OCF.make f orders, c)

    let fuse2 f1 f2 =
      let orders = OCF.fuse_orders [f1; f2] in
      List.map (incr_coupling_orders orders) (CM.fuse2 (flavor_all_orders f1) (flavor_all_orders f2))

    let fuse3 f1 f2 f3 =
      let orders = OCF.fuse_orders [f1; f2; f3] in
      List.map (incr_coupling_orders orders) (CM.fuse3 (flavor_all_orders f1) (flavor_all_orders f2) (flavor_all_orders f3))

    let fuse flavors =
      let orders = OCF.fuse_orders flavors in
      List.map (incr_coupling_orders orders) (CM.fuse (List.map flavor_all_orders flavors))

    let flavors () =
      List.map OCF.trivial (CM.flavors ())

    let all_coupling_orders = CM.all_coupling_orders
    let coupling_order_to_string = CM.coupling_order_to_string
    let coupling_orders = CM.coupling_orders

    let nc = CM.nc

    let external_flavors () =
      List.map
        (fun (group, flavors) ->
          (group, List.map OCF.trivial flavors))
        (CM.external_flavors ())

    let goldstone f =
      match CM.goldstone (OCF.all_orders f) with
      | None -> None
      | Some (f, c) -> Some (OCF.trivial f, c)

    let parameters = CM.parameters
    let flavor_of_string s = OCF.trivial (CM.flavor_of_string s)
    let flavor_to_string = OCF.to_string
    let flavor_to_TeX = OCF.pullback CM.flavor_to_TeX
    let flavor_symbol = OCF.to_symbol
    let gauge_symbol = CM.gauge_symbol
    let mass_symbol = OCF.pullback CM.mass_symbol
    let width_symbol = OCF.pullback CM.width_symbol
    let constant_symbol = CM.constant_symbol
    let options = CM.options
    let caveats = CM.caveats

    let amplitude orders fin fout =
      (List.map (fun f -> OCF.make f orders) fin,
       List.map (fun f -> OCF.make f orders) fout)

    let flow fin fout =
      CM.flow (List.map flavor_all_orders fin) (List.map flavor_all_orders fout)

  end

(* \thocwmodulesection{Unit Tests} *)
module Test =
  struct

    module O = Coupling_Orders (struct type coupling_order = int let coupling_order_to_string = string_of_int end)

    open OUnit

    let suite_add =

      "add" >:::
        [ "[(1,1); (2,4)] + [(1,2); (2,3)]" >::
            (fun () -> assert_equal [(1,3); (2,7)] (O.add [(1,1); (2,4)] [(1,2); (2,3)])) ]


    let suite_incr =

      "incr" >:::
        [ "[(1,1); (3,4)] + [(2,2); (3,3)]" >::
            (fun () -> assert_equal [(2,2); (3,7)] (O.incr [(1,1); (3,4)] [(2,2); (3,3)])) ]


    module M (* [: Model_CO] *) =
      struct
        type constant = E | G | G2 | L
        type coupling_order = EW | QCD | BSM
        let all_coupling_orders () = [EW; QCD; BSM]
        let coupling_order_to_string = function
          | EW -> "EW"
          | QCD -> "QCD"
          | BSM -> "BSM"
        let coupling_orders = function
          | E -> [(EW,1)]
          | G -> [(QCD,1)]
          | G2 -> [(QCD,2)]
          | L -> [(BSM,1)]
      end

    module C = Conditions (M)

    let pup expected slist =
      assert_equal ~printer:(fun s -> "\"" ^ s ^ "\"")
        expected (C.to_string (C.of_strings slist))

    let suite_parser =
      "parsing" >:::
        [ "EW=1" >:: (fun () -> pup "EW = 1" ["EW=1"]);
          "~EW" >:: (fun () -> pup "{QCD, BSM} = 0" ["~EW"]);
          "!BSM,QCD" >:: (fun () -> pup "BSM = 0; QCD = {1..2}" ["BSM; QCD={1..2}"]);
          "!BSM,QCD'" >:: (fun () -> pup "BSM = 0; QCD = {1..2}" ["BSM={0}; QCD={1..2}"]);
          "EW/QCD" >:: (fun () -> pup "EW = 2; QCD = 1" ["EW=2; QCD=1"]);
          "EW/QCD" >:: (fun () -> pup "EW = 1; QCD = 1" ["EW=1; QCD=1"]);
          "EW/QCD'" >:: (fun () -> pup "EW = 1; QCD = 1" ["{EW,QCD}=1"]);
          "EW=1,2,3" >:: (fun () -> pup "EW = 3" ["EW=1;EW=2;EW=3"]) ]

    let cos_option_to_string = function
      | None -> "*"
      | Some co_list ->
         ThoList.to_string (fun (co, n) -> M.coupling_order_to_string co ^ "=" ^ string_of_int n) co_list

    let sort orders =
      List.sort (fun (co1, _) (co2, _) -> compare co1 co2) orders

    let map_opt f = function
      | None -> None
      | Some a -> Some (f a)

    let assert_braket expected conditions orders =
      let conditions = C.of_strings conditions in
      assert_equal ~printer:cos_option_to_string
        (map_opt sort expected)
        (map_opt sort (C.braket conditions (sort orders)))

    let assert_fusion expected conditions orders =
      let conditions = C.of_strings conditions in
      assert_equal ~printer:string_of_bool expected (C.fusion conditions (sort orders))

    let suite_fusion =
      let open M in
      "fusion" >:::
        [ "BSM;EW=2;QCD=1: QCD=1" >::
            (fun () -> assert_fusion true ["BSM;EW=2;QCD=1"] [(QCD,1)]);

          "BSM;EW=2;QCD=1: EW=1" >::
            (fun () -> assert_fusion true ["BSM;EW=2;QCD=1"] [(EW,1)]);

          "BSM;EW=2;QCD=1: EW=1;QCD=1" >::
            (fun () -> assert_fusion true ["BSM;EW=2;QCD=1"] [(EW,1); (QCD,1)]);

          "BSM;EW=2;QCD=1: EW=2;QCD=1" >::
            (fun () -> assert_fusion true ["BSM;EW=2;QCD=1"] [(EW,2); (QCD,1)]);

          "BSM;EW=2;QCD=1: EW=1;QCD=2" >::
            (fun () -> assert_fusion false ["BSM;EW=2;QCD=1"] [(EW,1); (QCD,2)]);

          "BSM;EW=2;QCD=1: BSM=1" >::
            (fun () -> assert_fusion false ["BSM;EW=2;QCD=1"] [(BSM,1)]);

          "BSM;EW=2;QCD=1: BSM=0" >::
            (fun () -> assert_fusion true ["BSM;EW=2;QCD=1"] [(BSM,0)]) ]

    let suite_braket =
      let open M in
      "braket" >:::
        [ "BSM;EW=2;QCD=1: QCD=1" >::
            (fun () -> assert_braket None ["BSM;EW=2;QCD=1"] [(QCD,1)]);

          "BSM;EW=2;QCD=1: EW=1" >::
            (fun () -> assert_braket None ["BSM;EW=2;QCD=1"] [(EW,1)]);

          "BSM;EW=2;QCD=1: EW=1;QCD=1" >::
            (fun () -> assert_braket None ["BSM;EW=2;QCD=1"] [(EW,1); (QCD,1)]);

          "BSM;EW=2;QCD=1: EW=2;QCD=1" >::
            (fun () -> assert_braket (Some [(EW,2); (QCD,1)]) ["BSM;EW=2;QCD=1"] [(EW,2); (QCD,1)]);

          "BSM;EW=2;QCD=1: EW=1;QCD=2" >::
            (fun () -> assert_braket None ["BSM;EW=2;QCD=1"] [(EW,1); (QCD,2)]);

          "BSM;EW=2;QCD=1: BSM=1" >::
            (fun () -> assert_braket None ["BSM;EW=2;QCD=1"] [(BSM,1)]);

          "BSM;EW=2;QCD=1: BSM=0" >::
            (fun () -> assert_braket None ["BSM;EW=2;QCD=1"] [(BSM,0)]);

          "EW={0..}: BSM=0" >::
            (fun () -> assert_braket (Some [(EW,0)]) ["EW={0..}"] [(BSM,0)]);

          "EW={0..}: EW=1" >::
            (fun () -> assert_braket (Some [(EW,1)]) ["EW={0..}"] [(EW,1)]);

          "EW={0..}: BSM=1;EW=1" >::
            (fun () -> assert_braket (Some [(EW,1)]) ["EW={0..}"] [(BSM,1); (EW,1)]) ]

(* \begin{dubious}
     We should add more unit tests, time permitting.
   \end{dubious} *)

    let suite =
      "Orders" >:::
        [ suite_add;
          suite_incr;
          suite_parser;
          (*[ suite_fusion;] *)
          suite_braket ]

  end
