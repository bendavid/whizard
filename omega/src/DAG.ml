(* DAG.ml --

   Copyright (C) 1999-2025 by

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

module type Ord =
  sig
    type t
    val compare : t -> t -> int
  end

module type Forest =
  sig
    module Nodes : Ord
    type node = Nodes.t
    type edge
    type children
    type t = edge * children
    val compare : t -> t -> int
    val for_all : (node -> bool) -> t -> bool
    val fold : (node -> 'a -> 'a) -> t -> 'a -> 'a
  end

module type T =
  sig
    type node
    type edge
    type children
    type t
    val empty : t
    val add_node : node -> t -> t
    val add_offspring : node -> edge * children -> t -> t
    exception Cycle
    val add_offspring_unsafe : node -> edge * children -> t -> t
    val is_node : node -> t -> bool
    val is_sterile : node -> t -> bool
    val is_offspring : node -> edge * children -> t -> bool
    val iter_nodes : (node -> unit) -> t -> unit
    val map_nodes : (node -> node) -> t -> t
    val fold_nodes : (node -> 'a -> 'a) -> t -> 'a -> 'a
    val iter : (node -> edge * children -> unit) -> t -> unit
    val map : (node -> node) ->
      (node -> edge * children -> edge * children) -> t -> t
    val fold : (node -> edge * children -> 'a -> 'a) -> t -> 'a -> 'a
    val lists : t -> (node * (edge * children) list) list
    val dependencies : t -> node -> (node, edge) Tree2.t
    val harvest : t -> node -> t -> t
    val harvest_list : t -> node list -> t
    val size : t -> int
    val eval : (node -> 'a) -> (node -> edge -> 'c -> 'd) ->
      ('a -> 'c -> 'c) -> ('d -> 'a -> 'a) -> 'a -> 'c -> node -> t -> 'a
    val eval_memoized : (node -> 'a) -> (node -> edge -> 'c -> 'd) ->
      ('a -> 'c -> 'c) -> ('d -> 'a -> 'a) -> 'a -> 'c -> node -> t -> 'a
    val forest : node -> t -> (node * edge option, node) Tree.t list
    val forest_memoized : node -> t -> (node * edge option, node) Tree.t list
    val count_trees : node -> t -> int
   end

module type Graded_Ord =
  sig
    include Ord
    module G : Ord
    val rank : t -> G.t
  end

module type Grader = functor (O : Ord) -> Graded_Ord with type t = O.t

module type Graded_Forest =
  sig
    module Nodes : Graded_Ord
    type node = Nodes.t
    type edge
    type children
    type t = edge * children
    val compare : t -> t -> int
    val for_all : (node -> bool) -> t -> bool
    val fold : (node -> 'a -> 'a) -> t -> 'a -> 'a
  end

module type Forest_Grader = functor (G : Grader) -> functor (F : Forest) ->
  Graded_Forest with type Nodes.t = F.node
  and type node = F.node
  and type edge = F.edge
  and type children = F.children
  and type t = F.t

(* \thocwmodulesection{The [Forest] Functor} *)

module Forest (PT : Tuple.Poly) (N : Ord) (E : Ord) :
    Forest with module Nodes = N and type edge = E.t
    and type node = N.t and type children = N.t PT.t =
  struct
    module Nodes = N
    type edge = E.t
    type node = N.t
    type children = node PT.t
    type t = edge * children

    let compare (edge1, children1) (edge2, children2) =
      let c = PT.compare N.compare children1 children2 in
      if c <> 0 then
        c
      else
        E.compare edge1 edge2

    let for_all f (_, nodes) = PT.for_all f nodes
    let fold f (_, nodes) acc = PT.fold_right f nodes acc

  end

(* \thocwmodulesection{Gradings} *)

module Chaotic (O : Ord) =
  struct
    include O
    module G =
      struct
        type t = unit
        let compare _ _ = 0
      end
    let rank _ = ()  
  end

module Discrete (O : Ord) =
  struct
    include O
    module G = O
    let rank x = x
  end

module Fake_Grading (O : Ord) =
  struct
    include O
    exception Impossible of string
    module G =
      struct
        type t = unit
        let compare _ _ = raise (Impossible "G.compare")
      end
    let rank _ = raise (Impossible "G.compare")
  end

module Grade_Forest (G : Grader) (F : Forest) =
  struct
    module Nodes = G(F.Nodes)
    type node = Nodes.t
    type edge = F.edge
    type children = F.children
    type t = F.t
    let compare = F.compare
    let for_all = F.for_all
    let fold = F.fold
  end

(* A subset of [Map.S], with graded keys. The map is implemented
   as a two level map with the outer map from the rank of the key
   to a map from all key of this rank to the values.
   Thus we can find query the minimal and maximal ranks
   and find all keys with a given rank without having to
   scan the entire map.*)

module type Graded_Map =
  sig

(* We implement the subset of [Map.S] from the standard library
   that we need in our applications. The semantics is identical
   to [Map.S] so we don't need to duplicate the documentation.
   It would be trivial to implement the rest, if we ever need it. *)

    type key
    type 'a t
    val empty : 'a t
    val add : key -> 'a -> 'a t -> 'a t
    val find : key -> 'a t -> 'a
    val mem : key -> 'a t -> bool
    val iter : (key -> 'a -> unit) -> 'a t -> unit
    val fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b

(* Here come the additional functions dealing with the [rank].
   All could be implemented by inspecting all keys in a map,
   but the keeping track of the grading makes them much more
   efficient.*)
    type rank

(* Return a list of all ranks in a map.  The application should not
   rely on the fact that the list is sorted. *)
    val ranks : 'a t -> rank list

(* Return the minimal and maximal rank in the map, according to the
   order of [rank]. *)
    val min_max_rank : 'a t -> rank * rank

(* Return all keys with the given [rank].  *)
    val ranked : rank -> 'a t -> key list

  end

module type Graded_Map_Maker = functor (O : Graded_Ord) ->
  Graded_Map with type key = O.t and type rank = O.G.t

(* \begin{dubious}
      Nested ['a -> 'b opt] functions cry out for the
      monadic binding operators introduced by O'Caml 4.08.
   \end{dubious} *)
module Graded_Map (O : Graded_Ord) :
    Graded_Map with type key = O.t and type rank = O.G.t =
  struct
    module M1 = Map.Make(O.G)
    module M2 = Map.Make(O)

    type key = O.t
    type rank = O.G.t

    type (+'a) t = 'a M2.t M1.t

    let empty = M1.empty

    let map2_of_rank rank map1 = 
      match M1.find_opt rank map1 with
      | None -> M2.empty
      | Some map2 -> map2

    let add key data map1 =
      let rank = O.rank key in
      M1.add rank (M2.add key data (map2_of_rank rank map1)) map1

    let find key map1 =
      M2.find key (M1.find (O.rank key) map1)

    let mem key map1 =
      M2.mem key (map2_of_rank (O.rank key) map1)

    let iter f map1 =
      M1.iter (fun _rank -> M2.iter f) map1

    let fold f map1 acc1 =
      M1.fold (fun _rank -> M2.fold f) map1 acc1

(* \begin{dubious}
     The set of ranks and its minimum and maximum should be maintained
     explicitely!
   \end{dubious} *)
    module S1 = Set.Make(O.G)

    let ranks map =
      M1.fold (fun key _data acc -> key :: acc) map []

    let rank_set map =
      M1.fold (fun key _data -> S1.add key) map S1.empty

    let min_max_rank map =
      let s = rank_set map in
      (S1.min_elt s, S1.max_elt s)

    module S2 = Set.Make(O)

    let keys map =
      M2.fold (fun key _data acc -> key :: acc) map []

    let _sorted_keys map =
      S2.elements (M2.fold (fun key _data -> S2.add key) map S2.empty)

    let ranked rank map1 =
      keys (map2_of_rank rank map1)

  end

(* \thocwmodulesection{The DAG Functor} *)

(* Currently, we are \emph{not} using the grading in O'Mega.
   It seemed to be an interesting idea for structuring DAGs,
   but we have not yet come up with a real use case \ldots *)

module Maybe_Graded (GMM : Graded_Map_Maker) (F : Graded_Forest) =
  struct

    module G = F.Nodes.G

    type node = F.node
    type rank = G.t
    type edge = F.edge
    type children = F.children

(* If we get tired of graded DAGs, we just have to replace [Graded_Map] by
   [Map] here and remove [ranked] below and gain a tiny amount of simplicity
   and efficiency. *)

    module Parents = GMM(F.Nodes)
    module Offspring = Set.Make(F)

    type t = Offspring.t Parents.t

    let rank = F.Nodes.rank
    let ranks = Parents.ranks
    let min_max_rank = Parents.min_max_rank
    let ranked = Parents.ranked

    let empty = Parents.empty

    let add_node node dag =
      if Parents.mem node dag then
        dag
      else
        Parents.add node Offspring.empty dag

    let add_offspring_unsafe node offspring dag =
      let offsprings =
        try Parents.find node dag with Not_found -> Offspring.empty in
      Parents.add node (Offspring.add offspring offsprings)
        (F.fold add_node offspring dag)

(*i
    let c = ref 0
    let offspring_add offspring offsprings =
      if Offspring.mem offspring offsprings then
        (Printf.eprintf "<<<%d>>>\n" !c; incr c);
      Offspring.add offspring offsprings

    let add_offspring_unsafe node offspring dag =
      let offsprings =
        try Parents.find node dag with Not_found -> Offspring.empty in
      Parents.add node (offspring_add offspring offsprings)
        (F.fold add_node offspring dag)
i*)

    exception Cycle

    let add_offspring node offspring dag =
      if F.for_all (fun n -> F.Nodes.compare n node < 0) offspring then
        add_offspring_unsafe node offspring dag
      else
        raise Cycle

    let is_node node dag =
      Parents.mem node dag

    let is_sterile node dag =
      try
        Offspring.is_empty (Parents.find node dag)
      with
      | Not_found -> false

    let is_offspring node offspring dag =
      try
        Offspring.mem offspring (Parents.find node dag)
      with
      | Not_found -> false

    let iter_nodes f dag =
      Parents.iter (fun n _ -> f n) dag

    let iter f dag =
      Parents.iter (fun node -> Offspring.iter (f node)) dag

    let map_nodes f dag =
      Parents.fold (fun n -> Parents.add (f n)) dag Parents.empty

    let map fn fo dag =
      Parents.fold (fun node offspring ->
        Parents.add (fn node)
          (Offspring.fold (fun o -> Offspring.add (fo node o))
             offspring Offspring.empty)) dag Parents.empty

    let fold_nodes f dag acc =
      Parents.fold (fun n _ -> f n) dag acc

    let fold f dag acc =
      Parents.fold (fun node -> Offspring.fold (f node)) dag acc

(* \begin{dubious} 
     Note that in it's current incarnation,
     [fold add_offspring dag empty] copies \emph{only} the fertile nodes, while
     [fold add_offspring dag (fold_nodes add_node dag empty)]
     includes sterile ones, as does
     [map (fun n -> n) (fun n ec -> ec) dag].
   \end{dubious} *)

    let dependencies dag node =
      let rec dependencies' node' =
        let offspring = Parents.find node' dag in
        if Offspring.is_empty offspring then
          Tree2.leaf node'
        else
          Tree2.cons
            (Offspring.fold 
               (fun o acc ->
                 (fst o,
                  node',
                  F.fold (fun wf acc' -> dependencies' wf :: acc') o []) :: acc)
               offspring [])
      in
      dependencies' node
        
    let lists dag =
      List.sort (fun (n1, _) (n2, _) -> F.Nodes.compare n1 n2)
        (Parents.fold (fun node offspring l ->
          (node, Offspring.elements offspring) :: l) dag [])

    let size dag =
      Parents.fold (fun _ _ n -> succ n) dag 0

    let rec harvest dag node roots =
      Offspring.fold
        (fun offspring roots' ->
          if is_offspring node offspring roots' then
            roots'
          else
            F.fold (harvest dag)
              offspring (add_offspring_unsafe node offspring roots'))
        (Parents.find node dag) (add_node node roots)

    let harvest_list dag nodes =
      List.fold_left (fun roots node -> harvest dag node roots) empty nodes

(* Build a closure once, so that we can recurse faster: *)

    let eval f mule muln add null unit node dag =
      let rec eval' n =
        if is_sterile n dag then
          f n
        else
          Offspring.fold
            (fun (e, _ as offspring) v0 ->
              add (mule n e (F.fold muln' offspring unit)) v0)
            (Parents.find n dag) null
      and muln' n = muln (eval' n) in
      eval' node

    let count_trees node dag =
      eval (fun _ -> 1) (fun _ _ p -> p) ( * ) (+) 0 1 node dag

    let build_forest evaluator node dag =
      evaluator (fun n -> [Tree.leaf (n, None) n])
        (fun n e p -> List.map (fun p' -> Tree.cons (n, Some e) p') p)
        (fun p1 p2 -> Product.fold2 (fun n nl pl -> (n :: nl) :: pl) p1 p2 [])
        (@) [] [[]] node dag

    let forest = build_forest eval

(* At least for [count_trees], the memoizing variant [eval_memoized] is
   considerably slower than direct recursive evaluation with [eval].  *)

    let eval_offspring f mule muln add null unit dag values (node, offspring) =
      let muln' n = muln (Parents.find n values) in
      let v =
        if is_sterile node dag then
          f node
        else
          Offspring.fold
            (fun (e, _ as offspring) v0 ->
              add (mule node e (F.fold muln' offspring unit)) v0)
            offspring null
      in
      (v, Parents.add node v values)

    let eval_memoized' f mule muln add null unit dag =
      let result, _ =
        List.fold_left
          (fun (_v, values) -> eval_offspring f mule muln add null unit dag values)
          (null, Parents.empty)
          (List.sort (fun (n1, _) (n2, _) -> F.Nodes.compare n1 n2)
             (Parents.fold
                (fun node offspring l -> (node, offspring) :: l) dag [])) in
      result

    let eval_memoized f mule muln add null unit node dag =
      eval_memoized' f mule muln add null unit
        (harvest dag node empty)

    let forest_memoized = build_forest eval_memoized

  end

module type Graded =
  sig
    include T
    type rank
    val rank : node -> rank
    val ranks : t -> rank list
    val min_max_rank : t -> rank * rank
    val ranked : rank -> t -> node list
  end

module Graded (F : Graded_Forest) = Maybe_Graded(Graded_Map)(F)

(* The following is not a graded map, obviously.  But it can pass as one by the
   typechecker for constructing non-graded DAGs.  *)

module Fake_Graded_Map (O : Graded_Ord) :
    Graded_Map with type key = O.t and type rank = O.G.t =
  struct
    module M = Map.Make(O)
    type key = O.t
    type (+'a) t = 'a M.t
    let empty = M.empty
    let add = M.add
    let find = M.find
    let mem = M.mem
    let iter = M.iter
    let fold = M.fold

(* We make sure that the remaining three are never called inside [DAG] and
   are not visible outside. *)
    type rank = O.G.t
    exception Impossible of string
    let ranks _ = raise (Impossible "ranks")
    let min_max_rank _ = raise (Impossible "min_max_rank")
    let ranked _ _ = raise (Impossible "ranked")
  end

(* We could also have used signature projection with a chaotic or discrete
   grading, but the [Graded_Map] can cost some efficiency.  This is probably
   not the case for the current simple implementation, but future embellishment
   can change this.  Therefore, the ungraded DAG uses [Map] directly,
   without overhead. *)

module Make (F : Forest) =
  Maybe_Graded(Fake_Graded_Map)(Grade_Forest(Fake_Grading)(F))

(* \begin{dubious}
     If O'Caml had \textit{polymorphic recursion}, we could think
     of even more elegant implementations unifying nodes and offspring
     (cf.~the generalized tries in~\cite{Okasaki:1998:book}).
   \end{dubious} *)

(* \begin{dubious}
     GADTs to the rescue?
   \end{dubious} *)

(* \thocwmodulesection{Unit Tests} *)

module Test =
  struct

    let random_int_list imax n =
      let imax_plus = succ imax in
      Array.to_list (Array.init n (fun _ -> Random.int imax_plus))

(*i module OInts =
      struct
        type t = int
        let compare = compare
      end
i*)

    module GOInts =
      struct
        type t = int
        let compare = compare
        module G = 
          struct
            type t = int
            let compare = compare
          end
        let rank i = i mod 100
      end

    module GM = Graded_Map(GOInts)

    let int_list_to_string l =
      ThoList.to_string string_of_int l

    let _int_list2_to_string l =
      ThoList.to_string int_list_to_string l

    let int_pair_to_string (i1, i2) =
      int_list_to_string [i1; i2]

    let uniq l =
      ThoList.uniq (List.sort compare l)

    open OUnit

    let assert_equal_int_pair p1 p2 =
      assert_equal ~printer:int_pair_to_string p1 p2

    let assert_equal_unsorted_int_list l1 l2 =
      assert_equal ~printer:int_list_to_string
        (List.sort compare l1)
        (List.sort compare l2)

    let _assert_equal_unsorted_int_list_ignore_duplicates l1 l2 =
      assert_equal ~printer:int_list_to_string (uniq l1) (uniq l2)

    let squares n =
      let data =
        List.map (fun i -> (i, i * i)) (random_int_list 10000 n) in
      let map =
        List.fold_left (fun acc (i, s) -> GM.add i s acc) GM.empty data in
      (data, map)

    let suite_graded_map =

      "Graded_Map" >:::
        [ "ranks" >::
            (fun () ->
              let data, graded_map = squares 100 in
              assert_equal_unsorted_int_list
                (uniq (List.map (fun (i, _) -> GOInts.rank i) data))
                (GM.ranks graded_map));

          "min_max_rank" >::
            (fun () ->
              match squares 100 with
              | [], _ -> failwith "empty test data"
              | (r0, _) :: data, graded_map ->
                 assert_equal_int_pair
                   (List.fold_left
                      (fun (r_min, r_max) (i, _) ->
                        let r = GOInts.rank i in
                        (min r r_min, max r r_max))
                      (GOInts.rank r0, GOInts.rank r0) data)
                   (GM.min_max_rank graded_map)) ]

(* \begin{dubious}
     We should add more unit tests, time permitting.
   \end{dubious} *)

    let suite =
      "DAG" >:::
        [suite_graded_map]

  end
