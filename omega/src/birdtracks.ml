(* birdtracks.ml --

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

(* \thocwmodulesection{Types} *)

module QC = Algebra.QC
module L = Algebra.Laurent
module A = Arrow
open A.Infix

(* There can be one or more $\epsilon$ or $\bar\epsilon$, but
   not both at the same time. *)

(* I wanted to use a GADT with Peano numerals to track the number
   of $\epsilon$ and $\bar\epsilon$ in the type system.  However,
   I would have needed to implement a ``multiplication'' function
   of the type ['n1 term -> 'n2 term -> ('n1 + 'n2) term]
   that I have not been able to implement using Peano numerals for
   the type variables ['n1] and ['n2], due to the lack of an
   addition operator for Peano numerals in the type system.

Therefore I will use normal lists, sacrificing some type safety. *)

type 'a aterm = { coeff : L.t; arrows : 'a list }
type ('a, 'e) eterm = 'a aterm * 'e NEList.t
type ('a, 'b) bterm = 'a aterm * 'b NEList.t

type ('a, 'e, 'b) term =
  | Arrows of 'a aterm
  | Epsilons of ('a, 'e) eterm
  | Epsilon_Bars of ('a, 'b) bterm

(* \begin{dubious}
     Having already added type annotations for polymorphic
     recursion, I could use a simple GADT instead of an ADT at the toplevel, trying
     to maintain some unboxing potential:

  [ type ('a, 'e, 'b) term =
      | Arrows : 'a aterm -> ('a, 'e, 'b) term
      | Epsilons : ('a, 'e) eterm -> ('a, 'e, 'b) term
      | Epsilon_Bars : ('a, 'b) bterm -> ('a, 'e, 'b) term ]

     but it is not obvious that this produces a real performance benefit.
   \end{dubious} *)

type _afree = A.free aterm
type _efree = (A.free, A.free_eps) eterm
type _bfree = (A.free, A.free_eps_bar) bterm
type free = (A.free, A.free_eps, A.free_eps_bar) term

type afactor = A.factor aterm
type efactor = (A.factor, A.factor_eps) eterm
type bfactor = (A.factor, A.factor_eps_bar) bterm
type factor = (A.factor, A.factor_eps, A.factor_eps_bar) term

type t = free list

(* \thocwmodulesection{Functions} *)

let rev_aterm aterm =
  { aterm with arrows = List.map A.rev aterm.arrows }

let _rev1 = function
  | Arrows a -> Arrows (rev_aterm a)
  | Epsilons (a, e) -> Epsilon_Bars (rev_aterm a, NEList.map A.rev_eps e)
  | Epsilon_Bars (a, b) -> Epsilons (rev_aterm a, NEList.map A.rev_eps_bar b)

let aterm = function
  | Arrows a | Epsilons (a, _) | Epsilon_Bars (a, _) -> a

module ISet = Set.Make(Int)

let adjoints1 = function
  | Arrows a -> A.adjoints a.arrows
  | Epsilons (a, e) -> A.adjoints_eps a.arrows e
  | Epsilon_Bars (a, b) -> A.adjoints_eps_bar a.arrows b
    
let adjoints term =
  match List.map adjoints1 term with
  | [] -> []
  | a :: _ as all ->
     if ThoList.homogeneous all then
       a
     else
       invalid_arg
         ("Birdtracks.adjoints: inconsistent: " ^
            (ThoList.to_string (ThoList.to_string string_of_int) all))

let term_haunted term =
  List.exists A.is_ghost (aterm term).arrows

let haunted terms =
  List.exists term_haunted terms

let exorcise vertex =
  List.filter (Fun.negate term_haunted) vertex

let tips_and_tails_of_aterm aterm =
  List.fold_left
    (fun (tips, tails) arrow ->
      (List.rev_append (A.tips arrow) tips,
       List.rev_append (A.tails arrow) tails))
    ([], []) aterm.arrows
          
let tips_and_tails_raw : free -> A.tip list * A.tail list = function
  | Arrows aterm -> tips_and_tails_of_aterm aterm
  | Epsilons (aterm, epsilons) ->
     let tips, tails = tips_and_tails_of_aterm aterm in
     (List.concat (tips :: NEList.to_list epsilons), tails)
  | Epsilon_Bars (aterm, epsilon_bars) ->
     let tips, tails = tips_and_tails_of_aterm aterm in
     (tips, List.concat (tails :: NEList.to_list epsilon_bars))

let _tips_and_tails term =
  let tips, tails = tips_and_tails_raw term in
  (List.sort compare tips, List.sort compare tails)

(* Expressions *)
let const coeff = [ Arrows { coeff; arrows = [] } ]
let ints pairs = const (L.ints pairs)
let null = const L.null
let fraction n = const (L.fraction n)
let one = const L.unit
let two = const (L.int 2)
let minus = const (L.int (-1))
let int n = const (L.int n)
let nc = const (L.nc 1)
let over_nc = const (L.ints [(1, -1)])
let imag = const (L.imag 1)

module AMap = Pmap.Tree

let psort alist = List.sort compare alist
let ne_psort alist = NEList.sort compare alist

let find_term_opt term map =
  AMap.find_opt compare term map

let map_aterm fc fa aterm =
  { coeff = fc aterm.coeff; arrows = fa aterm.arrows }

let map_term_full fc fa fe fb = function
  | Arrows aterm -> Arrows (map_aterm fc fa aterm)
  | Epsilons (aterm, elist) -> Epsilons (map_aterm fc fa aterm, fe elist)
  | Epsilon_Bars (aterm, blist) -> Epsilon_Bars (map_aterm fc fa aterm, fb blist)

let map_term_deep fc fa fe fb term =
  map_term_full fc (List.map fa) (NEList.map fe) (NEList.map fb) term

let map_term f = function
  | Arrows a -> Arrows (f a)
  | Epsilons (a, e) -> Epsilons (f a, e)
  | Epsilon_Bars (a, b) -> Epsilon_Bars (f a, b)

let map_term_opt f = function
  | Arrows a ->
     begin match f a with
     | None -> None
     | Some arrows -> Some (Arrows arrows)
     end
  | Epsilons (a, e) ->
     begin match f a with
     | None -> None
     | Some arrows -> Some (Epsilons (arrows, e))
     end
  | Epsilon_Bars (a, b) ->
     begin match f a with
     | None -> None
     | Some arrows -> Some (Epsilon_Bars (arrows, b))
     end

let _canonicalize_aterm term =
  map_aterm Fun.id psort term

(* \begin{dubious}
     We're \emph{not yet} canonicalizing the $\epsilon$ and
     $\bar\epsilon$ themselves.  This could be done, if
     necessary, using [Combinatorics.sort_signed] to keep track of
     the signs.  While we're debugging, it could be beneficial to
     keep the indices where they are.
   \end{dubious} *)

let canonicalize_term : type a e b. (a, e, b) term -> (a, e, b) term =
  fun term ->
  map_term_full Fun.id psort ne_psort ne_psort term

let split_coeff : type a e b. (a, e, b) term -> L.t * (a, e, b) term  = function
  | Arrows aterm -> (aterm.coeff, Arrows { aterm with coeff = L.unit })
  | Epsilons (aterm, epsilons) ->
     (aterm.coeff, Epsilons ({ aterm with coeff = L.unit }, epsilons))
  | Epsilon_Bars (aterm, epsilon_bars) ->
     (aterm.coeff, Epsilon_Bars ({ aterm with coeff = L.unit }, epsilon_bars))

let inject_coeff : type a e b. L.t -> (a, e, b) term -> (a, e, b) term =
  fun coeff -> map_term_full (fun _ -> coeff) Fun.id Fun.id Fun.id

(* \begin{dubious}
     Note that the final result
     must be a homogeneous list with all elements containing the same
     number of $\epsilon$ and $\bar\epsilon$, because otherwise the number
     of incoming and outgoing color lince would not match.

     Nevertheless, we might have to work very hard to avoid too much code
     duplication.
   \end{dubious} *)

let canonicalize : type a e b. (a, e, b) term list -> (a, e, b) term list =
  fun terms ->
  let map =
    List.fold_left
      (fun acc term ->
        let coeff, term = split_coeff (canonicalize_term term) in
        if L.is_null coeff then
          acc
        else
          match find_term_opt term acc with
          | None -> AMap.add compare term coeff acc
          | Some coeff' ->
             let coeff'' = L.add coeff coeff' in
             if L.is_null coeff'' then
               AMap.remove compare term acc
             else
               AMap.add compare term coeff'' acc)
      AMap.empty terms in
  if AMap.is_empty map then
    []
  else
    AMap.fold (fun term coeff acc -> inject_coeff coeff term :: acc) map []

let number v =
  match canonicalize v with
  | [] -> Some L.null
  | [Arrows { coeff; arrows = [] }] -> Some coeff
  | _ -> None

let is_null v =
  match canonicalize v with
  | [] -> true
  | _ -> false

let is_unit v =
  match canonicalize v with
  | [Arrows { coeff; arrows = [] }] -> coeff = L.unit
  | _ -> false

let with_nc nc t =
  let substitute c = L.const (L.eval (QC.int nc) c) in
  canonicalize (List.map (map_term_full substitute Fun.id Fun.id Fun.id) t)

let aterm_to_string f term =
  match term.arrows with
  | [] -> Printf.sprintf "(%s)" (L.to_string "N" term.coeff)
  | arrows ->
     Printf.sprintf
       "(%s) * %s"
       (L.to_string "N" term.coeff) (ThoList.to_string f arrows)
      
let to_string1_aux fa fe fb = function
  | Arrows aterm -> aterm_to_string fa aterm
  | Epsilons (aterm, epsilons) ->
     aterm_to_string fa aterm ^ " * " ^ ThoList.to_string fe (NEList.to_list epsilons)
  | Epsilon_Bars (aterm, epsilon_bars) ->
     aterm_to_string fa aterm ^ " * " ^ ThoList.to_string fb (NEList.to_list epsilon_bars)

let to_string1 term =
  to_string1_aux A.free_to_string A.free_eps_to_string A.free_eps_bar_to_string term

let to_string_raw terms =
  ThoList.to_string to_string1 terms

let to_string terms =
  to_string_raw (canonicalize terms)

(*i
    let trivial terms =
      let result = trivial terms in
      Printf.eprintf
        "trivial %s -> %b\n"
        (to_string terms)
        result;
      trivial terms
i*)

let pp fmt v =
  Format.fprintf fmt "%s" (to_string v)

let relocate1 f term =
  map_term_deep Fun.id (A.relocate f) (List.map (A.relocate_tip f)) (List.map (A.relocate_tail f)) term

let relocate f = List.map (relocate1 f)

let rev_aterm aterm =
  { aterm with arrows = List.map A.rev aterm.arrows }

let rev1 = function
  | Arrows aterm -> Arrows (rev_aterm aterm)
  | Epsilons (aterm, elist) -> Epsilon_Bars (rev_aterm aterm, NEList.map A.rev_eps elist)
  | Epsilon_Bars (aterm, blist) -> Epsilons (rev_aterm aterm, NEList.map A.rev_eps_bar blist)

let rev = List.map rev1

let _of_afactor aterm =
  map_aterm Fun.id (List.map A.of_factor) aterm
      
let of_factor term =
  map_term_deep Fun.id A.of_factor A.of_factor_eps A.of_factor_eps_bar term
      
let to_left_factor is_sum term =
  map_term_deep Fun.id
    (A.to_left_factor is_sum)
    (A.to_left_factor_eps is_sum)
    (A.to_left_factor_eps_bar is_sum)
    term
      
let to_right_factor is_sum term =
  map_term_deep Fun.id
    (A.to_right_factor is_sum)
    (A.to_right_factor_eps is_sum)
    (A.to_right_factor_eps_bar is_sum)
    term

(* We start with the simply recursive evaluation functions,
   leaving the the more complicated mutually recursive
   functions for later. *)

(* Add one [arrow] to a list of arrows, updating [coeff]
   if necessary. Accumulate already processed arrows in [seen].
   Returns [None] if there is a mismatch (a gluon meeting
   a ghost) and [Some afactor] containing a coefficient and a
   list of arrows otherwise. *)

(* We assume that the trivial cases of no summation indices
   and the arrow looping back to itself have already been filtered
   out. *)

(* \label{pg:add_arrow} *)

let rec add_arrow_to_arrows_list' coeff seen arrow = function
  | [] -> (* visited all [arrows]: no opportunities for further matches *)
     Some ({ coeff; arrows = arrow :: seen })
  | arrow' :: arrows' ->
     begin match A.merge_arrow_arrow arrow arrow' with
     | A.Mismatch ->
        None
     | A.Ghost_Match -> (* replace matching ghosts by $-1/N_C$ *)
        Some ({ coeff = L.mul (L.over_nc (-1)) coeff;
                arrows = List.rev_append seen arrows' })
     | A.Loop_Match -> (* replace a loop by $N_C$ *)
        Some ({ coeff = L.mul (L.nc 1) coeff;
                arrows = List.rev_append seen arrows' })
     | A.Match arrow'' -> (* two arrows have been merged into one *)
        if A.is_free arrow'' then (* no opportunities for further matches *)
          Some ({ coeff; arrows = arrow'' :: List.rev_append seen arrows' })
        else (* the new [arrow''] ist not yet saturated, try again: *)
          add_arrow_to_arrows_list' coeff seen arrow'' arrows'
     | A.No_Match -> (* recurse to the remaining arrows *)
        add_arrow_to_arrows_list' coeff (arrow' :: seen)  arrow arrows'
     end

let add_arrow_to_arrows_list coeff arrow arrows =
  add_arrow_to_arrows_list' coeff [] arrow arrows

(* Similarly, add one [arrow] to a list of $\epsilon$ and
   accumulate already processed arrows in [seen].
   Returns [[]] if there is no match.  Note that there is
   never the need to update the coefficient and that only
   the tail of the [arrow] can match. *)

let rec add_arrow_to_epsilon_list' seen arrow = function
  | [] -> []
  | epsilon :: epsilons ->
     begin match A.merge_arrow_eps arrow epsilon with
     | A.Mismatch_Eps -> []
     | A.Match_Eps epsilon' -> List.rev_append seen (epsilon' :: epsilons)
     | A.No_Match_Eps -> add_arrow_to_epsilon_list' (epsilon :: seen) arrow epsilons
     end

let add_arrow_to_epsilon_list arrow epsilons =
  add_arrow_to_epsilon_list' [] arrow epsilons

(* Same preocedure for adding one [arrow] to a list of $\bar\epsilon$. *)

let rec add_arrow_to_epsilon_bar_list' seen arrow = function
  | [] -> []
  | epsilon_bar :: epsilon_bars ->
     begin match A.merge_arrow_eps_bar arrow epsilon_bar with
     | A.Mismatch_Eps -> []
     | A.Match_Eps epsilon_bar' -> List.rev_append seen (epsilon_bar' :: epsilon_bars)
     | A.No_Match_Eps -> add_arrow_to_epsilon_bar_list' (epsilon_bar :: seen) arrow epsilon_bars
     end

let add_arrow_to_epsilon_bar_list arrow epsilon_bars =
  add_arrow_to_epsilon_bar_list' [] arrow epsilon_bars

(* Avoid a recursion, if there is no summation index in [arrow].
   Likewise, if [arrow] loops back to itself, just replace it by
   a factor of~$N_C$. *)

let add_arrow_to_aterm_trivial : A.factor -> afactor -> afactor option =
  fun arrow term ->
  if A.is_free arrow then
    Some ({ coeff = term.coeff; arrows = arrow :: term.arrows })
  else if A.is_tadpole arrow then
    Some ({ coeff = L.mul (L.nc 1) term.coeff; arrows = term.arrows })
  else
    None

(* Straightforwardly add an arrow or an arrow list to a term
   containing no $\epsilon$ or $\bar\epsilon$, using the functions
   implemented above. *)

let add_arrow_to_aterm : A.factor -> afactor -> afactor option =
  fun arrow term ->
  match add_arrow_to_aterm_trivial arrow term with
  | None -> add_arrow_to_arrows_list term.coeff arrow term.arrows
  | term_opt -> term_opt

let add_arrow_list_to_aterm : A.factor list -> afactor -> afactor option =
  fun arrows term ->
  ThoList.fold_left_opt (Fun.flip add_arrow_to_aterm) term arrows

(* Adding an arrow or an arrow list to a term containing
   $\epsilon$ or $\bar\epsilon$ is not more complicated, we only
   have to make two attempts. *)

(* \begin{dubious}
     Caveat: if the arrow matches one of the $\epsilon$s and
     this $\epsilon$ has a tip appearing among the remaining
     tips of this $\epsilon$, the result should be set to zero
     explicitelty.  But such expressions are illegal anyway!
   \end{dubious} *)

let add_arrow_to_eterm : A.factor -> efactor -> efactor option =
  fun arrow (aterm, epsilons) ->
  match add_arrow_to_aterm_trivial arrow aterm with
  | Some aterm -> Some (aterm, epsilons)
  | None ->
     begin match add_arrow_to_epsilon_list arrow (NEList.to_list epsilons) with
     | [] ->
        begin match add_arrow_to_arrows_list aterm.coeff arrow aterm.arrows with
        | None -> None
        | Some aterm -> Some (aterm, epsilons)
        end
     | epsilon :: epsilons -> Some (aterm, NEList.make epsilon epsilons)
     end

let add_arrow_list_to_eterm : A.factor list -> efactor -> efactor option =
  fun arrows term ->
  ThoList.fold_left_opt (Fun.flip add_arrow_to_eterm) term arrows

let add_arrow_to_bterm : A.factor -> bfactor -> bfactor option =
  fun arrow (aterm, epsilon_bars) ->
  match add_arrow_to_aterm_trivial arrow aterm with
  | Some aterm -> Some (aterm, epsilon_bars)
  | None ->
     begin match add_arrow_to_epsilon_bar_list arrow (NEList.to_list epsilon_bars)  with
     | [] ->
        begin match add_arrow_to_arrows_list aterm.coeff arrow aterm.arrows with
        | None -> None
        | Some aterm -> Some (aterm, epsilon_bars)
        end
     | epsilon_bar :: epsilon_bars -> Some (aterm, NEList.make epsilon_bar epsilon_bars)
     end

let add_arrow_list_to_bterm : A.factor list -> bfactor -> bfactor option =
  fun arrows term ->
  ThoList.fold_left_opt (Fun.flip add_arrow_to_bterm) term arrows

(* Adding an $\epsilon$ to a term containing $\epsilon$s is trivial,
   if there are no summation indices.  Otherwise, we add the arrows
   back in to find matches.
   \begin{dubious}
     Here's potential for optimization, since the arrows can only
     match the new $\epsilon$.
   \end{dubious} *)

let add_epsilon_to_eterm : A.factor_eps -> efactor -> efactor option =
  fun epsilon (aterm, epsilons) ->
  if A.is_free_eps epsilon then
    Some (aterm, NEList.cons epsilon epsilons)
  else
    let coeff = { coeff = aterm.coeff; arrows = []} in
    add_arrow_list_to_eterm aterm.arrows (coeff, NEList.cons epsilon epsilons)

let add_epsilon_list_to_eterm : A.factor_eps list -> efactor -> efactor option =
  fun epsilons eterm ->
  ThoList.fold_left_opt (Fun.flip add_epsilon_to_eterm) eterm epsilons

(* Once more for $\bar\epsilon$. *)

let add_epsilon_bar_to_bterm : A.factor_eps_bar -> bfactor -> bfactor option =
  fun epsilon_bar (aterm, epsilon_bars) ->
  if A.is_free_eps_bar epsilon_bar then
    Some (aterm, NEList.cons epsilon_bar epsilon_bars)
  else
    let coeff = { coeff = aterm.coeff; arrows = []} in
    add_arrow_list_to_bterm aterm.arrows (coeff, NEList.cons epsilon_bar epsilon_bars)

let add_epsilon_bar_list_to_bterm : A.factor_eps_bar list -> bfactor -> bfactor option =
  fun epsilon_bars bterm ->
  ThoList.fold_left_opt (Fun.flip add_epsilon_bar_to_bterm) bterm epsilon_bars

(* Here we simply have to select the correct function. *)

let add_arrow_to_term : A.factor -> factor -> factor option =
  fun arrow -> function
  | Arrows aterm ->
     Option.map (fun a -> Arrows a) (add_arrow_to_aterm arrow aterm)
  | Epsilons eterm ->
     Option.map (fun e -> Epsilons e) (add_arrow_to_eterm arrow eterm)
  | Epsilon_Bars bterm ->
     Option.map (fun b -> Epsilon_Bars b) (add_arrow_to_bterm arrow bterm)

let add_arrow_list_to_term : A.factor list -> factor -> factor option =
  fun arrows term ->
  ThoList.fold_left_opt (Fun.flip add_arrow_to_term) term arrows

let scale_aterm : L.t -> afactor -> afactor =
  fun coeff aterm ->
  { coeff = L.mul coeff aterm.coeff; arrows = aterm.arrows}

let scale_eterm : L.t -> efactor -> efactor =
  fun coeff (aterm, epsilons) ->
  (scale_aterm coeff aterm, epsilons)

let scale_bterm : L.t -> bfactor -> bfactor =
  fun coeff (aterm, epsilon_bars) ->
  (scale_aterm coeff aterm, epsilon_bars)

let scale_term : L.t -> factor -> factor =
  fun coeff -> function
  | Arrows aterm -> Arrows (scale_aterm coeff aterm)
  | Epsilons eterm -> Epsilons (scale_eterm coeff eterm)
  | Epsilon_Bars bterm -> Epsilon_Bars (scale_bterm coeff bterm)

let aterm_times_aterm : afactor -> afactor -> afactor option =
  fun aterm1 aterm2 ->
  Option.map (scale_aterm aterm1.coeff) (add_arrow_list_to_aterm aterm1.arrows aterm2)

(* Almost the same as [aterm_times_term] below, but the arguments
   are exchanged an the result are [factor]s and not [free]. *)

let term_times_aterm : factor -> afactor -> factor list =
  fun term aterm ->
  match add_arrow_list_to_term aterm.arrows term with
  | None -> []
  | Some factor -> [scale_term aterm.coeff factor]

(* The return type is [factor list], because adding a product
   of~$\epsilon$ and~$\bar\epsilon$ will produce a sum of terms and
   the result can be a [afactor], [efactor] or [bfactor] depending on
   the number of~$\epsilon$s and~$\bar\epsilon$s in the arguments. *)

(* \begin{dubious}
     Add more tests for multiple $\epsilon$ and $\bar\epsilon$!
     I'm not yet convinced only from playing with the toplevel.
   \end{dubious} *)

(* \begin{dubious}
     Calling [aterm_times_aterm] in each recursion step and
     only using the last result ist wasteful.  Find a better
     way!
   \end{dubious} *)

(* \begin{dubious}
     This would fail if one of [epsilons] or [epsilon_bars] is
     empty (which does not happen).  We could try to replace
     the ['e list] in [type ('a, 'e) eterm] by a non empty list
     type (and similarly for ['e list] in [type ('a, 'b) bterm].

     But is it worth the effort?  It probably enough
     to hide the list in a [private] ADT that can be deconstructed,
     but requires a smart constructor that requires at least one
     element.
   \end{dubious} *)

let rec match_eterm_and_bterm : efactor -> bfactor -> factor list =
  fun (aterm1, epsilons) (aterm2, epsilon_bars) ->
  match NEList.snoc_opt epsilons, NEList.snoc_opt epsilon_bars with
  | (epsilon, epsilons_opt), (epsilon_bar, epsilon_bars_opt) ->
     begin match aterm_times_aterm aterm1 aterm2 with
     | None -> []
     | Some aterm ->
        match A.merge_eps_eps_bar epsilon epsilon_bar with
        | None -> []
        | Some (even, odd) ->
           let even = List.rev_map (fun arrows -> { coeff = L.unit; arrows }) even
           and odd = List.rev_map (fun arrows -> { coeff = L.neg L.unit; arrows }) odd in
           let terms =
             match epsilons_opt, epsilon_bars_opt with
             | None, None -> [Arrows aterm]
             | Some epsilons, None-> [Epsilons (aterm, epsilons)]
             | None, Some epsilon_bars-> [Epsilon_Bars (aterm, epsilon_bars)]
             | Some epsilon, Some epsilon_bars ->
                match_eterm_and_bterm (aterm1, epsilon) (aterm2, epsilon_bars) in
           Product.fold2
             (fun term aterm acc ->
               List.rev_append (term_times_aterm term aterm) acc)
             terms (List.rev_append even odd) []
     end

(* NB: we can reject the contributions with unsaturated summation indices
   from Ghost contributions to~$T_a$ only \emph{after} adding all
   arrows that might saturate an open index. *)
    
(* Note that a negative index might be summed only
   later in a sequence of binary products and must
   therefore be treated as free in this product.  Therefore,
   we have to classify the indices as summation indices
   \emph{not only} based on their sign, but in addition based on
   whether they appear in both factors. Only then can we reject
   surviving ghosts. *)

module ESet =
  Set.Make
    (struct
      type t = A.endpoint
      let compare = compare
    end)

let negatives_arrows arrows acc =
  List.fold_right (fun a -> List.fold_right ESet.add (A.negatives a)) arrows acc

let negatives_eps epsilons acc =
  NEList.fold_right
    (fun e -> List.fold_right ESet.add (A.negatives_eps e))
    epsilons acc

let negatives_eps_bar epsilon_bars acc =
  NEList.fold_right
    (fun b -> List.fold_right ESet.add (A.negatives_eps_bar b))
    epsilon_bars acc

let negatives = function
  | Arrows aterm -> negatives_arrows aterm.arrows ESet.empty
  | Epsilons (aterm, epsilons) ->
     negatives_eps epsilons (negatives_arrows aterm.arrows ESet.empty)
  | Epsilon_Bars (aterm, epsilon_bars) ->
     negatives_eps_bar epsilon_bars (negatives_arrows aterm.arrows ESet.empty)

let aterm_times_term : afactor -> factor -> free list =
  fun aterm term ->
  match add_arrow_list_to_term aterm.arrows term with
  | None -> []
  | Some factor -> [of_factor (scale_term aterm.coeff factor)]

let eterm_times_eterm : efactor -> efactor -> free list =
  fun (aterm, epsilons) eterm ->
  match add_epsilon_list_to_eterm (NEList.to_list epsilons) eterm with
  | None -> []
  | Some factor ->
     begin match add_arrow_list_to_eterm aterm.arrows factor with
     | None -> []
     | Some factor -> [of_factor (Epsilons (scale_eterm aterm.coeff factor))]
     end

let bterm_times_bterm : bfactor -> bfactor -> free list =
  fun (aterm, epsilon_bars) bterm ->
  match add_epsilon_bar_list_to_bterm (NEList.to_list epsilon_bars) bterm with
  | None -> []
  | Some factor ->
     begin match add_arrow_list_to_bterm aterm.arrows factor with
     | None -> []
     | Some factor -> [of_factor (Epsilon_Bars (scale_bterm aterm.coeff factor))]
     end

let eterm_times_bterm : efactor -> bfactor -> free list =
  fun eterm bterm ->
  List.map of_factor (match_eterm_and_bterm eterm bterm)

let times1 term1 term2 =
  let summations = ESet.inter (negatives term1) (negatives term2) in
  let is_sum i = ESet.mem i summations in
  match to_left_factor is_sum term1, to_right_factor is_sum term2 with
  | Arrows aterm, factor | factor, Arrows aterm ->
     aterm_times_term aterm factor
  | Epsilons eterm1, Epsilons eterm2 ->
     eterm_times_eterm eterm1 eterm2
  | Epsilon_Bars bterm1, Epsilon_Bars bterm2 ->
     bterm_times_bterm bterm1 bterm2
  | Epsilons eterm, Epsilon_Bars bterm
    | Epsilon_Bars bterm, Epsilons eterm ->
     eterm_times_bterm eterm bterm

let sum terms =
  canonicalize (List.concat terms)

let times term term' =
  canonicalize
    (Product.fold2
       (fun x y -> List.rev_append (times1 x y))
       term term' [])

(* \begin{dubious}
     Is that more efficient than the following implementation?
   \end{dubious} *)

(*i
    let rec multiply1' acc = function
      | [] -> [acc]
      | factor :: factors ->
         List.fold_right multiply1' (times1 acc factor) factors

    let multiply1 = function
      | [] -> [(L.unit, [])]
      | [factor] -> [factor]
      | factor :: factors -> multiply1' factor factors

    let multiply terms =
      canonicalize
        (Product.fold (fun x -> List.rev_append (multiply1 x)) terms [])

i*)
(* \begin{dubious}
     Isn't that the more straightforward implementation?
   \end{dubious} *)

let multiply = function
  | [] -> []
  | term :: terms ->
     canonicalize (List.fold_left times term terms)

let scale1 : type a e b. L.c -> (a, e, b) term -> (a, e, b) term =
  fun q term ->
  map_term_full (L.scale q) Fun.id Fun.id Fun.id term

let scale q = List.map (scale1 q)

let diff term1 term2 =
  canonicalize (List.rev_append term1 (scale (QC.int (-1)) term2))

module Infix =
  struct
    let ( +++ ) term term' = sum [term; term']
    let ( --- ) = diff
    let ( *** ) = times
  end

open Infix

let is_multiple1 x y =
  match x, y with
  | Arrows x, Arrows y ->
     if x.arrows = y.arrows then
       Some (x.coeff, y.coeff)
     else
       None
  | Epsilons (x, xe), Epsilons (y, ye) ->
     if x.arrows = y.arrows && xe = ye then
       Some (x.coeff, y.coeff)
     else
       None
  | Epsilon_Bars (x, xb), Epsilon_Bars (y, yb) ->
     if x.arrows = y.arrows && xb = yb then
       Some (x.coeff, y.coeff)
     else
       None
  | Arrows _, (Epsilons _ | Epsilon_Bars _) | (Epsilons _ | Epsilon_Bars _), Arrows _
  | Epsilons _, Epsilon_Bars _ | Epsilon_Bars _, Epsilons _ -> None

(* \begin{dubious}
     The following is not the most efficient way to implement [is_multiple].
     Multiplying all terms by the coefficients avoids having to compute
     a gcd in [is_multiple1], but gives up the opportunity of an early exit
     at the first mismatch.
   \end{dubious} *)

let _is_multiple x y =
  match canonicalize x, canonicalize y with
  | [], [] -> Some (L.unit, L.unit)
  | [], _ | _, [] -> None
  | x1 :: xtail, y1 :: ytail ->
     begin match is_multiple1 x1 y1 with
     | None -> None
     | Some (xc, yc) as result ->
        if const yc *** xtail = const xc *** ytail then
          result
        else
          None
     end

(* This should be more efficient: *)

let rec tail_is_multiple xc yc x y =
  match x, y with
  | [], [] -> true
  | [], _ | _, [] -> false
  | x1 :: xtail, y1 :: ytail ->
     begin match is_multiple1 x1 y1 with
     | None -> false
     | Some (x1c, y1c) ->
        if L.equal (L.product [yc; x1c]) (L.product [xc; y1c]) then
          tail_is_multiple xc yc xtail ytail
        else
          false
     end

let is_multiple x y =
  match canonicalize x, canonicalize y with
  | [], [] -> Some (L.unit, L.unit)
  | [], _ | _, [] -> None
  | x1 :: xtail, y1 :: ytail ->
     begin match is_multiple1 x1 y1 with
     | None -> None
     | Some (xc, yc) as result ->
        if tail_is_multiple xc yc xtail ytail then
          result
        else
          None
     end

(* Compute $ \tr(r(T_a) r(T_b) r(T_c)) $.  NB: this uses the
   summation indices $-1$, $-2$ and $-3$.  Therefore
   it \emph{must not} appear unevaluated more than once in a product! *)
let trace3 r a b c =
  r a (-1) (-2) *** r b (-2) (-3) *** r c (-3) (-1)

let f_of_rep r a b c =
  minus *** imag *** (trace3 r a b c --- trace3 r a c b)

(* $ d_{abc} = \tr(r(T_a) [r(T_b), r(T_c)]_+) $ *)
let d_of_rep r a b c =
  trace3 r a b c +++ trace3 r a c b

(* \thocwmodulesection{Unit Tests} *)

let vertices_equal v1 v2 =
  is_null (v1 --- v2)

let assert_zero_vertex v =
  OUnit.assert_equal ~printer:to_string ~cmp:vertices_equal null v

(* As an extra protection agains vacuous tests, we make
   sure that the LHS does not vanish.  *)
let equal v1 v2 =
  OUnit.assert_bool "LHS = 0" (not (is_null v1));
  OUnit.assert_equal ~printer:to_string ~cmp:vertices_equal v1 v2

module Test =
  struct
    open OUnit

    let vertices_equal v1 v2 =
      (canonicalize v1) = (canonicalize v2)

    let eq v1 v2 =
      assert_equal ~printer:to_string_raw ~cmp:vertices_equal v1 v2

    let suite_times1 =
      "times1" >:::
        [ "merge two" >::
	    (fun () ->
	      eq
                [Arrows { coeff = L.unit; arrows = 1 ==> 2 }]
                (times1
                   (Arrows { coeff = L.unit; arrows =  1 ==> -1 })
                   (Arrows { coeff = L.unit; arrows = -1 ==>  2 })));

          "merge two exchanged" >::
	    (fun () ->
	      eq
                [Arrows { coeff = L.unit; arrows = 1 ==> 2 }]
                (times1
                   (Arrows { coeff = L.unit; arrows = -1 ==>  2 })
                   (Arrows { coeff = L.unit; arrows =  1 ==> -1 })));

          "ghost1" >::
	    (fun () ->
	      eq
                [Arrows { coeff = L.over_nc (-1); arrows = 1 ==> 2 }]
                (times1
                   (Arrows { coeff = L.unit; arrows = [-1 =>  2; ?? (-3)] })
                   (Arrows { coeff = L.unit; arrows = [ 1 => -1; ?? (-3)] })));

          "ghost2" >::
	    (fun () ->
	      eq
                []
                (times1
                   (Arrows { coeff = L.unit; arrows = [ 1 => -1; ?? (-3)] })
                   (Arrows { coeff = L.unit; arrows = [-1 =>  2; -3 => -4; -4 => -3] })));

          "ghost2 exchanged" >::
	    (fun () ->
	      eq
                []
                (times1
                   (Arrows { coeff = L.unit; arrows = [-1 =>  2; -3 => -4; -4 => -3] })
                   (Arrows { coeff = L.unit; arrows = [ 1 => -1; ?? (-3)] }))) ]

    let suite_canonicalize =
      "canonicalize" >:::

        [ ]

    let multiple_to_string = function
      | None -> "None"
      | Some (x, y) -> Printf.sprintf "Some (%s, %s)" (L.to_string "N" x) (L.to_string "N" y)

    let assert_equal_multiple x y =
      assert_equal ~printer:multiple_to_string x y

    let suite_is_multiple =
      "is_multiple" >:::

        [ "1 // 2" >::
            (fun () ->
              assert_equal_multiple (Some (L.unit, L.int 2)) (is_multiple one two));

          "1 => 2 // 2 (1 => 2)" >::
            (fun () ->
              assert_equal_multiple (Some (L.unit, L.int 2))
                (is_multiple
                   [Arrows { coeff = L.unit;  arrows = [ 1 => 2 ] }]
                   [Arrows { coeff = L.int 2; arrows = [ 1 => 2 ] }] ));

          "1 => 2 // 2 (3 => 4)" >::
            (fun () ->
              assert_equal_multiple None
                (is_multiple
                   [Arrows { coeff = L.unit;  arrows = [ 1 => 2 ] }]
                   [Arrows { coeff = L.int 2; arrows = [ 3 => 4 ] }] ));

          "1 / 2 N" >::
            (fun () ->
              assert_equal_multiple (Some (L.unit, L.nc 2))
                (is_multiple
                   [Arrows { coeff = L.unit;         arrows = [ 1 => 2; 3 => 4 ] };
                    Arrows { coeff = L.over_nc (-1); arrows = [ 1 => 4; 3 => 2 ] }]
                   [Arrows { coeff = L.nc 2;         arrows = [ 1 => 2; 3 => 4 ] };
                    Arrows { coeff = L.int (-2);     arrows = [ 1 => 4; 3 => 2 ] }] )) ]

    let suite =
      "Birdtracks" >:::
	[suite_times1;
         suite_canonicalize;
         suite_is_multiple]

    let suite_long =
      "Birdtracks long" >:::
	[]
  end
