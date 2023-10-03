(* arrow.ml --

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

(* \newcommand{\setupFourAmp}{%
     \fmfleft{i1,i2}
     \fmfright{o1,o2}
     \fmf{phantom}{i1,v1,i2}
     \fmf{phantom}{o2,v2,o1}
     \fmf{phantom}{v1,v2}
     \fmffreeze}
   \fmfcmd{%
     numeric joindiameter;
     joindiameter := 7thick;}
   \fmfcmd{%
     vardef sideways_at (expr d, p, frac) =
       save len; len = length p;
       (point frac*len of p) shifted ((d,0) rotated (90 + angle direction frac*len of p))
     enddef;
     secondarydef p sideways d =
       for frac = 0 step 0.01 until 0.99:
         sideways_at (d, p, frac) ..
       endfor
       sideways_at (d, p, 1)
     enddef;
     secondarydef p choptail d =
      subpath (ypart (fullcircle scaled d shifted (point 0 of p) intersectiontimes p), infinity) of p
     enddef;
     secondarydef p choptip d =
      reverse ((reverse p) choptail d)
     enddef;
     secondarydef p pointtail d =
       fullcircle scaled d shifted (point 0 of p) intersectionpoint p
     enddef;
     secondarydef p pointtip d =
       (reverse p) pointtail d
     enddef;
     secondarydef pa join pb =
       pa choptip joindiameter .. pb choptail joindiameter
     enddef;
     vardef cyclejoin (expr p) =
       subpath (0.5*length p, infinity) of p join subpath (0, 0.5*length p) of p .. cycle
     enddef;}
   %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
   \fmfcmd{%
     style_def double_line_arrow expr p =
       save pi, po; 
       path pi, po;
       pi = reverse (p sideways thick);
       po = p sideways -thick;
       cdraw pi;
       cdraw po;
       cfill (arrow (subpath (0, 0.9 length pi) of pi));
       cfill (arrow (subpath (0, 0.9 length po) of po));
     enddef;}
   \fmfcmd{%
     style_def double_line_arrow_beg expr p =
       save pi, po, pc; 
       path pi, po, pc;
       pc = p choptail 7thick;
       pi = reverse (pc sideways thick);
       po = pc sideways -thick;
       cdraw pi .. p pointtail 5thick .. po;
       cfill (arrow pi);
       cfill (arrow po);
     enddef;}
   \fmfcmd{%
     style_def double_line_arrow_end expr p =
       save pi, po, pc; 
       path pi, po, pc;
       pc = p choptip 7thick;
       pi = reverse (pc sideways thick);
       po = pc sideways -thick;
       cdraw po .. p pointtip 5thick .. pi;
       cfill (arrow pi);
       cfill (arrow po);
     enddef;}
   \fmfcmd{%
     style_def double_line_arrow_both expr p =
       save pi, po, pc; 
       path pi, po, pc;
       pc = p choptip 7thick choptail 7thick;
       pi = reverse (pc sideways thick);
       po = pc sideways -thick;
       cdraw po .. p pointtip 5thick .. pi .. p pointtail 5thick .. cycle;
       cfill (arrow pi);
       cfill (arrow po);
     enddef;}
   \fmfcmd{%
     style_def double_arrow_parallel expr p =
       save pi, po; 
       path pi, po;
       pi = p sideways thick;
       po = p sideways -thick;
       save li, lo;
       li = length pi;
       lo = length po;
       cdraw pi;
       cdraw po;
       cfill (arrow pi);
       cfill (arrow po);
     enddef;}
   \fmfcmd{%
     style_def double_arrow_crossed_beg expr p =
       save lp;  lp = length p;
       save pi, po; 
       path pi, po;
       pi = p sideways thick;
       po = p sideways -thick;
       save li, lo;
       li = length pi;
       lo = length po;
       cdraw subpath (0, 0.1 li) of pi .. subpath (0.3 lo, lo) of po;
       cdraw subpath (0, 0.1 lo) of po .. subpath (0.3 li, li) of pi;
       cfill (arrow pi);
       cfill (arrow po);
     enddef;}
   \fmfcmd{%
     style_def double_arrow_crossed_end expr p =
       save lp;  lp = length p;
       save pi, po; 
       path pi, po;
       pi = p sideways thick;
       po = p sideways -thick;
       save li, lo;
       li = length pi;
       lo = length po;
       cdraw subpath (0, 0.7 li) of pi .. subpath (0.9 lo, lo) of po;
       cdraw subpath (0, 0.7 lo) of po .. subpath (0.9 li, li) of pi;
       cfill (arrow pi);
       cfill (arrow po);
     enddef;} *)

(* \thocwmodulesection{Arrows and Epsilons} *)

type endpoint =
  | I of int
  | M of int * int

let position_endpoint = function
  | I i -> i
  | M (i, _) -> i

let relocate_endpoint f = function
  | I i -> I (f i)
  | M (i, n) -> M (f i, n)

type tip = endpoint
type tail = endpoint
type ghost = endpoint

let position_tip = position_endpoint
let position_tail = position_endpoint
let position_ghost = position_endpoint
let relocate_tip = relocate_endpoint
let relocate_tail = relocate_endpoint
let relocate_ghost = relocate_endpoint

(* Note that in the case of double lines for the adjoint
   representation the \emph{same} [endpoint] appears twice:
   once as a [tip] and once as a [tail].  If we want to
   multiply two factors by merging arrows with matching
   [tip] and [tail], we must make sure that the [tip] is from
   one factor and the [tail] from the other factor. *)
               
(* The [Free] variant contains positive indices
   as well as negative indices that don't appear on both sides
   and will be summed in a later product.  [SumL] and [SumR]
   indices appear on both sides. *)
type 'a index =
  | Free of 'a
  | SumL of 'a
  | SumR of 'a

let is_free_index = function
  | Free _ -> true
  | SumL _ | SumR _ -> false

type ('tail, 'tip, 'ghost) t =
  | Arrow of 'tail * 'tip
  | Ghost of 'ghost
type 'tip eps = 'tip list
type 'tail eps_bar = 'tail list

type free = (tail, tip, ghost) t
type free_eps = tip eps
type factor_eps = tip index eps

type factor = (tail index, tip index, ghost index) t
type free_eps_bar = tail eps_bar
type factor_eps_bar = tail index eps_bar

let relocate f = function
  | Arrow (tail, tip) -> Arrow (relocate_tail f tail, relocate_tip f tip)
  | Ghost ghost -> Ghost (relocate_ghost f ghost)

let rev = function
  | Arrow (tail, tip) -> Arrow (tip, tail)
  | Ghost _ as ghost -> ghost
let rev_eps tips = tips
let rev_eps_bar tails = tails

let tips = function
  | Arrow (_, tip) -> [tip]
  | Ghost _ -> []
let tails = function
  | Arrow (tail, _) -> [tail]
  | Ghost _ -> []
let tips_eps tips = tips
let tails_eps_bar tails = tails

let endpoint_to_string = function
  | I i -> string_of_int i
  | M (i, n) -> Printf.sprintf "%d.%d" i n

let index_to_string = function
  | Free i -> endpoint_to_string i
  | SumL i -> endpoint_to_string i ^ "L"
  | SumR i -> endpoint_to_string i ^ "R"

let to_string i2s = function
  | Arrow (tail, tip) -> Printf.sprintf "%s>%s" (i2s tail) (i2s tip)
  | Ghost ghost -> Printf.sprintf "{%s}" (i2s ghost)
let to_string_eps i2s tips = Printf.sprintf ">>>%s" (ThoList.to_string i2s tips)
let to_string_eps_bar i2s tails = Printf.sprintf "<<<%s" (ThoList.to_string i2s tails)

let free_to_string = to_string endpoint_to_string
let free_eps_to_string = to_string_eps endpoint_to_string
let free_eps_bar_to_string = to_string_eps_bar endpoint_to_string

let factor_to_string = to_string index_to_string
let factor_eps_to_string = to_string_eps index_to_string
let factor_eps_bar_to_string = to_string_eps_bar index_to_string

let matching_summation i1 i2 =
  match i1, i2 with
  | SumL i1, SumR i2 | SumR i1, SumL i2 -> i1 = i2
  | _ -> false

let map f = function
  | Arrow (tail, tip) -> Arrow (f tail, f tip)
  | Ghost ghost -> Ghost (f ghost)
let map_eps = List.map
let map_eps_bar = List.map

let free_index = function
  | Free i -> i
  | SumL i -> invalid_arg "Arrow.free_index: leftover LHS summation"
  | SumR i -> invalid_arg "Arrow.free_index: leftover RHS summation"

let to_left_index is_sum i =
  if is_sum i then
    SumL i
  else
    Free i

let to_right_index is_sum i =
  if is_sum i then
    SumR i
  else
    Free i

let to_left_factor is_sum = map (to_left_index is_sum)
let to_right_factor is_sum = map (to_right_index is_sum)
let of_factor = map free_index

let to_left_factor_eps is_sum = map_eps (to_left_index is_sum)
let to_right_factor_eps is_sum = map_eps (to_right_index is_sum)
let of_factor_eps = map_eps free_index

let to_left_factor_eps_bar is_sum = map_eps_bar (to_left_index is_sum)
let to_right_factor_eps_bar is_sum = map_eps_bar (to_right_index is_sum)
let of_factor_eps_bar = map_eps_bar free_index

let negatives = function
  | Arrow (tail, tip) ->
     if position_tail tail < 0 then
       if position_tip tip < 0 then
         [tail; tip]
       else
         [tail]
     else if position_tip tip < 0 then
       [tip]
     else
       []
  | Ghost ghost ->
     if position_ghost ghost < 0 then
       [ghost]
     else
       []
let negatives_eps = List.filter (fun tip -> position_tip tip < 0)
let negatives_eps_bar = List.filter (fun tail -> position_tail tail < 0)

let is_free = function
  | Arrow (Free _, Free _) | Ghost (Free _) -> true
  | Arrow (_, _) | Ghost _ -> false
let is_free_eps = List.for_all is_free_index
let is_free_eps_bar = List.for_all is_free_index

let is_ghost = function
  | Ghost _ -> true
  | Arrow _ -> false
                 
let single tail tip =
  Arrow (tail, tip)

let double a b =
  if a = b then
    [single a b]
  else
    [single a b; single b a]

let ghost g =
  Ghost g

module Infix =
  struct
    let ( => ) i j = single (I i) (I j)
    let ( ==> ) i j = [i => j]
    let ( <=> ) i j = double (I i) (I j)
    let ( >=> ) (i, n) j = single (M (i, n)) (I j)
    let ( =>> ) i (j, m) = single (I i) (M (j, m))
    let ( >=>> ) (i, n) (j, m) = single (M (i, n)) (M (j, m))
    let ( ?? ) i = ghost (I i)
  end

open Infix

(* Split [a_list] at the first element equal to [a] according
   to [eq].  Return the reversed first part and the rest as a
   pair and wrap it in [Some]. Return [None] if there is no match.  *)
let take_first_match_opt ?(eq=(=)) a a_list =
  let rec take_first_match_opt' rev_head = function
    | [] -> None
    | elt :: tail ->
       if eq elt a then
         Some (rev_head, tail)
       else
         take_first_match_opt' (elt :: rev_head) tail in
  take_first_match_opt' [] a_list

(* Split [a_list] and [b_list] at the first element equal according
   to [eq].  Return the reversed first part and the rest of each
   as a pair of pairs wrap it in [Some].
   Return [None] if there is no match.
   \begin{dubious}
     This function remains from an earlier version and is no longer
     used.
   \end{dubious} *)
let take_first_matching_pair_opt ?(eq=(=)) a_list b_list =
  let rec take_first_matching_pair_opt' rev_a_head = function
    | [] -> None
    | a :: a_tail ->
       begin match take_first_match_opt ~eq a b_list with
       | Some (rev_b_head, b_tail) ->
          Some ((rev_a_head, a_tail), (rev_b_head, b_tail))
       | None ->
          take_first_matching_pair_opt' (a :: rev_a_head) a_tail
       end in
  take_first_matching_pair_opt' [] a_list

(* Replace the first occurence of an element equal to [a] according
   to [eq] in [a_list] by [a'] and wrap the new list in [Some].
   Return [None] if there is no match.  *)
let replace_first_opt ?(eq=(=)) a a' a_list =
  match take_first_match_opt ~eq a a_list with
  | Some (rev_head, tail) -> Some (List.rev_append rev_head (a' :: tail))
  | None -> None

let tee a = function
  | Arrow (tail, tip) -> [Arrow (tail, I a); Arrow (I a, tip)]
  | Ghost _ as g -> [g]

let dir i j = function
  | Arrow (tail, tip) ->
     let tail = position_tail tail
     and tip = position_tip tip in
     if tip = i && tail = j then
       1
     else if tip = j && tail = i then
       -1
     else
       invalid_arg "Arrow.dir"
  | Ghost _ -> 0

type merge =
  | Match of factor
  | Ghost_Match
  | Loop_Match
  | Mismatch
  | No_Match

(* As an optimization, don't attempt to merge if neither of the arrows
   contains a summation index and return immediately. *)

let merge_arrow_arrow arrow1 arrow2 =
  if is_free arrow1 || is_free arrow2 then
    No_Match
  else
    match arrow1, arrow2 with
    | Ghost g1, Ghost g2 ->
       if matching_summation g1 g2 then
         Ghost_Match
       else
         No_Match
    | Arrow (tail, tip), Ghost g
      | Ghost g, Arrow (tail, tip) ->
       if matching_summation g tail || matching_summation g tip then
         Mismatch
       else
         No_Match
    | Arrow (tail, tip), Arrow (tail', tip') ->
       if matching_summation tip tail' then
         if matching_summation tip' tail then
           Loop_Match
         else
           Match (Arrow (tail, tip'))
       else if matching_summation tip' tail then
         Match (Arrow (tail', tip))
       else
         No_Match

type 'a merge_eps =
  | Match_Eps of 'a
  | Mismatch_Eps
  | No_Match_Eps

let merge_arrow_eps arrow tips =
  if is_free_eps tips || is_free arrow then
    No_Match_Eps
  else
    match arrow with
    | Arrow (tail, tip) ->
       begin match replace_first_opt ~eq:matching_summation tail tip tips with
       | None -> No_Match_Eps
       | Some tips -> Match_Eps tips
       end
    | Ghost g ->
       if List.exists (matching_summation g) tips then
         Mismatch_Eps
       else
         No_Match_Eps

let merge_arrow_eps_bar arrow tails =
  if is_free_eps_bar tails || is_free arrow then
    No_Match_Eps
  else
    match arrow with
    | Arrow (tail, tip) ->
       begin match replace_first_opt ~eq:matching_summation tip tail tails with
       | None -> No_Match_Eps
       | Some tails -> Match_Eps tails
       end
    | Ghost g ->
       if List.exists (matching_summation g) tails then
         Mismatch_Eps
       else
         No_Match_Eps

(* \thocwmodulesection{Evaluation Rules for Epsilon Tensors}
   \label{sec:evaluation-of-epsilon-tensors} *)

(* In the case of matching dimension~$N=\delta_m^m$ and rank~$n$
   of~$\epsilon$ and $\bar\epsilon$, the tensor algebra of
   the $\delta_{i}^{j}$, $\epsilon_{i_1i_2\cdots i_n}$
   and $\bar\epsilon^{j_1j_2\cdots j_n}$ is \emph{not} freely generated.
   Indeed, introducing the \emph{generalized Kronecker~$\delta$} symbol
   \begin{equation}
   \label{eq:generalized-delta}
      \delta_{i_1i_2\cdots i_n}^{j_1j_2\cdots j_n}
        = \sum_{\sigma\in S_n} (-1)^{\varepsilon(\sigma)}
            \delta_{i_1}^{\sigma(j_1)} 
            \delta_{i_2}^{\sigma(j_2)} 
            \cdots
            \delta_{i_n}^{\sigma(j_n)}
        = \sum_{\sigma\in S_n} (-1)^{\varepsilon(\sigma)}
            \delta_{\sigma(i_1)}^{j_1} 
            \delta_{\sigma(i_2)}^{j_2} 
            \cdots
            \delta_{\sigma(i_n)}^{j_n}
        = \begin{vmatrix}
            \delta_{i_1}^{j_1} & \delta_{i_1}^{j_2} & \cdots & \delta_{i_1}^{j_n} \\
            \delta_{i_2}^{j_1} & \delta_{i_2}^{j_2} & \cdots & \delta_{i_2}^{j_n} \\
            \vdots             & \vdots             & \ddots & \vdots             \\
            \delta_{i_n}^{j_1} & \delta_{i_n}^{j_2} & \cdots & \delta_{i_n}^{j_n}
          \end{vmatrix} \,,
   \end{equation}
   there is the relation~$\forall n=N\in\mathbf{N}$ with~$N\ge2$:
   \begin{equation}
   \label{eq:epsilon*epsilonbar-0}
      \epsilon_{i_1i_2\cdots i_n} \bar\epsilon^{j_1j_2\cdots j_n}
        = \delta_{i_1i_2\cdots i_n}^{j_1j_2\cdots j_n}\,,
   \end{equation}
   which follows from anti-symmetry and the choice of normalization
   $\epsilon_{12\cdots n} = 1 = \bar\epsilon^{12\cdots n}$ alone.
   Contracting $k$ indices in the relation~\eqref{eq:epsilon*epsilonbar-0},
   we find~$\forall k, n, N \in \mathbf{N}$ with $0 \le k \le n = N\ge2$:
   \begin{equation}
   \label{eq:epsilon*epsilonbar}
      \epsilon_{m_1\cdots m_ki_{k+1}\cdots i_n}
      \bar\epsilon^{m_1\cdots m_kj_{k+1}\cdots j_n}
        = k!\, \delta_{i_{k+1}i_{k+2}\cdots i_n}^{j_{k+1}j_{k+2}\cdots j_n}\,.
   \end{equation} *)
    
(* Note that the generalized Kronecker delta~\eqref{eq:generalized-delta}
   is well defined for arbitrary rank~$n\ge1$, including $n<N$, and
   vanishes for $n>N$. It satisfies
   \begin{subequations}
   \label{eq:delta*delta/epsilon}
     \begin{align}
     \label{eq:delta*delta}
        \delta_{i_1i_2\cdots i_n}^{j_1j_2\cdots j_n}
        \delta_{j_1j_2\cdots j_n}^{k_1k_2\cdots k_n}
           &= n!\, \delta_{i_1i_2\cdots i_n}^{k_1k_2\cdots k_n} \\
     \label{eq:delta*epsilon}
        \delta_{i_1i_2\cdots i_n}^{j_1j_2\cdots j_n}
        \epsilon_{j_1j_2\cdots j_n}
           &= n!\, \epsilon_{i_1i_2\cdots i_n} \\
     \label{eq:delta*epsilonbar}
        \delta_{i_1i_2\cdots i_n}^{j_1j_2\cdots j_n}
        \bar\epsilon^{i_1i_2\cdots i_n}
           &= n!\, \bar\epsilon^{j_1j_2\cdots j_n}
     \end{align}
   \end{subequations}
   since every $\sigma\in S_n$ gives the same contribution when contracting
   totally antisymmetric combinations.  Note also that the
   relations~\eqref{eq:delta*delta/epsilon}
   are independent of the dimension~$N$ and remain valid for rank~$n\not=N$,
   as long as~$\epsilon_{i_1i_2\cdots i_n}$ 
   and~$\bar\epsilon^{j_1j_2\cdots j_n}$ are totally antisymmetric.
 
   In our birdtrack based evaluator, the condition~$N=n$ is not enforced.
   Indeed, $N$ is just a variable in Laurent polynomials [Algebra.Laurent.t]
   and $n$ is the arbitrary length of the lists in [tip Arrow.eps] and
   [tail Arrow.eps_bar] of colorflows.  Therefore, we can use
   neither~\eqref{eq:epsilon*epsilonbar-0}
   nor~\eqref{eq:epsilon*epsilonbar} directly to test our evaluator. *)

(* Nevertheless, for the purpose of testing our evaluator,
   we can \emph{define} a \emph{formal} evaluation rule for
   birdtracks in the general case~$N\not=n$,
   that is compatible with anti-symmetry and reduces
   to~\eqref{eq:epsilon*epsilonbar-0} for $N=n$
   \begin{equation}
   \label{eq:epsilon*epsilonbar-generalized}
%%%   \forall 2\le n \le N \in\mathbf{N}:\;
      \epsilon_{i_1i_2\cdots i_n} \bar\epsilon^{j_1j_2\cdots j_n}
        \to \delta_{i_1i_2\cdots i_n}^{j_1j_2\cdots j_n}\,,
   \end{equation}
   where we use the arrow $\to$ instead of the equal sign to stress
   that is a rule and not an equation, in contrast to the special
   case~\eqref{eq:epsilon*epsilonbar-0} for~$n=N$. *)

let merge_eps_eps_bar tips tails =
  if List.length tails <> List.length tips then
    None
  else
    Some (List.fold_left
            (fun (even, odd) (eps, tips) ->
              if eps > 0 then
                (List.rev_map2 single tails tips :: even, odd)
              else
                (even, List.rev_map2 single tails tips :: odd))
            ([], []) (Combinatorics.permute_signed tips))

(* Contracting one index, we find the equation
   \begin{multline}
      \delta_{mi_2\cdots i_n}^{mj_2\cdots j_n}
        = \delta_m^m 
          \sum_{\substack{\sigma\in S_n\\\sigma(m)=m}}
              (-1)^{\varepsilon(\sigma)}
            \delta_{i_2}^{\sigma(j_2)} 
            \cdots
            \delta_{i_n}^{\sigma(j_n)}
        + \sum_{\substack{\sigma\in S_n\\\sigma(m)\not=m}}
              (-1)^{\varepsilon(\sigma)}
            \delta_{m}^{\sigma(m)} 
            \delta_{i_2}^{\sigma(j_2)} 
            \cdots
            \delta_{i_n}^{\sigma(j_n)} \\
        = N \delta_{i_2\cdots i_n}^{j_2\cdots j_n}
            - (n-1)\, \delta_{i_2\cdots i_n}^{j_2\cdots j_n}
        = (N - n + 1)\, \delta_{i_2\cdots i_n}^{j_2\cdots j_n}\,,
   \end{multline}
   where the~$N=\delta_m^m$ comes from the permutations with~$\sigma(m)=m$
   that correspond to a loop in the color flow and the~$n-1$ from the
   permutations with~$\sigma(m)\in\{i_2,\ldots,i_n\}$ that do not
   lead to a loop.  The minus is due to the fact that there is exactly
   one transposition $m\leftrightarrow\sigma(m)$.  Thus the consistent
   evalution rule for a contracted $\epsilon$-$\bar\epsilon$-pair is
   \begin{equation}
   \label{eq:epsilon*epsilonbar-single-contraction}
%%%   \forall 2\le n \le N \in\mathbf{N}:\;
      \epsilon_{mi_2\cdots i_n} \bar\epsilon^{mj_2\cdots j_n}
        \to \delta_{mi_2\cdots i_n}^{mj_2\cdots j_n}
        = (N-n+1)\, \delta_{i_2\cdots i_n}^{j_2\cdots j_n}\,.
   \end{equation}
   Note that~$N-n+1=1$ in the special case~$N=n$ when
   rank and dimension match.
   Proceeding by induction, we obtain the equation
   \begin{equation}
%%%   \forall k, n, N \in \mathbf{N}, 2\le n \le N \land 1\le k \le n:\;
      \delta_{m_1\cdots m_ki_{k+1}\cdots i_n}^{m_1\cdots m_kj_{k+1}\cdots j_n}
        = \frac{(N-n+k)!}{(N-n)!}\,
            \delta_{i_{k+1}i_{k+2}\cdots i_n}^{j_{k+1}j_{k+2}\cdots j_n}
   \end{equation}
   and the corresponding evaluation rule
   \begin{equation}
   \label{eq:epsilon*epsilonbar-generalized-contracted}
%%%   \forall k, n, N \in \mathbf{N}, 2\le n \le N \land 1\le k \le n:\;
      \epsilon_{m_1\cdots m_ki_{k+1}\cdots i_n}
      \bar\epsilon^{m_1\cdots m_kj_{k+1}\cdots j_n}
        \to \delta_{m_1\cdots m_ki_{k+1}\cdots i_n}^{m_1\cdots m_kj_{k+1}\cdots j_n}
        = \frac{(N-n+k)!}{(N-n)!}\,
            \delta_{i_{k+1}i_{k+2}\cdots i_n}^{j_{k+1}j_{k+2}\cdots j_n}\,,
   \end{equation}
   where
   \begin{equation}
     \frac{(N-n+k)!}{(N-n)!} = (N-n+1)(N-n+2)\cdots(N-n+k)\,.
   \end{equation}
   In the case~$N=n$, we recover
   \begin{equation}
     \frac{(N-n+k)!}{(N-n)!} = k!
   \end{equation}
   as in~\eqref{eq:epsilon*epsilonbar}, of course. *)
 
(* \thocwmodulesubsection{Ambiguities for $n\not=N$} *)

(* While~\eqref{eq:epsilon*epsilonbar-generalized}
   and~\eqref{eq:epsilon*epsilonbar-generalized-contracted} can be used
   for a single pair of $\epsilon$ and $\bar\epsilon$, it must be stressed
   that~\eqref{eq:epsilon*epsilonbar-generalized} is \emph{not}
   a well defined rule for more general expressions in the case~$n\not=N$,
   because the result depends on the way pairs of $\epsilon$ and $\bar\epsilon$
   are chosen for the application of the rule.

   As a simple example
   consider the complete pairwise contractions of two $\epsilon$
   and two $\bar\epsilon$
   \begin{equation}
   \label{eq:eps2-epsbar2}
       \epsilon_{i_1i_2\cdots i_n} \bar\epsilon^{i_1i_2\cdots i_n}
       \epsilon_{j_1j_2\cdots j_n} \bar\epsilon^{j_1j_2\cdots j_n}\,.
   \end{equation}
   Using~\eqref{eq:epsilon*epsilonbar-generalized}, this can be evaluated in two ways
   \begin{subequations}
   \label{eq:eps2-epsbar2*}
   \begin{equation}
   \label{eq:eps2-epsbar2*a}
     \epsilon_{i_1i_2\cdots i_n} \bar\epsilon^{i_1i_2\cdots i_n}
     \epsilon_{j_1j_2\cdots j_n} \bar\epsilon^{j_1j_2\cdots j_n}
       = \left( \epsilon_{i_1i_2\cdots i_n} \bar\epsilon^{i_1i_2\cdots i_n} \right)^2
     \to \left(\frac{(N-n+n)!}{(N-n)!}\right)^2
       = \left(\frac{N!}{(N-n)!} \right)^2
   \end{equation}
   and
   \begin{multline}
   \label{eq:eps2-epsbar2*b}
     \epsilon_{i_1i_2\cdots i_n} \bar\epsilon^{i_1i_2\cdots i_n}
     \epsilon_{j_1j_2\cdots j_n} \bar\epsilon^{j_1j_2\cdots j_n}
       = \left(\epsilon_{i_1i_2\cdots i_n} \bar\epsilon^{j_1j_2\cdots j_n}\right)
         \left(\epsilon_{j_1j_2\cdots j_n} \bar\epsilon^{i_1i_2\cdots i_n}\right) \\
     \to \delta^{j_1j_2\cdots j_n}_{i_1i_2\cdots i_n}
         \delta_{j_1j_2\cdots j_n}^{i_1i_2\cdots i_n}
       = n!\, \delta^{j_1j_2\cdots j_n}_{i_1i_2\cdots i_n}
       = n!\, \frac{(N-n+n)!}{(N-n)!}
       = \frac{N!n!}{(N-n)!}\,,
   \end{multline}
   \end{subequations}
   which agree only for~$N=n$.
   This observation must be taken into account when interpreting the results
   of self tests.

   Even if the expressions~\eqref{eq:eps2-epsbar2*a} and~\eqref{eq:eps2-epsbar2*b}
   agree for~$n=N$, one might wonder if they correspond to two different
   physical interpretations of the color flows.
   The expression~\eqref{eq:eps2-epsbar2} appears in the color summed
   square matrix elements for $2n$~particles that contain
   color flows of the form
   \begin{equation}
     \epsilon_{i_1i_2\cdots i_n}\bar\epsilon^{j_1j_2\cdots j_n} = 
     \parbox{28\unitlength}{%
       \fmfframe(4,4)(4,4){%
       \begin{fmfgraph*}(25,15)
         \fmfleft{i1,i2,i3}
         \fmfright{j1,j2,j3}
         \fmfv{label=$\epsilon$,label.angle=0}{e}
         \fmfv{label=$\bar\epsilon$,label.angle=180}{eb}
         \fmf{fermion}{i1,e}
         \fmf{fermion}{i2,e}
         \fmf{fermion}{i3,e}
         \fmf{fermion}{eb,j1}
         \fmf{fermion}{eb,j2}
         \fmf{fermion}{eb,j3}
         \fmf{phantom,tension=1.5}{e,eb}
         \fmfdot{e,eb}
       \end{fmfgraph*}}}\,.
     \end{equation}
   The evaluation~\eqref{eq:eps2-epsbar2*a} corresponds to coupling
   $n$~particles carrying the flows $\epsilon_{i_1,i_2,\ldots i_n}$
   to the $n$ particles carrying the flows $\bar\epsilon^{j_1,j_2,\ldots j_n}$
   via an intermediate color singlet state.
   On the other hand, the evaluation~\eqref{eq:eps2-epsbar2*b} corresponds to
   substituting this flow by
   \begin{equation}
     \delta_{i_1i_2\cdots i_n}^{j_1j_2\cdots j_n} = 
     \parbox{28\unitlength}{%
       \fmfframe(4,4)(4,4){%
       \begin{fmfgraph}(20,10)
         \fmfleft{i1,i2,i3}
         \fmfright{j1,j2,j3}
         \fmf{fermion}{i1,j1}
         \fmf{fermion}{i2,j2}
         \fmf{fermion}{i3,j3}
       \end{fmfgraph}}} -
     \parbox{28\unitlength}{%
       \fmfframe(4,4)(4,4){%
       \begin{fmfgraph}(20,10)
         \fmfleft{i1,i2,i3}
         \fmfright{j1,j2,j3}
         \fmf{plain}{i1,d1}
         \fmf{plain,rubout}{i2,d2}
         \fmf{fermion,tension=2}{d1,j2}
         \fmf{fermion,tension=2}{d2,j1}
         \fmf{fermion}{i3,j3}
       \end{fmfgraph}}} +
     \parbox{28\unitlength}{%
       \fmfframe(4,4)(4,4){%
       \begin{fmfgraph}(20,10)
         \fmfleft{i1,i2,i3}
         \fmfright{j1,j2,j3}
         \fmf{fermion}{i1,d1}
         \fmf{plain,rubout}{i2,d2}
         \fmf{plain}{d1,j2}
         \fmf{fermion,rubout}{d2,j3}
         \fmf{fermion,rubout}{i3,j1}
       \end{fmfgraph}}} + \ldots\,,
     \end{equation}
   which, at first sight, appears to introduce colored intermediate states.

   However,
   this is not really the case, because the colors cancel out for $n=N=N_C$.
   This can be seen by looking at the scattering of such a state with a
   particle in the fundamental representation
   \begin{equation}
     \parbox{28\unitlength}{%
       \fmfframe(4,4)(4,4){%
       \begin{fmfgraph*}(25,15)
         \fmfleft{i1,i2}
         \fmfright{j1,j2}
         \fmflabel{$A_n$}{i2}
         \fmflabel{$A_n$}{j2}
         \fmflabel{$N$}{i1}
         \fmflabel{$N$}{j1}
         \fmf{fermion}{i1,v1,j1}
         \fmf{dbl_plain_arrow}{i2,v2,j2}
         \fmf{gluon,tension=0.4}{v1,v2}
         \fmfdot{v1,v2}
       \end{fmfgraph*}}}
   \end{equation}
   and calculating the spin summed squared matrix element
   \begin{multline}
     \label{eq:AnS1->AnS1}
     \sum \left|M_n\right|^2
       = \tr\left(T^{A_n}_a T^{A_n}_b\right) \tr\left(T_a T_b\right)
       = \tr\left(T^{A_n}_a T^{A_n}_a\right)
       = \dim(A_n) C_2(A_n) \\
%%%    = { N \choose n } \frac{n(N-n)(N+1)}{N}
       = \frac{N!}{n!(N-n)!} \frac{n(N-n)(N+1)}{N}
       = \frac{N+1}{(n-1)!}\frac{(N-1)!}{(N-n-1)!}
   \end{multline}
   where~$T^{A_n}$ denotes the generator,
   $\dim(A_n)$ the dimension and
   $C_2(A_n)$ the quadratic Casimir~\eqref{eq:C_2(A_n)}
   in the totally antisymmetric
   product of $n$ fundamental representations\footnote{%
   We can use~\eqref{eq:AnS1->AnS1} to test our
   evaluator and find agreement, e.\,g.~for $n=2,3,4,5$
   \begin{subequations}
   \begin{align}
      \sum \left|M_2\right|^2
%% %    = \left( N^3 - 2 N^2 - N + 2 \right)
       &= \left(N+1\right) \left(N-1\right) \left(N-2\right) \\
      \sum \left|M_3\right|^2
%% %    = \frac{1}{2} \left( N^4 - 5 N^3 + 5 N^2 + 5 N - 6 \right)
       &= \frac{N+1}{2} \left(N-1\right) \left(N-2\right)
            \left(N-3\right) \\
      \sum \left|M_4\right|^2
%% %    = \frac{1}{6} \left( N^5 - 9 N^4 + 25 N^3 - 15 N^2 - 26 N + 24 \right)
       &= \frac{N+1}{6} \left(N-1\right) \left(N-2\right)
            \left(N-3\right) \left(N-4\right) \\
      \sum \left|M_5\right|^2
       &= \frac{N+1}{24} \left(N-1\right) \left(N-2\right)
            \left(N-3\right) \left(N-4\right) \left(N-5\right) \,.
   \end{align}
   \end{subequations}}.
   This expression vanishes for $n\ge N$ and is non-zero for
   $n<N$.  The case $n>N$ is obvious from antisymmetry, but
   the case $n=N$ depends on the fact that the totally antisymmetric
   product of $N$ fundamental representations corresponds to a singlet.
   Therefore, we are free to choose arbitrary pairings of
   $\epsilon$ with $\bar\epsilon$ without affecting the our results for
   summed squared matrix elements.

   Nevertheless, there appear to remain ambiguities in amplitudes with
   more than one $\epsilon$ or $\bar\epsilon$. For $n=N=3$, they first appear
   in amplitudes for 5~particles.  These can contain
   color flows of the form
   \begin{equation}
     M_{i_1i_2,j_1j_2}^{k}
       = \epsilon_{i_1i_2m_1}\epsilon_{j_1j_2m_2} \bar\epsilon^{m_1m_2k}
   \end{equation}
   and we have to decide whether to evaluate this as
   \begin{subequations}
   \begin{equation}
     M_{i_1i_2,j_1j_2}^{k}
       \to M_{i_1i_2,j_1j_2}^{(j)\,k}
       = \epsilon_{i_1i_2m_1}
         (N-2)\,\left(   \delta_{j_1}^{k}\delta_{j_2}^{m_1}
                       - \delta_{j_1}^{m_1}\delta_{j_2}^{k} \right) \\
       = 
         (N-2)\,\left(   \epsilon_{i_1i_2j_2} \delta_{j_1}^{k}
                       - \epsilon_{i_1i_2j_1} \delta_{j_2}^{k} \right)
   \end{equation}
   or
   \begin{equation}
     M_{i_1i_2,j_1j_2}^{k}
       \to M_{i_1i_2,j_1j_2}^{(i)\,k}
       = \epsilon_{j_1j_2m_2}
         (N-2)\,\left(   \delta_{i_1}^{m_2}\delta_{i_2}^{k}
                       - \delta_{i_1}^{k}\delta_{i_2}^{m_2} \right)
       = (N-2)\,\left(   \epsilon_{j_1j_2i_1}\delta_{i_2}^{k}
                       - \epsilon_{j_1j_2i_2}\delta_{i_1}^{k} \right)\,,
   \end{equation}
   \end{subequations}
   where the superscript denotes which of the $\epsilon$ has been
   contracted with the $\bar\epsilon$ using~\eqref{eq:epsilon*epsilonbar}.
   These results are manifestly antisymmetric under the exchange of
   the elements of each of the two pairs of indices separately, but
   not under the exchange of the pairs.

   Fortunately, in the case~$n=N$, we can make use of relations
   of the form
   \begin{equation}
   \label{eq:sum(epsilon*delta)=0}
     \sum_{\sigma\in S_{n+1}} (-1)^{\varepsilon(\sigma)}
         \epsilon_{\sigma(i_1)\sigma(i_2)\cdots\sigma(i_n)}
         \delta_{\sigma(i_{n+1})}^j = 0\,,
   \end{equation}
   that follow from the fact that there is no totally antisymmetric
   tensor of rank~$n>N$ in $N$ dimensions.  For example
   \begin{equation}
       \epsilon_{ijk}\delta_l^m
     - \epsilon_{lij}\delta_k^m
     + \epsilon_{kli}\delta_j^m
     - \epsilon_{jkl}\delta_i^m
     = 0
   \end{equation}
   or
   \begin{equation}
       \epsilon_{ijk}\delta_l^m
     - \epsilon_{ijl}\delta_k^m
     = - \epsilon_{kli}\delta_j^m
       + \epsilon_{klj}\delta_i^m
   \end{equation}
   proves that
   \begin{equation}
       M_{i_1i_2,j_1j_2}^{(j)\,k}
       = - M_{j_1j_2,i_1i_2}^{(j)\,k}
   \end{equation}
   and equivalent relations for~$M^{(k)}$ and~$M^{(i)}$ in the
   case~$n=N=3$.  Therefore the amplitudes satisfy all symmetry
   requirements in the physical case, just not manifestly.

   Note that we could also observe that
   \begin{equation}
       M_{i_1i_2,j_1j_2}^{(i)\,k}
       = - M_{j_1j_2,i_1i_2}^{(j)\,k}
   \end{equation}
   and construct an equivalent amplitude that manifestly satisfies
   all required antisymmetries
   \begin{equation}
     M_{i_1i_2,j_1j_2}^{k}
       = \frac{1}{2}\left(   M_{i_1i_2,j_1j_2}^{(i)\,k}
                           + M_{j_1j_2,i_1i_2}^{(j)\,k} \right)
       = \frac{N-2}{2} \left(
             \epsilon_{i_1i_2j_2} \delta_{j_1}^{k}
           - \epsilon_{i_1i_2j_1} \delta_{j_2}^{k}
           + \epsilon_{j_1j_2i_1}\delta_{i_2}^{k}
           - \epsilon_{j_1j_2i_2}\delta_{i_1}^{k}
                       \right)\,.
   \end{equation}
   However, this approach conflicts with a recursive construction of the
   amplitudes, since it would require a consideration of the
   complete amplitude, using more and more complicated
   variations on~\eqref{eq:sum(epsilon*delta)=0}. *)


(* \thocwmodulesubsection{Evaluation Strategy}
   \label{sec:epsilon-evaluation-strategy} *)

(* Faced with a non-free tensor algebra, we have to choose
   an evaluation strategy.   If we encounter a pair of $\epsilon$
   and $\bar\epsilon$ with a joint contracted index, we should 
   use~\eqref{eq:epsilon*epsilonbar-single-contraction} immediately.
   Note this does not yet resolve all ambiguities because there are
   cases in which an $\epsilon$
   (or $\bar\epsilon$) can be contracted with more than one $\bar\epsilon$
   (or $\epsilon$) and we have to make a choice.  However, we will
   obtain equivalent, if not manifestly equal, results in the case $n=N$.

   In the case of disconnected pairs of~$\epsilon$ and $\bar\epsilon$,
   we have to decide whether to use~\eqref{eq:epsilon*epsilonbar-generalized}
   to produce an amplitude that contains \emph{only} $\epsilon$
   (or $\bar\epsilon$).  A disadvantage of this strategy is that
   each application of~\eqref{eq:epsilon*epsilonbar-generalized} produces
   $n!$ permutations of Kronecker deltas that have to be evaluated.
   However, keeping all disconnected $\epsilon$ and $\bar\epsilon$
   will require to try many more color flows for the complete amplitude
   since there can be both incoming and outgoing lines that are not
   continued through the diagram. Therefore we decide to \emph{always
   apply~\eqref{eq:epsilon*epsilonbar-generalized} as soon as possible}.

   There remains to determine a prescription for consistently selecting
   the $\epsilon$-$\bar\epsilon$-pairs to be contracted if there is more
   than one possibility.  In particular, we \emph{must not} give in to the
   temptation of premature optimization: when evaluating the color flows
   for a 1POW in a fusion (cf.~[Color_Fusion],
   pages~\pageref{sec:colorflow-fusions}\,f{}f),
   we know the color flows for all incoming lines.
   One is therefore tempted to choose a pair with disjoint color flows,
   since the evaluation for this color flow could be terminated immediately.
   Unfortunately, this would not be consistent, because a different choice
   would be made for different color flows.  Imagine, for example the fusion
   of $\bar\epsilon^{123}$ with $\epsilon_{123}\epsilon_{456}$ or
   $\epsilon_{456}\epsilon_{123}$.  In both cases, we will obtain
   $3!\,\epsilon_{456}$ or~$0$, depending of our choice.  If we were to
   attempt to optimize the evaluation and make the choice that results
   in~$0$, we would not get the correct result.

   Instead we have to make the \emph{same} choice for every external
   color flow.  This requires ignoring the
   external color flow indices.  For this to work, we must use an ordered
   data structure for the unprocessed $\epsilon$ and $\bar\epsilon$.   In
   particular, we \emph{must not} use a [Set], where the ordering of the
   elements will typically depend on the color flow indices.  Instead, we
   should use lists and apply~\eqref{eq:epsilon*epsilonbar-generalized}
   consequently to the heads of these lists.
   Note that selecting contracted mutually $\epsilon$-$\bar\epsilon$-pairs
   does not introduce a dependency on the external color flow indices! *)

let is_tadpole = function
  | Arrow (tail, tip) -> matching_summation tail tip
  | Ghost _ -> false

let epsilon = function
  | [] -> invalid_arg "Arrow.epsilon: rank 0"
  | [_] -> invalid_arg "Arrow.epsilon: rank 1"
  | tips -> List.map (fun tip -> I tip) tips

let epsilon_bar = function
  | [] -> invalid_arg "Arrow.epsilon_bar: rank 0"
  | [_] -> invalid_arg "Arrow.epsilon_bar: rank 1"
  | tails -> List.map (fun tail -> I tail) tails

(* Composite Arrows. *)

let rec chain = function
  | [] -> []
  | [a] -> [a => a]
  | [a; b] -> [a => b]
  | a :: (b :: _ as rest) -> (a => b) :: chain rest

let rec cycle' a = function
  | [] -> [a => a]
  | [b] -> [b => a]
  | b :: (c :: _ as rest) -> (b => c) :: cycle' a rest

let cycle = function
  | [] -> []
  | a :: _ as a_list -> cycle' a a_list

module Test =
  struct

    open OUnit

    let suite_chain =
      "chain" >:::
        [ "[]" >:: (fun () -> assert_equal [] (chain []));
          "[1]" >:: (fun () -> assert_equal [1 => 1] (chain [1]));
          "[1;2]" >:: (fun () -> assert_equal [1 => 2] (chain [1; 2]));
          "[1;2;3]" >:: (fun () -> assert_equal [1 => 2; 2 => 3] (chain [1; 2; 3]));
          "[1;2;3;4]" >:: (fun () -> assert_equal [1 => 2; 2 => 3; 3 => 4] (chain [1; 2; 3; 4])) ]

    let suite_cycle =
      "cycle" >:::
        [ "[]" >:: (fun () -> assert_equal [] (cycle []));
          "[1]" >:: (fun () -> assert_equal [1 => 1] (cycle [1]));
          "[1;2]" >:: (fun () -> assert_equal [1 => 2; 2 => 1] (cycle [1; 2]));
          "[1;2;3]" >:: (fun () -> assert_equal [1 => 2; 2 => 3; 3 => 1] (cycle [1; 2; 3]));

          "[1;2;3;4]" >:: (fun () -> assert_equal [1 => 2; 2 => 3; 3 => 4; 4 => 1] (cycle [1; 2; 3; 4])) ]

    let suite_take =
      "take" >:::
        [ "1 []" >:: (fun () -> assert_equal None (take_first_match_opt 1 []));
          "1 [1]" >:: (fun () -> assert_equal (Some ([], [])) (take_first_match_opt 1 [1]));
          "1 [2;3;4]" >:: (fun () -> assert_equal None (take_first_match_opt 1 [2;3;4]));
          "1 [1;2;3]" >:: (fun () -> assert_equal (Some ([], [2;3])) (take_first_match_opt 1 [1;2;3]));
          "2 [1;2;3]" >:: (fun () -> assert_equal (Some ([1], [3])) (take_first_match_opt 2 [1;2;3]));
          "3 [1;2;3]" >:: (fun () -> assert_equal (Some ([2;1], [])) (take_first_match_opt 3 [1;2;3])) ]

    let suite_take2 =
      "take2" >:::
        [ "[] []" >::
	    (fun () -> assert_equal None (take_first_matching_pair_opt [] []));

          "[] [1;2;3]" >::
	    (fun () -> assert_equal None (take_first_matching_pair_opt [] [1;2;3]));

          "[1] [2;3;4]" >::
	    (fun () -> assert_equal None (take_first_matching_pair_opt [1] [2;3;4]));

          "[2;3;4] [1]" >::
	    (fun () -> assert_equal None (take_first_matching_pair_opt [2;3;4] [1]));

          "[1;2;3] [4;5;6;7]" >::
	    (fun () -> assert_equal None (take_first_matching_pair_opt [1;2;3] [4;5;6;7]));

          "[1] [1;2;3]" >::
	    (fun () ->
              assert_equal
                (Some (([],[]), ([],[2;3])))
                (take_first_matching_pair_opt [1] [1;2;3]));

          "[1;2;3] [1;20;30]" >::
	    (fun () ->
              assert_equal
                (Some (([],[2;3]), ([],[20;30])))
                (take_first_matching_pair_opt [1;2;3] [1;20;30]));

          "[1;2;3;4;5;6] [10;20;4;30;40]" >::
	    (fun () ->
              assert_equal
                (Some (([3;2;1],[5;6]), ([20;10],[30;40])))
                (take_first_matching_pair_opt [1;2;3;4;5;6] [10;20;4;30;40])) ]

    let suite_replace =
      "replace" >:::
        [ "1 10 []" >:: (fun () -> assert_equal None (replace_first_opt 1 2 []));
          "1 10 [1]" >:: (fun () -> assert_equal (Some [10]) (replace_first_opt 1 10 [1]));
          "1 [2;3;4]" >:: (fun () -> assert_equal None (replace_first_opt 1 10 [2;3;4]));
          "1 [1;2;3]" >:: (fun () -> assert_equal (Some [10;2;3]) (replace_first_opt 1 10 [1;2;3]));
          "2 [1;2;3]" >:: (fun () -> assert_equal (Some [1;10;3]) (replace_first_opt 2 10 [1;2;3]));
          "3 [1;2;3]" >:: (fun () -> assert_equal (Some [1;2;10]) (replace_first_opt 3 10 [1;2;3])) ]

    let suite =
      "Arrow" >:::
	[suite_chain;
         suite_cycle;
         suite_take;
         suite_take2;
         suite_replace]

    let suite_long =
      "Arrow long" >:::
	[]

  end

let pp_free fmt f =
  Format.fprintf fmt "%s" (free_to_string f)

let pp_factor fmt f =
  Format.fprintf fmt "%s" (factor_to_string f)
