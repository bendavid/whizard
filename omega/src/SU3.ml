(* SU3.ml --

   Copyright (C) 2022-2024 by

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

(* \thocwmodulesection{Import Functions from [Birdtracks]} *)
module A = Arrow
open Arrow.Infix
module L = Algebra.Laurent

type t = Birdtracks.t
open Birdtracks
open Birdtracks.Infix

(* \thocwmodulesection{Constructors specific to $\mathrm{SU}(N_C)$} *)

(* \thocwmodulesubsection{Fundamental and Adjoint Representation} *)

let delta3 i j =
  [ Arrows { coeff = L.unit; arrows = j ==> i } ]

let delta8 a b =
  [ Arrows { coeff = L.unit; arrows = a <=> b } ]

(* If the~$\delta_{ab}$ originates from
   a~$\tr(T_aT_b)$, like an effective~$gg\to H$
   coupling, it makes a difference in the color
   flow basis and we must write the full expression~(6.2)
   from~\cite{Kilian:2012pz} including the ghosts instead.
   Note that the sign for the terms with one ghost
   has not been spelled out in that reference. *)

let delta8_loop a b =
  [ Arrows { coeff = L.unit; arrows = a <=> b };
    Arrows { coeff = L.int (-1); arrows = [a => a; ?? b] };
    Arrows { coeff = L.int (-1); arrows = [?? a; b => b] };
    Arrows { coeff = L.nc 1; arrows = [?? a; ?? b] } ]

(* The following can be used for computing polarization sums
   (eventually, this could make the [Flow] module redundant).
   Note that we have $-N_C$ instead of $-1/N_C$ in the ghost
   contribution here, because [add_arrow_to_arrows_list']
   from the module [Birdtracks] (cf.~page ~\pageref{pg:add_arrow})
   will produce a factor of $-1/N_C$ when contracting each one
   of the two ghost indices.
   Indeed, with this definition we can maintain all projection
   properties
   \begin{itemize}
     \item[] [gluon 1 (-3) *** gluon (-3) 2 = gluon 1 2],
     \item[] [delta8 1 (-3) *** delta8 (-3) 2 = delta8 1 2],
     \item[] [ghost 1 (-3) *** ghost (-3) 2 = ghost 1 2]
   \end{itemize}
   and most importantly
   \begin{itemize}
     \item[] [t (-1) 1 2 *** gluon (-1) (-2) *** t (-2) 3 4 = t (-1) 1 2 *** t (-1) 3 4].
   \end{itemize} *)

let ghost a b =
  [ Arrows { coeff = L.nc (-1); arrows = [?? a; ?? b] } ]

let gluon a b =
  delta8 a b @ ghost a b

(* Note that the arrow is directed from the second to the first
   index, opposite to our color flow paper~\cite{Kilian:2012pz}.
   Fortunately, this is just a matter of conventions.
\begin{subequations}
\begin{align}
\parbox{28\unitlength}{%
  \fmfframe(4,4)(4,4){%
  \begin{fmfgraph*}(20,20)
    \fmfleft{f1,f2}
    \fmfright{g}
    \fmfv{label=$i$}{f2}
    \fmfv{label=$j$}{f1}
    \fmfv{label=$a$}{g}
    \fmf{fermion}{f1,v}
    \fmf{fermion}{v,f2}
    \fmf{gluon}{v,g}
  \end{fmfgraph*}}} &\Longrightarrow
\parbox{28\unitlength}{%
  \fmfframe(4,4)(4,4){%
  \begin{fmfgraph*}(20,20)
    \fmfleft{f1,f2}
    \fmfright{g}
    \fmfv{label=$i$}{f2}
    \fmfv{label=$j$}{f1}
    \fmfv{label=$a$}{g}
    \fmf{phantom}{f1,v}
    \fmf{phantom}{v,f2}
    \fmf{phantom}{v,g}
    \fmffreeze
    \fmfi{phantom_arrow}{vpath (__v, __g) sideways -thick}
    \fmfi{phantom_arrow}{(reverse vpath (__v, __g)) sideways -thick}
    \fmfi{phantom_arrow}{vpath (__f1, __v)}
    \fmfi{phantom_arrow}{vpath (__v, __f2)}
    \fmfi{plain}{%
      (vpath (__f1, __v) join (vpath (__v, __g)) sideways -thick)}
    \fmfi{plain}{%
      ((reverse vpath (__g, __v) sideways -thick) join vpath (__v, __f2))}
  \end{fmfgraph*}}}
\parbox{28\unitlength}{%
  \fmfframe(4,4)(4,4){%
  \begin{fmfgraph*}(20,20)
    \fmfleft{f1,f2}
    \fmfright{g}
    \fmfv{label=$i$}{f1}
    \fmfv{label=$j$}{f2}
    \fmfv{label=$a$}{g}
    \fmf{fermion}{f1,v}
    \fmf{fermion}{v,f2}
    \fmf{dots}{v,g}
  \end{fmfgraph*}}}\\
  T_a^{ij} \qquad\quad
    &\Longrightarrow \qquad\quad \delta^{ia}\delta^{aj}
       \qquad\qquad\qquad - \delta^{ij}
\end{align}
\end{subequations} *)

let t a i j =
  [ Arrows { coeff = L.unit; arrows = [j => a; a => i] };
    Arrows { coeff = L.int (-1); arrows = [j => i; ?? a] } ]

(* Note that while we expect $\tr(T_a)=T_a^{ii}=0$,
   the evaluation of the expression [t 1 (-1) (-1)] will stop
   at [ [ -1 => 1; 1 => -1 ] --- [ -1 => -1; ?? 1 ] ], because the
   summation index appears in a single term.
   However, a naive further evaluation would get stuck at
   [ [ 1 => 1 ] --- nc *** [ ?? 1 ] ].
   Fortunately, traces of single generators are never needed in our
   applications.  We just have to resist the temptation to use them
   in unit tests. *)

(*
\begin{equation}
\parbox{29\unitlength}{%
  \fmfframe(2,2)(2,2){%
  \begin{fmfgraph*}(25,25)
    \fmfleft{g1,g2}
    \fmfright{g3}
    \fmfv{label=$a$}{g1}
    \fmfv{label=$b$}{g2}
    \fmfv{label=$c$}{g3}
    \fmf{gluon}{g1,v}
    \fmf{gluon}{g2,v}
    \fmf{gluon}{g3,v}
  \end{fmfgraph*}}}
\qquad\Longrightarrow
\parbox{29\unitlength}{%
  \fmfframe(2,2)(2,2){%
  \begin{fmfgraph*}(25,25)
    \fmfleft{g1,g2}
    \fmfright{g3}
    \fmfv{label=$a$}{g1}
    \fmfv{label=$b$}{g2}
    \fmfv{label=$c$}{g3}
    \fmf{phantom}{g1,v}
    \fmf{phantom}{g2,v}
    \fmf{phantom}{g3,v}
    \fmffreeze
    \fmfi{plain}{(vpath(__g1,__v) join (reverse vpath(__g2,__v))) 
                 sideways thick}
    \fmfi{plain}{(vpath(__g2,__v) join (reverse vpath(__g3,__v)))
                 sideways thick}
    \fmfi{plain}{(vpath(__g3,__v) join (reverse vpath(__g1,__v)))
                 sideways thick}
    \fmfi{phantom_arrow}{vpath (__g1, __v) sideways thick}
    \fmfi{phantom_arrow}{vpath (__g2, __v) sideways thick}
    \fmfi{phantom_arrow}{vpath (__g3, __v) sideways thick}
    \fmfi{phantom_arrow}{(reverse vpath (__g1, __v)) sideways thick}
    \fmfi{phantom_arrow}{(reverse vpath (__g2, __v)) sideways thick}
    \fmfi{phantom_arrow}{(reverse vpath (__g3, __v)) sideways thick}
  \end{fmfgraph*}}}
\qquad
\parbox{29\unitlength}{%
  \fmfframe(2,2)(2,2){%
  \begin{fmfgraph*}(25,25)
    \fmfleft{g1,g2}
    \fmfright{g3}
    \fmfv{label=$a$}{g1}
    \fmfv{label=$b$}{g2}
    \fmfv{label=$c$}{g3}
    \fmf{phantom}{g1,v}
    \fmf{phantom}{g2,v}
    \fmf{phantom}{g3,v}
    \fmffreeze
    \fmfi{plain}{(vpath(__g1,__v) join (reverse vpath(__g3,__v))) 
                 sideways thick}
    \fmfi{plain}{(vpath(__g2,__v) join (reverse vpath(__g1,__v)))
                 sideways thick}
    \fmfi{plain}{(vpath(__g3,__v) join (reverse vpath(__g2,__v)))
                 sideways thick}
    \fmfi{phantom_arrow}{vpath (__g1, __v) sideways thick}
    \fmfi{phantom_arrow}{vpath (__g2, __v) sideways thick}
    \fmfi{phantom_arrow}{vpath (__g3, __v) sideways thick}
    \fmfi{phantom_arrow}{(reverse vpath (__g1, __v)) sideways thick}
    \fmfi{phantom_arrow}{(reverse vpath (__g2, __v)) sideways thick}
    \fmfi{phantom_arrow}{(reverse vpath (__g3, __v)) sideways thick}
  \end{fmfgraph*}}}
\end{equation} *)

let f a b c =
  [ Arrows { coeff = L.imag ( 1); arrows = A.cycle [a; b; c] };
    Arrows { coeff = L.imag (-1); arrows = A.cycle [a; c; b] } ]

(* The generator in the adjoint representation $T_a^{bc}=-\ii f_{abc}$: *)

let t8 a b c =
  minus *** imag *** f a b c

(* This $d_{abc}$ is now compatible with~(6.11) in our color
   flow paper~\cite{Kilian:2012pz}.  The signs had been wrong
   in earlier versions of the code to match the missing
   sign in the ghost contribution to the generator~$T_a^{ij}$
   above. *)

let d a b c =
  [ Arrows { coeff = L.unit; arrows =  A.cycle [a; b; c] };
    Arrows { coeff = L.unit; arrows =  A.cycle [a; c; b] };
    Arrows { coeff = L.int (-2); arrows =  (a <=> b) @ [?? c] };
    Arrows { coeff = L.int (-2); arrows =  (b <=> c) @ [?? a] };
    Arrows { coeff = L.int (-2); arrows =  (c <=> a) @ [?? b] };
    Arrows { coeff = L.int 2; arrows =  [a => a; ?? b; ?? c] };
    Arrows { coeff = L.int 2; arrows =  [?? a; b => b; ?? c] };
    Arrows { coeff = L.int 2; arrows =  [?? a; ?? b; c => c] };
    Arrows { coeff = L.nc (-2); arrows =  [?? a; ?? b; ?? c] } ]

(* \thocwmodulesubsection{Decomposed Tensor Product Representations} *)

let pass_through m n incoming outgoing =
  List.rev_map2 (fun i o -> (m, i) >=>> (n, o)) incoming outgoing

let delta_of_permutations n permutations k l =
  let incoming = ThoList.range 0 (pred n)
  and normalization = List.length permutations in
  List.rev_map
    (fun (eps, outgoing) ->
      Arrows { coeff = L.fraction (eps * normalization);
               arrows = pass_through l k incoming outgoing } )
    permutations

let totally_symmetric n =
  List.map
    (fun p -> (1, p))
    (Combinatorics.permute (ThoList.range 0 (pred n)))

let totally_antisymmetric n =
  (Combinatorics.permute_signed (ThoList.range 0 (pred n)))

let delta_S n k l =
  delta_of_permutations n (totally_symmetric n) k l

let delta_A n k l =
  delta_of_permutations n (totally_antisymmetric n) k l

let delta6 = delta_S 2
let delta10 = delta_S 3
let delta15 = delta_S 4

let delta3bar = delta_A 2

(* Mixed symmetries, as in section 9.4 of the birdtracks book. *)

module IM = Partial.Make (Int)
module P = Permutation.Default

(* Map the elements of [original] to [permuted] in [all], with [all]
   a list of $n$ integers from $0$ to $n-1$ in order, and use the resulting
   list to define a permutation.
   E.\,g.~[permute_partial [1;3] [3;1] [0;1;2;3;4]] will define a
   permutation that transposes the second and fourth element in
   a 5 element list. *)

let permute_partial original permuted all =
  P.of_list (List.map (IM.auto (IM.of_lists original permuted)) all)
                         
let apply1 (sign, indices) (eps, p) =
  (eps * sign, P.list p indices)

let apply signed_permutations signed_indices =
  List.rev_map (apply1 signed_indices) signed_permutations

let apply_list signed_permutations signed_indices =
  ThoList.flatmap (apply signed_permutations) signed_indices

let symmetrizer_of_permutations n original signed_permutations =
  let incoming = ThoList.range 0 (pred n) in
  List.rev_map
    (fun (eps, permuted) ->
      (eps, permute_partial original permuted incoming))
    signed_permutations

let symmetrizer n indices =
  symmetrizer_of_permutations
    n indices
    (List.rev_map (fun p -> (1, p)) (Combinatorics.permute indices))

let anti_symmetrizer n indices =
  symmetrizer_of_permutations
    n indices
    (Combinatorics.permute_signed indices)

let symmetrize n elements indices =
  apply_list (symmetrizer n elements) indices

let anti_symmetrize n elements indices =
  apply_list (anti_symmetrizer n elements) indices
      
let id n =
  [(1, ThoList.range 0 (pred n))]

(* \begin{dubious}
     We can avoid the recursion here, if we use
     [Combinatorics.permute_tensor_signed] in
     [symmetrizer] above.
   \end{dubious} *)

let rec apply_tableau f n tableau indices =
  match tableau with
  | [] | [_] :: _ -> indices
  | cells :: rest ->
     apply_tableau f n rest (f n cells indices)

(* \begin{dubious}
     Here we should at a sanity test for [tableau]: all integers should
     be consecutive starting from 0 with no duplicates.  In additions
     the rows must not grow in length.
   \end{dubious} *)

let young_tableau_valid_omega y =
      Young.standard_tableau ~offset:0 y

let delta_of_tableau tableau i j =
  if young_tableau_valid_omega tableau then
    let n = Young.num_cells_tableau tableau
    and num, den = Young.normalization (Young.diagram_of_tableau tableau)
    and rows = tableau
    and cols = Young.conjugate_tableau tableau in
    let permutations =
      apply_tableau symmetrize n rows (apply_tableau anti_symmetrize n cols (id n)) in
    int num *** fraction den *** delta_of_permutations n permutations i j
  else
    let s = Young.tableau_to_string string_of_int tableau in
    invalid_arg ("SU3.delta_of_tableau: " ^ s ^ " is not standard!")

let _incomplete tensor =
  failwith ("SU3: " ^ tensor ^ " not supported yet!")

let _experimental tensor =
  Printf.eprintf "SU3: %s support still experimental and untested!\n" tensor

let distinct integers =
  let rec distinct' seen = function
    | [] -> true
    | i :: rest ->
       if Sets.Int.mem i seen then
         false
       else
         distinct' (Sets.Int.add i seen) rest in
  distinct' Sets.Int.empty integers
      
(* All lines start here: they point towards the vertex. *)
let epsilon0 tips =
  if distinct tips then
    [ Epsilons ({ coeff = L.unit; arrows = [] }, NEList.singleton (A.epsilon0 tips)) ]
  else
    []

(* All lines end here: they point away from the vertex. *)
let epsilon0_bar tails =
  if distinct tails then
    [ Epsilon_Bars ({ coeff = L.unit; arrows = [] },NEList.singleton (A.epsilon0_bar tails)) ]
  else
    []


(* In order to get the correct $N_C$ dependence of
   quadratic Casimir operators, the arrows in the vertex must
   have the same permutation symmetry as the propagator.  This
   is demonstrated by the unit tests involving Casimir operators
   on page \pageref{pg:casimir-tests} below.  These tests also
   provide a check of our normalization.

   The implementation takes a propagator and uses [Arrow.tee] to
   replace one arrow by the pair of arrows corresponding to the
   insertion of a gluon.  This is repeated for each arrow.
   The normalization remains unchanged from the propagator.
   A minus sign is added for antiparallel arrows, since the
   conjugate representation is~$-T^*_a$.

   To this, we add the diagrams with a gluon connected to one arrow.
   Since these are identical, only one diagram multiplied by the
   difference of the number of parallel and antiparallel arrows
   is added. *)

let insert_gluon a k l term =
  let rec insert_gluon' acc left = function
    | [] -> acc
    | arrow :: right ->
       insert_gluon'
         (Arrows { coeff = Algebra.Laurent.mul (L.int (A.dir k l arrow)) term.coeff;
                   arrows = List.rev_append left ((A.tee a arrow) @ right) } :: acc)
         (arrow :: left)
         right in
  insert_gluon' [] [] term.arrows

let t_of_delta delta a k l =
  match delta k l with
  | [] -> []
  | Arrows { arrows = arrows; _ } :: _ as delta_kl ->
     let n =
       List.fold_left
         (fun acc arrow -> acc + A.dir k l arrow)
         0 arrows in
     let ghosts =
       List.rev_map
         (fun term ->
           match term with
           | Arrows aterm ->
              Arrows { coeff = Algebra.Laurent.mul (L.int (-n)) aterm.coeff;
                       arrows = ?? a :: aterm.arrows }
           | Epsilons _ -> failwith "t_of_delta: unexpected epsilon"
           | Epsilon_Bars _ -> failwith "t_of_delta: unexpected epsilon_bar")
         delta_kl in
     List.fold_left
       (fun acc ->
         function
         | Arrows aterm -> insert_gluon a k l aterm @ acc
         | Epsilons _ -> failwith "t_of_delta: unexpected epsilon"
         | Epsilon_Bars _ -> failwith "t_of_delta: unexpected epsilon_bar")
       ghosts delta_kl
  | Epsilons _ :: _ -> failwith "t_of_delta: unexpected epsilon"
  | Epsilon_Bars _ :: _ -> failwith "t_of_delta: unexpected epsilon_bar"

let t_of_delta delta a k l =
  canonicalize (t_of_delta delta a k l)

let t_S n a k l =
  t_of_delta (delta_S n) a k l

let t_A n a k l =
  t_of_delta (delta_A n) a k l

let t6 = t_S 2
let t10 = t_S 3
let t15 = t_S 4
let t3bar = t_A 2

(* Equivalent definition: *)

let _t8' a b c =
  t_of_delta delta8 a b c

let t_of_tableau tableau a k l =
  t_of_delta (delta_of_tableau tableau) a k l

(* \begin{dubious}
     Check the following for a real live UFO file!
   \end{dubious} *)

(* In the UFO paper, the Clebsh-Gordan is defined
   as~$(K_6)^{\bar\imath\bar\jmath}_{\hphantom{\bar\imath\bar\jmath}m}$.  Therefore, keeping
   our convention for the generators~$T_{a\hphantom{(6),j}i}^{(6),j}$,
   the must arrows \emph{end} at~$m$.

   Naively, one might have expected a normalization factor~$1/\sqrt{2}$,
   but the~$1/2$ makes sure that
     $(K_6)^{\bar\imath\bar\jmath}_{\hphantom{\bar\imath\bar\jmath}m}
      (\overline K_6)^{\bar m}_{\hphantom{\bar m}i'j'}$ and
     $(\overline K_6)^{\bar m}_{\hphantom{\bar m}ij}
      (K_6)^{\bar\imath\bar\jmath}_{\hphantom{\bar\imath\bar\jmath}m'}$
   are projectors. *)

let k6 m i j =
  [ Arrows { coeff = L.fraction 2; arrows = [i =>> (m, 0); j =>> (m, 1)] };
    Arrows { coeff = L.fraction 2; arrows = [i =>> (m, 1); j =>> (m, 0)] } ]

(* The arrow are reversed for~$(\overline K_6)^{\bar m}_{\hphantom{\bar m}ij}$
   and \emph{start} at~$m$. *)

let k6bar m i j =
  [ Arrows { coeff = L.fraction 2; arrows = [(m, 0) >=> i; (m, 1) >=> j] };
    Arrows { coeff = L.fraction 2; arrows = [(m, 1) >=> i; (m, 0) >=> j] } ]

(* \begin{dubious}
     Playing around with an example, it appeared that people expect the
     opposite direction.  But this makes no sense. Investigate!
   \end{dubious} *)

let _k6 m i j =
  [ Arrows { coeff = L.unit; arrows = [(m, 0) >=> i; (m, 1) >=> j] };
    Arrows { coeff = L.unit; arrows = [(m, 1) >=> i; (m, 0) >=> j] } ]

let _k6bar m i j =
  [ Arrows { coeff = L.unit; arrows = [i =>> (m, 0); j =>> (m, 1)] };
    Arrows { coeff = L.unit; arrows = [i =>> (m, 1); j =>> (m, 0)] } ]

(* \thocwmodulesection{Ghosts} *)

let add_ghost_to_aterm a arrows aterm =
  { coeff = L.neg aterm.coeff; arrows = ?? a :: arrows }

let add_loop_to_aterm a arrows aterm =
  { coeff = L.product [L.nc (-1); aterm.coeff]; arrows = ?? a :: arrows }

let add_ghost_to_term a = function
  | Arrows aterm ->
     begin match A.adjoint_arrows_opt a aterm.arrows with
     | None -> Arrows aterm
     | Some (Tee, arrows) -> Arrows (add_ghost_to_aterm a arrows aterm)
     | Some (Reflex, arrows) -> Arrows (add_loop_to_aterm a arrows aterm)
     end
  | Epsilons (aterm, eps) as eterm ->
     begin match A.adjoint_eps_opt a aterm.arrows eps with
     | None -> eterm
     | Some (Tee, arrows, eps) -> Epsilons (add_ghost_to_aterm a arrows aterm, eps)
     | Some (Reflex, arrows, eps) -> Epsilons (add_loop_to_aterm a arrows aterm, eps)
     end
  | Epsilon_Bars (aterm, eps_bar) as bterm ->
     begin match A.adjoint_eps_bar_opt a aterm.arrows eps_bar with
     | None -> bterm
     | Some (Tee, arrows, eps_bar) -> Epsilon_Bars (add_ghost_to_aterm a arrows aterm, eps_bar)
     | Some (Reflex, arrows, eps_bar) -> Epsilon_Bars (add_loop_to_aterm a arrows aterm, eps_bar)
     end

let add_ghost_to_terms a terms =
  canonicalize (List.map (add_ghost_to_term a) terms)

exception Haunted

let evoke_some gluons terms =
  if haunted terms then
    raise Haunted
  else
    sum (Combinatorics.subfolds (Fun.flip add_ghost_to_terms) terms gluons)

let evoke terms =
  evoke_some (adjoints terms) terms
  
(* \thocwmodulesection{Unit Tests} *)

module Test =
  struct
    open OUnit
    module L = Algebra.Laurent

    let exorcised_equal v1 v2 =
      equal (exorcise v1) (exorcise v2)

    (* \thocwmodulesubsection{Trivia} *)

    let suite_sum =
      "sum" >:::

        [ "atoms" >::
            (fun () ->
              equal
                (int 2 *** delta3 1 2)
                (delta3 1 2 +++ delta3 1 2)) ]

    let suite_diff =
      "diff" >:::

        [ "atoms" >::
            (fun () ->
              equal
                (delta3 3 4)
                (delta3 1 2 +++ delta3 3 4 --- delta3 1 2)) ]


    (* \begin{equation}
         \prod_{k=i}^j f(k)
       \end{equation} *)
    let rec product f i j =
      if i > j then
        null
      else if i = j then
        f i
      else
        f i *** product f (succ i) j

    (* In particular
       \begin{equation}
          \text{[nc_minus_n_plus] n k}\, \mapsto N_C-n+k
       \end{equation}
       and
       \begin{multline}
          \text{[product (nc_minus_n_plus n) i j]}\, \mapsto \\
             \prod_{k=i}^j (N_C-n+k)
              = \frac{(N_C-n+j)!}{(N_C-n+i-1)!}
              = (N_C-n+j)(N_C-n+j-1)\cdots(N_C-n+i)
       \end{multline} *)
    let nc_minus_n_plus n k =
      const (L.ints [ (1, 1); (-n + k, 0) ])

    let contractions rank k =
      product (nc_minus_n_plus rank) 1 k

    let suite_times =
      let epsilon = epsilon0
      and epsilon_bar = epsilon0_bar in
      "times" >:::

        [ "reorder components t1*t2" >:: (* trivial $T_a^{ik}T_a^{kj}=T_a^{kj}T_a^{ik}$ *)
	    (fun () ->
              let t1 = t (-1) 1 (-2)
              and t2 = t (-1) (-2) 2 in
	      equal (t1 *** t2) (t2 *** t1));

          "reorder components tr(t1*t2)" >:: (* trivial $T_a^{ij}T_a^{ji}=T_a^{ji}T_a^{ij}$ *)
	    (fun () ->
              let t1 = t 1 (-1) (-2)
              and t2 = t 2 (-2) (-1) in
	      equal (t1 *** t2) (t2 *** t1));

          "reorderings" >::
	    (fun () ->
              let v1 = [Arrows { coeff = L.unit; arrows = [ 1 => -2; -2 => -1; -1 =>  1] }]
              and v2 = [Arrows { coeff = L.unit; arrows = [-1 =>  2;  2 => -2; -2 => -1] }]
              and v' = [Arrows { coeff = L.unit; arrows = [ 1 =>  1;  2 =>  2] }] in
	      equal v' (v1 *** v2));

          "eps*epsbar" >::
	    (fun () ->
	      equal
                (delta3 1 2 *** delta3 3 4 --- delta3 1 4 *** delta3 3 2)
                (epsilon [1; 3] *** epsilon_bar [2; 4]));

          "eps*epsbar -" >::
	    (fun () ->
	      equal
                (delta3 1 4 *** delta3 3 2 --- delta3 1 2 *** delta3 3 4)
                (epsilon [1; 3] *** epsilon_bar [4; 2]));

          "eps*epsbar 1" >::
	    (fun () ->
	      equal (* $N_C-3+1=(N_C-2)$, for $NC=3$: $1$ *)
                (contractions 3 1 ***
                   (delta3 1 2 *** delta3 3 4 --- delta3 1 4 *** delta3 3 2))
                (epsilon [-1; 1; 3] *** epsilon_bar [-1; 2; 4]));

          "eps*epsbar cyclic 1" >::
	    (fun () ->
	      equal (* $N_C-3+1=(N_C-2)$, for $NC=3$: $1$ *)
                (contractions 3 1 ***
                   (delta3 1 2 *** delta3 3 4 --- delta3 1 4 *** delta3 3 2))
                (epsilon [3; -1; 1] *** epsilon_bar [-1; 2; 4]));

          "eps*epsbar cyclic 2" >::
	    (fun () ->
	      equal (* $N_C-3+1=(N_C-2)$, for $NC=3$: $1$ *)
                (contractions 3 1 ***
                   (delta3 1 2 *** delta3 3 4 --- delta3 1 4 *** delta3 3 2))
                (epsilon [-1; 1; 3] *** epsilon_bar [4; -1; 2]));

          "eps*epsbar 2" >::
	    (fun () ->
	      equal (* $(N_C-3+2)(N_C-3+1)=(N_C-1)(N_C-2)$, for $NC=3$: $2$ *)
                (contractions 3 2 *** delta3 1 2)
                (epsilon [-1; -2; 1] *** epsilon_bar [-1; -2; 2]));

          "eps*epsbar 3" >::
	    (fun () ->
	      equal (* $(N_C-3+3)(N_C-3+2)(N_C-3+1)=N_C(N_C-1)(N_C-2)$, for $NC=3$: $3!$ *)
                (contractions 3 3)
                (epsilon [-1; -2; -3] *** epsilon_bar [-1; -2; -3]));

          "eps*epsbar big" >::
	    (fun () ->
	      equal (* $(N_C-5+3)(N_C-5+2)(N_C-5+1)=(N_C-2)(N_C-3)(N_C-4)$, for $NC=5$: $3!$ *)
                (contractions 5 3 ***
                   (epsilon [4; 5] *** epsilon_bar [6; 7]))
                (epsilon [-1; -2; -3; 4; 5] *** epsilon_bar [-1; -2; -3; 6; 7]));

          "eps*epsbar big -" >::
	    (fun () ->
	      equal (* $(N_C-5+3)(N_C-5+2)(N_C-5+1)=(N_C-2)(N_C-3)(N_C-4)$, for $NC=5$: $3!$ *)
                (contractions 5 3 ***
                   (epsilon [5; 4] *** epsilon_bar [6; 7]))
                (epsilon [-1; 4; -3; -2; 5] *** epsilon_bar [-1; -2; -3; 6; 7])) ]

    (* \thocwmodulesubsection{Propagators} *)

    (* Verify the normalization of the propagators by making sure
       that $D^{ij}D^{jk}=D^{ik}$ *)

    let projection_id rep_d =
      equal (rep_d 1 2) (rep_d 1 (-1) *** rep_d (-1) 2)

    let orthogonality d d' =
      assert_zero_vertex (d 1 (-1) *** d' (-1) 2)

    (* Pass every arrow straight through, without (anti-)symmetrization. *)
    let delta_unsymmetrized n k l =
      delta_of_permutations n [(1, ThoList.range 0 (pred n))] k l

    let completeness n tableaux =
      equal
        (delta_unsymmetrized n 1 2)
        (sum (List.map (fun t -> delta_of_tableau t 1 2) tableaux))

    (* The following names are of historical origin. From the time,
       when we didn't have full support for Young tableaux and
       implemented figure 9.1 from the birdtrack book.
       \ytableausetup{centertableaux,smalltableaux}
       \begin{equation}
         \ytableaushort{01,2}
       \end{equation} *)

    let delta_SAS i j =
      delta_of_tableau [[0;1];[2]] i j

    (* \begin{equation}
         \ytableaushort{02,1}
       \end{equation} *)

    let delta_ASA i j =
      delta_of_tableau [[0;2];[1]] i j

    let suite_propagators =
      "propagators" >:::
        [ "D*D=D" >:: (fun () -> projection_id delta3);
          "D8*D8=D8" >:: (fun () -> projection_id delta8);
          "G*G=G" >:: (fun () -> projection_id gluon);
          "D6*D6=D6" >:: (fun () -> projection_id delta6);
          "D10*D10=D10" >:: (fun () -> projection_id delta10);
          "D15*D15=D15" >:: (fun () -> projection_id delta15);
          "D3bar*D3bar=D3bar" >:: (fun () -> projection_id delta3bar);
          "D6*D3bar=0" >:: (fun () -> orthogonality delta6 delta3bar);
          "D_A3*D_A3=D_A3" >:: (fun () -> projection_id (delta_A 3));
          "D10*D_A3=0" >:: (fun () -> orthogonality delta10 (delta_A 3));
          "D_SAS*D_SAS=D_SAS" >:: (fun () -> projection_id delta_SAS);
          "D_ASA*D_ASA=D_ASA" >:: (fun () -> projection_id delta_ASA);
          "D_SAS*D_S3=0" >:: (fun () -> orthogonality delta_SAS (delta_S 3));
          "D_SAS*D_A3=0" >:: (fun () -> orthogonality delta_SAS (delta_A 3));
          "D_SAS*D_ASA=0" >:: (fun () -> orthogonality delta_SAS delta_ASA);
          "D_ASA*D_SAS=0" >:: (fun () -> orthogonality delta_ASA delta_SAS);
          "D_ASA*D_S3=0" >:: (fun () -> orthogonality delta_ASA (delta_S 3));
          "D_ASA*D_A3=0" >:: (fun () -> orthogonality delta_ASA (delta_A 3));
          "DU*DU=DU" >:: (fun () -> projection_id (delta_unsymmetrized 3));

          "S3=[0123]" >::
            (fun () ->
              equal (delta_S 4 1 2) (delta_of_tableau [[0;1;2;3]] 1 2));

          "A3=[0,1,2,3]" >::
            (fun () ->
              equal (delta_A 4 1 2) (delta_of_tableau [[0];[1];[2];[3]] 1 2));

          "[0123]*[012,3]=0" >::
            (fun () ->
              orthogonality
                (delta_of_tableau [[0;1;2;3]])
                (delta_of_tableau [[0;1;2];[3]]));

          "[0123]*[01,23]=0" >::
            (fun () ->
              orthogonality
                (delta_of_tableau [[0;1;2;3]])
                (delta_of_tableau [[0;1];[2;3]]));

          "[012,3]*[012,3]=[012,3]" >::
            (fun () -> projection_id (delta_of_tableau [[0;1;2];[3]]));

          (* \ytableausetup{centertableaux,smalltableaux}
             \begin{equation}
                \ytableaushort{01} + \ytableaushort{0,1}
             \end{equation} *)

          "completeness 2" >:: (fun () -> completeness 2 [ [[0;1]]; [[0];[1]] ]) ;

          "completeness 2'" >::
            (fun () ->
              equal
                (delta_unsymmetrized 2 1 2)
                (delta_S 2 1 2 +++ delta_A 2 1 2));

          (* The normalization factors are written for illustration.  They are
             added by [delta_of_tableau] automatically.
             \ytableausetup{centertableaux,smalltableaux}
             \begin{equation}
                                 \ytableaushort{012}
               + \frac{4}{3}\cdot\ytableaushort{01,2}
               + \frac{4}{3}\cdot\ytableaushort{02,1}
               +                 \ytableaushort{0,1,2}
             \end{equation} *)

          "completeness 3" >::
            (fun () -> completeness 3 [ [[0;1;2]]; [[0;1];[2]]; [[0;2];[1]]; [[0];[1];[2]] ]);

          "completeness 3'" >::
            (fun () ->
              equal
                (delta_unsymmetrized 3 1 2)
                (delta_S 3 1 2 +++ delta_SAS 1 2 +++ delta_ASA 1 2 +++ delta_A 3 1 2));

          (* \ytableausetup{centertableaux,smalltableaux}
             \begin{equation}
                                      \ytableaushort{0123}
                    + \frac{3}{2}\cdot\ytableaushort{012,3}
                    + \frac{3}{2}\cdot\ytableaushort{013,2}
                    + \frac{3}{2}\cdot\ytableaushort{023,1}
                    + \frac{4}{3}\cdot\ytableaushort{01,23}
                    + \frac{4}{3}\cdot\ytableaushort{02,13}
                    + \frac{3}{2}\cdot\ytableaushort{01,2,3}
                    + \frac{3}{2}\cdot\ytableaushort{02,1,3}
                    + \frac{3}{2}\cdot\ytableaushort{03,1,2}
                    +                 \ytableaushort{0,1,2,3}
              \end{equation} *)

          "completeness 4" >::
            (fun () ->
              completeness 4
                [ [[0;1;2;3]];
                  [[0;1;2];[3]]; [[0;1;3];[2]]; [[0;2;3];[1]];
                  [[0;1];[2;3]]; [[0;2];[1;3]];
                  [[0;1];[2];[3]]; [[0;2];[1];[3]]; [[0;3];[1];[2]];
                  [[0];[1];[2];[3]] ]) ]

    (* \thocwmodulesubsection{Normalization} *)

    let suite_normalization =
      "normalization" >:::

        [ "tr(t*t)" >:: (* $\tr(T_aT_b)=\delta_{ab} + \text{ghosts}$ *)
	    (fun () ->
	      equal
                (delta8_loop 1 2)
                (t 1 (-1) (-2) *** t 2 (-2) (-1)));

          "tr(t*t) sans ghosts" >:: (* $\tr(T_aT_b)=\delta_{ab}$ *)
	    (fun () ->
	      exorcised_equal
                (delta8 1 2)
                (t 1 (-1) (-2) *** t 2 (-2) (-1)));

          (* The additional ghostly terms were unexpected, but 
             arises like~(6.2) in our color flow paper~\cite{Kilian:2012pz}. *)
          "t*t*t" >:: (* $T_aT_bT_a=-T_b/N_C + \ldots$ *)
	    (fun () ->
	      equal
                (minus *** over_nc *** t 1 2 3
                 +++ [Arrows { coeff = L.unit; arrows = [1 => 1; 3 => 2] };
                      Arrows { coeff = L.nc (-1); arrows = [3 => 2; ?? 1] }])
                (t (-1) 2 (-2) *** t 1 (-2) (-3) *** t (-1) (-3) 3));

          (* As expected, these ghostly terms cancel in the summed squares
             \begin{equation}
               \tr(T_aT_bT_aT_cT_bT_c)
                 = \tr(T_bT_b)/N_C^2
                 = \delta_{bb}/N_C^2
                 = (N_C^2-1) / N_C^2
                 = 1 - 1 / N_C^2
             \end{equation} *)
          "sum((t*t*t)^2)" >:: 
	    (fun () ->
	      equal
                (ints [(1, 0); (-1, -2)])
                (t (-1) (-11) (-12) *** t (-2) (-12) (-13) *** t (-1) (-13) (-14)
                 *** t (-3) (-14) (-15) *** t (-2) (-15) (-16) *** t (-3) (-16) (-11)));

          "d*d" >::
            (fun () ->
              exorcised_equal
                [ Arrows { coeff = L.ints [(2, 1); (-8,-1)]; arrows = 1 <=> 2 };
                  Arrows { coeff = L.ints [(2, 0); ( 4,-2)]; arrows = [1=>1; 2=>2] }]
                (d 1 (-1) (-2) *** d 2 (-2) (-1))) ]


    (* As proposed in our color flow paper~\cite{Kilian:2012pz},
       we can get the correct (anti-)symmetrized generators
       by sandwiching the following unsymmetrized generators
       between the corresponding (anti-)symmetrized projectors.
       Therefore, the unsymmetrized generators work as long as
       they're used in Feynman diagrams, where they are connected
       by propagators that contain (anti-)symmetrized projectors.
       They even work in the Lie algebra relations and give the
       correct normalization there.

       They fail however for more general color algebra expressions
       that can appear in UFO files.
       In particular, the Casimir operators come out really wrong. *)

    let t_unsymmetrized n k l =
      t_of_delta (delta_unsymmetrized n) k l

    (* The following trivial vertices are \emph{not} used anymore,
       since they don't get the normalization of the Ward identities
       right.  For the quadratic casimir operators, they always produce a
       result proportional to~$C_F=C_2(S_1)$.  This can be understood because
       they correspond to a fundamental representation with spectators.

       (Anti-)symmetrizing by sandwiching with projectors almost works,
       but they must be multiplied by hand by the number of arrows to get the
       normalization right.
       They're here just for documenting what doesn't work. *)
    let t_trivial n a k l =
      let sterile =
        List.map (fun i -> (l, i) >=>> (k, i)) (ThoList.range 1 (pred n)) in
      [ Arrows { coeff = L.int ( 1); arrows = ((l, 0) >=> a) :: (a =>> (k, 0)) :: sterile };
        Arrows { coeff = L.int (-1); arrows = (?? a) :: ((l, 0) >=>> (k, 0)) :: sterile }]

    let t6_trivial = t_trivial 2
    let t10_trivial = t_trivial 3
    let t15_trivial = t_trivial 4

    let t_SAS = t_of_delta delta_SAS
    let t_ASA = t_of_delta delta_ASA

    let symmetrization ?rep_ts rep_tu rep_d =
      let rep_ts =
        match rep_ts with
        | None -> rep_tu
        | Some rep_t -> rep_t in
      equal
        (rep_ts 1 2 3)
        (gluon 1 (-1) *** rep_d 2 (-2) *** rep_tu (-1) (-2) (-3) *** rep_d (-3) 3)

    let suite_symmetrization =
      "symmetrization" >:::

        [ "t6" >:: (fun () -> symmetrization t6 delta6);
          "t10" >:: (fun () -> symmetrization t10 delta10);
          "t15" >:: (fun () -> symmetrization t15 delta15);
          "t3bar" >:: (fun () -> symmetrization t3bar delta3bar);
          "t_SAS" >:: (fun () -> symmetrization t_SAS delta_SAS);
          "t_ASA" >:: (fun () -> symmetrization t_ASA delta_ASA);
          "t6'" >:: (fun () -> symmetrization ~rep_ts:t6 (t_unsymmetrized 2) delta6);
          "t10'" >:: (fun () -> symmetrization ~rep_ts:t10 (t_unsymmetrized 3) delta10);
          "t15'" >:: (fun () -> symmetrization ~rep_ts:t15 (t_unsymmetrized 4) delta15);

          "t6''" >::
            (fun () ->
              equal
                (t6 1 2 3)
                (int 2 *** delta6 2 (-1) *** t6_trivial 1 (-1) (-2) *** delta6 (-2) 3));

          "t10''" >::
            (fun () ->
              equal
                (t10 1 2 3)
                (int 3 *** delta10 2 (-1) *** t10_trivial 1 (-1) (-2) *** delta10 (-2) 3));

          "t15''" >::
            (fun () ->
              equal
                (t15 1 2 3)
                (int 4 *** delta15 2 (-1) *** t15_trivial 1 (-1) (-2) *** delta15 (-2) 3)) ]

    (* \thocwmodulesubsection{Traces} *)

    (* Compute (anti-)commutators of generators in the representation~$r$,
       i.\,e.~$[r(t_a)r(t_b)]_{ij}\mp[r(t_b)r(t_a)]_{ij}$, using
       [isum<0] as summation index in the matrix products. *)
    let commutator rep_t i_sum a b i j =
      multiply [rep_t a i i_sum; rep_t b i_sum j]
      --- multiply [rep_t b i i_sum; rep_t a i_sum j]

    let anti_commutator rep_t i_sum a b i j =
      multiply [rep_t a i i_sum; rep_t b i_sum j]
      +++ multiply [rep_t b i i_sum; rep_t a i_sum j]

    (* Trace of the product of three generators in the representation~$r$,
       i.\,e.~$\tr_r(r(t_a)r(t_b)r(t_c))$, using $-1,-2,-3$ as summation indices
       in the matrix products. *)
    let trace3 rep_t a b c =
      rep_t a (-1) (-2) *** rep_t b (-2) (-3) *** rep_t c (-3) (-1)

    let loop3 a b c =
      [ Arrows { coeff = L.unit; arrows =  A.cycle (List.rev [a; b; c]) };
        Arrows { coeff = L.int (-1); arrows =  (a <=> b) @ [?? c] };
        Arrows { coeff = L.int (-1); arrows =  (b <=> c) @ [?? a] };
        Arrows { coeff = L.int (-1); arrows =  (c <=> a) @ [?? b] };
        Arrows { coeff = L.unit; arrows =  [a => a; ?? b; ?? c] };
        Arrows { coeff = L.unit; arrows =  [?? a; b => b; ?? c] };
        Arrows { coeff = L.unit; arrows =  [?? a; ?? b; c => c] };
        Arrows { coeff = L.nc (-1); arrows =  [?? a; ?? b; ?? c] } ]

    let suite_trace =
      "trace" >:::

        [ "tr(ttt)" >::
            (fun () -> equal (trace3 t 1 2 3) (loop3 1 2 3));

          "tr(ttt) cyclic 1" >:: (* $\tr(T_aT_bT_c)=\tr(T_bT_cT_a)$ *)
            (fun () -> equal (trace3 t 1 2 3) (trace3 t 2 3 1));

          "tr(ttt) cyclic 2" >:: (* $\tr(T_aT_bT_c)=\tr(T_cT_aT_b)$ *)
            (fun () -> equal (trace3 t 1 2 3) (trace3 t 3 1 2));

          (* \begin{dubious}
                Do we expect this?
             \end{dubious} *)
          "tr(tttt)" >:: (* $\tr(T_aT_bT_cT_d)=\ldots$ *)
            (fun () ->
              exorcised_equal
                [ Arrows { coeff = L.unit; arrows = A.cycle [4; 3; 2; 1] }]
                (t 1 (-1) (-2) *** t 2 (-2) (-3) *** t 3 (-3) (-4) *** t 4 (-4) (-1))) ]

    let suite_ghosts =
      "ghosts" >:::

        [ "H->gg" >::
	    (fun () ->
	      equal
                (delta8_loop 1 2)
                (t 1 (-1) (-2) *** t 2 (-2) (-1)));

          "|H->gg|^2" >::
	    (fun () ->
	      equal
                (const (L.ints [ (1, 2);  (-1, 0) ]))
                (delta8_loop (-1) (-2) *** delta8_loop (-2) (-1)));

          "H->ggg f" >::
	    (fun () ->
	      equal
                (imag *** f 1 2 3)
                (trace3 t 1 2 3 --- trace3 t 1 3 2));

          "H->ggg d" >::
	    (fun () ->
	      equal
                (d 1 2 3)
                (trace3 t 1 2 3 +++ trace3 t 1 3 2));

          "H->ggg f'" >::
	    (fun () ->
	      equal
                (imag *** f 1 2 3)
                (t 1 (-3) (-2) *** commutator t (-1) 2 3 (-2) (-3)));

          "H->ggg d'" >::
	    (fun () ->
	      equal
                (d 1 2 3)
                (t 1 (-3) (-2) *** anti_commutator t (-1) 2 3 (-2) (-3)));

          "H->ggg cyclic'" >::
	    (fun () ->
              let trace a b c =
                t a (-3) (-2) *** commutator t (-1) b c (-2) (-3) in
	      equal (trace 1 2 3) (trace 2 3 1)) ]

    let equal_evoke t =
      equal t (evoke (exorcise t))

    let suite_evoke =
      "evoke" >:::

        [ "delta8" >:: (fun () -> equal (delta8_loop 1 2) (evoke (delta8 1 2)));
          "delta8'" >:: (fun () -> equal_evoke (delta8_loop 1 2));
          "d" >:: (fun () -> equal_evoke (d 1 2 3));
          "f" >:: (fun () -> equal_evoke (f 1 2 3));
          "f'" >:: (fun () -> equal (f 1 2 3) (evoke (f 1 2 3)));
          "f''" >:: (fun () -> equal (f 1 2 3) (exorcise (f 1 2 3)));
          "tr(ttt)" >:: (fun () -> equal_evoke (trace3 t 1 2 3));
          "tr(t6t6t6)" >:: (fun () -> equal_evoke (trace3 t6 1 2 3));
          "eps8" >:: (fun () -> equal_evoke (epsilon0_bar [1;2;-1] *** t 4 (-1) 3));
          "eps88" >:: (fun () -> equal_evoke (epsilon0_bar [1;-1;-2] *** t 4 (-1) 2 *** t 5 (-2) 3));
          "eps888" >:: (fun () -> equal_evoke (epsilon0_bar [-1;-2;-3] *** t 4 (-1) 1 ***
                                                 t 5 (-2) 2 *** t 6 (-3) 3));
          "epsbar8" >:: (fun () -> equal_evoke (epsilon0 [1;2;-1] *** t 4 3 (-1)));
          "epsbar88" >:: (fun () -> equal_evoke (epsilon0 [1;-1;-2] *** t 4 2 (-1) *** t 5 3 (-2)));
          "epsbar888" >:: (fun () -> equal_evoke (epsilon0 [-1;-2;-3] *** t 4 1 (-1) ***
                                                    t 5 2 (-2) *** t 6 3 (-3)));
          "epseps88" >:: (fun () -> equal_evoke (epsilon0_bar [1;2;-1] *** t 4 (-1) 3 ***
                                                   epsilon0_bar [5;6;-2] *** t 8 (-2) 9));
          "epsbarepsbar88" >:: (fun () -> equal_evoke (epsilon0 [1;2;-1] *** t 4 3 (-1) ***
                                                         epsilon0 [5;6;-2] *** t 8 9 (-2))) ]


    let ff a1 a2 a3 a4 =
      [ Arrows { coeff = L.int (-1); arrows = A.cycle [a1; a2; a3; a4] };
        Arrows { coeff = L.int ( 1); arrows = A.cycle [a2; a1; a3; a4] };
        Arrows { coeff = L.int ( 1); arrows = A.cycle [a1; a2; a4; a3] };
        Arrows { coeff = L.int (-1); arrows = A.cycle [a2; a1; a4; a3] } ]

    let tf j i a b =
      [ Arrows { coeff = L.imag ( 1); arrows = A.chain [i; a; b; j] };
        Arrows { coeff = L.imag (-1); arrows = A.chain [i; b; a; j] } ]

    let suite_ff =
      "f*f" >:::
        [ "1" >:: (fun () -> equal (ff 1 2 3 4) (f (-1) 1 2 *** f (-1) 3 4));
          "2" >:: (fun () -> equal (ff 1 2 3 4) (f (-1) 1 2 *** f 3 4 (-1)));
          "3" >:: (fun () -> equal (ff 1 2 3 4) (f (-1) 1 2 *** f 4 (-1) 3)) ]

    let suite_tf =
      "t*f" >:::
        [ "1" >:: (fun () -> equal (tf 1 2 3 4) (t (-1) 1 2 *** f (-1) 3 4)) ]

    (* \thocwmodulesubsection{Completeness Relation} *)

    (* Check the completeness relation corresponding
       to $q\bar q$-scattering:
       \begin{equation}
         \parbox{38\unitlength}{%
           \fmfframe(4,2)(4,4){%
           \begin{fmfgraph*}(30,20)
             \setupFourAmp
             \fmflabel{$i$}{i2}
             \fmflabel{$j$}{i1}
             \fmflabel{$k$}{o1}
             \fmflabel{$l$}{o2}
             \fmf{fermion}{i1,v1,i2}
             \fmf{fermion}{o2,v2,o1}
             \fmf{gluon}{v1,v2}
           \end{fmfgraph*}}} =
         \parbox{38\unitlength}{%
           \fmfframe(4,2)(4,4){%
           \begin{fmfgraph*}(30,20)
             \setupFourAmp
             \fmflabel{$i$}{i2}
             \fmflabel{$j$}{i1}
             \fmflabel{$k$}{o1}
             \fmflabel{$l$}{o2}
             \fmfi{phantom_arrow}{vpath (__i1, __v1)}
             \fmfi{phantom_arrow}{vpath (__v1, __v2) sideways -thick}
             \fmfi{phantom_arrow}{vpath (__v2, __o1)}
             \fmfi{phantom_arrow}{vpath (__o2, __v2)}
             \fmfi{phantom_arrow}{reverse vpath (__v1, __v2) sideways -thick}
             \fmfi{phantom_arrow}{vpath (__v1, __i2)}
             \fmfi{plain}{vpath (__i1, __v1) join 
                          (vpath (__v1, __v2) sideways -thick) join
                          vpath (__v2, __o1)}
             \fmfi{plain}{vpath (__o2, __v2) join
                          (reverse vpath (__v1, __v2) sideways -thick) join
                          vpath (__v1, __i2)}
           \end{fmfgraph*}}} +
         \parbox{38\unitlength}{%
           \fmfframe(4,2)(4,4){%
           \begin{fmfgraph*}(30,20)
             \setupFourAmp
             \fmflabel{$i$}{i2}
             \fmflabel{$j$}{i1}
             \fmflabel{$k$}{o1}
             \fmflabel{$l$}{o2}
             \fmfi{phantom_arrow}{vpath (__i1, __v1)}
             \fmfi{phantom_arrow}{vpath (__v2, __o1)}
             \fmfi{phantom_arrow}{vpath (__o2, __v2)}
             \fmfi{phantom_arrow}{vpath (__v1, __i2)}
             \fmfi{plain}{vpath (__i1, __v1) join 
                          vpath (__v1, __i2)}
             \fmfi{plain}{vpath (__o2, __v2) join
                          vpath (__v2, __o1)}
             \fmfi{dots,label=$-1/N_C$}{vpath (__v1, __v2)}
           \end{fmfgraph*}}}
       \end{equation} *)

    (* $T_{a}^{ij} T_{a}^{kl}$ *)
    let tt i j k l =
      t (-1) i j *** t (-1) k l

    (* $ \delta^{il}\delta^{kj} - \delta^{ij}\delta^{kl}/N_C$ *)
    let tt_expected i j k l =
      [ Arrows { coeff = L.unit; arrows = [l => i; j => k] };
        Arrows { coeff = L.over_nc (-1); arrows = [j => i; l => k] }]

    let suite_tt =
      "t*t" >:::
        [ "1" >:: (* $T_{a}^{ij} T_{a}^{kl} = \delta^{il}\delta^{kj} - \delta^{ij}\delta^{kl}/N_C$ *)
	    (fun () -> equal (tt_expected 1 2 3 4) (tt 1 2 3 4)) ]

    (* \thocwmodulesubsection{Lie Algebra} *)

    (* Check the commutation relations $[T_a,T_b]=\ii f_{abc} T_c$
       in various representations. *)
    let lie_algebra_id _rep_t =
      let lhs = imag *** f 1 2 (-1) *** t (-1) 3 4
      and rhs = commutator t (-1) 1 2 3 4 in
      equal lhs rhs

    (* Check the normalization of the structure consistants
       $\mathcal{N} f_{abc} = - \ii \tr(T_a[T_b,T_c])$ *)
    let f_of_rep_id norm rep_t =
      let lhs = norm *** f 1 2 3
      and rhs = f_of_rep rep_t 1 2 3 in
      equal lhs rhs

    (* \begin{dubious}
         Are the normalization factors for the traces of the higher dimensional
         representations correct?
       \end{dubious} *)
    (* \begin{dubious}
         The traces don't work for the symmetrized generators
         that we need elsewhere!
       \end{dubious} *)
    let suite_lie =
      "Lie algebra relations" >:::
        [ "[t,t]=ift" >:: (fun () -> lie_algebra_id t);
          "[t8,t8]=ift8" >:: (fun () -> lie_algebra_id t8);
          "[t6,t6]=ift6" >:: (fun () -> lie_algebra_id t6);
          "[t10,t10]=ift10" >:: (fun () -> lie_algebra_id t10);
          "[t15,t15]=ift15" >:: (fun () -> lie_algebra_id t15);
          "[t3bar,t3bar]=ift3bar" >:: (fun () -> lie_algebra_id t3bar);
          "[tSAS,tSAS]=iftSAS" >:: (fun () -> lie_algebra_id t_SAS);
          "[tASA,tASA]=iftASA" >:: (fun () -> lie_algebra_id t_ASA);
          "[t6,t6]=ift6'" >:: (fun () -> lie_algebra_id (t_unsymmetrized 2));
          "[t10,t10]=ift10'" >:: (fun () -> lie_algebra_id (t_unsymmetrized 3));
          "[t15,t15]=ift15'" >:: (fun () -> lie_algebra_id (t_unsymmetrized 4));
          "[t6,t6]=ift6''" >:: (fun () -> lie_algebra_id t6_trivial);
          "[t10,t10]=ift10''" >:: (fun () -> lie_algebra_id t10_trivial);
          "[t15,t15]=ift15''" >:: (fun () -> lie_algebra_id t15_trivial);
          "if = tr(t[t,t])" >:: (fun () -> f_of_rep_id one t);
          "2n*if = tr(t8[t8,t8])" >:: (fun () -> f_of_rep_id (two *** nc) t8);
          "n*if = tr(t6[t6,t6])" >:: (fun () -> f_of_rep_id nc t6_trivial);
          "n^2*if = tr(t10[t10,t10])" >:: (fun () -> f_of_rep_id (nc *** nc) t10_trivial);
          "n^3*if = tr(t15[t15,t15])" >:: (fun () -> f_of_rep_id (nc *** nc *** nc) t15_trivial) ]

    (* \thocwmodulesubsection{Ward Identities} *)

    (* Testing the color part of basic Ward identities is essentially
       the same as testing the Lie algebra equations above, but with
       generators sandwiched between propagators, as in Feynman diagrams,
       where the relative signs come from the kinematic part of the
       diagrams after applying the equations of motion..   *)

    (* First the diagram with the three gluon vertex
       $\ii f_{abc} D_{cd}^{\text{gluon}} D^{ik} T_d^{kl} D^{lj}$ *)
    let ward_ft rep_t rep_d a b i j =
      imag *** f a b (-11) *** gluon (-11) (-12)
      *** rep_d i (-1) *** rep_t (-12) (-1) (-2) *** rep_d (-2) j

    (* then one diagram with two gauge couplings
       $D^{ik} T_c^{kl} D^{lm} T_c^{mn} D^{nj}$ *)
    let ward_tt1 rep_t rep_d a b i j =
      rep_d i (-1) *** rep_t a (-1) (-2) *** rep_d (-2) (-3)
      *** rep_t b (-3) (-4) *** rep_d (-4) j

    (* finally the difference of exchanged orders:
       $D^{ik} T_a^{kl} D^{lm} T_b^{mn} D^{nj}
       -D^{ik} T_b^{kl} D^{lm} T_a^{mn} D^{nj}$ *)
    let ward_tt rep_t rep_d a b i j =
      ward_tt1 rep_t rep_d a b i j --- ward_tt1 rep_t rep_d b a i j

    (* \begin{dubious}
         The optional [~fudge] factor was used for
         debugging normalizations.
       \end{dubious} *)
    let ward_id ?(fudge=one) rep_t rep_d =
      let lhs = ward_ft rep_t rep_d 1 2 3 4
      and rhs = ward_tt rep_t rep_d 1 2 3 4 in
      equal lhs (fudge *** rhs)

    let suite_ward =
      "Ward identities" >:::
        [ "fund." >:: (fun () -> ward_id t delta3);
          "adj." >:: (fun () -> ward_id t8 delta8);
          "S2" >:: (fun () -> ward_id t6 delta6);
          "S3" >:: (fun () -> ward_id t10 delta10);
          "A2" >:: (fun () -> ward_id t3bar delta3bar);
          "A3" >:: (fun () -> ward_id (t_A 3) (delta_A 3));
          "SAS" >:: (fun () -> ward_id t_SAS delta_SAS);
          "ASA" >:: (fun () -> ward_id t_ASA delta_ASA);
          "S2'" >:: (fun () -> ward_id ~fudge:two t6_trivial delta6);
          "S3'" >:: (fun () -> ward_id ~fudge:(int 3) t10_trivial delta10) ]

    let suite_ward_long =
      "Ward identities" >:::
        [ "S4" >:: (fun () -> ward_id t15 delta15);
          "S4'" >:: (fun () -> ward_id ~fudge:(int 4) t15_trivial delta15) ]

    (* \thocwmodulesubsection{Jacobi Identities} *)

    (* $T_aT_bT_c$ *)
    let prod3 rep_t a b c i j =
      rep_t a i (-1) *** rep_t b (-1) (-2) *** rep_t c (-2) j

    (* $[T_a,[T_b,T_c]]$ *)
    let jacobi1 rep_t a b c i j =
      (prod3 rep_t a b c i j --- prod3 rep_t a c b i j)
      --- (prod3 rep_t b c a i j --- prod3 rep_t c b a i j)

    (* sum of cyclic permutations of $[T_a,[T_b,T_c]]$ *)
    let jacobi rep_t =
      sum [jacobi1 rep_t 1 2 3 4 5;
           jacobi1 rep_t 2 3 1 4 5;
           jacobi1 rep_t 3 1 2 4 5]

    let jacobi_id rep_t =
      assert_zero_vertex (jacobi rep_t)

    let suite_jacobi =
      "Jacobi identities" >:::
        [ "fund." >:: (fun () -> jacobi_id t);
          "adj." >:: (fun () -> jacobi_id f);
          "S2" >:: (fun () -> jacobi_id t6);
          "S3" >:: (fun () -> jacobi_id t10);
          "A2" >:: (fun () -> jacobi_id (t_A 2));
          "A3" >:: (fun () -> jacobi_id (t_A 3));
          "SAS" >:: (fun () -> jacobi_id t_SAS);
          "ASA" >:: (fun () -> jacobi_id t_ASA);
          "S2'" >:: (fun () -> jacobi_id t6_trivial);
          "S3'" >:: (fun () -> jacobi_id t10_trivial) ]

    let suite_jacobi_long =
      "Jacobi identities" >:::
        [ "S4" >:: (fun () -> jacobi_id t15);
          "S4'" >:: (fun () -> jacobi_id t15_trivial) ]

    (* \thocwmodulesubsection{Casimir Operators}
       \label{pg:casimir-tests} *)

    (* We can read of the eigenvalues of the Casimir operators for
       the adjoint, totally symmetric and totally antisymmetric
       representations of~$\mathrm{SU}(N)$ from table~II of
       \texttt{hep-ph/0611341}
       \begin{subequations}
         \begin{align}
           C_2(\text{adj}) &= 2N \\
           C_2(S_n) &= \frac{n(N-1)(N+n)}{N} \\
         \label{eq:C_2(A_n)}
           C_2(A_n) &= \frac{n(N-n)(N+1)}{N}
         \end{align}
       \end{subequations}
       adjusted for our normalization.
       Also from \texttt{arxiv:1912.13302}
       \begin{equation}
           C_3(S_1) =(N^2-1)(N^2-4)/N^2=\frac{N_C^4-5N_C^2+4}{N_C^2}
       \end{equation} *)

    (* Building blocks $n/N_C$ and $N_C+n$ *)
    let n_over_nc n = const (L.ints [ (n, -1) ])
    let nc_plus n = const (L.ints [ (1, 1); (n,0) ])

    (* $C_2(S_n) = n/N_C(N_C-1)(N_C+n)$ *)
    let c2_S n = n_over_nc n *** nc_plus (-1) *** nc_plus n

    (* $C_2(A_n) = n/N_C(N_C-n)(N_C+1)$ *)
    let c2_A n = n_over_nc n *** nc_plus (-n) *** nc_plus 1
               
    let casimir_tt i j = c2_S 1 *** delta3 i j
    let casimir_t6t6 i j = c2_S 2 *** delta6 i j
    let casimir_t10t10 i j = c2_S 3 *** delta10 i j
    let casimir_t15t15 i j = c2_S 4 *** delta15 i j
    let casimir_t3bart3bar i j = c2_A 2 *** delta3bar i j
    let casimir_tA3tA3 i j = c2_A 3 *** delta_A 3 i j

    (* $C_2(\text{adj})=2N_C$ *)
    let ca = L.ints [(2, 1)]
    let casimir_ff a b =
      [ Arrows { coeff = ca; arrows = a <=> b };
        Arrows { coeff = L.int (-2); arrows = [a=>a; b=>b] }]

    (* $C_3(S_1)=N_C^2-5+4/N_C^2$ *)
    let c3f = L.ints [(1, 2); (-5, 0); (4, -2)]
    let casimir_ttt i j = const c3f *** delta3 i j

    let suite_casimir =
      "Casimir operators" >:::

        [ "t*t" >::
	    (fun () ->
	      equal
                (casimir_tt 1 2)
                (t (-1) 1 (-2) *** t (-1) (-2) 2));

          "t*t*t" >::
	    (fun () ->
	      equal
                (casimir_ttt 1 2)
                (d (-1) (-2) (-3) ***
                   t (-1) 1 (-4) *** t (-2) (-4) (-5) *** t (-3) (-5) 2));

          "f*f" >::
	    (fun () ->
	      equal
                (casimir_ff 1 2)
                (minus *** f (-1) 1 (-2) *** f (-1) (-2) 2));

          "t6*t6" >::
	    (fun () ->
	      equal
                (casimir_t6t6 1 2)
                (t6 (-1) 1 (-2) *** t6 (-1) (-2) 2));

          "t3bar*t3bar" >::
	    (fun () ->
	      equal
                (casimir_t3bart3bar 1 2)
                (t3bar (-1) 1 (-2) *** t3bar (-1) (-2) 2));

          "tA3*tA3" >::
	    (fun () ->
	      equal
                (casimir_tA3tA3 1 2)
                (t_A 3 (-1) 1 (-2) *** t_A 3 (-1) (-2) 2));

          "t_SAS*t_SAS" >::
	    (fun () ->
	      equal
                (const (L.ints [(3,1); (-9,-1)]) *** delta_SAS 1 2)
                (t_SAS (-1) 1 (-2) *** t_SAS (-1) (-2) 2));

          "t_ASA*t_ASA" >::
	    (fun () ->
	      equal
                (const (L.ints [(3,1); (-9,-1)]) *** delta_ASA 1 2)
                (t_ASA (-1) 1 (-2) *** t_ASA (-1) (-2) 2));

          "t10*t10" >::
	    (fun () ->
	      equal
                (casimir_t10t10 1 2)
                (t10 (-1) 1 (-2) *** t10 (-1) (-2) 2)) ]

    let suite_casimir_long =
      "Casimir operators" >:::

        [ "t15*t15" >::
	    (fun () ->
	      equal
                (casimir_t15t15 1 2)
                (t15 (-1) 1 (-2) *** t15 (-1) (-2) 2)) ]

    (* \thocwmodulesubsection{Color Sums} *)

    let suite_colorsums =
      "(squared) color sums" >:::

        [ "gluon normalization" >::
	    (fun () ->
	      equal
                (delta8 1 2)
                (delta8 1 (-1) *** gluon (-1) (-2) *** delta8 (-2) 2));

          "f*f" >::
	    (fun () ->
              let sum_ff =
                multiply [ f (-11) (-12) (-13);
                           f (-21) (-22) (-23);
                           gluon (-11) (-21);
                           gluon (-12) (-22);
                           gluon (-13) (-23) ]
              and expected = ints [(2, 3); (-2, 1)] in
	      equal expected sum_ff);

          "d*d" >::
	    (fun () ->
              let sum_dd =
                multiply [ d (-11) (-12) (-13);
                           d (-21) (-22) (-23);
                           gluon (-11) (-21);
                           gluon (-12) (-22);
                           gluon (-13) (-23) ]
              and expected = ints [(2, 3); (-10, 1); (8, -1)] in
	      equal expected sum_dd);

          "f*d" >::
	    (fun () ->
              let sum_fd =
                multiply [ f (-11) (-12) (-13);
                           d (-21) (-22) (-23);
                           gluon (-11) (-21);
                           gluon (-12) (-22);
                           gluon (-13) (-23) ] in
	      assert_zero_vertex sum_fd);

          "Hgg" >::
	    (fun () ->
              let sum_hgg =
                multiply [ delta8_loop (-11) (-12);
                           delta8_loop (-21) (-22);
                           gluon (-11) (-21);
                           gluon (-12) (-22) ]
              and expected = ints [(1, 2); (-1, 0)] in
	      equal expected sum_hgg) ]

    (* \thocwmodulesubsection{Sextet Clebsh-Gordans} *)

    let suite_k6 =
      "k6/k6bar" >:::

        [ "k6bar*k6" >:: (fun () -> equal (delta6 2 1) (k6bar 1 (-1) (-2) *** k6 2 (-1) (-2)));
          "k6*k6bar" >:: (fun () -> equal
                                      ((delta3 3 1 *** delta3 4 2 +++ delta3 4 1 *** delta3 3 2))
                                      (two *** k6 (-1) 1 2 *** k6bar (-1) 3 4)) ]

    let suite =
      "SU3" >:::
	[suite_sum;
         suite_diff;
         suite_times;
         suite_normalization;
         suite_symmetrization;
	 suite_ghosts;
         suite_evoke;
	 suite_propagators;
	 suite_trace;
	 suite_ff;
	 suite_tf;
	 suite_tt;
         suite_lie;
         suite_ward;
         suite_jacobi;
	 suite_casimir;
         suite_colorsums;
         suite_k6]

    let suite_long =
      "SU3 long" >:::
	[suite_ward_long;
         suite_jacobi_long;
         suite_casimir_long]

  end

