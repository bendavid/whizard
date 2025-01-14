(* color.ml --

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

module type Test =
  sig
    val suite : OUnit.test
    val suite_long : OUnit.test
  end

(* \thocwmodulesection{Quantum Numbers} *)

type t =
  | Singlet
  | SUN of int
  | AdjSUN of int
  | YT of int Young.tableau
  | YTC of int Young.tableau

let conjugate = function
  | Singlet -> Singlet
  | SUN n -> SUN (-n)
  | AdjSUN n -> AdjSUN n
  | YT y -> YTC y
  | YTC y -> YT y

let compare c1 c2 =
  match c1, c2 with
  | Singlet, Singlet -> 0
  | Singlet, _ -> -1
  | _, Singlet -> 1
  | SUN n, SUN n' -> compare n n'
  | SUN _, AdjSUN _ -> -1
  | AdjSUN _, SUN _ -> 1
  | AdjSUN n, AdjSUN n' -> compare n n'
  | YT y, YT y' -> compare y y'
  | YT _, YTC _ -> -1
  | YTC _, YT _ -> 1
  | YTC y, YTC y' -> compare y y'
  | _, (YT _ | YTC _) -> -1
  | (YT _ | YTC _) , _ -> 1

(* \thocwmodulesection{Color Flows} *)

module type Flow =
  sig
    type color
    type t = color list * color list
    val rank : t -> int
    val of_list : int list -> color
    val ghost : unit -> color
    val to_lists : t -> int list list
    val in_to_lists : t -> int list list
    val out_to_lists : t -> int list list
    val ghost_flags : t -> bool list
    val in_ghost_flags : t -> bool list
    val out_ghost_flags : t -> bool list
    type power = { num : int; den : int; power : int }
    type factor = power list
    val factor : t -> t -> factor
    val zero : factor
    val factor_table : t list -> factor array array
    module Test : Test
  end

module Flow : Flow = 
  struct

    (* All [int]s are non-zero! *)
    type color =
      | Flow of Color_Propagator.flow
      | Ghost

    let to_cp = function
      | Flow cf -> Color_Propagator.Flow cf
      | Ghost -> Color_Propagator.Ghost

    let _color_to_string c =
      Color_Propagator.to_string (to_cp c)

    (* Incoming and outgoing, since we need to cross the incoming states. *)
    type t = color list * color list

    let rank _cflow =
      2

(* \thocwmodulesubsection{Constructors} *)

    let ghost () =
      Ghost

    let of_list = function
      | [0; 0] -> Flow (PArray.empty, PArray.empty)
      | [c; 0] -> Flow (PArray.of_pairs [(1, c)], PArray.empty)
      | [0; c] -> Flow (PArray.empty, PArray.of_pairs [(1, -c)])
      | [c1; c2] -> Flow (PArray.of_pairs [(1, c1)], PArray.of_pairs [(1, -c2)])
      | _ -> invalid_arg "Color.Flow.of_list: num_lines != 2"

    let to_list = function
      | Ghost -> [0; 0]
      | Flow (cfi, cfo) ->
         begin match PArray.to_pairs cfi, PArray.to_pairs cfo with
         | [], [] -> [0; 0]
         | [(1, c)], [] -> [c; 0]
         | [], [(1, c)] -> [0; -c]
         | [(1, c1)], [(1, c2)] -> [c1; -c2]
         | _, _ -> failwith "Color.Flow.to_list: incomplete"
         end

    let to_lists (cfin, cfout) =
      (List.map to_list cfin) @ (List.map to_list cfout)

    let in_to_lists (cfin, _) =
      List.map to_list cfin

    let out_to_lists (_, cfout) =
      List.map to_list cfout

    let ghost_flag = function
      | Flow _ -> false
      | Ghost -> true

    let ghost_flags (cfin, cfout) =
      (List.map ghost_flag cfin) @ (List.map ghost_flag cfout)

    let in_ghost_flags (cfin, _) =
      List.map ghost_flag cfin

    let out_ghost_flags (_, cfout) =
      List.map ghost_flag cfout

(* \thocwmodulesubsection{Evaluation} *)

    type power = { num : int; den : int; power : int }
    type factor = power list
    let zero = []

    let _factor_to_string = function
      | [] -> "0"
      | factor ->
         String.concat "+"
           (List.map
              (fun p ->
                Printf.sprintf
                  "%d%s%s"
                  p.num
                  (if p.den <> 1 then "/" ^ string_of_int p.den else "")
                  (match p.power with
                   | 0 -> ""
                   | 1 -> "*N"
                   | n -> "*N^" ^ string_of_int n))
              factor)

    let conjugate = function
      | Flow (cfi, cfo) -> Flow (cfo, cfi)
      | Ghost -> Ghost

    let _cross_in (cin, cout) =
      cin @ (List.map conjugate cout)

    let cross_out (cin, cout) =
      (List.map conjugate cin) @ cout
      
(* \thocwmodulesubsection{Handling $\tr(F_{\mu\nu}F^{\mu\nu})$ couplings, a.k.a.~$Hgg$}

   If the model contains couplings of the form $\tr(F_{\mu\nu}F^{\mu\nu})$,
   e.\,g.~the effective $Hgg$ couplings, the color flow rules and the evaluation
   of color weights require special care.
   These couplings are problematic in our recursive construction, since
   fusing a colorless state with a $\mathrm{U}(1)$ ghost produces
   a trace gluon in addition to a $\mathrm{U}(1)$ ghost.  But for this
   fresh trace gluon, no canonical color flow index is available!
   \begin{dubious}
     A possible solution could be the introduction of ``wild card'' color flow
     that are replaced be concrete color flows only at the matching of the
     brakets.  This is worth investigating, but can be postponed in favor of the
     well tested pragmatic approach.
   \end{dubious} *)

(* There are three different cases to consider:
   \begin{enumerate}
   \item
   First consider the case that neither gluon is directly connected by a string
   of such couplings to the external states.  In this case, the gluons must be
   connected to matter, since the gluon self couplings contain no ghost terms.
   Fortunately, if suffices to ajust the ghost-ghost coupling to account for the
   missing ghost-trace couplings.

   The prototypical example is Higgs production in $q\bar q$ scattering via the
   effective $Hgg$ coupling expanded as in~\cite{Kilian:2012pz}:
   \newcommand{\setupFiveAmp}{%
     \fmfleft{i1,i2}
     \fmfright{o1,o2}
     \fmftop{H}
     \fmf{phantom}{i1,v1,i2}
     \fmf{phantom}{o2,v2,o1}
     \fmf{phantom}{v1,vH,v2}
     \fmffreeze}
   \begin{subequations}
   \label{eq:qqqqH}
   \begin{multline}
     \label{eq:qqqqH-full}
     \parbox{38\unitlength}{%
       \fmfframe(4,2)(4,4){%
       \begin{fmfgraph*}(30,20)
         \setupFiveAmp
         \fmflabel{$H$}{H}
         \fmflabel{$q$}{i2}
         \fmflabel{$q$}{i1}
         \fmflabel{$\bar q$}{o1}
         \fmflabel{$\bar q$}{o2}
         \fmf{fermion}{i1,v1,i2}
         \fmf{fermion}{o2,v2,o1}
         \fmf{gluon}{v1,vH,v2}
         \fmf{plain}{vH,H}
       \end{fmfgraph*}}} =
     \parbox{38\unitlength}{%
       \fmfframe(4,2)(4,4){%
       \begin{fmfgraph*}(30,20)
         \setupFiveAmp
         \fmfi{phantom_arrow}{vpath (__i1, __v1)}
         \fmfi{phantom_arrow}{vpath (__v1, __vH) sideways -thick}
         \fmfi{phantom_arrow}{vpath (__vH, __v2) sideways -thick}
         \fmfi{phantom_arrow}{vpath (__v2, __o1)}
         \fmfi{phantom_arrow}{vpath (__o2, __v2)}
         \fmfi{phantom_arrow}{reverse vpath (__v1, __vH) sideways -thick}
         \fmfi{phantom_arrow}{reverse vpath (__vH, __v2) sideways -thick}
         \fmfi{phantom_arrow}{vpath (__v1, __i2)}
         \fmfi{plain}{vpath (__i1, __v1) join 
                      (vpath (__v1, __vH) sideways -thick) join
                      (vpath (__vH, __v2) sideways -thick) join
                      vpath (__v2, __o1)}
         \fmfi{plain}{vpath (__o2, __v2) join
                      (reverse vpath (__vH, __v2) sideways -thick) join
                      (reverse vpath (__v1, __vH) sideways -thick) join
                      vpath (__v1, __i2)}
         \fmf{plain}{H,vH}
       \end{fmfgraph*}}} + \left(-\frac{1}{N_C}\right)
     \parbox{38\unitlength}{%
       \fmfframe(4,2)(4,4){%
       \begin{fmfgraph*}(30,20)
         \setupFiveAmp
         \fmfi{phantom_arrow}{vpath (__i1, __v1)}
         \fmfi{phantom_arrow}{vpath (__v2, __o1)}
         \fmfi{phantom_arrow}{vpath (__v1, __vH) sideways -thick}
         \fmfi{phantom_arrow}{reverse vpath (__v1, __vH) sideways -thick}
         \fmfi{phantom_arrow}{vpath (__o2, __v2)}
         \fmfi{phantom_arrow}{vpath (__v1, __i2)}
         \fmfi{plain}{vpath (__i1, __v1) join 
                      (vpath (__v1, __vH) sideways -thick) join
                      (reverse vpath (__v1, __vH) sideways -thick) join
                      vpath (__v1, __i2)}
         \fmfi{plain}{vpath (__o2, __v2) join
                      vpath (__v2, __o1)}
         \fmf{dots}{vH,v2}
         \fmf{plain}{vH,H}
       \end{fmfgraph*}}} \\ + \left(-\frac{1}{N_C}\right)
     \parbox{38\unitlength}{%
       \fmfframe(4,2)(4,4){%
       \begin{fmfgraph*}(30,20)
         \setupFiveAmp
         \fmfi{phantom_arrow}{vpath (__i1, __v1)}
         \fmfi{phantom_arrow}{vpath (__v1, __i2)}
         \fmfi{phantom_arrow}{vpath (__v2, __o1)}
         \fmfi{phantom_arrow}{vpath (__vH, __v2) sideways -thick}
         \fmfi{phantom_arrow}{reverse vpath (__vH, __v2) sideways -thick}
         \fmfi{phantom_arrow}{vpath (__o2, __v2)}
         \fmfi{plain}{vpath (__i1, __v1) join 
                      vpath (__v1, __i2)}
         \fmfi{plain}{vpath (__o2, __v2) join
                      (reverse vpath (__vH, __v2) sideways -thick) join
                      (vpath (__vH, __v2) sideways -thick) join
                      vpath (__v2, __o1)}
         \fmf{dots}{v1,vH}
         \fmf{plain}{vH,H}
       \end{fmfgraph*}}} + N_C \left(-\frac{1}{N_C}\right)^2
     \parbox{38\unitlength}{%
       \fmfframe(4,2)(4,4){%
       \begin{fmfgraph*}(30,20)
         \setupFiveAmp
         \fmfi{phantom_arrow}{vpath (__i1, __v1)}
         \fmfi{phantom_arrow}{vpath (__v2, __o1)}
         \fmfi{phantom_arrow}{vpath (__o2, __v2)}
         \fmfi{phantom_arrow}{vpath (__v1, __i2)}
         \fmfi{plain}{vpath (__i1, __v1) join 
                      vpath (__v1, __i2)}
         \fmfi{plain}{vpath (__o2, __v2) join
                      vpath (__v2, __o1)}
         \fmf{dots}{v1,vH,v2}
         \fmf{plain}{vH,H}
       \end{fmfgraph*}}}
   \end{multline}
   the sum of which corresponds to the same simple color flows as gluon exchange
   \begin{equation}
     \parbox{28\unitlength}{%
       \fmfframe(4,2)(4,4){%
       \begin{fmfgraph*}(20,20)
         \setupFiveAmp
         \fmfi{phantom_arrow}{vpath (__i1, __v1)}
         \fmfi{phantom_arrow}{vpath (__v1, __vH) sideways -thick}
         \fmfi{phantom_arrow}{vpath (__vH, __v2) sideways -thick}
         \fmfi{phantom_arrow}{vpath (__v2, __o1)}
         \fmfi{phantom_arrow}{vpath (__o2, __v2)}
         \fmfi{phantom_arrow}{reverse vpath (__v1, __vH) sideways -thick}
         \fmfi{phantom_arrow}{reverse vpath (__vH, __v2) sideways -thick}
         \fmfi{phantom_arrow}{vpath (__v1, __i2)}
         \fmfi{plain}{vpath (__i1, __v1) join 
                      (vpath (__v1, __vH) sideways -thick) join
                      (vpath (__vH, __v2) sideways -thick) join
                      vpath (__v2, __o1)}
         \fmfi{plain}{vpath (__o2, __v2) join
                      (reverse vpath (__vH, __v2) sideways -thick) join
                      (reverse vpath (__v1, __vH) sideways -thick) join
                      vpath (__v1, __i2)}
       \end{fmfgraph*}}} - \frac{1}{N_C}
     \parbox{28\unitlength}{%
       \fmfframe(4,2)(4,4){%
       \begin{fmfgraph*}(20,20)
         \setupFiveAmp
         \fmfi{phantom_arrow}{vpath (__i1, __v1)}
         \fmfi{phantom_arrow}{vpath (__v2, __o1)}
         \fmfi{phantom_arrow}{vpath (__o2, __v2)}
         \fmfi{phantom_arrow}{vpath (__v1, __i2)}
         \fmfi{plain}{vpath (__i1, __v1) join 
                      vpath (__v1, __i2)}
         \fmfi{plain}{vpath (__o2, __v2) join
                      vpath (__v2, __o1)}
       \end{fmfgraph*}}}\,.
   \end{equation}
   Squaring and summing these produces the correct result
   \begin{equation}
     N_C^2 + N_C \left(-\frac{1}{N_C}\right) + N_C \left(-\frac{1}{N_C}\right)
           + N_C^2 \left(-\frac{1}{N_C}\right)^2 = N_C^2 - 1\,.
   \end{equation}
   \end{subequations}
   This result can be reproduced without coupling of trace gluons to ghosts
   by simply replacing the ghost-ghost
   coupling~$N_C$ by $-N_C$ in order to cancel the minus sign from the
   additional ghost propagator\footnote{%
   For comparison, naively leaving out the coupling of ghosts to traces results in
   different color flows
   \begin{equation*}
     \parbox{28\unitlength}{%
       \fmfframe(4,2)(4,4){%
       \begin{fmfgraph*}(20,20)
         \setupFiveAmp
         \fmfi{phantom_arrow}{vpath (__i1, __v1)}
         \fmfi{phantom_arrow}{vpath (__v1, __vH) sideways -thick}
         \fmfi{phantom_arrow}{vpath (__vH, __v2) sideways -thick}
         \fmfi{phantom_arrow}{vpath (__v2, __o1)}
         \fmfi{phantom_arrow}{vpath (__o2, __v2)}
         \fmfi{phantom_arrow}{reverse vpath (__v1, __vH) sideways -thick}
         \fmfi{phantom_arrow}{reverse vpath (__vH, __v2) sideways -thick}
         \fmfi{phantom_arrow}{vpath (__v1, __i2)}
         \fmfi{plain}{vpath (__i1, __v1) join 
                      (vpath (__v1, __vH) sideways -thick) join
                      (vpath (__vH, __v2) sideways -thick) join
                      vpath (__v2, __o1)}
         \fmfi{plain}{vpath (__o2, __v2) join
                      (reverse vpath (__vH, __v2) sideways -thick) join
                      (reverse vpath (__v1, __vH) sideways -thick) join
                      vpath (__v1, __i2)}
       \end{fmfgraph*}}} + \frac{1}{N_C}
     \parbox{28\unitlength}{%
       \fmfframe(4,2)(4,4){%
       \begin{fmfgraph*}(20,20)
         \setupFiveAmp
         \fmfi{phantom_arrow}{vpath (__i1, __v1)}
         \fmfi{phantom_arrow}{vpath (__v2, __o1)}
         \fmfi{phantom_arrow}{vpath (__o2, __v2)}
         \fmfi{phantom_arrow}{vpath (__v1, __i2)}
         \fmfi{plain}{vpath (__i1, __v1) join 
                      vpath (__v1, __i2)}
         \fmfi{plain}{vpath (__o2, __v2) join
                      vpath (__v2, __o1)}
       \end{fmfgraph*}}}
   \end{equation*}
   Squaring and summing these would produce the incorrect result
   \begin{equation*}
     N_C^2 + N_C \frac{1}{N_C} + N_C \frac{1}{N_C}
           + N_C^2 \left(\frac{1}{N_C}\right)^2 = N_C^2 + 3\,.
   \end{equation*}}.

   \item
   In the second case of one gluon connected to matter and the other to
   an external state, no special treatment is required.  The prototypical
   example is $q\bar q\to Hg$
   \begin{multline}
     \label{eq:qqHg-full}
     \parbox{38\unitlength}{%
       \fmfframe(4,2)(4,4){%
       \begin{fmfgraph*}(30,20)
         \setupFiveAmp
         \fmflabel{$H$}{H}
         \fmflabel{$q$}{i2}
         \fmflabel{$q$}{i1}
         \fmf{fermion}{i1,v1,i2}
         \fmf{phantom}{o2,v2,o1}
         \fmf{gluon}{v1,vH,v2}
         \fmf{plain}{vH,H}
       \end{fmfgraph*}}} =
     \parbox{38\unitlength}{%
       \fmfframe(4,2)(4,4){%
       \begin{fmfgraph*}(30,20)
         \setupFiveAmp
         \fmfi{phantom_arrow}{vpath (__i1, __v1)}
         \fmfi{phantom_arrow}{vpath (__v1, __vH) sideways -thick}
         \fmfi{phantom_arrow}{vpath (__vH, __v2) sideways -thick}
         \fmfi{phantom_arrow}{reverse vpath (__v1, __vH) sideways -thick}
         \fmfi{phantom_arrow}{reverse vpath (__vH, __v2) sideways -thick}
         \fmfi{phantom_arrow}{vpath (__v1, __i2)}
         \fmfi{plain}{vpath (__i1, __v1) join 
                      (vpath (__v1, __vH) sideways -thick) join
                      (vpath (__vH, __v2) sideways -thick)}
         \fmfi{plain}{(reverse vpath (__vH, __v2) sideways -thick) join
                      (reverse vpath (__v1, __vH) sideways -thick) join
                      vpath (__v1, __i2)}
         \fmf{plain}{H,vH}
       \end{fmfgraph*}}} +
     \parbox{38\unitlength}{%
       \fmfframe(4,2)(4,4){%
       \begin{fmfgraph*}(30,20)
         \setupFiveAmp
         \fmfi{phantom_arrow}{vpath (__i1, __v1)}
         \fmfi{phantom_arrow}{vpath (__v1, __vH) sideways -thick}
         \fmfi{phantom_arrow}{reverse vpath (__v1, __vH) sideways -thick}
         \fmfi{phantom_arrow}{vpath (__v1, __i2)}
         \fmfi{plain}{vpath (__i1, __v1) join 
                      (vpath (__v1, __vH) sideways -thick) join
                      (reverse vpath (__v1, __vH) sideways -thick) join
                      vpath (__v1, __i2)}
         \fmf{dots}{vH,v2}
         \fmf{plain}{vH,H}
       \end{fmfgraph*}}} \\ + \left(-\frac{1}{N_C}\right)
     \parbox{38\unitlength}{%
       \fmfframe(4,2)(4,4){%
       \begin{fmfgraph*}(30,20)
         \setupFiveAmp
         \fmfi{phantom_arrow}{vpath (__i1, __v1)}
         \fmfi{phantom_arrow}{vpath (__v1, __i2)}
         \fmfi{phantom_arrow}{vpath (__vH, __v2) sideways -thick}
         \fmfi{phantom_arrow}{reverse vpath (__vH, __v2) sideways -thick}
         \fmfi{plain}{vpath (__i1, __v1) join 
                      vpath (__v1, __i2)}
         \fmfi{plain}{(reverse vpath (__vH, __v2) sideways -thick) join
                      (vpath (__vH, __v2) sideways -thick)}
         \fmf{dots}{v1,vH}
         \fmf{plain}{vH,H}
       \end{fmfgraph*}}} + N_C \left(-\frac{1}{N_C}\right)
     \parbox{38\unitlength}{%
       \fmfframe(4,2)(4,4){%
       \begin{fmfgraph*}(30,20)
         \setupFiveAmp
         \fmfi{phantom_arrow}{vpath (__i1, __v1)}
         \fmfi{phantom_arrow}{vpath (__v1, __i2)}
         \fmfi{plain}{vpath (__i1, __v1) join 
                      vpath (__v1, __i2)}
         \fmf{dots}{v1,vH,v2}
         \fmf{plain}{vH,H}
       \end{fmfgraph*}}}
   \end{multline}
   The correct result for the summed square is
   again $N_C^2-1$, where the two color flow diagrams with an external ghost
   cancel.  In the simplified rules, the $\mathrm{U}(N_C)$ gluons contribute
   $N_C^2$ and the ghost $-1$.

   \item
   In the third and final case of both gluons connected to external states, we have
   to apply a fudge factor replacing $N_C^2$ by $N_C^2-2$ for each cycle
   of color disconnected gluons.
   The calculation is straightforward, since there is no interference of
   external ghosts and $\mathrm{U}(N_C)$ gluons in the sum of squares.  
   \begin{multline}
     \label{eq:gHg-full}
     \parbox{38\unitlength}{%
       \fmfframe(4,2)(4,4){%
       \begin{fmfgraph*}(30,20)
         \setupFiveAmp
         \fmflabel{$H$}{H}
         \fmf{phantom}{i1,v1,i2}
         \fmf{phantom}{o2,v2,o1}
         \fmf{gluon}{v1,vH,v2}
         \fmf{plain}{vH,H}
       \end{fmfgraph*}}} =
     \parbox{38\unitlength}{%
       \fmfframe(4,2)(4,4){%
       \begin{fmfgraph*}(30,20)
         \setupFiveAmp
         \fmfi{phantom_arrow}{vpath (__v1, __vH) sideways -thick}
         \fmfi{phantom_arrow}{vpath (__vH, __v2) sideways -thick}
         \fmfi{phantom_arrow}{reverse vpath (__v1, __vH) sideways -thick}
         \fmfi{phantom_arrow}{reverse vpath (__vH, __v2) sideways -thick}
         \fmfi{plain}{(vpath (__v1, __vH) sideways -thick) join
                      (vpath (__vH, __v2) sideways -thick)}
         \fmfi{plain}{(reverse vpath (__vH, __v2) sideways -thick) join
                      (reverse vpath (__v1, __vH) sideways -thick)}
         \fmf{plain}{H,vH}
       \end{fmfgraph*}}} +
     \parbox{38\unitlength}{%
       \fmfframe(4,2)(4,4){%
       \begin{fmfgraph*}(30,20)
         \setupFiveAmp
         \fmfi{phantom_arrow}{vpath (__v1, __vH) sideways -thick}
         \fmfi{phantom_arrow}{reverse vpath (__v1, __vH) sideways -thick}
         \fmfi{plain}{(vpath (__v1, __vH) sideways -thick) join
                      (reverse vpath (__v1, __vH) sideways -thick)}
         \fmf{dots}{vH,v2}
         \fmf{plain}{vH,H}
       \end{fmfgraph*}}} \\ +
     \parbox{38\unitlength}{%
       \fmfframe(4,2)(4,4){%
       \begin{fmfgraph*}(30,20)
         \setupFiveAmp
         \fmfi{phantom_arrow}{vpath (__vH, __v2) sideways -thick}
         \fmfi{phantom_arrow}{reverse vpath (__vH, __v2) sideways -thick}
         \fmfi{plain}{(reverse vpath (__vH, __v2) sideways -thick) join
                      (vpath (__vH, __v2) sideways -thick)}
         \fmf{dots}{v1,vH}
         \fmf{plain}{vH,H}
       \end{fmfgraph*}}} + N_C
     \parbox{38\unitlength}{%
       \fmfframe(4,2)(4,4){%
       \begin{fmfgraph*}(30,20)
         \setupFiveAmp
         \fmf{dots}{v1,vH,v2}
         \fmf{plain}{vH,H}
       \end{fmfgraph*}}}
   \end{multline}
   The latter contributes a factor of~$N_C^2$ (two loops) and the former
   a factor of~$(-N_C)^2(-1/N_C)^2=1$ (one $-N_C$ fom each vertex and one $-1/N_C$
   from each line across the cut).  Therefore the sum would be $N_C^2+1$ in contrast
   to the correct result~$N_C^2-1$.  The correct result is then obtained by
   multiplying the gluon term~$N_C^2$ by $1-2/N_C^2$
   \begin{equation}
      N_C^2 + 1 \to N_C^2 \left(1-\frac{2}{N_C^2}\right) + 1
                  = N_C^2 - 2 + 1 = N_C^2 -1\,.
   \end{equation}
   \end{enumerate} *)

(* The factor $(1-2/N_C^2)^n$ in the formula
   \begin{equation}
      N_C^{l} \left(-\frac{1}{N_C}\right)^{k}
      \left(\frac{N_C^2-2}{N_C^2}\right)^{n}\,,
   \end{equation}
   where $l$ is the number of closed color cycles ([cycles] below),
   $k$ is the number of external ghosts ([ghosts]) and
   $n$ is the number of gluon cycles ([gluon_cycles]).
   is the fudge factor taking care of the couplings of $\mathrm{U}(1)$
   ghosts to trace gluons. *)

    (* [endpoints_of_colors colors] creates maps from the position of the
       external colors in [colors] to the tips and tails connected by color
       flow lines. Also produce a set of the positions of external ghosts. *)

    module IMap = Map.Make(Int)
    module ISet = Set.Make(Int)

    type endpoints =
      { tails : int IMap.t;
        tips : int IMap.t;
        ghosts : ISet.t }

    type color_kind =
      | CK_Flow of int * int
      | CK_Ghost

    let color_kind = function
      | Flow (cfi, cfo) -> CK_Flow (List.length (PArray.to_pairs cfi), List.length (PArray.to_pairs cfo))
      | Ghost -> CK_Ghost

    let equal_color_kind1 c1 c2 =
      color_kind c1 = color_kind c2

    let equal_color_kind f1 f2 =
      List.for_all2 equal_color_kind1 f1 f2

    let empty_endpoints =
      { tails = IMap.empty;
        tips = IMap.empty;
        ghosts = ISet.empty }

    let add_endpoint endpoints n = function
      | Ghost -> { endpoints with ghosts = ISet.add n endpoints.ghosts }
      | Flow (cfi, cfo) ->
         begin match PArray.to_pairs cfi, PArray.to_pairs cfo with
         | [], [] -> endpoints
         | [(1, c)], [] -> { endpoints with tips = IMap.add (abs c) n endpoints.tips }
         | [], [(1, c)] -> { endpoints with tails = IMap.add (abs c) n endpoints.tails }
         | [(1, c1)], [(1, c2)] ->
            { endpoints with
              tips = IMap.add (abs c1) n endpoints.tips;
              tails = IMap.add (abs c2) n endpoints.tails }
         | _, _ -> failwith "Color.Flow.add_endpoint: incomplete"
         end

    let endpoints_of_colors colors =
      let _, endpoints =
        List.fold_left
          (fun (n, endpoints) endpoint -> (succ n, add_endpoint endpoints n endpoint))
          (1, empty_endpoints) colors in
      endpoints

    (* Merge the maps of tips and tails to find the pair of connected external
       colors. *)

    let links_of_endpoints endpoints =
      IMap.merge
        (fun _ tail tip ->
          match tail, tip with
          | None, None -> None
          | Some tail, Some tip -> Some (tail, tip)
          | Some tail, None -> invalid_arg ("no tip for tail " ^ string_of_int tail)
          | None, Some tip -> invalid_arg ("no tail for tip " ^ string_of_int tip))
        endpoints.tails endpoints.tips 

    (* Create an [Arrow.free list] that can be used by [Birdtracks]. *)
    let arrows_of_links links =
      IMap.fold (fun _ (tail, tip) acc -> Arrow.Infix.( tail => tip ) :: acc) links []

    module LSet = Set.Make (struct type t = int * int let compare = Stdlib.compare end)

    (* Find the set bidirectional links by computing the intersection of
       the set of links with the set of reversed links.
       We must keep both directions for [Birdtracks.multiply] to succeed. *)
    let double_links links =
      let links, rev_links =
        IMap.fold
          (fun _ (tail, tip) (links, rev_links) ->
            (LSet.add (tail, tip) links, LSet.add (tip, tail) rev_links))
          links (LSet.empty, LSet.empty) in
      LSet.inter links rev_links

(*i
    let f, g = birdtracks [N 5; N_bar 6; SUN (1,2); SUN (6, 5); SUN (2,1); Ghost]
    let f' : Birdtracks.t =
      Birdtracks.( relocate (~-) [ Arrows {coeff = Algebra.Laurent.unit; arrows = f } ] )
    let _ =
      Birdtracks.Infix.( f' *** Birdtracks.rev f' )
i*)

    let birdtracks_of_arrows arrows =
      Birdtracks.( relocate (~-) [ Arrows { coeff = Algebra.Laurent.unit; arrows } ] )

    type flow =
      { flows : Birdtracks.t;
        gluons : Birdtracks.t }

    let birdtracks colors =
      let endpoints = endpoints_of_colors colors in
      let links = links_of_endpoints endpoints in
      let gluons = double_links links in
      let flow =
        ISet.fold
          (fun ghost acc -> Arrow.Infix.( ?? ghost) :: acc)
          endpoints.ghosts (arrows_of_links links)
      and gluons =
        LSet.fold (fun (tail, tip) acc -> Arrow.Infix.( tail => tip ) :: acc) gluons [] in
      { flows = birdtracks_of_arrows flow;
        gluons = birdtracks_of_arrows gluons }

    (* $1-2/N_C^2$ *)
    let fudge_factor =
      Algebra.Laurent.ints [(1,0); (-2,-2)]

    let factor_birdtracks f1 f2 =
      let open Birdtracks in
      match number (Infix.( f1.flows *** rev f2.flows )) with
      | None -> failwith "factor_new"
      | Some result ->
         if Algebra.Laurent.is_null result then
           result
         else
           let gluons = Infix.( f1.gluons *** rev f2.gluons ) in
           match number gluons with
           | None -> result
           | Some gluons ->
              begin match Algebra.Laurent.log gluons with
              | None -> failwith "factor_birdtracks log"
              | Some (_, 0) -> result
              | Some (coeff, n) ->
                 if not (Algebra.QC.is_unit coeff) then
                   failwith "factor_birdtracks log is_unit";
                 if n mod 2 <> 0 then
                   failwith "factor_birdtracks log is odd";
                 Algebra.Laurent.mul result (Algebra.Laurent.pow fudge_factor (n/2))
              end

    let factor f1 f2 =
      let f1 = cross_out f1
      and f2 = cross_out f2 in
      if equal_color_kind f1 f2 then
        factor_birdtracks (birdtracks f1) (birdtracks f2)
      else
        Algebra.Laurent.null

    let factor_of_laurent l =
      List.map
        (fun (c, power) ->
          let num, den = Algebra.Q.to_ratio (Algebra.QC.re c) in
          { num; den; power} )
        (Algebra.Laurent.to_list l)

    let factor_birdtracks f1 f2 =
      factor_of_laurent (factor_birdtracks f1 f2)

    let factor f1 f2 =
      factor_of_laurent (factor f1 f2)

    let factor_table cf_list =
      let cf_array = Array.of_list (List.map cross_out cf_list) in
      let birdtracks_array = Array.map birdtracks cf_array in
      let ncf = Array.length cf_array in
      let cf_table = Array.make_matrix ncf ncf zero in
      for i = 0 to pred ncf do
        for j = 0 to i do
          if equal_color_kind cf_array.(i) cf_array.(j) then
            begin
              cf_table.(i).(j) <- factor_birdtracks birdtracks_array.(i) birdtracks_array.(j);
              cf_table.(j).(i) <- cf_table.(i).(j)
            end
        done
      done;
      cf_table
      
    module Test : Test =
      struct

        open OUnit

(* Here and elsewhere, we have to resist the temptation to define
   these tests as functions with an additional argument [()] in the
   hope to avoid having to package them into an explicit thunk
   [fun () -> eq v1 v2] in order to delay
   evaluation. It turns out that the runtime would then sometimes
   evaluate the argument [v1] or [v2] even \emph{before} the test
   is run.  For pure functions, there is no difference, but the
   compiler appears to treat explicit thunks specially.
   \begin{dubious}
     I haven't yet managed to construct a small demonstrator to find
     out in which circumstances the premature evaluation happens.
   \end{dubious} *)

(*i
        let suite_factor =
          "factor" >:::

            [ "gg->gg interference" >::
                (fun () ->
                  assert_equal
                    [ { num = 1; den = 1; power = 2 }; { num = -2; den = 1; power = 0 } ]
                    (factor
                       ([SUN(3,-1); SUN(4,-2)], [SUN(3,-1); SUN(4,-2)])
                       ([SUN(2,-1); SUN(1,-2)], [SUN(3,-4); SUN(4,-3)])));

              "???" >::
                (fun () ->
                  assert_equal
                    [ ]
                    (factor
                       ([N_bar (-1); N 1], [Ghost])
                       ([N 1; N_bar (-1)], [Ghost]))) ]

        let suite =
          "Color.Flow" >:::
	    [suite_factor]
i*)
        let suite =
          "Color.Flow" >:::
	    []

        let suite_long =
          "Color.Flow long" >:::
	    []

      end
  end

(* \thocwmodulesection{$\mathrm{SU}(N_C)$} *)

module Vertex = SU3
