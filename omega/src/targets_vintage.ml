(* targets_vintage.ml --

   Copyright (C) 1999-2026 by

       Wolfgang Kilian <kilian@physik.uni-siegen.de>
       Thorsten Ohl <ohl@physik.uni-wuerzburg.de>
       Juergen Reuter <juergen.reuter@desy.de>
       with contributions from
       Christian Speckner <cnspeckn@googlemail.com>
       Fabian Bach <fabian.bach@t-online.de> (only parts of this file)
       Marco Sekulla <marco.sekulla@kit.edu> (only parts of this file)
       Bijan Chokoufe Nejad <bijan.chokoufe@desy.de> (only parts of this file)
       So Young Shim <soyoung.shim@desy.de>

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

(* \thocwmodulesection{\texttt{Fortran\,90/95}} *)

(* \thocwmodulesubsection{Dirac Fermions}
   We factor out the code for fermions so that we can use the simpler
   implementation for Dirac fermions if the model contains no Majorana
   fermions. *)

module type Fermions =
  sig
    open Coupling
    val print_current : int * fermionbar * boson * fermion ->
      string -> string -> string -> fuse2 -> unit
    val print_current_mom : int * fermionbar * boson * fermion ->
      string -> string -> string -> string -> string -> string
      -> fuse2 -> unit
    val print_current_p : int * fermion * boson * fermion ->
      string -> string -> string -> fuse2 -> unit
    val print_current_b : int * fermionbar * boson * fermionbar ->
      string -> string -> string -> fuse2 -> unit
    val print_current_g : int * fermionbar * boson * fermion ->
      string -> string -> string -> string -> string -> string
      -> fuse2 -> unit
    val print_current_g4 : int * fermionbar * boson2 * fermion ->
      string -> string -> string -> string -> fuse3 -> unit
    val reverse_braket : bool -> lorentz -> lorentz list -> bool
   end

module type Fermion_Maker = functor (N : Target_Fortran_Names.T) -> Fermions

module Fortran_Fermions (Names : Target_Fortran_Names.T) : Fermions =
  struct

    open Coupling
    open Format

    let format_coupling coeff c =
      match coeff with
      | 1 -> c
      | -1 -> "(-" ^ c ^")"
      | coeff -> string_of_int coeff ^ "*" ^ c

    let format_coupling_2 coeff c =
      match coeff with
      | 1 -> c
      | -1 -> "-" ^ c
      | coeff -> string_of_int coeff ^ "*" ^ c

(* \begin{dubious}
     JR's coupling constant HACK, necessitated by tho's bad design descition.
   \end{dubious} *)

    let fastener s i ?p ?q () =
      try
        let offset = (String.index s '(') in
        if ((String.get s (String.length s - 1)) != ')') then
          failwith "fastener: wrong usage of parentheses"
        else
          let func_name = (String.sub s 0 offset) and
              tail =
            (String.sub s (succ offset) (String.length s - offset - 2)) in
          if (String.contains func_name ')') ||
             (String.contains tail '(') ||
             (String.contains tail ')') then
            failwith "fastener: wrong usage of parentheses"
          else
            func_name ^ "(" ^ string_of_int i ^ "," ^ tail ^ ")"
      with
      | Not_found ->
          if (String.contains s ')') then
            failwith "fastener: wrong usage of parentheses"
          else
            match p with
            | None   -> s ^ "(" ^ string_of_int i ^ ")"
            | Some p ->
              match q with
              | None   -> s ^ "(" ^ p ^ "*" ^ p ^ "," ^ string_of_int i ^ ")"
              | Some q -> s ^ "(" ^ p ^ "," ^ q ^ "," ^ string_of_int i ^ ")"

    let print_fermion_current coeff f c wf1 wf2 fusion =
      let c = format_coupling coeff c in
      match fusion with
      | F13 -> printf "%s_ff(%s,%s,%s)" f c wf1 wf2
      | F31 -> printf "%s_ff(%s,%s,%s)" f c wf2 wf1
      | F23 -> printf "f_%sf(%s,%s,%s)" f c wf1 wf2
      | F32 -> printf "f_%sf(%s,%s,%s)" f c wf2 wf1
      | F12 -> printf "f_f%s(%s,%s,%s)" f c wf1 wf2
      | F21 -> printf "f_f%s(%s,%s,%s)" f c wf2 wf1

(* \begin{dubious}
     Using a two element array for the combined vector-axial and scalar-pseudo
     couplings helps to support HELAS as well.  Since we will probably never
     support general boson couplings with HELAS, it might be retired in favor
     of two separate variables.  For this [Model.constant_symbol] has to be
     generalized.
   \end{dubious} *)

(* \begin{dubious}
     NB: passing the array instead of two separate constants would be a
     \emph{bad} idea, because the support for Majorana spinors below will
     have to flip signs!
   \end{dubious} *)

    let print_fermion_current2 coeff f c wf1 wf2 fusion =
      let c = format_coupling_2 coeff c in
      let c1 = fastener c 1 ()
      and c2 = fastener c 2 () in
      match fusion with
      | F13 -> printf "%s_ff(%s,%s,%s,%s)" f c1 c2 wf1 wf2
      | F31 -> printf "%s_ff(%s,%s,%s,%s)" f c1 c2 wf2 wf1
      | F23 -> printf "f_%sf(%s,%s,%s,%s)" f c1 c2 wf1 wf2
      | F32 -> printf "f_%sf(%s,%s,%s,%s)" f c1 c2 wf2 wf1
      | F12 -> printf "f_f%s(%s,%s,%s,%s)" f c1 c2 wf1 wf2
      | F21 -> printf "f_f%s(%s,%s,%s,%s)" f c1 c2 wf2 wf1

    let print_fermion_current_mom_v1 coeff f c wf1 wf2 p1 p2 p12 fusion =
      let c = format_coupling coeff c in
      let c1 = fastener c 1 and
          c2 = fastener c 2 in
      match fusion with
      | F13 -> printf "%s_ff(%s,%s,%s,%s)" f (c1 ~p:p12 ()) (c2 ~p:p12 ()) wf1 wf2
      | F31 -> printf "%s_ff(%s,%s,%s,%s)" f (c1 ~p:p12 ()) (c2 ~p:p12 ()) wf2 wf1
      | F23 -> printf "f_%sf(%s,%s,%s,%s)" f (c1 ~p:p1 ()) (c2 ~p:p1 ()) wf1 wf2
      | F32 -> printf "f_%sf(%s,%s,%s,%s)" f (c1 ~p:p2 ()) (c2 ~p:p2 ()) wf2 wf1
      | F12 -> printf "f_f%s(%s,%s,%s,%s)" f (c1 ~p:p2 ()) (c2 ~p:p2 ()) wf1 wf2
      | F21 -> printf "f_f%s(%s,%s,%s,%s)" f (c1 ~p:p1 ()) (c2 ~p:p1 ()) wf2 wf1

    let print_fermion_current_mom_v2 coeff f c wf1 wf2 p1 p2 p12 fusion =
      let c = format_coupling coeff c in
      let c1 = fastener c 1 and
          c2 = fastener c 2 in
      match fusion with
      | F13 -> printf "%s_ff(%s,%s,@,%s,%s,%s)" f (c1 ~p:p12 ()) (c2 ~p:p12 ()) wf1 wf2 p12
      | F31 -> printf "%s_ff(%s,%s,@,%s,%s,%s)" f (c1 ~p:p12 ()) (c2 ~p:p12 ()) wf2 wf1 p12
      | F23 -> printf "f_%sf(%s,%s,@,%s,%s,%s)" f (c1 ~p:p1 ()) (c2 ~p:p1 ()) wf1 wf2 p1
      | F32 -> printf "f_%sf(%s,%s,@,%s,%s,%s)" f (c1 ~p:p2 ()) (c2 ~p:p2 ()) wf2 wf1 p2
      | F12 -> printf "f_f%s(%s,%s,@,%s,%s,%s)" f (c1 ~p:p2 ()) (c2 ~p:p2 ()) wf1 wf2 p2
      | F21 -> printf "f_f%s(%s,%s,@,%s,%s,%s)" f (c1 ~p:p1 ()) (c2 ~p:p1 ()) wf2 wf1 p1

    let print_fermion_current_mom_ff coeff f c wf1 wf2 p1 p2 p12 fusion =
      let c = format_coupling coeff c in
      let c1 = fastener c 1 and
          c2 = fastener c 2 in
      match fusion with
      | F13 -> printf "%s_ff(%s,%s,%s,%s)" f (c1 ~p:p1 ~q:p2 ()) (c2 ~p:p1 ~q:p2 ()) wf1 wf2
      | F31 -> printf "%s_ff(%s,%s,%s,%s)" f (c1 ~p:p1 ~q:p2 ()) (c2 ~p:p1 ~q:p2 ()) wf2 wf1
      | F23 -> printf "f_%sf(%s,%s,%s,%s)" f (c1 ~p:p12 ~q:p2 ()) (c2 ~p:p12 ~q:p2 ()) wf1 wf2
      | F32 -> printf "f_%sf(%s,%s,%s,%s)" f (c1 ~p:p12 ~q:p1 ()) (c2 ~p:p12 ~q:p1 ()) wf2 wf1
      | F12 -> printf "f_f%s(%s,%s,%s,%s)" f (c1 ~p:p12 ~q:p1 ()) (c2 ~p:p12 ~q:p1 ()) wf1 wf2
      | F21 -> printf "f_f%s(%s,%s,%s,%s)" f (c1 ~p:p12 ~q:p2 ()) (c2 ~p:p12 ~q:p2 ()) wf2 wf1

    let print_current = function
      | coeff, Psibar, VA, Psi -> print_fermion_current2 coeff "va"
      | coeff, Psibar, VA2, Psi -> print_fermion_current coeff "va2"
      | coeff, Psibar, VA3, Psi -> print_fermion_current coeff "va3"
      | coeff, Psibar, V, Psi -> print_fermion_current coeff "v"
      | coeff, Psibar, A, Psi -> print_fermion_current coeff "a"
      | coeff, Psibar, VL, Psi -> print_fermion_current coeff "vl"
      | coeff, Psibar, VR, Psi -> print_fermion_current coeff "vr"
      | coeff, Psibar, VLR, Psi -> print_fermion_current2 coeff "vlr"
      | coeff, Psibar, SP, Psi -> print_fermion_current2 coeff "sp"
      | coeff, Psibar, S, Psi -> print_fermion_current coeff "s"
      | coeff, Psibar, P, Psi -> print_fermion_current coeff "p"
      | coeff, Psibar, SL, Psi -> print_fermion_current coeff "sl"
      | coeff, Psibar, SR, Psi -> print_fermion_current coeff "sr"
      | coeff, Psibar, SLR, Psi -> print_fermion_current2 coeff "slr"
      | _, Psibar, _, Psi -> invalid_arg
            "Targets.Fortran_Fermions: no superpotential here"
      | _, Chibar, _, _ | _, _, _, Chi -> invalid_arg
            "Targets.Fortran_Fermions: Majorana spinors not handled"
      | _, Gravbar, _, _ | _, _, _, Grav -> invalid_arg
            "Targets.Fortran_Fermions: Gravitinos not handled"

    let print_current_mom = function
      | coeff, Psibar, VLRM, Psi -> print_fermion_current_mom_v1 coeff "vlr"
      | coeff, Psibar, VAM, Psi -> print_fermion_current_mom_ff coeff "va"
      | coeff, Psibar, VA3M, Psi -> print_fermion_current_mom_ff coeff "va3"
      | coeff, Psibar, SPM, Psi -> print_fermion_current_mom_v1 coeff "sp"
      | coeff, Psibar, TVA, Psi -> print_fermion_current_mom_v1 coeff "tva"
      | coeff, Psibar, TVAM, Psi -> print_fermion_current_mom_v2 coeff "tvam"
      | coeff, Psibar, TLR, Psi -> print_fermion_current_mom_v1 coeff "tlr"
      | coeff, Psibar, TLRM, Psi -> print_fermion_current_mom_v2 coeff "tlrm"
      | coeff, Psibar, TRL, Psi -> print_fermion_current_mom_v1 coeff "trl"
      | coeff, Psibar, TRLM, Psi -> print_fermion_current_mom_v2 coeff "trlm"
      | _, Psibar, _, Psi -> invalid_arg
            "Targets.Fortran_Fermions: only sigma tensor coupling here"
      | _, Chibar, _, _ | _, _, _, Chi -> invalid_arg
            "Targets.Fortran_Fermions: Majorana spinors not handled"
      | _, Gravbar, _, _ | _, _, _, Grav -> invalid_arg
            "Targets.Fortran_Fermions: Gravitinos not handled"

    let print_current_p = function
      | _, _, _, _ -> invalid_arg
            "Targets.Fortran_Fermions: No clashing arrows here"

    let print_current_b = function
      | _, _, _, _ -> invalid_arg
            "Targets.Fortran_Fermions: No clashing arrows here"

    let print_current_g = function
      | _, _, _, _ -> invalid_arg
            "Targets.Fortran_Fermions: No gravitinos here"

    let print_current_g4 = function
      | _, _, _, _ -> invalid_arg
            "Targets.Fortran_Fermions: No gravitinos here"

    let reverse_braket vintage bra ket =
      match bra with
      | Spinor -> true
      | _ -> false

  end

(* \thocwmodulesubsection{Majorana Fermions} *)

(* \begin{JR}
   For this function we need a different approach due to our aim of
   implementing the fermion vertices with the right line as ingoing (in a
   calculational sense) and the left line in a fusion as outgoing. In
   defining all external lines and the fermionic wavefunctions built out of
   them as ingoing we have to invert the left lines to make them outgoing.
   This happens by multiplying them with the inverse charge conjugation
   matrix in an appropriate representation and then transposing it. We must
   distinguish whether the direction of calculation and the physical direction
   of the fermion number flow are parallel or antiparallel. In the first case
   we can use the "normal" Feynman rules for Dirac particles, while in the
   second, according to the paper of Denner et al., we have to reverse the
   sign of the vector and antisymmetric bilinears of the Dirac spinors, cf.
   the [Coupling] module.

   Note the subtlety for the left- and righthanded couplings: Only the vector
   part of these couplings changes in the appropriate cases its sign,
   changing the chirality to the negative of the opposite.
   \end{JR} *)

module Fortran_Majorana_Fermions (Names : Target_Fortran_Names.T) : Fermions =
  struct

    open Coupling
    open Format

    let format_coupling coeff c =
      match coeff with
      | 1 -> c
      | -1 -> "(-" ^ c ^")"
      | coeff -> string_of_int coeff ^ "*" ^ c

    let format_coupling_2 coeff c =
      match coeff with
      | 1 -> c
      | -1 -> "-" ^ c
      | coeff -> string_of_int coeff ^ "*" ^ c

(* \begin{dubious}
     JR's coupling constant HACK, necessitated by tho's bad design descition.
   \end{dubious} *)

    let fastener s i =
      try
        let offset = (String.index s '(') in
        if ((String.get s (String.length s - 1)) != ')') then
          failwith "fastener: wrong usage of parentheses"
        else
          let func_name = (String.sub s 0 offset) and
              tail =
                (String.sub s (succ offset) (String.length s - offset - 2)) in
          if (String.contains func_name ')') ||
             (String.contains tail '(') ||
             (String.contains tail ')') then
            failwith "fastener: wrong usage of parentheses"
          else
            func_name ^ "(" ^ string_of_int i ^ "," ^ tail ^ ")"
      with
      | Not_found ->
          if (String.contains s ')') then
            failwith "fastener: wrong usage of parentheses"
          else
            s ^ "(" ^ string_of_int i ^ ")"

    let print_fermion_current coeff f c wf1 wf2 fusion =
      let c = format_coupling coeff c in
      match fusion with
      | F13 | F31 -> printf "%s_ff(%s,%s,%s)" f c wf1 wf2
      | F23 | F21 -> printf "f_%sf(%s,%s,%s)" f c wf1 wf2
      | F32 | F12 -> printf "f_%sf(%s,%s,%s)" f c wf2 wf1

    let print_fermion_current2 coeff f c wf1 wf2 fusion =
      let c = format_coupling_2 coeff c in
      let c1 = fastener c 1 and
          c2 = fastener c 2 in
      match fusion with
      | F13 | F31 -> printf "%s_ff(%s,%s,%s,%s)" f c1 c2 wf1 wf2
      | F23 | F21 -> printf "f_%sf(%s,%s,%s,%s)" f c1 c2 wf1 wf2
      | F32 | F12 -> printf "f_%sf(%s,%s,%s,%s)" f c1 c2 wf2 wf1

    let print_fermion_current_mom_v1 coeff f c wf1 wf2 p1 p2 p12 fusion =
      let c = format_coupling coeff c in
      let c1 = fastener c 1 and
          c2 = fastener c 2 in
      match fusion with
      | F13 -> printf "%s_ff(%s,%s,%s,%s)" f c1 c2 wf1 wf2
      | F31 -> printf "%s_ff(-(%s),%s,%s,%s)" f c1 c2 wf1 wf2
      | F23 -> printf "f_%sf(%s,%s,%s,%s)" f c1 c2 wf1 wf2
      | F32 -> printf "f_%sf(%s,%s,%s,%s)" f c1 c2 wf2 wf1
      | F12 -> printf "f_f%s(-(%s),%s,%s,%s)" f c1 c2 wf2 wf1
      | F21 -> printf "f_f%s(-(%s),%s,%s,%s)" f c1 c2 wf1 wf2

    let print_fermion_current_mom_v1_chiral coeff f c wf1 wf2 p1 p2 p12 fusion =
      let c = format_coupling coeff c in
      let c1 = fastener c 1 and
          c2 = fastener c 2 in
      match fusion with
      | F13 -> printf "%s_ff(%s,%s,%s,%s)" f c1 c2 wf1 wf2
      | F31 -> printf "%s_ff(-(%s),-(%s),%s,%s)" f c2 c1 wf1 wf2
      | F23 -> printf "f_%sf(%s,%s,%s,%s)" f c1 c2 wf1 wf2
      | F32 -> printf "f_%sf(%s,%s,%s,%s)" f c1 c2 wf2 wf1
      | F12 -> printf "f_f%s(-(%s),-(%s),%s,%s)" f c2 c1 wf2 wf1
      | F21 -> printf "f_f%s(-(%s),-(%s),%s,%s)" f c2 c1 wf2 wf1

    let print_fermion_current_mom_v2 coeff f c wf1 wf2 p1 p2 p12 fusion =
      let c = format_coupling coeff c in
      let c1 = fastener c 1 and
          c2 = fastener c 2 in
      match fusion with
      | F13 -> printf "%s_ff(%s,%s,%s,%s,%s)" f c1 c2 wf1 wf2 p12
      | F31 -> printf "%s_ff(-(%s),%s,%s,%s,%s)" f c1 c2 wf1 wf2 p12
      | F23 -> printf "f_%sf(%s,%s,%s,%s,%s)" f c1 c2 wf1 wf2 p1
      | F32 -> printf "f_%sf(%s,%s,%s,%s,%s)" f c1 c2 wf2 wf1 p2
      | F12 -> printf "f_f%s(-(%s),%s,%s,%s,%s)" f c1 c2 wf2 wf1 p2
      | F21 -> printf "f_f%s(-(%s),%s,%s,%s,%s)" f c1 c2 wf1 wf2 p1

    let print_fermion_current_mom_v2_chiral coeff f c wf1 wf2 p1 p2 p12 fusion =
      let c = format_coupling coeff c in
      let c1 = fastener c 1 and
          c2 = fastener c 2 in
      match fusion with
      | F13 -> printf "%s_ff(%s,%s,%s,%s,%s)" f c1 c2 wf1 wf2 p12
      | F31 -> printf "%s_ff(-(%s),-(%s),%s,%s,%s)" f c2 c1 wf2 wf1 p12
      | F23 -> printf "f_%sf(%s,%s,%s,%s,%s)" f c1 c2 wf1 wf2 p1
      | F32 -> printf "f_%sf(%s,%s,%s,%s,%s)" f c1 c2 wf2 wf1 p2
      | F12 -> printf "f_f%s(-(%s),-(%s),%s,%s,%s)" f c2 c1 wf1 wf2 p2
      | F21 -> printf "f_f%s(-(%s),-(%s),%s,%s,%s)" f c2 c1 wf2 wf1 p1

    let print_fermion_current_vector coeff f c wf1 wf2 fusion =
      let c = format_coupling coeff c in
      match fusion with
      | F13 -> printf "%s_ff(%s,%s,%s)" f c wf1 wf2
      | F31 -> printf "%s_ff(-%s,%s,%s)" f c wf1 wf2
      | F23 -> printf "f_%sf(%s,%s,%s)" f c wf1 wf2
      | F32 -> printf "f_%sf(%s,%s,%s)" f c wf2 wf1
      | F12 -> printf "f_%sf(-%s,%s,%s)" f c wf2 wf1
      | F21 -> printf "f_%sf(-%s,%s,%s)" f c wf1 wf2

    let print_fermion_current2_vector coeff f c wf1 wf2 fusion =
      let c  = format_coupling_2 coeff c in
      let c1 = fastener c 1 and
          c2 = fastener c 2 in
      match fusion with
      | F13 -> printf "%s_ff(%s,%s,%s,%s)" f c1 c2 wf1 wf2
      | F31 -> printf "%s_ff(-(%s),%s,%s,%s)" f c1 c2 wf1 wf2
      | F23 -> printf "f_%sf(%s,%s,%s,%s)" f c1 c2 wf1 wf2
      | F32 -> printf "f_%sf(%s,%s,%s,%s)" f c1 c2 wf2 wf1
      | F12 -> printf "f_%sf(-(%s),%s,%s,%s)" f c1 c2 wf2 wf1
      | F21 -> printf "f_%sf(-(%s),%s,%s,%s)" f c1 c2 wf1 wf2

    let print_fermion_current_chiral coeff f1 f2 c wf1 wf2 fusion =
      let c = format_coupling coeff c in
      match fusion with
      | F13 -> printf "%s_ff(%s,%s,%s)" f1 c wf1 wf2
      | F31 -> printf "%s_ff(-%s,%s,%s)" f2 c wf1 wf2
      | F23 -> printf "f_%sf(%s,%s,%s)" f1 c wf1 wf2
      | F32 -> printf "f_%sf(%s,%s,%s)" f1 c wf2 wf1
      | F12 -> printf "f_%sf(-%s,%s,%s)" f2 c wf2 wf1
      | F21 -> printf "f_%sf(-%s,%s,%s)" f2 c wf1 wf2

    let print_fermion_current2_chiral coeff f c wf1 wf2 fusion =
      let c = format_coupling_2 coeff c in
      let c1 = fastener c 1 and
          c2 = fastener c 2 in
      match fusion with
      | F13 -> printf "%s_ff(%s,%s,%s,%s)" f c1 c2 wf1 wf2
      | F31 -> printf "%s_ff(-(%s),-(%s),%s,%s)" f c2 c1 wf1 wf2
      | F23 -> printf "f_%sf(%s,%s,%s,%s)" f c1 c2 wf1 wf2
      | F32 -> printf "f_%sf(%s,%s,%s,%s)" f c1 c2 wf2 wf1
      | F12 -> printf "f_%sf(-(%s),-(%s),%s,%s)" f c2 c1 wf2 wf1
      | F21 -> printf "f_%sf(-(%s),-(%s),%s,%s)" f c2 c1 wf1 wf2

    let print_current = function
      | coeff, _, VA, _ -> print_fermion_current2_vector coeff "va"
      | coeff, _, V, _ -> print_fermion_current_vector coeff "v"
      | coeff, _, A, _ -> print_fermion_current coeff "a"
      | coeff, _, VL, _ -> print_fermion_current_chiral coeff "vl" "vr"
      | coeff, _, VR, _ -> print_fermion_current_chiral coeff "vr" "vl"
      | coeff, _, VLR, _ -> print_fermion_current2_chiral coeff "vlr"
      | coeff, _, SP, _ -> print_fermion_current2 coeff "sp"
      | coeff, _, S, _ -> print_fermion_current coeff "s"
      | coeff, _, P, _ -> print_fermion_current coeff "p"
      | coeff, _, SL, _ -> print_fermion_current coeff "sl"
      | coeff, _, SR, _ -> print_fermion_current coeff "sr"
      | coeff, _, SLR, _ -> print_fermion_current2 coeff "slr"
      | coeff, _, POT, _ -> print_fermion_current_vector coeff "pot"
      | _, _, _, _ -> invalid_arg
            "Targets.Fortran_Majorana_Fermions: Not needed in the models"

    let print_current_p = function
      | coeff, Psi, SL, Psi -> print_fermion_current coeff "sl"
      | coeff, Psi, SR, Psi -> print_fermion_current coeff "sr"
      | coeff, Psi, SLR, Psi -> print_fermion_current2 coeff "slr"
      | _, _, _, _ -> invalid_arg
            "Targets.Fortran_Majorana_Fermions: Not needed in the used models"

    let print_current_b = function
      | coeff, Psibar, SL, Psibar -> print_fermion_current coeff "sl"
      | coeff, Psibar, SR, Psibar -> print_fermion_current coeff "sr"
      | coeff, Psibar, SLR, Psibar -> print_fermion_current2 coeff "slr"
      | _, _, _, _  -> invalid_arg
            "Targets.Fortran_Majorana_Fermions: Not needed in the used models"

(* This function is for the vertices with three particles including two
   fermions but also a momentum, therefore with a dimensionful coupling
   constant, e.g. the gravitino vertices. One has to dinstinguish between
   the two kinds of canonical orders in the string of gamma matrices. Of
   course, the direction of the string of gamma matrices is reversed if one
   goes from the [Gravbar, _, Psi] to the [Psibar, _, Grav] vertices, and
   the same is true for the couplings of the gravitino to the Majorana
   fermions. For more details see the tables in the [coupling]
   implementation. *)

(* We now have to fix the directions of the momenta. For making the compiler
   happy and because we don't want to make constructions of infinite
   complexity we list the momentum including vertices without gravitinos
   here; the pattern matching says that's better. Perhaps we have to find a
   better name now.

   For the cases of $MOM$, $MOM5$, $MOML$ and $MOMR$ which arise only in
   BRST transformations we take the mass as a coupling constant. For
   $VMOM$ we don't need a mass either. These vertices are like kinetic terms
   and so need not have a coupling constant. By this we avoid a strange and
   awful construction with a new variable. But be careful with a
   generalization if you want to use these vertices for other purposes.
*)

    let format_coupling_mom coeff c =
      match coeff with
      | 1 -> c
      | -1 -> "(-" ^ c ^")"
      | coeff -> string_of_int coeff ^ "*" ^ c

    let commute_proj f =
      match f with
      | "moml" -> "lmom"
      | "momr" -> "rmom"
      | "lmom" -> "moml"
      | "rmom" -> "momr"
      | "svl"  -> "svr"
      | "svr"  -> "svl"
      | "sl" -> "sr"
      | "sr" -> "sl"
      | "s" -> "s"
      | "p" -> "p"
      | _ -> invalid_arg "Targets:Fortran_Majorana_Fermions: wrong case"

    let print_fermion_current_mom coeff f c wf1 wf2 p1 p2 p12 fusion =
      let c = format_coupling_mom coeff c in
      let c1 = fastener c 1 and
          c2 = fastener c 2 in
      match fusion with
      | F13 -> printf "%s_ff(%s,%s,%s,%s,%s)" f c1 c2 wf1 wf2 p12
      | F31 -> printf "%s_ff(%s,%s,%s,%s,%s)" f c1 c2 wf1 wf2 p12
      | F23 -> printf "f_%sf(%s,%s,%s,%s,%s)" f c1 c2 wf1 wf2 p1
      | F32 -> printf "f_%sf(%s,%s,%s,%s,%s)" f c1 c2 wf2 wf1 p2
      | F12 -> printf "f_%sf(%s,%s,%s,%s,%s)" f c1 c2 wf2 wf1 p2
      | F21 -> printf "f_%sf(%s,%s,%s,%s,%s)" f c1 c2 wf1 wf2 p1

    (*i unused value
    let print_fermion_current_mom_vector coeff f c wf1 wf2 p1 p2 p12 fusion =
      let c = format_coupling_mom coeff c in
      let c1 = fastener c 1 and
          c2 = fastener c 2 in
      match fusion with
      | F13 -> printf "%s_ff(%s,%s,%s,%s,%s)" f c1 c2 wf1 wf2 p12
      | F31 -> printf "%s_ff(-%s,%s,%s,%s,%s)" f c1 c2 wf1 wf2 p12
      | F23 -> printf "f_%sf(%s,%s,%s,%s,%s)" f c1 c2 wf1 wf2 p1
      | F32 -> printf "f_%sf(%s,%s,%s,%s,%s)" f c1 c2 wf2 wf1 p2
      | F12 -> printf "f_%sf(-%s,%s,%s,%s,%s)" f c1 c2 wf2 wf1 p2
      | F21 -> printf "f_%sf(-%s,%s,%s,%s,%s)" f c1 c2 wf1 wf2 p1
      i*)

    let print_fermion_current_mom_sign coeff f c wf1 wf2 p1 p2 p12 fusion =
      let c = format_coupling_mom coeff c in
      let c1 = fastener c 1 and
          c2 = fastener c 2 in
      match fusion with
      | F13 -> printf "%s_ff(%s,%s,%s,%s,%s)" f c1 c2 wf1 wf2 p12
      | F31 -> printf "%s_ff(%s,%s,%s,%s,-(%s))" f c1 c2 wf1 wf2 p12
      | F23 -> printf "f_%sf(%s,%s,%s,%s,%s)" f c1 c2 wf1 wf2 p1
      | F32 -> printf "f_%sf(%s,%s,%s,%s,%s)" f c1 c2 wf2 wf1 p2
      | F12 -> printf "f_%sf(%s,%s,%s,%s,-(%s))" f c1 c2 wf2 wf1 p2
      | F21 -> printf "f_%sf(%s,%s,%s,%s,-(%s))" f c1 c2 wf1 wf2 p1

    let print_fermion_current_mom_sign_1 coeff f c wf1 wf2 p1 p2 p12 fusion =
      let c = format_coupling coeff c in
      match fusion with
      | F13 -> printf "%s_ff(%s,%s,%s,%s)" f c wf1 wf2 p12
      | F31 -> printf "%s_ff(%s,%s,%s,-(%s))" f c wf1 wf2 p12
      | F23 -> printf "f_%sf(%s,%s,%s,%s)" f c wf1 wf2 p1
      | F32 -> printf "f_%sf(%s,%s,%s,%s)" f c wf2 wf1 p2
      | F12 -> printf "f_%sf(%s,%s,%s,-(%s))" f c wf2 wf1 p2
      | F21 -> printf "f_%sf(%s,%s,%s,-(%s))" f c wf1 wf2 p1

    let print_fermion_current_mom_chiral coeff f c wf1 wf2 p1 p2 p12 fusion =
      let c  = format_coupling_mom coeff c and
          cf = commute_proj f in
      let c1 = fastener c 1 and
          c2 = fastener c 2 in
      match fusion with
      | F13 -> printf "%s_ff(%s,%s,%s,%s,%s)" f c1 c2 wf1 wf2 p12
      | F31 -> printf "%s_ff(%s,%s,%s, %s,-(%s))" cf c1 c2 wf1 wf2 p12
      | F23 -> printf "f_%sf(%s,%s,%s,%s,%s)" f c1 c2 wf1 wf2 p1
      | F32 -> printf "f_%sf(%s,%s,%s,%s,%s)" f c1 c2 wf2 wf1 p2
      | F12 -> printf "f_%sf(%s,%s,%s,%s,-(%s))" cf c1 c2 wf2 wf1 p2
      | F21 -> printf "f_%sf(%s,%s,%s,%s,-(%s))" cf c1 c2 wf1 wf2 p1

    let print_fermion_g_current coeff f c wf1 wf2 p1 p2 p12 fusion =
      let c = format_coupling coeff c in
      match fusion with
      | F13 -> printf "%s_grf(%s,%s,%s,%s)" f c wf1 wf2 p12
      | F31 -> printf "%s_fgr(%s,%s,%s,%s)" f c wf1 wf2 p12
      | F23 -> printf "gr_%sf(%s,%s,%s,%s)" f c wf1 wf2 p1
      | F32 -> printf "gr_%sf(%s,%s,%s,%s)" f c wf2 wf1 p2
      | F12 -> printf "f_%sgr(%s,%s,%s,%s)" f c wf2 wf1 p2
      | F21 -> printf "f_%sgr(%s,%s,%s,%s)" f c wf1 wf2 p1

    let print_fermion_g_2_current coeff f c wf1 wf2 p1 p2 p12 fusion =
      let c = format_coupling coeff c in
      match fusion with
      | F13 -> printf "%s_grf(%s(1),%s(2),%s,%s,%s)" f c c wf1 wf2 p12
      | F31 -> printf "%s_fgr(%s(1),%s(2),%s,%s,%s)" f c c wf1 wf2 p12
      | F23 -> printf "gr_%sf(%s(1),%s(2),%s,%s,%s)" f c c wf1 wf2 p1
      | F32 -> printf "gr_%sf(%s(1),%s(2),%s,%s,%s)" f c c wf2 wf1 p2
      | F12 -> printf "f_%sgr(%s(1),%s(2),%s,%s,%s)" f c c wf2 wf1 p2
      | F21 -> printf "f_%sgr(%s(1),%s(2),%s,%s,%s)" f c c wf1 wf2 p1

    let print_fermion_g_current_rev coeff f c wf1 wf2 p1 p2 p12 fusion =
      let c = format_coupling coeff c in
      match fusion with
      | F13 -> printf "%s_fgr(%s,%s,%s,%s)" f c wf1 wf2 p12
      | F31 -> printf "%s_grf(%s,%s,%s,%s)" f c wf1 wf2 p12
      | F23 -> printf "f_%sgr(%s,%s,%s,%s)" f c wf1 wf2 p1
      | F32 -> printf "f_%sgr(%s,%s,%s,%s)" f c wf2 wf1 p2
      | F12 -> printf "gr_%sf(%s,%s,%s,%s)" f c wf2 wf1 p2
      | F21 -> printf "gr_%sf(%s,%s,%s,%s)" f c wf1 wf2 p1

    let print_fermion_g_2_current_rev coeff f c wf1 wf2 p1 p2 p12 fusion =
      let c = format_coupling coeff c in
      match fusion with
      | F13 -> printf "%s_fgr(%s(1),%s(2),%s,%s,%s)" f c c wf1 wf2 p12
      | F31 -> printf "%s_grf(%s(1),%s(2),%s,%s,%s)" f c c wf1 wf2 p12
      | F23 -> printf "f_%sgr(%s(1),%s(2),%s,%s,%s)" f c c wf1 wf2 p1
      | F32 -> printf "f_%sgr(%s(1),%s(2),%s,%s,%s)" f c c wf2 wf1 p2
      | F12 -> printf "gr_%sf(%s(1),%s(2),%s,%s,%s)" f c c wf2 wf1 p2
      | F21 -> printf "gr_%sf(%s(1),%s(2),%s,%s,%s)" f c c wf1 wf2 p1

    let print_fermion_g_current_vector coeff f c wf1 wf2 _ _ _ fusion =
      let c = format_coupling coeff c in
      match fusion with
      | F13 -> printf "%s_grf(%s,%s,%s)" f c wf1 wf2
      | F31 -> printf "%s_fgr(-%s,%s,%s)" f c wf1 wf2
      | F23 -> printf "gr_%sf(%s,%s,%s)" f c wf1 wf2
      | F32 -> printf "gr_%sf(%s,%s,%s)" f c wf2 wf1
      | F12 -> printf "f_%sgr(-%s,%s,%s)" f c wf2 wf1
      | F21 -> printf "f_%sgr(-%s,%s,%s)" f c wf1 wf2

    let print_fermion_g_current_vector_rev coeff f c wf1 wf2 _ _ _ fusion =
      let c = format_coupling coeff c in
      match fusion with
      | F13 -> printf "%s_fgr(%s,%s,%s)" f c wf1 wf2
      | F31 -> printf "%s_grf(-%s,%s,%s)" f c wf1 wf2
      | F23 -> printf "f_%sgr(%s,%s,%s)" f c wf1 wf2
      | F32 -> printf "f_%sgr(%s,%s,%s)" f c wf2 wf1
      | F12 -> printf "gr_%sf(-%s,%s,%s)" f c wf2 wf1
      | F21 -> printf "gr_%sf(-%s,%s,%s)" f c wf1 wf2

    let print_current_g = function
      | coeff, _, MOM, _ -> print_fermion_current_mom_sign coeff "mom"
      | coeff, _, MOM5, _ -> print_fermion_current_mom coeff "mom5"
      | coeff, _, MOML, _ -> print_fermion_current_mom_chiral coeff "moml"
      | coeff, _, MOMR, _ -> print_fermion_current_mom_chiral coeff "momr"
      | coeff, _, LMOM, _ -> print_fermion_current_mom_chiral coeff "lmom"
      | coeff, _, RMOM, _ -> print_fermion_current_mom_chiral coeff "rmom"
      | coeff, _, VMOM, _ -> print_fermion_current_mom_sign_1 coeff "vmom"
      | coeff, Gravbar, S, _ -> print_fermion_g_current coeff "s"
      | coeff, Gravbar, SL, _ -> print_fermion_g_current coeff "sl"
      | coeff, Gravbar, SR, _ -> print_fermion_g_current coeff "sr"
      | coeff, Gravbar, SLR, _ -> print_fermion_g_2_current coeff "slr"
      | coeff, Gravbar, P, _ -> print_fermion_g_current coeff "p"
      | coeff, Gravbar, V, _ -> print_fermion_g_current coeff "v"
      | coeff, Gravbar, VLR, _ -> print_fermion_g_2_current coeff "vlr"
      | coeff, Gravbar, POT, _ -> print_fermion_g_current_vector coeff "pot"
      | coeff, _, S, Grav -> print_fermion_g_current_rev coeff "s"
      | coeff, _, SL, Grav -> print_fermion_g_current_rev coeff "sl"
      | coeff, _, SR, Grav -> print_fermion_g_current_rev coeff "sr"
      | coeff, _, SLR, Grav -> print_fermion_g_2_current_rev coeff "slr"
      | coeff, _, P, Grav -> print_fermion_g_current_rev (-coeff) "p"
      | coeff, _, V, Grav -> print_fermion_g_current_rev coeff "v"
      | coeff, _, VLR, Grav -> print_fermion_g_2_current_rev coeff "vlr"
      | coeff, _, POT, Grav -> print_fermion_g_current_vector_rev coeff "pot"
      | _, _, _, _ -> invalid_arg
          "Targets.Fortran_Majorana_Fermions: not used in the models"

    let print_current_mom = function
      | coeff, _, TVA, _ -> print_fermion_current_mom_v1 coeff "tva"
      | coeff, _, TVAM, _ -> print_fermion_current_mom_v2 coeff "tvam"
      | coeff, _, TLR, _ -> print_fermion_current_mom_v1_chiral coeff "tlr"
      | coeff, _, TLRM, _ -> print_fermion_current_mom_v2_chiral coeff "tlrm"
      | _, _, _, _ -> invalid_arg
            "Targets.Fortran_Majorana_Fermions: Not needed in the models"

(* We need support for dimension-5 vertices with two fermions and two
   bosons, appearing in theories of supergravity and also together with in
   insertions of the supersymmetric current. There is a canonical order
   [fermionbar], [boson_1], [boson_2], [fermion], so what one has to do is a
   mapping from the fusions [F123] etc. to the order of the three wave
   functions [wf1], [wf2] and [wf3]. *)

(* The function [d_p] (for distinct the particle) distinguishes which particle
   (scalar or vector) must be fused to in the special functions. *)

    let d_p = function
      | 1, ("sv"|"pv"|"svl"|"svr"|"slrv") -> "1"
      | 1, _ -> ""
      | 2, ("sv"|"pv"|"svl"|"svr"|"slrv") -> "2"
      | 2, _ -> ""
      | _, _ -> invalid_arg "Targets.Fortran_Majorana_Fermions: not used"

    let wf_of_f wf1 wf2 wf3 f =
      match f with
      | (F123|F423) -> [wf2; wf3; wf1]
      | (F213|F243|F143|F142|F413|F412) -> [wf1; wf3; wf2]
      | (F132|F432) -> [wf3; wf2; wf1]
      | (F231|F234|F134|F124|F431|F421) -> [wf1; wf2; wf3]
      | (F312|F342) -> [wf3; wf1; wf2]
      | (F321|F324|F314|F214|F341|F241) -> [wf2; wf1; wf3]

    let print_fermion_g4_brs_vector_current coeff f c wf1 wf2 wf3 fusion =
      let cf = commute_proj f and
          cp = format_coupling coeff c and
          cm = if f = "pv" then
            format_coupling coeff c
          else
            format_coupling (-coeff) c
      and
          d1 = d_p (1,f) and
          d2 = d_p (2,f) and
          f1 = (List.nth (wf_of_f wf1 wf2 wf3 fusion) 0) and
          f2 = (List.nth (wf_of_f wf1 wf2 wf3 fusion) 1) and
          f3 = (List.nth (wf_of_f wf1 wf2 wf3 fusion) 2) in
      match fusion with
      | (F123|F213|F132|F231|F312|F321) ->
          printf "f_%sf(%s,%s,%s,%s)" cf cm f1 f2 f3
      | (F423|F243|F432|F234|F342|F324) ->
          printf "f_%sf(%s,%s,%s,%s)" f cp f1 f2 f3
      | (F134|F143|F314) -> printf "%s%s_ff(%s,%s,%s,%s)" f d1 cp f1 f2 f3
      | (F124|F142|F214) -> printf "%s%s_ff(%s,%s,%s,%s)" f d2 cp f1 f2 f3
      | (F413|F431|F341) -> printf "%s%s_ff(%s,%s,%s,%s)" cf d1 cm f1 f2 f3
      | (F241|F412|F421) -> printf "%s%s_ff(%s,%s,%s,%s)" cf d2 cm f1 f2 f3

    let print_fermion_g4_svlr_current coeff _ c wf1 wf2 wf3 fusion =
      let c = format_coupling_2 coeff c and
          f1 = (List.nth (wf_of_f wf1 wf2 wf3 fusion) 0) and
          f2 = (List.nth (wf_of_f wf1 wf2 wf3 fusion) 1) and
          f3 = (List.nth (wf_of_f wf1 wf2 wf3 fusion) 2) in
      let c1 = fastener c 1 and
          c2 = fastener c 2 in
      match fusion with
      | (F123|F213|F132|F231|F312|F321) ->
          printf "f_svlrf(-(%s),-(%s),%s,%s,%s)" c2 c1 f1 f2 f3
      | (F423|F243|F432|F234|F342|F324) ->
          printf "f_svlrf(%s,%s,%s,%s,%s)" c1 c2 f1 f2 f3
      | (F134|F143|F314) ->
          printf "svlr2_ff(%s,%s,%s,%s,%s)" c1 c2 f1 f2 f3
      | (F124|F142|F214) ->
          printf "svlr1_ff(%s,%s,%s,%s,%s)" c1 c2 f1 f2 f3
      | (F413|F431|F341) ->
          printf "svlr2_ff(-(%s),-(%s),%s,%s,%s)" c2 c1 f1 f2 f3
      | (F241|F412|F421) ->
          printf "svlr1_ff(-(%s),-(%s),%s,%s,%s)" c2 c1 f1 f2 f3

    let print_fermion_s2_current coeff f c wf1 wf2 wf3 fusion =
      let cp = format_coupling coeff c and
          cm = if f = "p" then
            format_coupling (-coeff) c
          else
            format_coupling coeff c
      and
          cf = commute_proj f and
          f1 = (List.nth (wf_of_f wf1 wf2 wf3 fusion) 0) and
          f2 = (List.nth (wf_of_f wf1 wf2 wf3 fusion) 1) and
          f3 = (List.nth (wf_of_f wf1 wf2 wf3 fusion) 2) in
      match fusion with
      | (F123|F213|F132|F231|F312|F321) ->
          printf "%s * f_%sf(%s,%s,%s)" f1 cf cm f2 f3
      | (F423|F243|F432|F234|F342|F324) ->
          printf "%s * f_%sf(%s,%s,%s)" f1 f cp f2 f3
      | (F134|F143|F314) ->
          printf "%s * %s_ff(%s,%s,%s)" f2 f cp f1 f3
      | (F124|F142|F214) ->
          printf "%s * %s_ff(%s,%s,%s)" f2 f cp f1 f3
      | (F413|F431|F341) ->
          printf "%s * %s_ff(%s,%s,%s)" f2 cf cm f1 f3
      | (F241|F412|F421) ->
          printf "%s * %s_ff(%s,%s,%s)" f2 cf cm f1 f3

    let print_fermion_s2p_current coeff f c wf1 wf2 wf3 fusion =
      let c = format_coupling_2 coeff c and
          f1 = (List.nth (wf_of_f wf1 wf2 wf3 fusion) 0) and
          f2 = (List.nth (wf_of_f wf1 wf2 wf3 fusion) 1) and
          f3 = (List.nth (wf_of_f wf1 wf2 wf3 fusion) 2) in
      let c1 = fastener c 1 and
          c2 = fastener c 2 in
      match fusion with
      | (F123|F213|F132|F231|F312|F321) ->
          printf "%s * f_%sf(%s,-(%s),%s,%s)" f1 f c1 c2 f2 f3
      | (F423|F243|F432|F234|F342|F324) ->
          printf "%s * f_%sf(%s,%s,%s,%s)" f1 f c1 c2 f2 f3
      | (F134|F143|F314) ->
          printf "%s * %s_ff(%s,%s,%s,%s)" f2 f c1 c2 f1 f3
      | (F124|F142|F214) ->
          printf "%s * %s_ff(%s,%s,%s,%s)" f2 f c1 c2 f1 f3
      | (F413|F431|F341) ->
          printf "%s * %s_ff(%s,-(%s),%s,%s)" f2 f c1 c2 f1 f3
      | (F241|F412|F421) ->
          printf "%s * %s_ff(%s,-(%s),%s,%s)" f2 f c1 c2 f1 f3

    let print_fermion_s2lr_current coeff f c wf1 wf2 wf3 fusion =
      let c = format_coupling_2 coeff c and
          f1 = (List.nth (wf_of_f wf1 wf2 wf3 fusion) 0) and
          f2 = (List.nth (wf_of_f wf1 wf2 wf3 fusion) 1) and
          f3 = (List.nth (wf_of_f wf1 wf2 wf3 fusion) 2) in
      let c1 = fastener c 1 and
          c2 = fastener c 2 in
      match fusion with
      | (F123|F213|F132|F231|F312|F321) ->
          printf "%s * f_%sf(%s,%s,%s,%s)" f1 f c2 c1 f2 f3
      | (F423|F243|F432|F234|F342|F324) ->
          printf "%s * f_%sf(%s,%s,%s,%s)" f1 f c1 c2 f2 f3
      | (F134|F143|F314) ->
          printf "%s * %s_ff(%s,%s,%s,%s)" f2 f c1 c2 f1 f3
      | (F124|F142|F214) ->
          printf "%s * %s_ff(%s,%s,%s,%s)" f2 f c1 c2 f1 f3
      | (F413|F431|F341) ->
          printf "%s * %s_ff(%s,%s,%s,%s)" f2 f c2 c1 f1 f3
      | (F241|F412|F421) ->
          printf "%s * %s_ff(%s,%s,%s,%s)" f2 f c2 c1 f1 f3

    let print_fermion_g4_current coeff f c wf1 wf2 wf3 fusion =
      let c = format_coupling coeff c and
          f1 = (List.nth (wf_of_f wf1 wf2 wf3 fusion) 0) and
          f2 = (List.nth (wf_of_f wf1 wf2 wf3 fusion) 1) and
          f3 = (List.nth (wf_of_f wf1 wf2 wf3 fusion) 2) in
      match fusion with
      | (F123|F213|F132|F231|F312|F321) ->
          printf "f_%sgr(-%s,%s,%s,%s)" f c f1 f2 f3
      | (F423|F243|F432|F234|F342|F324) ->
          printf "gr_%sf(%s,%s,%s,%s)" f c f1 f2 f3
      | (F134|F143|F314|F124|F142|F214) ->
          printf "%s_grf(%s,%s,%s,%s)" f c f1 f2 f3
      | (F413|F431|F341|F241|F412|F421) ->
          printf "%s_fgr(-%s,%s,%s,%s)" f c f1 f2 f3

    (*i unused value
    let print_fermion_2_g4_current coeff f c wf1 wf2 wf3 fusion =
      let f1 = (List.nth (wf_of_f wf1 wf2 wf3 fusion) 0) and
          f2 = (List.nth (wf_of_f wf1 wf2 wf3 fusion) 1) and
          f3 = (List.nth (wf_of_f wf1 wf2 wf3 fusion) 2) in
      let c = format_coupling_2 coeff c in
      let c1 = fastener c 1 and
          c2 = fastener c 2 in
      match fusion with
      | (F123|F213|F132|F231|F312|F321) ->
          printf "f_%sgr(-(%s),-(%s),%s,%s,%s)" f c2 c1 f1 f2 f3
      | (F423|F243|F432|F234|F342|F324) ->
          printf "gr_%sf(%s,%s,%s,%s,%s)" f c1 c2 f1 f2 f3
      | (F134|F143|F314|F124|F142|F214) ->
          printf "%s_grf(%s,%s,%s,%s,%s)" f c1 c2 f1 f2 f3
      | (F413|F431|F341|F241|F412|F421) ->
          printf "%s_fgr(-(%s),-(%s),%s,%s,%s)" f c2 c1 f1 f2 f3
    i*)

    let print_fermion_2_g4_current coeff f c wf1 wf2 wf3 fusion =
      let f1 = (List.nth (wf_of_f wf1 wf2 wf3 fusion) 0) and
          f2 = (List.nth (wf_of_f wf1 wf2 wf3 fusion) 1) and
          f3 = (List.nth (wf_of_f wf1 wf2 wf3 fusion) 2) in
      let c = format_coupling_2 coeff c in
      let c1 = fastener c 1 and
          c2 = fastener c 2 in
      match fusion with
      | (F123|F213|F132|F231|F312|F321) ->
          printf "f_%sgr(-(%s),-(%s),%s,%s,%s)" f c2 c1 f1 f2 f3
      | (F423|F243|F432|F234|F342|F324) ->
          printf "gr_%sf(%s,%s,%s,%s,%s)" f c1 c2 f1 f2 f3
      | (F134|F143|F314|F124|F142|F214) ->
          printf "%s_grf(%s,%s,%s,%s,%s)" f c1 c2 f1 f2 f3
      | (F413|F431|F341|F241|F412|F421) ->
          printf "%s_fgr(-(%s),-(%s),%s,%s,%s)" f c2 c1 f1 f2 f3


    let print_fermion_g4_current_rev coeff f c wf1 wf2 wf3 fusion =
      let c = format_coupling coeff c and
          f1 = (List.nth (wf_of_f wf1 wf2 wf3 fusion) 0) and
          f2 = (List.nth (wf_of_f wf1 wf2 wf3 fusion) 1) and
          f3 = (List.nth (wf_of_f wf1 wf2 wf3 fusion) 2) in
      match fusion with
      | (F123|F213|F132|F231|F312|F321) ->
          printf "f_%sgr(%s,%s,%s,%s)" f c f1 f2 f3
      | (F423|F243|F432|F234|F342|F324) ->
          printf "gr_%sf(-%s,%s,%s,%s)" f c f1 f2 f3
      | (F134|F143|F314|F124|F142|F214) ->
          printf "%s_grf(-%s,%s,%s,%s)" f c f1 f2 f3
      | (F413|F431|F341|F241|F412|F421) ->
          printf "%s_fgr(%s,%s,%s,%s)" f c f1 f2 f3

(* Here we have to distinguish which of the two bosons is produced in the
   fusion of three particles which include both fermions. *)

    let print_fermion_g4_vector_current coeff f c wf1 wf2 wf3 fusion =
      let c = format_coupling coeff c and
          d1 = d_p (1,f) and
          d2 = d_p (2,f) and
          f1 = (List.nth (wf_of_f wf1 wf2 wf3 fusion) 0) and
          f2 = (List.nth (wf_of_f wf1 wf2 wf3 fusion) 1) and
          f3 = (List.nth (wf_of_f wf1 wf2 wf3 fusion) 2) in
      match fusion with
      | (F123|F213|F132|F231|F312|F321) ->
          printf "f_%sgr(%s,%s,%s,%s)" f c f1 f2 f3
      | (F423|F243|F432|F234|F342|F324) ->
          printf "gr_%sf(%s,%s,%s,%s)" f c f1 f2 f3
      | (F134|F143|F314) -> printf "%s%s_grf(%s,%s,%s,%s)" f d1 c f1 f2 f3
      | (F124|F142|F214) -> printf "%s%s_grf(%s,%s,%s,%s)" f d2 c f1 f2 f3
      | (F413|F431|F341) -> printf "%s%s_fgr(%s,%s,%s,%s)" f d1 c f1 f2 f3
      | (F241|F412|F421) -> printf "%s%s_fgr(%s,%s,%s,%s)" f d2 c f1 f2 f3

    let print_fermion_2_g4_vector_current coeff f c wf1 wf2 wf3 fusion =
      let d1 = d_p (1,f) and
          d2 = d_p (2,f) and
          f1 = (List.nth (wf_of_f wf1 wf2 wf3 fusion) 0) and
          f2 = (List.nth (wf_of_f wf1 wf2 wf3 fusion) 1) and
          f3 = (List.nth (wf_of_f wf1 wf2 wf3 fusion) 2) in
      let c = format_coupling_2 coeff c in
      let c1 = fastener c 1 and
          c2 = fastener c 2 in
      match fusion with
      | (F123|F213|F132|F231|F312|F321) ->
          printf "f_%sgr(%s,%s,%s,%s,%s)" f c1 c2 f1 f2 f3
      | (F423|F243|F432|F234|F342|F324) ->
          printf "gr_%sf(%s,%s,%s,%s,%s)" f c1 c2 f1 f2 f3
      | (F134|F143|F314) -> printf "%s%s_grf(%s,%s,%s,%s,%s)" f d1 c1 c2 f1 f2 f3
      | (F124|F142|F214) -> printf "%s%s_grf(%s,%s,%s,%s,%s)" f d2 c1 c2 f1 f2 f3
      | (F413|F431|F341) -> printf "%s%s_fgr(%s,%s,%s,%s,%s)" f d1 c1 c2 f1 f2 f3
      | (F241|F412|F421) -> printf "%s%s_fgr(%s,%s,%s,%s,%s)" f d2 c1 c2 f1 f2 f3

    let print_fermion_g4_vector_current_rev coeff f c wf1 wf2 wf3 fusion =
      let c = format_coupling coeff c and
          d1 = d_p (1,f) and
          d2 = d_p (2,f) and
          f1 = (List.nth (wf_of_f wf1 wf2 wf3 fusion) 0) and
          f2 = (List.nth (wf_of_f wf1 wf2 wf3 fusion) 1) and
          f3 = (List.nth (wf_of_f wf1 wf2 wf3 fusion) 2) in
      match fusion with
      | (F123|F213|F132|F231|F312|F321) ->
          printf "gr_%sf(%s,%s,%s,%s)" f c f1 f2 f3
      | (F423|F243|F432|F234|F342|F324) ->
          printf "f_%sgr(%s,%s,%s,%s)" f c f1 f2 f3
      | (F134|F143|F314) -> printf "%s%s_fgr(%s,%s,%s,%s)" f d1 c f1 f2 f3
      | (F124|F142|F214) -> printf "%s%s_fgr(%s,%s,%s,%s)" f d2 c f1 f2 f3
      | (F413|F431|F341) -> printf "%s%s_grf(%s,%s,%s,%s)" f d1 c f1 f2 f3
      | (F241|F412|F421) -> printf "%s%s_grf(%s,%s,%s,%s)" f d2 c f1 f2 f3

    let print_fermion_2_g4_current_rev coeff f c wf1 wf2 wf3 fusion =
      let c = format_coupling_2 coeff c in
      let c1 = fastener c 1 and
          c2 = fastener c 2  and
          d1 = d_p (1,f) and
          d2 = d_p (2,f) in
      let f1 = (List.nth (wf_of_f wf1 wf2 wf3 fusion) 0) and
          f2 = (List.nth (wf_of_f wf1 wf2 wf3 fusion) 1) and
          f3 = (List.nth (wf_of_f wf1 wf2 wf3 fusion) 2) in
      match fusion with
      | (F123|F213|F132|F231|F312|F321) ->
          printf "gr_%sf(%s,%s,%s,%s,%s)" f c1 c2 f1 f2 f3
      | (F423|F243|F432|F234|F342|F324) ->
          printf "f_%sgr(-(%s),-(%s),%s,%s,%s)" f c1 c2 f1 f2 f3
      | (F134|F143|F314) ->
          printf "%s%s_fgr(-(%s),-(%s),%s,%s,%s)" f d1 c1 c2 f1 f2 f3
      | (F124|F142|F214) ->
          printf "%s%s_fgr(-(%s),-(%s),%s,%s,%s)" f d2 c1 c2 f1 f2 f3
      | (F413|F431|F341) ->
          printf "%s%s_grf(%s,%s,%s,%s,%s)" f d1 c1 c2 f1 f2 f3
      | (F241|F412|F421) ->
          printf "%s%s_grf(%s,%s,%s,%s,%s)" f d2 c1 c2 f1 f2 f3

    let print_fermion_2_g4_vector_current_rev coeff f c wf1 wf2 wf3 fusion =
      (* Here we put in the extra minus sign from the coeff. *)
      let c = format_coupling coeff c in
      let c1 = fastener c 1 and
          c2 = fastener c 2 in
      let d1 = d_p (1,f) and
          d2 = d_p (2,f) and
          f1 = (List.nth (wf_of_f wf1 wf2 wf3 fusion) 0) and
          f2 = (List.nth (wf_of_f wf1 wf2 wf3 fusion) 1) and
          f3 = (List.nth (wf_of_f wf1 wf2 wf3 fusion) 2) in
      match fusion with
      | (F123|F213|F132|F231|F312|F321) ->
          printf "gr_%sf(%s,%s,%s,%s,%s)" f c1 c2 f1 f2 f3
      | (F423|F243|F432|F234|F342|F324) ->
          printf "f_%sgr(%s,%s,%s,%s,%s)" f c1 c2 f1 f2 f3
      | (F134|F143|F314) -> printf "%s%s_fgr(%s,%s,%s,%s,%s)" f d1 c1 c2 f1 f2 f3
      | (F124|F142|F214) -> printf "%s%s_fgr(%s,%s,%s,%s,%s)" f d2 c1 c2 f1 f2 f3
      | (F413|F431|F341) -> printf "%s%s_grf(%s,%s,%s,%s,%s)" f d1 c1 c2 f1 f2 f3
      | (F241|F412|F421) -> printf "%s%s_grf(%s,%s,%s,%s,%s)" f d2 c1 c2 f1 f2 f3


    let print_current_g4 = function
      | coeff, Gravbar, S2, _ -> print_fermion_g4_current coeff "s2"
      | coeff, Gravbar, SV, _ -> print_fermion_g4_vector_current coeff "sv"
      | coeff, Gravbar, SLV, _ -> print_fermion_g4_vector_current coeff "slv"
      | coeff, Gravbar, SRV, _ -> print_fermion_g4_vector_current coeff "srv"
      | coeff, Gravbar, SLRV, _ -> print_fermion_2_g4_vector_current coeff "slrv"
      | coeff, Gravbar, PV, _ -> print_fermion_g4_vector_current coeff "pv"
      | coeff, Gravbar, V2, _ -> print_fermion_g4_current coeff "v2"
      | coeff, Gravbar, V2LR, _ -> print_fermion_2_g4_current coeff "v2lr"
      | _, Gravbar, _, _ -> invalid_arg "print_current_g4: not implemented"
      | coeff, _, S2, Grav -> print_fermion_g4_current_rev coeff "s2"
      | coeff, _, SV, Grav -> print_fermion_g4_vector_current_rev (-coeff) "sv"
      | coeff, _, SLV, Grav -> print_fermion_g4_vector_current_rev (-coeff) "slv"
      | coeff, _, SRV, Grav -> print_fermion_g4_vector_current_rev (-coeff) "srv"
      | coeff, _, SLRV, Grav -> print_fermion_2_g4_vector_current_rev coeff "slrv"
      | coeff, _, PV, Grav -> print_fermion_g4_vector_current_rev coeff "pv"
      | coeff, _, V2, Grav -> print_fermion_g4_vector_current_rev coeff "v2"
      | coeff, _, V2LR, Grav -> print_fermion_2_g4_current_rev coeff "v2lr"
      | _, _, _, Grav -> invalid_arg "print_current_g4: not implemented"
      | coeff, _, S2, _ -> print_fermion_s2_current coeff "s"
      | coeff, _, P2, _ -> print_fermion_s2_current coeff "p"
      | coeff, _, S2P, _ -> print_fermion_s2p_current coeff "sp"
      | coeff, _, S2L, _ -> print_fermion_s2_current coeff "sl"
      | coeff, _, S2R, _ -> print_fermion_s2_current coeff "sr"
      | coeff, _, S2LR, _ -> print_fermion_s2lr_current coeff "slr"
      | coeff, _, V2, _ -> print_fermion_g4_brs_vector_current coeff "v2"
      | coeff, _, SV, _ -> print_fermion_g4_brs_vector_current coeff "sv"
      | coeff, _, PV, _ -> print_fermion_g4_brs_vector_current coeff "pv"
      | coeff, _, SLV, _ -> print_fermion_g4_brs_vector_current coeff "svl"
      | coeff, _, SRV, _ -> print_fermion_g4_brs_vector_current coeff "svr"
      | coeff, _, SLRV, _ -> print_fermion_g4_svlr_current coeff "svlr"
      | _, _, V2LR, _ -> invalid_arg "Targets.print_current: not available"

    let reverse_braket vintage bra ket =
      if vintage then
        false
      else
        match bra, ket with
        | Majorana, Majorana :: _ -> true
        | _, _ -> false

  end

(* \thocwmodulesubsection{Currents for [Coupling.V3] and [Coupling.V4]} *)

module type T =
  sig
    type amplitude
    type constant
    type wf
    type rhs
    val print_current_V3 :
      (amplitude -> (amplitude -> wf -> int) -> wf -> string) -> (wf -> string) ->
      amplitude -> (amplitude -> wf -> int) -> rhs ->
      constant Coupling.vertex3 -> Coupling.fuse2 -> constant -> unit
    val print_current_V4 :
      (amplitude -> (amplitude -> wf -> int) -> wf -> string) -> (wf -> string) ->
      amplitude -> (amplitude -> wf -> int) -> rhs ->
      constant Coupling.vertex4 -> Coupling.fuse3 -> constant -> unit
  end

module type Maker =
  functor (N : Target_Fortran_Names.T) -> functor (F : Fermion_Maker) ->
  functor (FM : Fusion.Maker) -> functor (P : Momentum.T) -> functor (M : Model.T) -> T
  with type amplitude = Fusion.Multi(FM)(P)(M).amplitude
   and type constant = Orders.Slice(Colorize.It(M)).constant
   and type wf = FM(P)(M).wf
   and type rhs = FM(P)(M).rhs

module Make_Fortran (Names : Target_Fortran_Names.T) (Fermion_Maker : Fermion_Maker)
         (FM : Fusion.Maker) (P : Momentum.T) (M : Model.T) =
  struct

    open Coupling
    open Format

    module Fermions = Fermion_Maker(Names)

    module CM = Colorize.It(M)
    module SCM = Orders.Slice(Colorize.It(M))
    module F = FM(P)(M)
    module CF = Fusion.Multi(FM)(P)(M)

    type amplitude = CF.amplitude
    type constant = Orders.Slice(Colorize.It(M)).constant
    type wf = F.wf
    type rhs = F.rhs

    let children2 rhs =
      match F.children rhs with
      | [wf1; wf2] -> (wf1, wf2)
      | _ -> failwith "Targets.children2: can't happen"

    let children3 rhs =
      match F.children rhs with
      | [wf1; wf2; wf3] -> (wf1, wf2, wf3)
      | _ -> invalid_arg "Targets.children3: can't happen"

(* Note that it is (marginally) faster to multiply the two scalar products
   with the coupling constant than the four vector components.
   \begin{dubious}
     This could be part of \verb+omegalib+ as well \ldots
   \end{dubious} *)

    let format_coeff = function
      | 1 -> ""
      | -1 -> "-"
      | coeff -> "(" ^ string_of_int coeff ^ ")*"

    let format_coupling coeff c =
      match coeff with
      | 1 -> c
      | -1 -> "(-" ^ c ^")"
      | coeff -> string_of_int coeff ^ "*" ^ c

(* \begin{dubious}
     The following is error prone and should be generated automagically.
   \end{dubious} *)

    let print_vector4 c wf1 wf2 wf3 fusion (coeff, contraction) =
      match contraction, fusion with
      | C_12_34, (F341|F431|F342|F432|F123|F213|F124|F214)
      | C_13_42, (F241|F421|F243|F423|F132|F312|F134|F314)
      | C_14_23, (F231|F321|F234|F324|F142|F412|F143|F413) ->
          printf "((%s%s)*(%s*%s))*%s" (format_coeff coeff) c wf1 wf2 wf3
      | C_12_34, (F134|F143|F234|F243|F312|F321|F412|F421)
      | C_13_42, (F124|F142|F324|F342|F213|F231|F413|F431)
      | C_14_23, (F123|F132|F423|F432|F214|F241|F314|F341) ->
          printf "((%s%s)*(%s*%s))*%s" (format_coeff coeff) c wf2 wf3 wf1
      | C_12_34, (F314|F413|F324|F423|F132|F231|F142|F241)
      | C_13_42, (F214|F412|F234|F432|F123|F321|F143|F341)
      | C_14_23, (F213|F312|F243|F342|F124|F421|F134|F431) ->
          printf "((%s%s)*(%s*%s))*%s" (format_coeff coeff) c wf1 wf3 wf2
          
    let print_vector4_t_0 c wf1 p1 wf2 p2 wf3 p3 fusion (coeff, contraction) =
      match contraction, fusion with
      | C_12_34, (F234|F243|F134|F143|F421|F321|F412|F312)
      | C_13_42, (F324|F342|F124|F142|F431|F231|F413|F213)
      | C_14_23, (F423|F432|F123|F132|F341|F241|F314|F214) ->
          printf "g_dim8g3_t_0(%s,%s,%s,%s,%s,%s,%s)" c wf1 p1 wf2 p2 wf3 p3
      | C_12_34, (F324|F314|F423|F413|F142|F132|F241|F231)
      | C_13_42, (F234|F214|F432|F412|F143|F123|F341|F321)
      | C_14_23, (F243|F213|F342|F312|F134|F124|F431|F421) ->
          printf "g_dim8g3_t_0(%s,%s,%s,%s,%s,%s,%s)" c wf2 p2 wf1 p1 wf3 p3
      | C_12_34, (F342|F341|F432|F431|F124|F123|F214|F213)
      | C_13_42, (F243|F241|F423|F421|F134|F132|F314|F312)
      | C_14_23, (F234|F231|F324|F321|F143|F142|F413|F412) ->
          printf "g_dim8g3_t_0(%s,%s,%s,%s,%s,%s,%s)" c wf3 p3 wf1 p1 wf2 p2

    let print_vector4_t_1 c wf1 p1 wf2 p2 wf3 p3 fusion (coeff, contraction) =
      match contraction, fusion with
      | C_12_34, (F234|F243|F134|F143|F421|F321|F412|F312)
      | C_13_42, (F324|F342|F124|F142|F431|F231|F413|F213)
      | C_14_23, (F423|F432|F123|F132|F341|F241|F314|F214) ->
          printf "g_dim8g3_t_1(%s,%s,%s,%s,%s,%s,%s)" c wf1 p1 wf2 p2 wf3 p3
      | C_12_34, (F324|F314|F423|F413|F142|F132|F241|F231)
      | C_13_42, (F234|F214|F432|F412|F143|F123|F341|F321)
      | C_14_23, (F243|F213|F342|F312|F134|F124|F431|F421) ->
          printf "g_dim8g3_t_1(%s,%s,%s,%s,%s,%s,%s)" c wf2 p2 wf1 p1 wf3 p3
      | C_12_34, (F342|F341|F432|F431|F124|F123|F214|F213)
      | C_13_42, (F243|F241|F423|F421|F134|F132|F314|F312)
      | C_14_23, (F234|F231|F324|F321|F143|F142|F413|F412) ->
          printf "g_dim8g3_t_1(%s,%s,%s,%s,%s,%s,%s)" c wf3 p3 wf1 p1 wf2 p2          

    let print_vector4_t_2 c wf1 p1 wf2 p2 wf3 p3 fusion (coeff, contraction) =
      match contraction, fusion with
      | C_12_34, (F234|F243|F134|F143|F421|F321|F412|F312)
      | C_13_42, (F324|F342|F124|F142|F431|F231|F413|F213)
      | C_14_23, (F423|F432|F123|F132|F341|F241|F314|F214) ->
          printf "g_dim8g3_t_2(%s,%s,%s,%s,%s,%s,%s)" c wf1 p1 wf2 p2 wf3 p3
      | C_12_34, (F324|F314|F423|F413|F142|F132|F241|F231)
      | C_13_42, (F234|F214|F432|F412|F143|F123|F341|F321)
      | C_14_23, (F243|F213|F342|F312|F134|F124|F431|F421) ->
          printf "g_dim8g3_t_2(%s,%s,%s,%s,%s,%s,%s)" c wf2 p2 wf1 p1 wf3 p3
      | C_12_34, (F342|F341|F432|F431|F124|F123|F214|F213)
      | C_13_42, (F243|F241|F423|F421|F134|F132|F314|F312)
      | C_14_23, (F234|F231|F324|F321|F143|F142|F413|F412) ->
          printf "g_dim8g3_t_2(%s,%s,%s,%s,%s,%s,%s)" c wf3 p3 wf1 p1 wf2 p2

    let print_vector4_m_0 c wf1 p1 wf2 p2 wf3 p3 fusion (coeff, contraction) =
      match contraction, fusion with
      | C_12_34, (F234|F243|F134|F143|F421|F321|F412|F312)
      | C_13_42, (F324|F342|F124|F142|F431|F231|F413|F213)
      | C_14_23, (F423|F432|F123|F132|F341|F241|F314|F214) ->
          printf "g_dim8g3_m_0(%s,%s,%s,%s,%s,%s,%s)" c wf1 p1 wf2 p2 wf3 p3
      | C_12_34, (F324|F314|F423|F413|F142|F132|F241|F231)
      | C_13_42, (F234|F214|F432|F412|F143|F123|F341|F321)
      | C_14_23, (F243|F213|F342|F312|F134|F124|F431|F421) ->
          printf "g_dim8g3_m_0(%s,%s,%s,%s,%s,%s,%s)" c wf2 p2 wf1 p1 wf3 p3
      | C_12_34, (F342|F341|F432|F431|F124|F123|F214|F213)
      | C_13_42, (F243|F241|F423|F421|F134|F132|F314|F312)
      | C_14_23, (F234|F231|F324|F321|F143|F142|F413|F412) ->
          printf "g_dim8g3_m_0(%s,%s,%s,%s,%s,%s,%s)" c wf3 p3 wf1 p1 wf2 p2

    let print_vector4_m_1 c wf1 p1 wf2 p2 wf3 p3 fusion (coeff, contraction) =
      match contraction, fusion with
      | C_12_34, (F234|F243|F134|F143|F421|F321|F412|F312)
      | C_13_42, (F324|F342|F124|F142|F431|F231|F413|F213)
      | C_14_23, (F423|F432|F123|F132|F341|F241|F314|F214) ->
          printf "g_dim8g3_m_1(%s,%s,%s,%s,%s,%s,%s)" c wf1 p1 wf2 p2 wf3 p3
      | C_12_34, (F324|F314|F423|F413|F142|F132|F241|F231)
      | C_13_42, (F234|F214|F432|F412|F143|F123|F341|F321)
      | C_14_23, (F243|F213|F342|F312|F134|F124|F431|F421) ->
          printf "g_dim8g3_m_1(%s,%s,%s,%s,%s,%s,%s)" c wf2 p2 wf1 p1 wf3 p3
      | C_12_34, (F342|F341|F432|F431|F124|F123|F214|F213)
      | C_13_42, (F243|F241|F423|F421|F134|F132|F314|F312)
      | C_14_23, (F234|F231|F324|F321|F143|F142|F413|F412) ->
          printf "g_dim8g3_m_1(%s,%s,%s,%s,%s,%s,%s)" c wf3 p3 wf1 p1 wf2 p2
          
     let print_vector4_m_7 c wf1 p1 wf2 p2 wf3 p3 fusion (coeff, contraction) =
      match contraction, fusion with
      | C_12_34, (F234|F243|F134|F143|F421|F321|F412|F312)
      | C_13_42, (F324|F342|F124|F142|F431|F231|F413|F213)
      | C_14_23, (F423|F432|F123|F132|F341|F241|F314|F214) ->
          printf "g_dim8g3_m_7(%s,%s,%s,%s,%s,%s,%s)" c wf1 p1 wf2 p2 wf3 p3
      | C_12_34, (F324|F314|F423|F413|F142|F132|F241|F231)
      | C_13_42, (F234|F214|F432|F412|F143|F123|F341|F321)
      | C_14_23, (F243|F213|F342|F312|F134|F124|F431|F421) ->
          printf "g_dim8g3_m_7(%s,%s,%s,%s,%s,%s,%s)" c wf2 p2 wf1 p1 wf3 p3
      | C_12_34, (F342|F341|F432|F431|F124|F123|F214|F213)
      | C_13_42, (F243|F241|F423|F421|F134|F132|F314|F312)
      | C_14_23, (F234|F231|F324|F321|F143|F142|F413|F412) ->
          printf "g_dim8g3_m_7(%s,%s,%s,%s,%s,%s,%s)" c wf3 p3 wf1 p1 wf2 p2      

    let print_add_vector4 c wf1 wf2 wf3 fusion (coeff, contraction) =
      printf "@ + ";
      print_vector4 c wf1 wf2 wf3 fusion (coeff, contraction)

    let print_vector4_km c pa pb wf1 wf2 wf3 fusion (coeff, contraction) =
      match contraction, fusion with
      | C_12_34, (F341|F431|F342|F432|F123|F213|F124|F214)
      | C_13_42, (F241|F421|F243|F423|F132|F312|F134|F314)
      | C_14_23, (F231|F321|F234|F324|F142|F412|F143|F413) ->
          printf "((%s%s%s+%s))*(%s*%s))*%s"
            (format_coeff coeff) c pa pb wf1 wf2 wf3
      | C_12_34, (F134|F143|F234|F243|F312|F321|F412|F421)
      | C_13_42, (F124|F142|F324|F342|F213|F231|F413|F431)
      | C_14_23, (F123|F132|F423|F432|F214|F241|F314|F341) ->
          printf "((%s%s%s+%s))*(%s*%s))*%s"
            (format_coeff coeff) c pa pb wf2 wf3 wf1
      | C_12_34, (F314|F413|F324|F423|F132|F231|F142|F241)
      | C_13_42, (F214|F412|F234|F432|F123|F321|F143|F341)
      | C_14_23, (F213|F312|F243|F342|F124|F421|F134|F431) ->
          printf "((%s%s%s+%s))*(%s*%s))*%s"
            (format_coeff coeff) c pa pb wf1 wf3 wf2
            
    let print_vector4_km_t_0 c pa pb wf1 p1 wf2 p2 wf3 p3 fusion (coeff, contraction) =
      match contraction, fusion with
      | C_12_34, (F234|F243|F134|F143|F421|F321|F412|F312)
      | C_13_42, (F324|F342|F124|F142|F431|F231|F413|F213)
      | C_14_23, (F423|F432|F123|F132|F341|F241|F314|F214) ->
          printf "@[(%s%s%s+%s)*g_dim8g3_t_0(cmplx(1,kind=default),@ %s,%s,%s,%s,%s,%s))@]"
            (format_coeff coeff) c pa pb wf1 p1 wf2 p2 wf3 p3
      | C_12_34, (F324|F314|F423|F413|F142|F132|F241|F231)
      | C_13_42, (F234|F214|F432|F412|F143|F123|F341|F321)
      | C_14_23, (F243|F213|F342|F312|F134|F124|F431|F421) ->
          printf "@[(%s%s%s+%s)*g_dim8g3_t_0(cmplx(1,kind=default),@ %s,%s,%s,%s,%s,%s))@]"
            (format_coeff coeff) c pa pb wf2 p2 wf1 p1 wf3 p3
      | C_12_34, (F342|F341|F432|F431|F124|F123|F214|F213)
      | C_13_42, (F243|F241|F423|F421|F134|F132|F314|F312)
      | C_14_23, (F234|F231|F324|F321|F143|F142|F413|F412) ->
          printf "@[(%s%s%s+%s)*g_dim8g3_t_0(cmplx(1,kind=default),@ %s,%s,%s,%s,%s,%s))@]"
            (format_coeff coeff) c pa pb wf3 p3 wf1 p1 wf2 p2 

    let print_vector4_km_t_1 c pa pb wf1 p1 wf2 p2 wf3 p3 fusion (coeff, contraction) =
      match contraction, fusion with
      | C_12_34, (F234|F243|F134|F143|F421|F321|F412|F312)
      | C_13_42, (F324|F342|F124|F142|F431|F231|F413|F213)
      | C_14_23, (F423|F432|F123|F132|F341|F241|F314|F214) ->
          printf "@[(%s%s%s+%s)*g_dim8g3_t_1(cmplx(1,kind=default),@ %s,%s,%s,%s,%s,%s))@]"
            (format_coeff coeff) c pa pb wf1 p1 wf2 p2 wf3 p3
      | C_12_34, (F324|F314|F423|F413|F142|F132|F241|F231)
      | C_13_42, (F234|F214|F432|F412|F143|F123|F341|F321)
      | C_14_23, (F243|F213|F342|F312|F134|F124|F431|F421) ->
          printf "@[(%s%s%s+%s)*g_dim8g3_t_1(cmplx(1,kind=default),@ %s,%s,%s,%s,%s,%s))@]"
            (format_coeff coeff) c pa pb wf2 p2 wf1 p1 wf3 p3
      | C_12_34, (F342|F341|F432|F431|F124|F123|F214|F213)
      | C_13_42, (F243|F241|F423|F421|F134|F132|F314|F312)
      | C_14_23, (F234|F231|F324|F321|F143|F142|F413|F412) ->
          printf "@[(%s%s%s+%s)*g_dim8g3_t_1(cmplx(1,kind=default),@ %s,%s,%s,%s,%s,%s))@]"
            (format_coeff coeff) c pa pb wf3 p3 wf1 p1 wf2 p2
            
    let print_vector4_km_t_2 c pa pb wf1 p1 wf2 p2 wf3 p3 fusion (coeff, contraction) =
      match contraction, fusion with
      | C_12_34, (F234|F243|F134|F143|F421|F321|F412|F312)
      | C_13_42, (F324|F342|F124|F142|F431|F231|F413|F213)
      | C_14_23, (F423|F432|F123|F132|F341|F241|F314|F214) ->
          printf "@[(%s%s%s+%s)*g_dim8g3_t_2(cmplx(1,kind=default),@ %s,%s,%s,%s,%s,%s))@]"
            (format_coeff coeff) c pa pb wf1 p1 wf2 p2 wf3 p3
      | C_12_34, (F324|F314|F423|F413|F142|F132|F241|F231)
      | C_13_42, (F234|F214|F432|F412|F143|F123|F341|F321)
      | C_14_23, (F243|F213|F342|F312|F134|F124|F431|F421) ->
          printf "@[(%s%s%s+%s)*g_dim8g3_t_2(cmplx(1,kind=default),@ %s,%s,%s,%s,%s,%s))@]"
            (format_coeff coeff) c pa pb wf2 p2 wf1 p1 wf3 p3
      | C_12_34, (F342|F341|F432|F431|F124|F123|F214|F213)
      | C_13_42, (F243|F241|F423|F421|F134|F132|F314|F312)
      | C_14_23, (F234|F231|F324|F321|F143|F142|F413|F412) ->
          printf "@[(%s%s%s+%s)*g_dim8g3_t_2(cmplx(1,kind=default),@ %s,%s,%s,%s,%s,%s))@]"
            (format_coeff coeff) c pa pb wf3 p3 wf1 p1 wf2 p2
            
    let print_vector4_km_t_rsi c pa pb pc wf1 p1 wf2 p2 wf3 p3 fusion (coeff, contraction) =
      match contraction, fusion with
      | C_12_34, (F234|F243|F134|F143|F421|F321|F412|F312)
      | C_13_42, (F324|F342|F124|F142|F431|F231|F413|F213)
      | C_14_23, (F423|F432|F123|F132|F341|F241|F314|F214) ->
          printf "@[(%s%s%s+%s)*g_dim8g3_t_0(cmplx(1,kind=default),@ %s,%s,%s,%s,%s,%s))@]"
            (format_coeff coeff) c pa pb wf1 p1 wf2 p2 wf3 p3
      | C_12_34, (F324|F314|F423|F413|F142|F132|F241|F231)
      | C_13_42, (F234|F214|F432|F412|F143|F123|F341|F321)
      | C_14_23, (F243|F213|F342|F312|F134|F124|F431|F421) ->
          printf "@[(%s%s%s+%s)*g_dim8g3_t_0(cmplx(1,kind=default),@ %s,%s,%s,%s,%s,%s))*((%s+%s)*(%s+%s)/((%s+%s)*(%s+%s)))@]"
            (format_coeff coeff) c pa pb wf2 p2 wf1 p1 wf3 p3 pa pb pa pb pb pc pb pc
      | C_12_34, (F342|F341|F432|F431|F124|F123|F214|F213)
      | C_13_42, (F243|F241|F423|F421|F134|F132|F314|F312)
      | C_14_23, (F234|F231|F324|F321|F143|F142|F413|F412) ->
          printf "@[(%s%s%s+%s)*g_dim8g3_t_0(cmplx(1,kind=default),@ %s,%s,%s,%s,%s,%s))*((%s+%s)*(%s+%s)/((%s+%s)*(%s+%s)))@]"
            (format_coeff coeff) c pa pb wf3 p3 wf1 p1 wf2 p2 pa pb pa pb pa pc pa pc            

    let print_vector4_km_m_0 c pa pb wf1 p1 wf2 p2 wf3 p3 fusion (coeff, contraction) =
      match contraction, fusion with
      | C_12_34, (F234|F243|F134|F143|F421|F321|F412|F312)
      | C_13_42, (F324|F342|F124|F142|F431|F231|F413|F213)
      | C_14_23, (F423|F432|F123|F132|F341|F241|F314|F214) ->
          if (String.contains c 'w' || String.contains c '4') then
             printf "@[(%s%s%s+%s)*g_dim8g3_m_0(cmplx(1,kind=default),cmplx(1,kind=default),@ %s,%s,%s,%s,%s,%s))@]"
                (format_coeff coeff) c pa pb wf1 p1 wf2 p2 wf3 p3
          else
             printf "@[((%s%s%s+%s))*g_dim8g3_m_0(cmplx(costhw**(-2),kind=default),cmplx(costhw**2,kind=default),@ %s,%s,%s,%s,%s,%s))@]"
                (format_coeff coeff) c pa pb wf1 p1 wf2 p2 wf3 p3
      | C_12_34, (F324|F314|F423|F413|F142|F132|F241|F231)
      | C_13_42, (F234|F214|F432|F412|F143|F123|F341|F321)
      | C_14_23, (F243|F213|F342|F312|F134|F124|F431|F421) ->
          if (String.contains c 'w' || String.contains c '4') then
             printf "@[(%s%s%s+%s)*g_dim8g3_m_0(cmplx(1,kind=default),cmplx(1,kind=default),@ %s,%s,%s,%s,%s,%s))@]"
                (format_coeff coeff) c pa pb  wf2 p2 wf1 p1 wf3 p3
          else
             printf "@[(%s%s%s+%s)*g_dim8g3_m_0(cmplx(costhw**(-2),kind=default),cmplx(costhw**2,kind=default),@ %s,%s,%s,%s,%s,%s))@]"
                (format_coeff coeff) c pa pb wf2 p2 wf1 p1 wf3 p3
      | C_12_34, (F342|F341|F432|F431|F124|F123|F214|F213)
      | C_13_42, (F243|F241|F423|F421|F134|F132|F314|F312)
      | C_14_23, (F234|F231|F324|F321|F143|F142|F413|F412) ->
          if (String.contains c 'w' || String.contains c '4') then
             printf "@[(%s%s%s+%s)*g_dim8g3_m_0(cmplx(1,kind=default),cmplx(1,kind=default),@ %s,%s,%s,%s,%s,%s))@]"
                (format_coeff coeff) c pa pb wf3 p3 wf1 p1 wf2 p2
          else
             printf "@[(%s%s%s+%s)*g_dim8g3_m_0(cmplx(costhw**(-2),kind=default),cmplx(costhw**2,kind=default),@ %s,%s,%s,%s,%s,%s))@]"
                (format_coeff coeff) c pa pb wf3 p3 wf1 p1 wf2 p2

    let print_vector4_km_m_1 c pa pb wf1 p1 wf2 p2 wf3 p3 fusion (coeff, contraction) =
      match contraction, fusion with
      | C_12_34, (F234|F243|F134|F143|F421|F321|F412|F312)
      | C_13_42, (F324|F342|F124|F142|F431|F231|F413|F213)
      | C_14_23, (F423|F432|F123|F132|F341|F241|F314|F214) ->
          if (String.contains c 'w' || String.contains c '4') then
             printf "@[(%s%s%s+%s)*g_dim8g3_m_1(cmplx(1,kind=default),cmplx(1,kind=default),@ %s,%s,%s,%s,%s,%s))@]"
                (format_coeff coeff) c pa pb wf1 p1 wf2 p2 wf3 p3
          else
             printf "@[(%s%s%s+%s)*g_dim8g3_m_1(cmplx(costhw**(-2),kind=default),cmplx(costhw**2,kind=default),@ %s,%s,%s,%s,%s,%s))@]"
                (format_coeff coeff) c pa pb wf1 p1 wf2 p2 wf3 p3
      | C_12_34, (F324|F314|F423|F413|F142|F132|F241|F231)
      | C_13_42, (F234|F214|F432|F412|F143|F123|F341|F321)
      | C_14_23, (F243|F213|F342|F312|F134|F124|F431|F421) ->
          if (String.contains c 'w' || String.contains c '4') then
             printf "@[(%s%s%s+%s)*g_dim8g3_m_1(cmplx(1,kind=default),cmplx(1,kind=default),@  %s,%s,%s,%s,%s,%s))@]"
                (format_coeff coeff) c pa pb wf2 p2 wf1 p1 wf3 p3
          else
             printf "@[(%s%s%s+%s)*g_dim8g3_m_1(cmplx(costhw**(-2),kind=default),cmplx(costhw**2,kind=default),@  %s,%s,%s,%s,%s,%s))@]"
                (format_coeff coeff) c pa pb wf2 p2 wf1 p1 wf3 p3
      | C_12_34, (F342|F341|F432|F431|F124|F123|F214|F213)
      | C_13_42, (F243|F241|F423|F421|F134|F132|F314|F312)
      | C_14_23, (F234|F231|F324|F321|F143|F142|F413|F412) ->
          if (String.contains c 'w' || String.contains c '4') then
             printf "@[(%s%s%s+%s)*g_dim8g3_m_1(cmplx(1,kind=default),cmplx(1,kind=default),@ %s,%s,%s,%s,%s,%s))@]"
                (format_coeff coeff) c pa pb wf3 p3 wf1 p1 wf2 p2
          else
             printf "@[(%s%s%s+%s)*g_dim8g3_m_1(cmplx(costhw**(-2),kind=default),cmplx(costhw**2,kind=default),@ %s,%s,%s,%s,%s,%s))@]"
                (format_coeff coeff) c pa pb wf3 p3 wf1 p1 wf2 p2
                
    let print_vector4_km_m_7 c pa pb wf1 p1 wf2 p2 wf3 p3 fusion (coeff, contraction) =
      match contraction, fusion with
      | C_12_34, (F234|F243|F134|F143|F421|F321|F412|F312)
      | C_13_42, (F324|F342|F124|F142|F431|F231|F413|F213)
      | C_14_23, (F423|F432|F123|F132|F341|F241|F314|F214) ->
          if (String.contains c 'w' || String.contains c '4') then
             printf "@[(%s%s%s+%s)*@ g_dim8g3_m_7(cmplx(1,kind=default),cmplx(1,kind=default),cmplx(1,kind=default),@ %s,%s,%s,%s,%s,%s))@]"
                (format_coeff coeff) c pa pb wf1 p1 wf2 p2 wf3 p3
          else
             printf "@[(%s%s%s+%s)*@ g_dim8g3_m_7(cmplx(costhw**(-2),kind=default),cmplx(1,kind=default),cmplx(costhw**2,kind=default),@ %s,%s,%s,%s,%s,%s))@]"
                (format_coeff coeff) c pa pb wf1 p1 wf2 p2 wf3 p3
      | C_12_34, (F324|F314|F423|F413|F142|F132|F241|F231)
      | C_13_42, (F234|F214|F432|F412|F143|F123|F341|F321)
      | C_14_23, (F243|F213|F342|F312|F134|F124|F431|F421) ->
          if (String.contains c 'w' || String.contains c '4') then
             printf "@[(%s%s%s+%s)*@ g_dim8g3_m_7(cmplx(1,kind=default),cmplx(1,kind=default),cmplx(1,kind=default),@ %s,%s,%s,%s,%s,%s))@]"
                (format_coeff coeff) c pa pb wf2 p2 wf1 p1 wf3 p3
          else
             printf "@[(%s%s%s+%s)*@ g_dim8g3_m_7(cmplx(costhw**(-2),kind=default),cmplx(1,kind=default),cmplx(costhw**2,kind=default),@ %s,%s,%s,%s,%s,%s))@]"
                (format_coeff coeff) c pa pb wf2 p2 wf1 p1 wf3 p3
      | C_12_34, (F342|F341|F432|F431|F124|F123|F214|F213)
      | C_13_42, (F243|F241|F423|F421|F134|F132|F314|F312)
      | C_14_23, (F234|F231|F324|F321|F143|F142|F413|F412) ->
          if (String.contains c 'w' || String.contains c '4') then
             printf "@[(%s%s%s+%s)*@ g_dim8g3_m_7(cmplx(1,kind=default),cmplx(1,kind=default),cmplx(1,kind=default),@ %s,%s,%s,%s,%s,%s))@]"
                (format_coeff coeff) c pa pb wf3 p3 wf1 p1 wf2 p2
          else
             printf "@[(%s%s%s+%s)*@ g_dim8g3_m_7(cmplx(costhw**(-2),kind=default),cmplx(1,kind=default),cmplx(costhw**2,kind=default),@ %s,%s,%s,%s,%s,%s))@]"
                (format_coeff coeff) c pa pb wf3 p3 wf1 p1 wf2 p2        

    let print_add_vector4_km c pa pb wf1 wf2 wf3 fusion (coeff, contraction) =
      printf "@ + ";
      print_vector4_km c pa pb wf1 wf2 wf3 fusion (coeff, contraction)

    let print_dscalar4 c wf1 wf2 wf3 p1 p2 p3 p123
        fusion (coeff, contraction) =
      match contraction, fusion with
      | C_12_34, (F341|F431|F342|F432|F123|F213|F124|F214)
      | C_13_42, (F241|F421|F243|F423|F132|F312|F134|F314)
      | C_14_23, (F231|F321|F234|F324|F142|F412|F143|F413) ->
          printf "((%s%s)*(%s*%s)*(%s*%s)*%s*%s*%s)"
            (format_coeff coeff) c p1 p2 p3 p123 wf1 wf2 wf3
      | C_12_34, (F134|F143|F234|F243|F312|F321|F412|F421)
      | C_13_42, (F124|F142|F324|F342|F213|F231|F413|F431)
      | C_14_23, (F123|F132|F423|F432|F214|F241|F314|F341) ->
          printf "((%s%s)*(%s*%s)*(%s*%s)*%s*%s*%s)"
            (format_coeff coeff) c p2 p3 p1 p123 wf1 wf2 wf3
      | C_12_34, (F314|F413|F324|F423|F132|F231|F142|F241)
      | C_13_42, (F214|F412|F234|F432|F123|F321|F143|F341)
      | C_14_23, (F213|F312|F243|F342|F124|F421|F134|F431) ->
          printf "((%s%s)*(%s*%s)*(%s*%s)*%s*%s*%s)"
            (format_coeff coeff) c p1 p3 p2 p123 wf1 wf2 wf3

    let print_add_dscalar4 c wf1 wf2 wf3 p1 p2 p3 p123
        fusion (coeff, contraction) =
      printf "@ + ";
      print_dscalar4 c wf1 wf2 wf3 p1 p2 p3 p123 fusion (coeff, contraction)

    let print_dscalar2_vector2 c wf1 wf2 wf3 p1 p2 p3 p123 fusion (coeff, contraction) =
      match contraction, fusion with
      | C_12_34, (F123|F213|F124|F214) ->
          printf "(%s%s)*(%s*%s)*(%s*%s)*%s"
            (format_coeff coeff) c p1 p2 wf1 wf2 wf3
      | C_12_34, (F134|F143|F234|F243) ->
          printf "(%s%s)*(%s*%s)*(%s*%s)*%s"
            (format_coeff coeff) c p1 p123 wf2 wf3 wf1
      | C_12_34, (F132|F231|F142|F241) ->
          printf "(%s%s)*(%s*%s)*(%s*%s)*%s"
            (format_coeff coeff) c p1 p3 wf1 wf3 wf2  
      | C_12_34, (F312|F321|F412|F421) ->
          printf "(%s%s)*(%s*%s)*(%s*%s)*%s"
            (format_coeff coeff) c p2 p3 wf2 wf3 wf1 
      | C_12_34, (F314|F413|F324|F423) ->
          printf "(%s%s)*(%s*%s)*(%s*%s)*%s"
            (format_coeff coeff) c p2 p123 wf1 wf3 wf2  
      | C_12_34, (F341|F431|F342|F432) ->
          printf "(%s%s)*(%s*%s)*(%s*%s)*%s"
            (format_coeff coeff) c p3 p123 wf1 wf2 wf3
      | C_13_42, (F123|F214) 
      | C_14_23, (F124|F213) ->
          printf "((%s%s)*(%s*%s*%s)*%s*%s)"
            (format_coeff coeff) c wf1 p1 wf3 wf2 p2
      | C_13_42, (F124|F213) 
      | C_14_23, (F123|F214) ->
          printf "((%s%s)*(%s*%s*%s)*%s*%s)"
            (format_coeff coeff) c wf2 p2 wf3 wf1 p1
      | C_13_42, (F132|F241) 
      | C_14_23, (F142|F231) ->
          printf "((%s%s)*(%s*%s*%s)*%s*%s)"
            (format_coeff coeff) c wf1 p1 wf2 wf3 p3
      | C_13_42, (F142|F231) 
      | C_14_23, (F132|F241) ->
          printf "((%s%s)*(%s*%s*%s)*%s*%s)"
            (format_coeff coeff) c wf3 p3 wf2 wf1 p1
      | C_13_42, (F312|F421) 
      | C_14_23, (F412|F321) ->
          printf "((%s%s)*(%s*%s*%s)*%s*%s)"
            (format_coeff coeff) c wf2 p2 wf1 wf3 p3
      | C_13_42, (F321|F412) 
      | C_14_23, (F421|F312) ->
          printf "((%s%s)*(%s*%s*%s)*%s*%s)"
            (format_coeff coeff) c wf3 p3 wf1 wf2 p2
      | C_13_42, (F134|F243) 
      | C_14_23, (F143|F234) ->
          printf "((%s%s)*(%s*%s)*(%s*%s*%s))"
            (format_coeff coeff) c wf3 p123 wf1 p1 wf2
      | C_13_42, (F143|F234) 
      | C_14_23, (F134|F243) ->
          printf "((%s%s)*(%s*%s)*(%s*%s*%s))"
            (format_coeff coeff) c wf2 p123 wf1 p1 wf3
      | C_13_42, (F314|F423) 
      | C_14_23, (F413|F324) ->
          printf "((%s%s)*(%s*%s)*(%s*%s*%s))"
            (format_coeff coeff) c wf3 p123 wf2 p2 wf1
      | C_13_42, (F324|F413) 
      | C_14_23, (F423|F314) ->
          printf "((%s%s)*(%s*%s)*(%s*%s*%s))"
            (format_coeff coeff) c wf1 p123 wf2 p2 wf3
      | C_13_42, (F341|F432) 
      | C_14_23, (F431|F342) ->
          printf "((%s%s)*(%s*%s)*(%s*%s*%s))"
            (format_coeff coeff) c wf2 p123 wf3 p3 wf1
      | C_13_42, (F342|F431) 
      | C_14_23, (F432|F341) ->
          printf "((%s%s)*(%s*%s)*(%s*%s*%s))"
            (format_coeff coeff) c wf1 p123 wf3 p3 wf2

    let print_add_dscalar2_vector2 c wf1 wf2 wf3 p1 p2 p3 p123
        fusion (coeff, contraction) =
      printf "@ + ";
      print_dscalar2_vector2 c wf1 wf2 wf3 p1 p2 p3 p123
        fusion (coeff, contraction)

    let print_dscalar2_vector2_km c pa pb wf1 wf2 wf3 p1 p2 p3 p123 fusion (coeff, contraction) =
      match contraction, fusion with
      | C_12_34, (F123|F213|F124|F214) ->
          printf "(%s%s%s+%s))*(%s*%s)*(%s*%s)*%s"
            (format_coeff coeff) c pa pb p1 p2 wf1 wf2 wf3
      | C_12_34, (F134|F143|F234|F243) ->
          printf "(%s%s%s+%s))*(%s*%s)*(%s*%s)*%s"
            (format_coeff coeff) c pa pb p1 p123 wf2 wf3 wf1
      | C_12_34, (F132|F231|F142|F241) ->
          printf "(%s%s%s+%s))*(%s*%s)*(%s*%s)*%s"
            (format_coeff coeff) c pa pb p1 p3 wf1 wf3 wf2  
      | C_12_34, (F312|F321|F412|F421) ->
          printf "(%s%s%s+%s))*(%s*%s)*(%s*%s)*%s"
            (format_coeff coeff) c pa pb p2 p3 wf2 wf3 wf1 
      | C_12_34, (F314|F413|F324|F423) ->
          printf "(%s%s%s+%s))*(%s*%s)*(%s*%s)*%s"
            (format_coeff coeff) c pa pb p2 p123 wf1 wf3 wf2  
      | C_12_34, (F341|F431|F342|F432) ->
          printf "(%s%s%s+%s))*(%s*%s)*(%s*%s)*%s"
            (format_coeff coeff) c pa pb p3 p123 wf1 wf2 wf3
      | C_13_42, (F123|F214) 
      | C_14_23, (F124|F213) ->
          printf "((%s%s%s+%s))*(%s*%s*%s)*%s*%s)"
            (format_coeff coeff) c pa pb wf1 p1 wf3 wf2 p2
      | C_13_42, (F124|F213) 
      | C_14_23, (F123|F214) ->
          printf "((%s%s%s+%s))*(%s*%s*%s)*%s*%s)"
            (format_coeff coeff) c pa pb wf2 p2 wf3 wf1 p1
      | C_13_42, (F132|F241) 
      | C_14_23, (F142|F231) ->
          printf "((%s%s%s+%s))*(%s*%s*%s)*%s*%s)"
            (format_coeff coeff) c pa pb wf1 p1 wf2 wf3 p3
      | C_13_42, (F142|F231) 
      | C_14_23, (F132|F241) ->
          printf "((%s%s%s+%s))*(%s*%s*%s)*%s*%s)"
            (format_coeff coeff) c pa pb wf3 p3 wf2 wf1 p1
      | C_13_42, (F312|F421) 
      | C_14_23, (F412|F321) ->
          printf "((%s%s%s+%s))*(%s*%s*%s)*%s*%s)"
            (format_coeff coeff) c pa pb wf2 p2 wf1 wf3 p3
      | C_13_42, (F321|F412) 
      | C_14_23, (F421|F312) ->
          printf "((%s%s%s+%s))*(%s*%s*%s)*%s*%s)"
            (format_coeff coeff) c pa pb wf3 p3 wf1 wf2 p2
      | C_13_42, (F134|F243) 
      | C_14_23, (F143|F234) ->
          printf "((%s%s%s+%s))*(%s*%s)*(%s*%s*%s))"
            (format_coeff coeff) c pa pb wf3 p123 wf1 p1 wf2
      | C_13_42, (F143|F234) 
      | C_14_23, (F134|F243) ->
          printf "((%s%s%s+%s))*(%s*%s)*(%s*%s*%s))"
            (format_coeff coeff) c pa pb wf2 p123 wf1 p1 wf3
      | C_13_42, (F314|F423) 
      | C_14_23, (F413|F324) ->
          printf "((%s%s%s+%s))*(%s*%s)*(%s*%s*%s))"
            (format_coeff coeff) c pa pb wf3 p123 wf2 p2 wf1
      | C_13_42, (F324|F413) 
      | C_14_23, (F423|F314) ->
          printf "((%s%s%s+%s))*(%s*%s)*(%s*%s*%s))"
            (format_coeff coeff) c pa pb wf1 p123 wf2 p2 wf3
      | C_13_42, (F341|F432) 
      | C_14_23, (F431|F342) ->
          printf "((%s%s%s+%s))*(%s*%s)*(%s*%s*%s))"
            (format_coeff coeff) c pa pb wf2 p123 wf3 p3 wf1
      | C_13_42, (F342|F431) 
      | C_14_23, (F432|F341) ->
          printf "((%s%s%s+%s))*(%s*%s)*(%s*%s*%s))"
            (format_coeff coeff) c pa pb wf1 p123 wf3 p3 wf2

    let print_add_dscalar2_vector2_km c pa pb wf1 wf2 wf3 p1 p2 p3 p123 fusion (coeff, contraction) =
      printf "@ + ";
      print_dscalar2_vector2_km c pa pb wf1 wf2 wf3 p1 p2 p3 p123 fusion (coeff, contraction)
      
    let print_dscalar2_vector2_m_0_km c pa pb wf1 wf2 wf3 p1 p2 p3 fusion (coeff, contraction) =
      match contraction, fusion with
      | C_12_34, (F123|F213|F124|F214) ->
          printf "@[((%s%s%s+%s))*v_phi2v_m_0(cmplx(1,kind=default),@ %s,%s,%s,%s,%s,%s))@]"
            (format_coeff coeff) c pa pb wf1 p1 wf2 p2 wf3 p3
      | C_12_34, (F134|F143|F234|F243) ->
          printf "@[((%s%s%s+%s))*phi_phi2v_m_0(cmplx(1,kind=default),@ %s,%s,%s,%s,%s,%s))@]"
            (format_coeff coeff) c pa pb wf1 p1 wf2 p2 wf3 p3
      | C_12_34, (F132|F231|F142|F241) ->
          printf "@[((%s%s%s+%s))*v_phi2v_m_0(cmplx(1,kind=default),@ %s,%s,%s,%s,%s,%s))@]"
            (format_coeff coeff) c pa pb wf1 p1 wf3 p3 wf2 p2
      | C_12_34, (F312|F321|F412|F421) ->
          printf "@[((%s%s%s+%s))*v_phi2v_m_0(cmplx(1,kind=default),@ %s,%s,%s,%s,%s,%s))@]"
            (format_coeff coeff) c pa pb wf3 p3 wf2 p2 wf1 p1
      | C_12_34, (F314|F413|F324|F423) ->
          printf "@[((%s%s%s+%s))*phi_phi2v_m_0(cmplx(1,kind=default),@ %s,%s,%s,%s,%s,%s))@]"
            (format_coeff coeff) c pa pb wf2 p2 wf1 p1 wf3 p3
      | C_12_34, (F341|F431|F342|F432) ->
          printf "@[((%s%s%s+%s))*phi_phi2v_m_0(cmplx(1,kind=default),@ %s,%s,%s,%s,%s,%s))@]"
            (format_coeff coeff) c pa pb wf3 p3 wf2 p2 wf1 p1
      | C_13_42, (F123|F214)
      | C_14_23, (F124|F213) ->
          printf "@[((%s%s%s+%s))*v_phi2v_m_0(cmplx(1,kind=default),@ %s,%s,%s,%s,%s,%s))@]"
            (format_coeff coeff) c pa pb wf1 p1 wf2 p3 wf3 p2
      | C_13_42, (F124|F213)
      | C_14_23, (F123|F214) ->
          printf "@[((%s%s%s+%s))*v_phi2v_m_0(cmplx(1,kind=default),@ %s,%s,%s,%s,%s,%s))@]"
            (format_coeff coeff) c pa pb wf2 p2 wf1 p3 wf3 p1
      | C_13_42, (F132|F241)
      | C_14_23, (F142|F231) ->
          printf "@[((%s%s%s+%s))*v_phi2v_m_0(cmplx(1,kind=default),@ %s,%s,%s,%s,%s,%s))@]"
            (format_coeff coeff) c pa pb wf1 p1 wf3 p2 wf2 p3
      | C_13_42, (F142|F231)
      | C_14_23, (F132|F241) ->
          printf "@[((%s%s%s+%s))*v_phi2v_m_0(cmplx(1,kind=default),@ %s,%s,%s,%s,%s,%s))@]"
            (format_coeff coeff) c pa pb wf3 p3 wf1 p2 wf2 p1
      | C_13_42, (F312|F421)
      | C_14_23, (F412|F321) ->
          printf "@[((%s%s%s+%s))*v_phi2v_m_0(cmplx(1,kind=default),@ %s,%s,%s,%s,%s,%s))@]"
            (format_coeff coeff) c pa pb wf2 p2 wf3 p1 wf1 p3
      | C_13_42, (F321|F412)
      | C_14_23, (F421|F312) ->
          printf "@[((%s%s%s+%s))*v_phi2v_m_0(cmplx(1,kind=default),@ %s,%s,%s,%s,%s,%s))@]"
            (format_coeff coeff) c pa pb wf3 p3 wf2 p1 wf1 p2
      | C_13_42, (F134|F243)
      | C_14_23, (F143|F234) ->
          printf "@[((%s%s%s+%s))*phi_phi2v_m_0(cmplx(1,kind=default),@ %s,%s,%s,%s,%s,%s))@]"
            (format_coeff coeff) c pa pb wf1 p3 wf3 p1 wf2 p2
      | C_13_42, (F143|F234)
      | C_14_23, (F134|F243) ->
          printf "@[((%s%s%s+%s))*phi_phi2v_m_0(cmplx(1,kind=default),@ %s,%s,%s,%s,%s,%s))@]"
            (format_coeff coeff) c pa pb wf1 p2 wf2 p1 wf3 p3
      | C_13_42, (F314|F423)
      | C_14_23, (F413|F324) ->
          printf "@[((%s%s%s+%s))*phi_phi2v_m_0(cmplx(1,kind=default),@ %s,%s,%s,%s,%s,%s))@]"
            (format_coeff coeff) c pa pb wf2 p3 wf3 p2 wf1 p1
      | C_13_42, (F324|F413)
      | C_14_23, (F423|F314) ->
          printf "@[((%s%s%s+%s))*phi_phi2v_m_0(cmplx(1,kind=default),@ %s,%s,%s,%s,%s,%s))@]"
            (format_coeff coeff) c pa pb wf2 p1 wf1 p2 wf3 p3
      | C_13_42, (F341|F432)
      | C_14_23, (F431|F342) ->
          printf "@[((%s%s%s+%s))*phi_phi2v_m_0(cmplx(1,kind=default),@ %s,%s,%s,%s,%s,%s))@]"
            (format_coeff coeff) c pa pb wf3 p2 wf2 p3 wf1 p1
      | C_13_42, (F342|F431)
      | C_14_23, (F432|F341) ->
          printf "@[((%s%s%s+%s))*phi_phi2v_m_0(cmplx(1,kind=default),@ %s,%s,%s,%s,%s,%s))@]"
            (format_coeff coeff) c pa pb wf3 p1 wf1 p3 wf2 p2

    let print_add_dscalar2_vector2_m_0_km c pa pb wf1 wf2 wf3 p1 p2 p3 fusion (coeff, contraction) =
      printf "@ + ";
      print_dscalar2_vector2_m_0_km c pa pb wf1 wf2 wf3 p1 p2 p3 fusion (coeff, contraction)

   let print_dscalar2_vector2_m_1_km c pa pb wf1 wf2 wf3 p1 p2 p3 fusion (coeff, contraction) =
      match contraction, fusion with
      | C_12_34, (F123|F213|F124|F214) ->
          printf "@[((%s%s%s+%s))*v_phi2v_m_1(cmplx(1,kind=default),@ %s,%s,%s,%s,%s,%s))@]"
            (format_coeff coeff) c pa pb wf1 p1 wf2 p2 wf3 p3
      | C_12_34, (F134|F143|F234|F243) ->
          printf "@[((%s%s%s+%s))*phi_phi2v_m_1(cmplx(1,kind=default),@ %s,%s,%s,%s,%s,%s))@]"
            (format_coeff coeff) c pa pb wf1 p1 wf2 p2 wf3 p3
      | C_12_34, (F132|F231|F142|F241) ->
          printf "@[((%s%s%s+%s))*v_phi2v_m_1(cmplx(1,kind=default),@ %s,%s,%s,%s,%s,%s))@]"
            (format_coeff coeff) c pa pb wf1 p1 wf3 p3 wf2 p2
      | C_12_34, (F312|F321|F412|F421) ->
          printf "@[((%s%s%s+%s))*v_phi2v_m_1(cmplx(1,kind=default),@ %s,%s,%s,%s,%s,%s))@]"
            (format_coeff coeff) c pa pb wf3 p3 wf2 p2 wf1 p1
      | C_12_34, (F314|F413|F324|F423) ->
          printf "@[((%s%s%s+%s))*phi_phi2v_m_1(cmplx(1,kind=default),@ %s,%s,%s,%s,%s,%s))@]"
            (format_coeff coeff) c pa pb wf2 p2 wf1 p1 wf3 p3
      | C_12_34, (F341|F431|F342|F432) ->
          printf "@[((%s%s%s+%s))*phi_phi2v_m_1(cmplx(1,kind=default),@ %s,%s,%s,%s,%s,%s))@]"
            (format_coeff coeff) c pa pb wf3 p3 wf2 p2 wf1 p1
      | C_13_42, (F123|F214)
      | C_14_23, (F124|F213) ->
          printf "@[((%s%s%s+%s))*v_phi2v_m_1(cmplx(1,kind=default),@ %s,%s,%s,%s,%s,%s))@]"
            (format_coeff coeff) c pa pb wf1 p1 wf2 p3 wf3 p2
      | C_13_42, (F124|F213)
      | C_14_23, (F123|F214) ->
          printf "@[((%s%s%s+%s))*v_phi2v_m_1(cmplx(1,kind=default),@ %s,%s,%s,%s,%s,%s))@]"
            (format_coeff coeff) c pa pb wf2 p2 wf1 p3 wf3 p1
      | C_13_42, (F132|F241)
      | C_14_23, (F142|F231) ->
          printf "@[((%s%s%s+%s))*v_phi2v_m_1(cmplx(1,kind=default),@ %s,%s,%s,%s,%s,%s))@]"
            (format_coeff coeff) c pa pb wf1 p1 wf3 p2 wf2 p3
      | C_13_42, (F142|F231)
      | C_14_23, (F132|F241) ->
          printf "@[((%s%s%s+%s))*v_phi2v_m_1(cmplx(1,kind=default),@ %s,%s,%s,%s,%s,%s))@]"
            (format_coeff coeff) c pa pb wf3 p3 wf1 p2 wf2 p1
      | C_13_42, (F312|F421)
      | C_14_23, (F412|F321) ->
          printf "@[((%s%s%s+%s))*v_phi2v_m_1(cmplx(1,kind=default),@ %s,%s,%s,%s,%s,%s))@]"
            (format_coeff coeff) c pa pb wf2 p2 wf3 p1 wf1 p3
      | C_13_42, (F321|F412)
      | C_14_23, (F421|F312) ->
          printf "@[((%s%s%s+%s))*v_phi2v_m_1(cmplx(1,kind=default),@ %s,%s,%s,%s,%s,%s))@]"
            (format_coeff coeff) c pa pb wf3 p3 wf2 p1 wf1 p2
      | C_13_42, (F134|F243)
      | C_14_23, (F143|F234) ->
          printf "@[((%s%s%s+%s))*phi_phi2v_m_1(cmplx(1,kind=default),@ %s,%s,%s,%s,%s,%s))@]"
            (format_coeff coeff) c pa pb wf1 p3 wf3 p1 wf2 p2
      | C_13_42, (F143|F234)
      | C_14_23, (F134|F243) ->
          printf "@[((%s%s%s+%s))*phi_phi2v_m_1(cmplx(1,kind=default),@ %s,%s,%s,%s,%s,%s))@]"
            (format_coeff coeff) c pa pb wf1 p2 wf2 p1 wf3 p3
      | C_13_42, (F314|F423)
      | C_14_23, (F413|F324) ->
          printf "@[((%s%s%s+%s))*phi_phi2v_m_1(cmplx(1,kind=default),@ %s,%s,%s,%s,%s,%s))@]"
            (format_coeff coeff) c pa pb wf2 p3 wf3 p2 wf1 p1
      | C_13_42, (F324|F413)
      | C_14_23, (F423|F314) ->
          printf "@[((%s%s%s+%s))*phi_phi2v_m_1(cmplx(1,kind=default),@ %s,%s,%s,%s,%s,%s))@]"
            (format_coeff coeff) c pa pb wf2 p1 wf1 p2 wf3 p3
      | C_13_42, (F341|F432)
      | C_14_23, (F431|F342) ->
          printf "@[((%s%s%s+%s))*phi_phi2v_m_1(cmplx(1,kind=default),@ %s,%s,%s,%s,%s,%s))@]"
            (format_coeff coeff) c pa pb wf3 p2 wf2 p3 wf1 p1
      | C_13_42, (F342|F431)
      | C_14_23, (F432|F341) ->
          printf "@[((%s%s%s+%s))*phi_phi2v_m_1(cmplx(1,kind=default),@ %s,%s,%s,%s,%s,%s))@]"
            (format_coeff coeff) c pa pb wf3 p1 wf1 p3 wf2 p2

    let print_add_dscalar2_vector2_m_1_km c pa pb wf1 wf2 wf3 p1 p2 p3 fusion (coeff, contraction) =
      printf "@ + ";
      print_dscalar2_vector2_m_1_km c pa pb wf1 wf2 wf3 p1 p2 p3 fusion (coeff, contraction)
      
   let print_dscalar2_vector2_m_7_km c pa pb wf1 wf2 wf3 p1 p2 p3 fusion (coeff, contraction) =
      match contraction, fusion with
      | C_12_34, (F123|F213|F124|F214) ->
          printf "@[((%s%s%s+%s))*v_phi2v_m_7(cmplx(1,kind=default),@ %s,%s,%s,%s,%s,%s))@]"
            (format_coeff coeff) c pa pb wf1 p1 wf2 p2 wf3 p3
      | C_12_34, (F134|F143|F234|F243) ->
          printf "@[((%s%s%s+%s))*phi_phi2v_m_7(cmplx(1,kind=default),@ %s,%s,%s,%s,%s,%s))@]"
            (format_coeff coeff) c pa pb wf1 p1 wf2 p2 wf3 p3
      | C_12_34, (F132|F231|F142|F241) ->
          printf "@[((%s%s%s+%s))*v_phi2v_m_7(cmplx(1,kind=default),@ %s,%s,%s,%s,%s,%s))@]"
            (format_coeff coeff) c pa pb wf1 p1 wf3 p3 wf2 p2
      | C_12_34, (F312|F321|F412|F421) ->
          printf "@[((%s%s%s+%s))*v_phi2v_m_7(cmplx(1,kind=default),@ %s,%s,%s,%s,%s,%s))@]"
            (format_coeff coeff) c pa pb wf3 p3 wf2 p2 wf1 p1
      | C_12_34, (F314|F413|F324|F423) ->
          printf "@[((%s%s%s+%s))*phi_phi2v_m_7(cmplx(1,kind=default),@ %s,%s,%s,%s,%s,%s))@]"
            (format_coeff coeff) c pa pb wf2 p2 wf1 p1 wf3 p3
      | C_12_34, (F341|F431|F342|F432) ->
          printf "@[((%s%s%s+%s))*phi_phi2v_m_7(cmplx(1,kind=default),@ %s,%s,%s,%s,%s,%s))@]"
            (format_coeff coeff) c pa pb wf3 p3 wf2 p2 wf1 p1
      | C_13_42, (F123|F214)
      | C_14_23, (F124|F213) ->
          printf "@[((%s%s%s+%s))*v_phi2v_m_7(cmplx(1,kind=default),@ %s,%s,%s,%s,%s,%s))@]"
            (format_coeff coeff) c pa pb wf1 p1 wf2 p3 wf3 p2
      | C_13_42, (F124|F213)
      | C_14_23, (F123|F214) ->
          printf "@[((%s%s%s+%s))*v_phi2v_m_7(cmplx(1,kind=default),@ %s,%s,%s,%s,%s,%s))@]"
            (format_coeff coeff) c pa pb wf2 p2 wf1 p3 wf3 p1
      | C_13_42, (F132|F241)
      | C_14_23, (F142|F231) ->
          printf "@[((%s%s%s+%s))*v_phi2v_m_7(cmplx(1,kind=default),@ %s,%s,%s,%s,%s,%s))@]"
            (format_coeff coeff) c pa pb wf1 p1 wf3 p2 wf2 p3
      | C_13_42, (F142|F231)
      | C_14_23, (F132|F241) ->
          printf "@[((%s%s%s+%s))*v_phi2v_m_7(cmplx(1,kind=default),@ %s,%s,%s,%s,%s,%s))@]"
            (format_coeff coeff) c pa pb wf3 p3 wf1 p2 wf2 p1
      | C_13_42, (F312|F421)
      | C_14_23, (F412|F321) ->
          printf "@[((%s%s%s+%s))*v_phi2v_m_7(cmplx(1,kind=default),@ %s,%s,%s,%s,%s,%s))@]"
            (format_coeff coeff) c pa pb wf2 p2 wf3 p1 wf1 p3
      | C_13_42, (F321|F412)
      | C_14_23, (F421|F312) ->
          printf "@[((%s%s%s+%s))*v_phi2v_m_7(cmplx(1,kind=default),@ %s,%s,%s,%s,%s,%s))@]"
            (format_coeff coeff) c pa pb wf3 p3 wf2 p1 wf1 p2
      | C_13_42, (F134|F243)
      | C_14_23, (F143|F234) ->
          printf "@[((%s%s%s+%s))*phi_phi2v_m_7(cmplx(1,kind=default),@ %s,%s,%s,%s,%s,%s))@]"
            (format_coeff coeff) c pa pb wf1 p3 wf3 p1 wf2 p2
      | C_13_42, (F143|F234)
      | C_14_23, (F134|F243) ->
          printf "@[((%s%s%s+%s))*phi_phi2v_m_7(cmplx(1,kind=default),@ %s,%s,%s,%s,%s,%s))@]"
            (format_coeff coeff) c pa pb wf1 p2 wf2 p1 wf3 p3
      | C_13_42, (F314|F423)
      | C_14_23, (F413|F324) ->
          printf "@[((%s%s%s+%s))*phi_phi2v_m_7(cmplx(1,kind=default),@ %s,%s,%s,%s,%s,%s))@]"
            (format_coeff coeff) c pa pb wf2 p3 wf3 p2 wf1 p1
      | C_13_42, (F324|F413)
      | C_14_23, (F423|F314) ->
          printf "@[((%s%s%s+%s))*phi_phi2v_m_7(cmplx(1,kind=default),@ %s,%s,%s,%s,%s,%s))@]"
            (format_coeff coeff) c pa pb wf2 p1 wf1 p2 wf3 p3
      | C_13_42, (F341|F432)
      | C_14_23, (F431|F342) ->
          printf "@[((%s%s%s+%s))*phi_phi2v_m_7(cmplx(1,kind=default),@ %s,%s,%s,%s,%s,%s))@]"
            (format_coeff coeff) c pa pb wf3 p2 wf2 p3 wf1 p1
      | C_13_42, (F342|F431)
      | C_14_23, (F432|F341) ->
          printf "@[((%s%s%s+%s))*phi_phi2v_m_7(cmplx(1,kind=default),@ %s,%s,%s,%s,%s,%s))@]"
            (format_coeff coeff) c pa pb wf3 p1 wf1 p3 wf2 p2

    let print_add_dscalar2_vector2_m_7_km c pa pb wf1 wf2 wf3 p1 p2 p3 fusion (coeff, contraction) =
      printf "@ + ";
      print_dscalar2_vector2_m_7_km c pa pb wf1 wf2 wf3 p1 p2 p3 fusion (coeff, contraction)  

    let print_dscalar4_km c pa pb wf1 wf2 wf3 p1 p2 p3 p123 fusion (coeff, contraction) =
      match contraction, fusion with
      | C_12_34, (F341|F431|F342|F432|F123|F213|F124|F214)
      | C_13_42, (F241|F421|F243|F423|F132|F312|F134|F314)
      | C_14_23, (F231|F321|F234|F324|F142|F412|F143|F413) ->
          printf "((%s%s%s+%s))*(%s*%s)*(%s*%s)*%s*%s*%s)"
            (format_coeff coeff) c pa pb p1 p2 p3 p123 wf1 wf2 wf3
      | C_12_34, (F134|F143|F234|F243|F312|F321|F412|F421)
      | C_13_42, (F124|F142|F324|F342|F213|F231|F413|F431)
      | C_14_23, (F123|F132|F423|F432|F214|F241|F314|F341) ->
          printf "((%s%s%s+%s))*(%s*%s)*(%s*%s)*%s*%s*%s)"
            (format_coeff coeff) c pa pb p2 p3 p1 p123 wf1 wf2 wf3
      | C_12_34, (F314|F413|F324|F423|F132|F231|F142|F241)
      | C_13_42, (F214|F412|F234|F432|F123|F321|F143|F341)
      | C_14_23, (F213|F312|F243|F342|F124|F421|F134|F431) ->
          printf "((%s%s%s+%s))*(%s*%s)*(%s*%s)*%s*%s*%s)"
            (format_coeff coeff) c pa pb p1 p3 p2 p123 wf1 wf2 wf3

    let print_add_dscalar4_km c pa pb wf1 wf2 wf3 p1 p2 p3 p123 fusion (coeff, contraction) =
      printf "@ + ";
      print_dscalar4_km c pa pb wf1 wf2 wf3 p1 p2 p3 p123 fusion (coeff, contraction)

    let print_current_V3 format_wf format_p amplitude dictionary rhs vertex fusion constant =
      let ch1, ch2 = children2 rhs in
      let wf1 = format_wf amplitude dictionary ch1
      and wf2 = format_wf amplitude dictionary ch2
      and p1 = format_p ch1
      and p2 = format_p ch2
      and m1 = SCM.mass_symbol (F.flavor ch1)
      and m2 = SCM.mass_symbol (F.flavor ch2) in
      let c = SCM.constant_symbol constant in
      printf "@, %s " (if (F.sign rhs) < 0 then "-" else "+");
      begin match vertex with

      (* Fermionic currents $\bar\psi\fmslash{A}\psi$ and $\bar\psi\phi\psi$
   are handled by the [Fermions] module, since they depend on the
   choice of Feynman rules: Dirac or Majorana. *)

      | FBF (coeff, fb, b, f) ->
         begin match coeff, fb, b, f with
         | _, _, (VLRM|SPM|VAM|VA3M|TVA|TVAM|TLR|TLRM|TRL|TRLM), _ ->
            let p12 = Printf.sprintf "(-%s-%s)" p1 p2 in
            Fermions.print_current_mom (coeff, fb, b, f) c wf1 wf2 p1 p2
              p12 fusion
         | _, _, _, _ ->
            Fermions.print_current (coeff, fb, b, f) c wf1 wf2 fusion
         end
      | PBP (coeff, f1, b, f2) ->
         Fermions.print_current_p (coeff, f1, b, f2) c wf1 wf2 fusion
      | BBB (coeff, fb1, b, fb2) ->
         Fermions.print_current_b (coeff, fb1, b, fb2) c wf1 wf2 fusion
      | GBG (coeff, fb, b, f) ->
         let p12 = Printf.sprintf "(-%s-%s)" p1 p2 in
         Fermions.print_current_g (coeff, fb, b, f) c wf1 wf2 p1 p2 p12 fusion

      (* Table~\ref{tab:dim4-bosons} is a bit misleading, since if includes
   totally antisymmetric structure constants.  The space-time part alone
   is also totally antisymmetric: *)

      | Gauge_Gauge_Gauge coeff ->
         let c = format_coupling coeff c in
         begin match fusion with
         | (F23|F31|F12) -> printf "g_gg(%s,%s,%s,%s,%s)" c wf1 p1 wf2 p2
         | (F32|F13|F21) -> printf "g_gg(%s,%s,%s,%s,%s)" c wf2 p2 wf1 p1
         end

      | I_Gauge_Gauge_Gauge coeff ->
         let c = format_coupling coeff c in
         begin match fusion with
         | (F23|F31|F12) -> printf "g_gg((0,1)*(%s),%s,%s,%s,%s)" c wf1 p1 wf2 p2
         | (F32|F13|F21) -> printf "g_gg((0,1)*(%s),%s,%s,%s,%s)" c wf2 p2 wf1 p1
         end

      (* In [Aux_Gauge_Gauge], we can not rely on antisymmetry alone, because of the
   different Lorentz representations of the auxialiary and the gauge field.
   Instead we have to provide the sign in
   \begin{equation}
     (V_2 \wedge V_3) \cdot T_1 =
       \begin{cases}
          V_2 \cdot (T_1 \cdot V_3) = - V_2 \cdot (V_3 \cdot T_1) & \\
          V_3 \cdot (V_2 \cdot T_1) = - V_3 \cdot (T_1 \cdot V_2) &
       \end{cases}
   \end{equation}
   ourselves. Alternatively, one could provide \verb+g_xg+ mirroring
   \verb+g_gx+. *)

      | Aux_Gauge_Gauge coeff ->
         let c = format_coupling coeff c in
         begin match fusion with
         | F23 -> printf "x_gg(%s,%s,%s)" c wf1 wf2
         | F32 -> printf "x_gg(%s,%s,%s)" c wf2 wf1
         | F12 -> printf "g_gx(%s,%s,%s)" c wf2 wf1
         | F21 -> printf "g_gx(%s,%s,%s)" c wf1 wf2
         | F13 -> printf "(-1)*g_gx(%s,%s,%s)" c wf2 wf1
         | F31 -> printf "(-1)*g_gx(%s,%s,%s)" c wf1 wf2
         end

      (* These cases are symmetric and we just have to juxtapose the correct fields
   and provide parentheses to minimize the number of multiplications. *)

      | Scalar_Vector_Vector coeff ->
         let c = format_coupling coeff c in
         begin match fusion with
         | (F23|F32) -> printf "%s*(%s*%s)" c wf1 wf2
         | (F12|F13) -> printf "(%s*%s)*%s" c wf1 wf2
         | (F21|F31) -> printf "(%s*%s)*%s" c wf2 wf1
         end

      | Aux_Vector_Vector coeff ->
         let c = format_coupling coeff c in
         begin match fusion with
         | (F23|F32) -> printf "%s*(%s*%s)" c wf1 wf2
         | (F12|F13) -> printf "(%s*%s)*%s" c wf1 wf2
         | (F21|F31) -> printf "(%s*%s)*%s" c wf2 wf1
         end

      (* Even simpler: *)

      | Scalar_Scalar_Scalar coeff ->
         printf "(%s*%s*%s)" (format_coupling coeff c) wf1 wf2

      | Aux_Scalar_Scalar coeff ->
         printf "(%s*%s*%s)" (format_coupling coeff c) wf1 wf2

      | Aux_Scalar_Vector coeff ->
         let c = format_coupling coeff c in
         begin match fusion with
         | (F13|F31) -> printf "%s*(%s*%s)" c wf1 wf2
         | (F23|F21) -> printf "(%s*%s)*%s" c wf1 wf2
         | (F32|F12) -> printf "(%s*%s)*%s" c wf2 wf1
         end

      | Vector_Scalar_Scalar coeff ->
         let c = format_coupling coeff c in
         begin match fusion with
         | F23 -> printf "v_ss(%s,%s,%s,%s,%s)" c wf1 p1 wf2 p2
         | F32 -> printf "v_ss(%s,%s,%s,%s,%s)" c wf2 p2 wf1 p1
         | F12 -> printf "s_vs(%s,%s,%s,%s,%s)" c wf1 p1 wf2 p2
         | F21 -> printf "s_vs(%s,%s,%s,%s,%s)" c wf2 p2 wf1 p1
         | F13 -> printf "(-1)*s_vs(%s,%s,%s,%s,%s)" c wf1 p1 wf2 p2
         | F31 -> printf "(-1)*s_vs(%s,%s,%s,%s,%s)" c wf2 p2 wf1 p1
         end

      | Graviton_Scalar_Scalar coeff ->
         let c = format_coupling coeff c in
         begin match fusion with
         | F12 -> printf "s_gravs(%s,%s,-(%s+%s),%s,%s,%s)" c m2 p1 p2 p2 wf1 wf2
         | F21 -> printf "s_gravs(%s,%s,-(%s+%s),%s,%s,%s)" c m1 p1 p2 p1 wf2 wf1
         | F13 -> printf "s_gravs(%s,%s,%s,-(%s+%s),%s,%s)" c m2 p2 p1 p2 wf1 wf2
         | F31 -> printf "s_gravs(%s,%s,%s,-(%s+%s),%s,%s)" c m1 p1 p1 p2 wf2 wf1
         | F23 -> printf "grav_ss(%s,%s,%s,%s,%s,%s)" c m1 p1 p2 wf1 wf2
         | F32 -> printf "grav_ss(%s,%s,%s,%s,%s,%s)" c m1 p2 p1 wf2 wf1
         end

      (* In producing a vector in the fusion we always contract the rightmost index with the
         vector wavefunction from [rhs]. So the first momentum is always the one of the
         vector boson produced in the fusion, while the second one is that from the [rhs].
         This makes the cases [F12] and [F13] as well as [F21] and [F31] equal. In principle,
         we could have already done this for the [Graviton_Scalar_Scalar] case. *)


      | Graviton_Vector_Vector coeff ->
         let c = format_coupling coeff c in
         begin match fusion with
         | (F12|F13) -> printf "v_gravv(%s,%s,-(%s+%s),%s,%s,%s)" c m2 p1 p2 p2 wf1 wf2
         | (F21|F31) -> printf "v_gravv(%s,%s,-(%s+%s),%s,%s,%s)" c m1 p1 p2 p1 wf2 wf1
         | F23 -> printf "grav_vv(%s,%s,%s,%s,%s,%s)" c m1 p1 p2 wf1 wf2
         | F32 -> printf "grav_vv(%s,%s,%s,%s,%s,%s)" c m1 p2 p1 wf2 wf1
         end

      | Graviton_Spinor_Spinor coeff ->
         let c = format_coupling coeff c in
         begin match fusion with
         | F23 -> printf "f_gravf(%s,%s,-(%s+%s),(-%s),%s,%s)" c m2 p1 p2 p2 wf1 wf2
         | F32 -> printf "f_gravf(%s,%s,-(%s+%s),(-%s),%s,%s)" c m1 p1 p2 p1 wf2 wf1
         | F12 -> printf "f_fgrav(%s,%s,%s,%s+%s,%s,%s)" c m1 p1 p1 p2 wf1 wf2
         | F21 -> printf "f_fgrav(%s,%s,%s,%s+%s,%s,%s)" c m2 p2 p1 p2 wf2 wf1
         | F13 -> printf "grav_ff(%s,%s,%s,(-%s),%s,%s)" c m1 p1 p2 wf1 wf2
         | F31 -> printf "grav_ff(%s,%s,%s,(-%s),%s,%s)" c m1 p2 p1 wf2 wf1
         end

      | Dim4_Vector_Vector_Vector_T coeff ->
         let c = format_coupling coeff c in
         begin match fusion with
         | F23 -> printf "tkv_vv(%s,%s,%s,%s,%s)" c wf1 p1 wf2 p2
         | F32 -> printf "tkv_vv(%s,%s,%s,%s,%s)" c wf2 p2 wf1 p1
         | F12 -> printf "tv_kvv(%s,%s,%s,%s,%s)" c wf1 p1 wf2 p2
         | F21 -> printf "tv_kvv(%s,%s,%s,%s,%s)" c wf2 p2 wf1 p1
         | F13 -> printf "(-1)*tv_kvv(%s,%s,%s,%s,%s)" c wf1 p1 wf2 p2
         | F31 -> printf "(-1)*tv_kvv(%s,%s,%s,%s,%s)" c wf2 p2 wf1 p1
         end

      | Dim4_Vector_Vector_Vector_L coeff ->
         let c = format_coupling coeff c in
         begin match fusion with
         | F23 -> printf "lkv_vv(%s,%s,%s,%s,%s)" c wf1 p1 wf2 p2
         | F32 -> printf "lkv_vv(%s,%s,%s,%s,%s)" c wf2 p2 wf1 p1
         | F12 | F13 -> printf "lv_kvv(%s,%s,%s,%s)" c wf1 p1 wf2
         | F21 | F31 -> printf "lv_kvv(%s,%s,%s,%s)" c wf2 p2 wf1
         end

      | Dim6_Gauge_Gauge_Gauge coeff ->
         let c = format_coupling coeff c in
         begin match fusion with
         | F23 | F31 | F12 -> printf "kg_kgkg(%s,%s,%s,%s,%s)" c wf1 p1 wf2 p2
         | F32 | F13 | F21 -> printf "kg_kgkg(%s,%s,%s,%s,%s)" c wf2 p2 wf1 p1
         end

      | Dim4_Vector_Vector_Vector_T5 coeff ->
         let c = format_coupling coeff c in
         begin match fusion with
         | F23 -> printf "t5kv_vv(%s,%s,%s,%s,%s)" c wf1 p1 wf2 p2
         | F32 -> printf "t5kv_vv(%s,%s,%s,%s,%s)" c wf2 p2 wf1 p1
         | F12 | F13 -> printf "t5v_kvv(%s,%s,%s,%s,%s)" c wf1 p1 wf2 p2
         | F21 | F31 -> printf "t5v_kvv(%s,%s,%s,%s,%s)" c wf2 p2 wf1 p1
         end

      | Dim4_Vector_Vector_Vector_L5 coeff ->
         let c = format_coupling coeff c in
         begin match fusion with
         | F23 -> printf "l5kv_vv(%s,%s,%s,%s,%s)" c wf1 p1 wf2 p2
         | F32 -> printf "l5kv_vv(%s,%s,%s,%s,%s)" c wf2 p2 wf1 p1
         | F12 -> printf "l5v_kvv(%s,%s,%s,%s)" c wf1 p1 wf2
         | F21 -> printf "l5v_kvv(%s,%s,%s,%s)" c wf2 p2 wf1
         | F13 -> printf "(-1)*l5v_kvv(%s,%s,%s,%s)" c wf1 p1 wf2
         | F31 -> printf "(-1)*l5v_kvv(%s,%s,%s,%s)" c wf2 p2 wf1
         end

      | Dim6_Gauge_Gauge_Gauge_5 coeff ->
         let c = format_coupling coeff c in
         begin match fusion with
         | F23 -> printf "kg5_kgkg(%s,%s,%s,%s,%s)" c wf1 p1 wf2 p2
         | F32 -> printf "kg5_kgkg(%s,%s,%s,%s,%s)" c wf2 p2 wf1 p1
         | F12 -> printf "kg_kg5kg(%s,%s,%s,%s,%s)" c wf1 p1 wf2 p2
         | F21 -> printf "kg_kg5kg(%s,%s,%s,%s,%s)" c wf2 p2 wf1 p1
         | F13 -> printf "(-1)*kg_kg5kg(%s,%s,%s,%s,%s)" c wf1 p1 wf2 p2
         | F31 -> printf "(-1)*kg_kg5kg(%s,%s,%s,%s,%s)" c wf2 p2 wf1 p1
         end

      | Aux_DScalar_DScalar coeff ->
         let c = format_coupling coeff c in
         begin match fusion with
         | (F23|F32) -> printf "%s*(%s*%s)*(%s*%s)" c p1 p2 wf1 wf2
         | (F12|F13) -> printf "%s*(-((%s+%s)*%s))*(%s*%s)" c p1 p2 p2 wf1 wf2
         | (F21|F31) -> printf "%s*(-((%s+%s)*%s))*(%s*%s)" c p1 p2 p1 wf1 wf2
         end

      | Aux_Vector_DScalar coeff ->
         let c = format_coupling coeff c in
         begin match fusion with
         | F23 -> printf "%s*(%s*%s)*%s" c wf1 p2 wf2
         | F32 -> printf "%s*(%s*%s)*%s" c wf2 p1 wf1
         | F12 -> printf "%s*(-((%s+%s)*%s))*%s" c p1 p2 wf2 wf1
         | F21 -> printf "%s*(-((%s+%s)*%s))*%s" c p1 p2 wf1 wf2
         | (F13|F31) -> printf "(-(%s+%s))*(%s*%s*%s)" p1 p2 c wf1 wf2
         end

      | Dim5_Scalar_Gauge2 coeff ->
         let c = format_coupling coeff c in
         begin match fusion with
         | (F23|F32) ->
            printf "(%s)*((%s*%s)*(%s*%s) - (%s*%s)*(%s*%s))" c p1 wf2 p2 wf1 p1 p2 wf2 wf1
         | (F12|F13) ->
            printf "(%s)*%s*((-((%s+%s)*%s))*%s - ((-(%s+%s)*%s))*%s)" c wf1 p1 p2 wf2 p2 p1 p2 p2 wf2
         | (F21|F31) ->
            printf "(%s)*%s*((-((%s+%s)*%s))*%s - ((-(%s+%s)*%s))*%s)" c wf2 p2 p1 wf1 p1 p1 p2 p1 wf1
         end

      | Dim5_Scalar_Gauge2_Skew coeff ->
         let c = format_coupling coeff c in
         begin match fusion with
         | (F23|F32) -> printf "(- phi_vv (%s, %s, %s, %s, %s))" c p1 p2 wf1 wf2
         | (F12|F13) -> printf "(- v_phiv (%s, %s, %s, %s, %s))" c wf1 p1 p2 wf2
         | (F21|F31) -> printf "v_phiv (%s, %s, %s, %s, %s)" c wf2 p1 p2 wf1
         end

      | Dim5_Scalar_Vector_Vector_T coeff ->
         let c = format_coupling coeff c in
         begin match fusion with
         | (F23|F32) -> printf "(%s)*(%s*%s)*(%s*%s)" c p1 wf2 p2 wf1
         | (F12|F13) -> printf "(%s)*%s*(-((%s+%s)*%s))*%s" c wf1 p1 p2 wf2 p2
         | (F21|F31) -> printf "(%s)*%s*(-((%s+%s)*%s))*%s" c wf2 p2 p1 wf1 p1
         end

      | Dim5_Scalar_Vector_Vector_U coeff ->
         let c = format_coupling coeff c in
         begin match fusion with
         | (F23|F32) -> printf "phi_u_vv (%s, %s, %s, %s, %s)" c p1 p2 wf1 wf2
         | (F12|F13) -> printf "v_u_phiv (%s, %s, %s, %s, %s)" c wf1 p1 p2 wf2
         | (F21|F31) -> printf "v_u_phiv (%s, %s, %s, %s, %s)" c wf2 p2 p1 wf1
         end

      | Dim5_Scalar_Vector_Vector_TU coeff ->
         let c = format_coupling coeff c in
         begin match fusion with
         | F23 -> printf "(%s)*((%s*%s)*(-(%s+%s)*%s) - (-(%s+%s)*%s)*(%s*%s))"
                    c p1 wf2 p1 p2 wf1 p1 p2 p1 wf1 wf2
         | F32 -> printf "(%s)*((%s*%s)*(-(%s+%s)*%s) - (-(%s+%s)*%s)*(%s*%s))"
                    c p2 wf1 p1 p2 wf2 p1 p2 p2 wf1 wf2
         | F12 -> printf "(%s)*%s*((%s*%s)*%s - (%s*%s)*%s)"
                    c wf1 p1 wf2 p2 p1 p2 wf2
         | F21 -> printf "(%s)*%s*((%s*%s)*%s - (%s*%s)*%s)"
                    c wf2 p2 wf1 p1 p1 p2 wf1
         | F13 -> printf "(%s)*%s*((-(%s+%s)*%s)*%s - (-(%s+%s)*%s)*%s)"
                    c wf1 p1 p2 wf2 p1 p1 p2 p1 wf2
         | F31 -> printf "(%s)*%s*((-(%s+%s)*%s)*%s - (-(%s+%s)*%s)*%s)"
                    c wf2 p1 p2 wf1 p2 p1 p2 p2 wf1
         end

      | Dim5_Scalar_Scalar2 coeff->
         let c = format_coupling coeff c in
	 begin match fusion with
	 | (F23|F32) ->
            printf "phi_dim5s2(%s, %s ,%s, %s, %s)" c wf1 p1 wf2 p2 
	 | (F12|F13) ->
            let p12 = Printf.sprintf "(-%s-%s)" p1 p2 in
	    printf "phi_dim5s2(%s,%s,%s,%s,%s)" c wf1 p12 wf2 p2
	 | (F21|F31) ->
            let p12 = Printf.sprintf "(-%s-%s)" p1 p2 in
	    printf "phi_dim5s2(%s,%s,%s,%s,%s)" c wf1 p1 wf2 p12
	 end

      | Scalar_Vector_Vector_t coeff ->
         let c = format_coupling coeff c in
         begin match fusion with
         | (F23|F32) -> printf "s_vv_t(%s,%s,%s,%s,%s)" c wf1 p1 wf2 p2
         | (F12|F13) -> printf "v_sv_t(%s,%s,%s,%s,%s)" c wf1 p1 wf2 p2
         | (F21|F31) -> printf "v_sv_t(%s,%s,%s,%s,%s)" c wf2 p2 wf1 p1
         end

      | Dim6_Vector_Vector_Vector_T coeff ->
         let c = format_coupling coeff c in
         begin match fusion with
         | F23 -> printf "(%s)*(%s*%s)*(%s*%s)*(%s-%s)" c p2 wf1 p1 wf2 p1 p2
         | F32 -> printf "(%s)*(%s*%s)*(%s*%s)*(%s-%s)" c p1 wf2 p2 wf1 p2 p1
         | (F12|F13) ->
            printf "(%s)*((%s+2*%s)*%s)*(-((%s+%s)*%s))*%s" c p1 p2 wf1 p1 p2 wf2 p2
         | (F21|F31) ->
            printf "(%s)*((-((%s+%s)*%s))*(%s+2*%s)*%s)*%s" c p2 p1 wf1 p2 p1 wf2 p1
         end

      | Tensor_2_Vector_Vector coeff ->
         let c = format_coupling coeff c in
         begin match fusion with
         | (F23|F32) -> printf "t2_vv(%s,%s,%s)" c wf1 wf2
         | (F12|F13) -> printf "v_t2v(%s,%s,%s)" c wf1 wf2
         | (F21|F31) -> printf "v_t2v(%s,%s,%s)" c wf2 wf1
         end

      | Tensor_2_Scalar_Scalar coeff->
         let c = format_coupling coeff c in
         begin match fusion with
         | (F23|F32) -> printf "t2_phi2(%s,%s,%s,%s,%s)" c wf1 p1 wf2 p2
         | (F12|F13) -> printf "phi_t2phi(%s,%s,%s,%s,%s)" c wf1 p1 wf2 p2
         | (F21|F31) -> printf "phi_t2phi(%s,%s,%s,%s,%s)" c wf2 p2 wf1 p1
         end

      | Tensor_2_Vector_Vector_1 coeff ->
         let c = format_coupling coeff c in
         begin match fusion with
         | (F23|F32) -> printf "t2_vv_1(%s,%s,%s)" c wf1 wf2
         | (F12|F13) -> printf "v_t2v_1(%s,%s,%s)" c wf1 wf2
         | (F21|F31) -> printf "v_t2v_1(%s,%s,%s)" c wf2 wf1
         end

      | Tensor_2_Vector_Vector_cf coeff->
         let c = format_coupling coeff c in
         begin match fusion with
         | (F23|F32) -> printf "t2_vv_cf(%s,%s,%s)" c wf1 wf2 
         | (F12|F13) -> printf "v_t2v_cf(%s,%s,%s)" c wf1 wf2
         | (F21|F31) -> printf "v_t2v_cf(%s,%s,%s)" c wf2 wf1
         end

      | Tensor_2_Scalar_Scalar_cf coeff->
         let c = format_coupling coeff c in
         begin match fusion with
         | (F23|F32) -> printf "t2_phi2_cf(%s,%s,%s,%s, %s)" c wf1 p1 wf2 p2 
         | (F12|F13) -> printf "phi_t2phi_cf(%s,%s,%s,%s,%s)" c wf1 p1 wf2 p2
         | (F21|F31) -> printf "phi_t2phi_cf(%s,%s,%s,%s,%s)" c wf2 p2 wf1 p1
         end

      | Dim5_Tensor_2_Vector_Vector_1 coeff ->
         let c = format_coupling coeff c in
         begin match fusion with
         | (F23|F32) -> printf "t2_vv_d5_1(%s,%s,%s,%s,%s)" c wf1 p1 wf2 p2
         | (F12|F13) -> printf "v_t2v_d5_1(%s,%s,%s,%s,%s)" c wf1 p1 wf2 p2
         | (F21|F31) -> printf "v_t2v_d5_1(%s,%s,%s,%s,%s)" c wf2 p2 wf1 p1
         end

      | Tensor_2_Vector_Vector_t coeff ->
         let c = format_coupling coeff c in
         begin match fusion with
         | (F23|F32) -> printf "t2_vv_t(%s,%s,%s,%s,%s)" c wf1 p1 wf2 p2
         | (F12|F13) -> printf "v_t2v_t(%s,%s,%s,%s,%s)" c wf1 p1 wf2 p2
         | (F21|F31) -> printf "v_t2v_t(%s,%s,%s,%s,%s)" c wf2 p2 wf1 p1
         end

      | Dim5_Tensor_2_Vector_Vector_2 coeff ->
         let c = format_coupling coeff c in
         begin match fusion with
         | F23 -> printf "t2_vv_d5_2(%s,%s,%s,%s,%s)" c wf1 p1 wf2 p2
         | F32 -> printf "t2_vv_d5_2(%s,%s,%s,%s,%s)" c wf2 p2 wf1 p1
         | (F12|F13) -> printf "v_t2v_d5_2(%s,%s,%s,%s,%s)" c wf1 p1 wf2 p2
         | (F21|F31) -> printf "v_t2v_d5_2(%s,%s,%s,%s,%s)" c wf2 p2 wf1 p1
         end

      | TensorVector_Vector_Vector coeff->
         let c = format_coupling coeff c in
         begin match fusion with
         | (F23|F32) -> printf "dv_vv(%s,%s,%s,%s,%s)" c wf1 p1 wf2 p2 
         | (F12|F13) -> printf "v_dvv(%s,%s,%s,%s)" c wf1 p1 wf2 
         | (F21|F31) -> printf "v_dvv(%s,%s,%s,%s)" c wf2 p2 wf1 
         end

      | TensorVector_Vector_Vector_cf coeff->
         let c = format_coupling coeff c in
         begin match fusion with
         | (F23|F32) -> printf "dv_vv_cf(%s,%s,%s,%s,%s)" c wf1 p1 wf2 p2 
         | (F12|F13) -> printf "v_dvv_cf(%s,%s,%s,%s)" c wf1 p1 wf2
         | (F21|F31) -> printf "v_dvv_cf(%s,%s,%s,%s)" c wf2 p2 wf1
         end

      | TensorVector_Scalar_Scalar coeff->
         let c = format_coupling coeff c in
         begin match fusion with
         | (F23|F32) -> printf "dv_phi2(%s,%s,%s,%s,%s)" c wf1 p1 wf2 p2 
         | (F12|F13) -> printf "phi_dvphi(%s,%s,%s,%s,%s)" c wf1 p1 wf2 p2
         | (F21|F31) -> printf "phi_dvphi(%s,%s,%s,%s,%s)" c wf2 p2 wf1 p1
         end

      | TensorVector_Scalar_Scalar_cf coeff->
         let c = format_coupling coeff c in
         begin match fusion with
         | (F23|F32) -> printf "dv_phi2_cf(%s,%s,%s,%s,%s)" c wf1 p1 wf2 p2 
         | (F12|F13) -> printf "phi_dvphi_cf(%s,%s,%s,%s,%s)" c wf1 p1 wf2 p2
         | (F21|F31) -> printf "phi_dvphi_cf(%s,%s,%s,%s,%s)" c wf2 p2 wf1 p1
         end

      | TensorScalar_Vector_Vector coeff->
         let c = format_coupling coeff c in
         begin match fusion with
         | (F23|F32) -> printf "tphi_vv(%s,%s,%s,%s,%s)" c wf1 p1 wf2 p2 
         | (F12|F13) -> printf "v_tphiv(%s,%s,%s,%s,%s)" c wf1 p1 wf2 p2
         | (F21|F31) -> printf "v_tphiv(%s,%s,%s,%s,%s)" c wf2 p2 wf1 p1
         end

      | TensorScalar_Vector_Vector_cf coeff->
         let c = format_coupling coeff c in
         begin match fusion with
         | (F23|F32) -> printf "tphi_vv_cf(%s,%s,%s,%s,%s)" c wf1 p1 wf2 p2 
         | (F12|F13) -> printf "v_tphiv_cf(%s,%s,%s,%s,%s)" c wf1 p1 wf2 p2
         | (F21|F31) -> printf "v_tphiv_cf(%s,%s,%s,%s,%s)" c wf2 p2 wf1 p1
         end

      | TensorScalar_Scalar_Scalar coeff->
         let c = format_coupling coeff c in
         begin match fusion with
         | (F23|F32) -> printf "tphi_ss(%s,%s,%s,%s,%s)" c wf1 p1 wf2 p2 
         | (F12|F13) -> printf "s_tphis(%s,%s,%s,%s,%s)" c wf1 p1 wf2 p2
         | (F21|F31) -> printf "s_tphis(%s,%s,%s,%s,%s)" c wf2 p2 wf1 p1
         end

      | TensorScalar_Scalar_Scalar_cf coeff->
         let c = format_coupling coeff c in
         begin match fusion with
         | (F23|F32) -> printf "tphi_ss_cf(%s,%s,%s,%s,%s)" c wf1 p1 wf2 p2 
         | (F12|F13) -> printf "s_tphis_cf(%s,%s,%s,%s,%s)" c wf1 p1 wf2 p2
         | (F21|F31) -> printf "s_tphis_cf(%s,%s,%s,%s,%s)" c wf2 p2 wf1 p1
         end

      | Dim7_Tensor_2_Vector_Vector_T coeff ->
         let c = format_coupling coeff c in
         begin match fusion with
         | F23 -> printf "t2_vv_d7(%s,%s,%s,%s,%s)" c wf1 p1 wf2 p2
         | F32 -> printf "t2_vv_d7(%s,%s,%s,%s,%s)" c wf2 p2 wf1 p1
         | (F12|F13) -> printf "v_t2v_d7(%s,%s,%s,%s,%s)" c wf1 p1 wf2 p2
         | (F21|F31) -> printf "v_t2v_d7(%s,%s,%s,%s,%s)" c wf2 p2 wf1 p1
         end

      | Dim6_Scalar_Vector_Vector_D coeff ->
         let c = format_coupling coeff c in
         begin match fusion with
         | (F23|F32) -> printf "s_vv_6D(%s,%s,%s,%s,%s)" c wf1 p1 wf2 p2
         | (F12|F13) -> printf "v_sv_6D(%s,%s,%s,%s,%s)" c wf1 p1 wf2 p2
         | (F21|F31) -> printf "v_sv_6D(%s,%s,%s,%s,%s)" c wf2 p2 wf1 p1
         end

      | Dim6_Scalar_Vector_Vector_DP coeff ->
         let c = format_coupling coeff c in
         begin match fusion with
         | (F23|F32) -> printf "s_vv_6DP(%s,%s,%s,%s,%s)" c wf1 p1 wf2 p2
         | (F12|F13) -> printf "v_sv_6DP(%s,%s,%s,%s,%s)" c wf1 p1 wf2 p2
         | (F21|F31) -> printf "v_sv_6DP(%s,%s,%s,%s,%s)" c wf2 p2 wf1 p1
         end

      | Dim6_HAZ_D coeff ->
         let c = format_coupling coeff c in
         begin match fusion with
         | F23 -> printf "h_az_D(%s,%s,%s,%s,%s)" c wf1 p1 wf2 p2
         | F32 -> printf "h_az_D(%s,%s,%s,%s,%s)" c wf2 p2 wf1 p1
         | F13 -> printf "a_hz_D(%s,%s,%s,%s,%s)" c wf1 p1 wf2 p2
         | F31 -> printf "a_hz_D(%s,%s,%s,%s,%s)" c wf2 p2 wf1 p1
         | F12 -> printf "z_ah_D(%s,%s,%s,%s,%s)" c wf2 p2 wf1 p1
         | F21 -> printf "z_ah_D(%s,%s,%s,%s,%s)" c wf1 p1 wf2 p2
         end
      | Dim6_HAZ_DP coeff ->
         let c = format_coupling coeff c in
         begin match fusion with
         | F23 -> printf "h_az_DP(%s,%s,%s,%s,%s)" c wf1 p1 wf2 p2
         | F32 -> printf "h_az_DP(%s,%s,%s,%s,%s)" c wf2 p2 wf1 p1
         | F13 -> printf "a_hz_DP(%s,%s,%s,%s,%s)" c wf1 p1 wf2 p2
         | F31 -> printf "a_hz_DP(%s,%s,%s,%s,%s)" c wf2 p2 wf1 p1
         | F12 -> printf "z_ah_DP(%s,%s,%s,%s,%s)" c wf2 p2 wf1 p1
         | F21 -> printf "z_ah_DP(%s,%s,%s,%s,%s)" c wf1 p1 wf2 p2
         end
      | Gauge_Gauge_Gauge_i coeff ->
         let c = format_coupling coeff c in
         begin match fusion with
         | F23 -> printf "g_gg_23(%s,%s,%s,%s,%s)" c wf1 p1 wf2 p2
         | F32 -> printf "g_gg_23(%s,%s,%s,%s,%s)" c wf2 p2 wf1 p1
         | F13 -> printf "g_gg_13(%s,%s,%s,%s,%s)" c wf1 p1 wf2 p2
         | F31 -> printf "g_gg_13(%s,%s,%s,%s,%s)" c wf2 p2 wf1 p1
         | F12 -> printf "(-1) * g_gg_13(%s,%s,%s,%s,%s)" c wf1 p1 wf2 p2
         | F21 -> printf "(-1) * g_gg_13(%s,%s,%s,%s,%s)" c wf2 p2 wf1 p1
         end

      | Dim6_GGG coeff ->
         let c = format_coupling coeff c in
         begin match fusion with
         | F23 -> printf "g_gg_6(%s,%s,%s,%s,%s)" c wf1 p1 wf2 p2
         | F32 -> printf "g_gg_6(%s,%s,%s,%s,%s)" c wf2 p2 wf1 p1
         | F12 -> printf "g_gg_6(%s,%s,%s,%s,%s)" c wf1 p1 wf2 p2
         | F21 -> printf "g_gg_6(%s,%s,%s,%s,%s)" c wf2 p2 wf1 p1
         | F13 -> printf "(-1) * g_gg_6(%s,%s,%s,%s,%s)" c wf1 p1 wf2 p2
         | F31 -> printf "(-1) * g_gg_6(%s,%s,%s,%s,%s)" c wf2 p2 wf1 p1
         end 

      | Dim6_AWW_DP coeff ->
         let c = format_coupling coeff c in
         begin match fusion with
         | F23 -> printf "a_ww_DP(%s,%s,%s,%s,%s)" c wf1 p1 wf2 p2
         | F32 -> printf "a_ww_DP(%s,%s,%s,%s,%s)" c wf2 p2 wf1 p1
         | F13 -> printf "w_aw_DP(%s,%s,%s,%s,%s)" c wf1 p1 wf2 p2
         | F31 -> printf "w_aw_DP(%s,%s,%s,%s,%s)" c wf2 p2 wf1 p1
         | F12 -> printf "(-1) * w_aw_DP(%s,%s,%s,%s,%s)" c wf1 p1 wf2 p2
         | F21 -> printf "(-1) * w_aw_DP(%s,%s,%s,%s,%s)" c wf2 p2 wf1 p1
         end

      | Dim6_AWW_DW coeff ->
         let c = format_coupling coeff c in
         begin match fusion with
         | F23 -> printf "a_ww_DW(%s,%s,%s,%s,%s)" c wf1 p1 wf2 p2
         | F32 -> printf "a_ww_DW(%s,%s,%s,%s,%s)" c wf2 p2 wf1 p1
         | F13 -> printf "(-1) * a_ww_DW(%s,%s,%s,%s,%s)" c wf1 p1 wf2 p2
         | F31 -> printf "(-1) * a_ww_DW(%s,%s,%s,%s,%s)" c wf2 p2 wf1 p1
         | F12 -> printf "a_ww_DW(%s,%s,%s,%s,%s)" c wf1 p1 wf2 p2
         | F21 -> printf "a_ww_DW(%s,%s,%s,%s,%s)" c wf2 p2 wf1 p1
         end

      | Dim6_Gauge_Gauge_Gauge_i coeff ->
         let c = format_coupling coeff c in
         begin match fusion with
         | F23 | F31 | F12 ->
            printf "kg_kgkg_i(%s,%s,%s,%s,%s)" c wf1 p1 wf2 p2
         | F32 | F13 | F21 ->
            printf "kg_kgkg_i(%s,%s,%s,%s,%s)" c wf2 p2 wf1 p1
         end

      | Dim6_HHH coeff ->
         let c = format_coupling coeff c in
         begin match fusion with
         | (F23|F32|F12|F21|F13|F31) -> 
	    printf "h_hh_6(%s,%s,%s,%s,%s)" c wf1 p1 wf2 p2
         end

      | Dim6_WWZ_DPWDW coeff ->
         let c = format_coupling coeff c in
         begin match fusion with
         | F23 -> printf "w_wz_DPW(%s,%s,%s,%s,%s)" c wf1 p1 wf2 p2
         | F32 -> printf "w_wz_DPW(%s,%s,%s,%s,%s)" c wf2 p2 wf1 p1
         | F13 -> printf "(-1) * w_wz_DPW(%s,%s,%s,%s,%s)" c wf1 p1 wf2 p2
         | F31 -> printf "(-1) * w_wz_DPW(%s,%s,%s,%s,%s)" c wf2 p2 wf1 p1
         | F12 -> printf "z_ww_DPW(%s,%s,%s,%s,%s)" c wf1 p1 wf2 p2
         | F21 -> printf "z_ww_DPW(%s,%s,%s,%s,%s)" c wf2 p2 wf1 p1
         end
      | Dim6_WWZ_DW coeff ->
         let c = format_coupling coeff c in
         begin match fusion with
         | F23 -> printf "w_wz_DW(%s,%s,%s,%s,%s)" c wf1 p1 wf2 p2
         | F32 -> printf "w_wz_DW(%s,%s,%s,%s,%s)" c wf2 p2 wf1 p1
         | F13 -> printf "(-1) * w_wz_DW(%s,%s,%s,%s,%s)" c wf1 p1 wf2 p2
         | F31 -> printf "(-1) * w_wz_DW(%s,%s,%s,%s,%s)" c wf2 p2 wf1 p1
         | F12 -> printf "z_ww_DW(%s,%s,%s,%s,%s)" c wf1 p1 wf2 p2
         | F21 -> printf "z_ww_DW(%s,%s,%s,%s,%s)" c wf2 p2 wf1 p1
         end
      | Dim6_WWZ_D coeff ->
         let c = format_coupling coeff c in
         begin match fusion with
         | F23 -> printf "w_wz_D(%s,%s,%s,%s,%s)" c wf1 p1 wf2 p2
         | F32 -> printf "w_wz_D(%s,%s,%s,%s,%s)" c wf2 p2 wf1 p1
         | F13 -> printf "(-1) * w_wz_D(%s,%s,%s,%s,%s)" c wf1 p1 wf2 p2
         | F31 -> printf "(-1) * w_wz_D(%s,%s,%s,%s,%s)" c wf2 p2 wf1 p1
         | F12 -> printf "z_ww_D(%s,%s,%s,%s,%s)" c wf1 p1 wf2 p2
         | F21 -> printf "z_ww_D(%s,%s,%s,%s,%s)" c wf2 p2 wf1 p1
         end

      (*i
          | Dim6_Glu_Glu_Glu coeff ->
              let c = format_coupling coeff c in
              begin match fusion with
              | (F23|F31|F12) -> 
                   printf "g_gg_glu(%s,%s,%s,%s,%s)" c wf1 p1 wf2 p2
              | (F32|F13|F21) -> 
                   printf "g_gg_glu(%s,%s,%s,%s,%s)" c wf2 p2 wf1 p1
              end   
i*)

      end

(* Flip the sign to account for the~$\mathrm{i}^2$ relative to diagrams
   with only cubic couplings.
   \label{hack:sign(V4)} *)
(* \begin{dubious}
     That's an \emph{slightly dangerous} hack!!!  How do we accnount
     for such signs when treating $n$-ary vertices uniformly?
   \end{dubious} *)
    let print_current_V4 format_wf format_p amplitude dictionary rhs vertex fusion constant =
      let c = CM.constant_symbol constant
      and ch1, ch2, ch3 = children3 rhs in
      let wf1 = format_wf amplitude dictionary ch1
      and wf2 = format_wf amplitude dictionary ch2
      and wf3 = format_wf amplitude dictionary ch3
      and p1 = format_p ch1
      and p2 = format_p ch2
      and p3 = format_p ch3 in
      printf "@, %s " (if (F.sign rhs) < 0 then "+" else "-");
      begin match vertex with
      | Scalar4 coeff -> printf "(%s*%s*%s*%s)" (format_coupling coeff c) wf1 wf2 wf3
      | Scalar2_Vector2 coeff ->
         let c = format_coupling coeff c in
         begin match fusion with
         | F134 | F143 | F234 | F243 -> printf "%s*%s*(%s*%s)" c wf1 wf2 wf3
         | F314 | F413 | F324 | F423 -> printf "%s*%s*(%s*%s)" c wf2 wf1 wf3
         | F341 | F431 | F342 | F432 -> printf "%s*%s*(%s*%s)" c wf3 wf1 wf2
         | F312 | F321 | F412 | F421 -> printf "(%s*%s*%s)*%s" c wf2 wf3 wf1
         | F231 | F132 | F241 | F142 -> printf "(%s*%s*%s)*%s" c wf1 wf3 wf2
         | F123 | F213 | F124 | F214 -> printf "(%s*%s*%s)*%s" c wf1 wf2 wf3
         end
      | Vector4 contractions ->
         begin match contractions with
         | [] -> invalid_arg "Targets.print_current: Vector4 []"
         | head :: tail ->
            printf "(";
            print_vector4 c wf1 wf2 wf3 fusion head;
            List.iter (print_add_vector4 c wf1 wf2 wf3 fusion) tail;
            printf ")"
         end
      | Dim8_Vector4_t_0 contractions ->
         begin match contractions with
         | [] -> invalid_arg "Targets.print_current: Vector4 []"
         | head :: tail ->
            print_vector4_t_0 c wf1 p1 wf2 p2 wf3 p3 fusion head;
            List.iter (print_add_vector4 c wf1 wf2 wf3 fusion) tail;
         end
      | Dim8_Vector4_t_1 contractions ->
         begin match contractions with
         | [] -> invalid_arg "Targets.print_current: Vector4 []"
         | head :: tail ->
            print_vector4_t_1 c wf1 p1 wf2 p2 wf3 p3 fusion head;
            List.iter (print_add_vector4 c wf1 wf2 wf3 fusion) tail;
         end              
      | Dim8_Vector4_t_2 contractions ->
         begin match contractions with
         | [] -> invalid_arg "Targets.print_current: Vector4 []"
         | head :: tail ->
            print_vector4_t_2 c wf1 p1 wf2 p2 wf3 p3 fusion head;
            List.iter (print_add_vector4 c wf1 wf2 wf3 fusion) tail;
         end
      | Dim8_Vector4_m_0 contractions ->
         begin match contractions with
         | [] -> invalid_arg "Targets.print_current: Vector4 []"
         | head :: tail ->
            print_vector4_m_0 c wf1 p1 wf2 p2 wf3 p3 fusion head;
            List.iter (print_add_vector4 c wf1 wf2 wf3 fusion) tail;
         end
      | Dim8_Vector4_m_1 contractions ->
         begin match contractions with
         | [] -> invalid_arg "Targets.print_current: Vector4 []"
         | head :: tail ->
            print_vector4_m_1 c wf1 p1 wf2 p2 wf3 p3 fusion head;
            List.iter (print_add_vector4 c wf1 wf2 wf3 fusion) tail;
         end
      | Dim8_Vector4_m_7 contractions ->
         begin match contractions with
         | [] -> invalid_arg "Targets.print_current: Vector4 []"
         | head :: tail ->
            print_vector4_m_7 c wf1 p1 wf2 p2 wf3 p3 fusion head;
            List.iter (print_add_vector4 c wf1 wf2 wf3 fusion) tail;
         end    
      | Vector4_K_Matrix_tho (_, poles) ->
         let pa, pb =
           begin match fusion with
           | (F341|F431|F342|F432|F123|F213|F124|F214) -> (p1, p2)
           | (F134|F143|F234|F243|F312|F321|F412|F421) -> (p2, p3)
           | (F314|F413|F324|F423|F132|F231|F142|F241) -> (p1, p3)
           end in
         printf "(%s*(%s*%s)*(%s*%s)*(%s*%s)@,*("
           c p1 wf1 p2 wf2 p3 wf3;
         List.iter (fun (coeff, pole) ->
             printf "+%s/((%s+%s)*(%s+%s)-%s)"
               (SCM.constant_symbol coeff) pa pb pa pb
               (SCM.constant_symbol pole))
           poles;
         printf ")*(-%s-%s-%s))" p1 p2 p3
      | Vector4_K_Matrix_jr (disc, contractions) ->
         let pa, pb =
           begin match disc, fusion with
           | 3, (F143|F413|F142|F412|F321|F231|F324|F234) -> (p1, p2)
           | 3, (F314|F341|F214|F241|F132|F123|F432|F423) -> (p2, p3)
           | 3, (F134|F431|F124|F421|F312|F213|F342|F243) -> (p1, p3)
           | _, (F341|F431|F342|F432|F123|F213|F124|F214) -> (p1, p2)
           | _, (F134|F143|F234|F243|F312|F321|F412|F421) -> (p2, p3)
           | _, (F314|F413|F324|F423|F132|F231|F142|F241) -> (p1, p3)
           end in
         begin match contractions with
         | [] -> invalid_arg "Targets.print_current: Vector4_K_Matrix_jr []"
         | head :: tail ->
            printf "(";
            print_vector4_km c pa pb wf1 wf2 wf3 fusion head;
            List.iter (print_add_vector4_km c pa pb wf1 wf2 wf3 fusion)
              tail;
            printf ")"
         end
      | Vector4_K_Matrix_cf_t0 (disc, contractions) ->
         let pa, pb, pc =
           begin match disc, fusion with
           | 3, (F143|F413|F142|F412|F321|F231|F324|F234) -> (p1, p2, p3)
           | 3, (F314|F341|F214|F241|F132|F123|F432|F423) -> (p2, p3, p1)
           | 3, (F134|F431|F124|F421|F312|F213|F342|F243) -> (p1, p3, p2)
           | _, (F341|F431|F342|F432|F123|F213|F124|F214) -> (p1, p2, p3)
           | _, (F134|F143|F234|F243|F312|F321|F412|F421) -> (p2, p3, p1)
           | _, (F314|F413|F324|F423|F132|F231|F142|F241) -> (p1, p3, p2)
           end in
         begin match contractions with
         | [] -> invalid_arg "Targets.print_current: Vector4_K_Matrix_cf_t0 []"
         | head :: tail ->
            printf "(";
            print_vector4_km_t_0 c pa pb wf1 p1 wf2 p2 wf3 p3 fusion head;
            List.iter (print_add_vector4_km c pa pb wf1 wf2 wf3 fusion) tail;
            printf ")"
         end              
      | Vector4_K_Matrix_cf_t1 (disc, contractions) ->
         let pa, pb =
           begin match disc, fusion with
           | 3, (F143|F413|F142|F412|F321|F231|F324|F234) -> (p1, p2)
           | 3, (F314|F341|F214|F241|F132|F123|F432|F423) -> (p2, p3)
           | 3, (F134|F431|F124|F421|F312|F213|F342|F243) -> (p1, p3)
           | _, (F341|F431|F342|F432|F123|F213|F124|F214) -> (p1, p2)
           | _, (F134|F143|F234|F243|F312|F321|F412|F421) -> (p2, p3)
           | _, (F314|F413|F324|F423|F132|F231|F142|F241) -> (p1, p3)
           end in
         begin match contractions with
         | [] -> invalid_arg "Targets.print_current: Vector4_K_Matrix_cf_t1 []"
         | head :: tail ->
            printf "(";
            print_vector4_km_t_1 c pa pb wf1 p1 wf2 p2 wf3 p3 fusion head;
            List.iter (print_add_vector4_km c pa pb wf1 wf2 wf3 fusion) tail;
            printf ")"
         end              
      | Vector4_K_Matrix_cf_t2 (disc, contractions) ->
         let pa, pb =
           begin match disc, fusion with
           | 3, (F143|F413|F142|F412|F321|F231|F324|F234) -> (p1, p2)
           | 3, (F314|F341|F214|F241|F132|F123|F432|F423) -> (p2, p3)
           | 3, (F134|F431|F124|F421|F312|F213|F342|F243) -> (p1, p3)
           | _, (F341|F431|F342|F432|F123|F213|F124|F214) -> (p1, p2)
           | _, (F134|F143|F234|F243|F312|F321|F412|F421) -> (p2, p3)
           | _, (F314|F413|F324|F423|F132|F231|F142|F241) -> (p1, p3)
           end in
         begin match contractions with
         | [] -> invalid_arg "Targets.print_current: Vector4_K_Matrix_cf_t2 []"
         | head :: tail ->
            printf "(";
            print_vector4_km_t_2 c pa pb wf1 p1 wf2 p2 wf3 p3 fusion head;
            List.iter (print_add_vector4_km c pa pb wf1 wf2 wf3 fusion)
              tail;
            printf ")"
         end
      | Vector4_K_Matrix_cf_t_rsi (disc, contractions) ->
         let pa, pb, pc =
           begin match disc, fusion with
           | 3, (F143|F413|F142|F412|F321|F231|F324|F234) -> (p1, p2, p3)
           | 3, (F314|F341|F214|F241|F132|F123|F432|F423) -> (p2, p3, p1)
           | 3, (F134|F431|F124|F421|F312|F213|F342|F243) -> (p1, p3, p2)
           | _, (F341|F431|F342|F432|F123|F213|F124|F214) -> (p1, p2, p3)
           | _, (F134|F143|F234|F243|F312|F321|F412|F421) -> (p2, p3, p1)
           | _, (F314|F413|F324|F423|F132|F231|F142|F241) -> (p1, p3, p2)
           end in
         begin match contractions with
         | [] -> invalid_arg "Targets.print_current: Vector4_K_Matrix_cf_t_rsi []"
         | head :: tail ->
            printf "(";
            print_vector4_km_t_rsi c pa pb pc wf1 p1 wf2 p2 wf3 p3 fusion head;
            List.iter (print_add_vector4_km c pa pb wf1 wf2 wf3 fusion)
              tail;
            printf ")"
         end              
      | Vector4_K_Matrix_cf_m0 (disc, contractions) ->
         let pa, pb =
           begin match disc, fusion with
           | 3, (F143|F413|F142|F412|F321|F231|F324|F234) -> (p1, p2)
           | 3, (F314|F341|F214|F241|F132|F123|F432|F423) -> (p2, p3)
           | 3, (F134|F431|F124|F421|F312|F213|F342|F243) -> (p1, p3)
           | _, (F341|F431|F342|F432|F123|F213|F124|F214) -> (p1, p2)
           | _, (F134|F143|F234|F243|F312|F321|F412|F421) -> (p2, p3)
           | _, (F314|F413|F324|F423|F132|F231|F142|F241) -> (p1, p3)
           end in
         begin match contractions with
         | [] -> invalid_arg "Targets.print_current: Vector4_K_Matrix_cf_m0 []"
         | head :: tail ->
            printf "(";
            print_vector4_km_m_0 c pa pb wf1 p1 wf2 p2 wf3 p3 fusion head;
            List.iter (print_add_vector4_km c pa pb wf1 wf2 wf3 fusion) tail;
            printf ")"
         end
      | Vector4_K_Matrix_cf_m1 (disc, contractions) ->
         let pa, pb =
           begin match disc, fusion with
           | 3, (F143|F413|F142|F412|F321|F231|F324|F234) -> (p1, p2)
           | 3, (F314|F341|F214|F241|F132|F123|F432|F423) -> (p2, p3)
           | 3, (F134|F431|F124|F421|F312|F213|F342|F243) -> (p1, p3)
           | _, (F341|F431|F342|F432|F123|F213|F124|F214) -> (p1, p2)
           | _, (F134|F143|F234|F243|F312|F321|F412|F421) -> (p2, p3)
           | _, (F314|F413|F324|F423|F132|F231|F142|F241) -> (p1, p3)
           end in
         begin match contractions with
         | [] -> invalid_arg "Targets.print_current: Vector4_K_Matrix_cf_m1 []"
         | head :: tail ->
            printf "(";
            print_vector4_km_m_1 c pa pb wf1 p1 wf2 p2 wf3 p3 fusion head;
            List.iter (print_add_vector4_km c pa pb wf1 wf2 wf3 fusion)
              tail;
            printf ")"
         end
      | Vector4_K_Matrix_cf_m7 (disc, contractions) ->
         let pa, pb =
           begin match disc, fusion with
           | 3, (F143|F413|F142|F412|F321|F231|F324|F234) -> (p1, p2)
           | 3, (F314|F341|F214|F241|F132|F123|F432|F423) -> (p2, p3)
           | 3, (F134|F431|F124|F421|F312|F213|F342|F243) -> (p1, p3)
           | _, (F341|F431|F342|F432|F123|F213|F124|F214) -> (p1, p2)
           | _, (F134|F143|F234|F243|F312|F321|F412|F421) -> (p2, p3)
           | _, (F314|F413|F324|F423|F132|F231|F142|F241) -> (p1, p3)
           end in
         begin match contractions with
         | [] -> invalid_arg "Targets.print_current: Vector4_K_Matrix_cf_m7 []"
         | head :: tail ->
            printf "(";
            print_vector4_km_m_7 c pa pb wf1 p1 wf2 p2 wf3 p3 fusion head;
            List.iter (print_add_vector4_km c pa pb wf1 wf2 wf3 fusion) tail;
            printf ")"
         end    
      | DScalar2_Vector2_K_Matrix_ms (disc, contractions) ->
         let p123 = Printf.sprintf "(-%s-%s-%s)" p1 p2 p3 in
         let pa, pb =
           begin match disc, fusion with
           | 3, (F143|F413|F142|F412|F321|F231|F324|F234) -> (p1, p2)
           | 3, (F314|F341|F214|F241|F132|F123|F432|F423) -> (p2, p3)
           | 3, (F134|F431|F124|F421|F312|F213|F342|F243) -> (p1, p3)
           | 4, (F143|F413|F142|F412|F321|F231|F324|F234) -> (p1, p2)
           | 4, (F314|F341|F214|F241|F132|F123|F432|F423) -> (p2, p3)
           | 4, (F134|F431|F124|F421|F312|F213|F342|F243) -> (p1, p3)
           | 5, (F143|F413|F142|F412|F321|F231|F324|F234) -> (p1, p2)
           | 5, (F314|F341|F214|F241|F132|F123|F432|F423) -> (p2, p3)
           | 5, (F134|F431|F124|F421|F312|F213|F342|F243) -> (p1, p3)
           | 6, (F134|F132|F314|F312|F241|F243|F421|F423) -> (p1, p2)
           | 6, (F213|F413|F231|F431|F124|F324|F142|F342) -> (p2, p3)
           | 6, (F143|F123|F341|F321|F412|F214|F432|F234) -> (p1, p3)
           | 7, (F134|F132|F314|F312|F241|F243|F421|F423) -> (p1, p2)
           | 7, (F213|F413|F231|F431|F124|F324|F142|F342) -> (p2, p3)
           | 7, (F143|F123|F341|F321|F412|F214|F432|F234) -> (p1, p3)
           | 8, (F134|F132|F314|F312|F241|F243|F421|F423) -> (p1, p2)
           | 8, (F213|F413|F231|F431|F124|F324|F142|F342) -> (p2, p3)
           | 8, (F143|F123|F341|F321|F412|F214|F432|F234) -> (p1, p3)
           | _, (F341|F431|F342|F432|F123|F213|F124|F214) -> (p1, p2)
           | _, (F134|F143|F234|F243|F312|F321|F412|F421) -> (p2, p3)
           | _, (F314|F413|F324|F423|F132|F231|F142|F241) -> (p1, p3)
           end in
         begin match contractions with
         | [] -> invalid_arg "Targets.print_current: DScalar2_Vector4_K_Matrix_ms []"
         | head :: tail ->
            printf "(";
            print_dscalar2_vector2_km c pa pb wf1 wf2 wf3 p1 p2 p3 p123 fusion head; 
            List.iter (print_add_dscalar2_vector2_km c pa pb wf1 wf2 wf3 p1 p2 p3 p123 fusion) tail;
            printf ")"
         end
      | DScalar2_Vector2_m_0_K_Matrix_cf (disc, contractions) ->
         let pa, pb =
           begin match disc, fusion with
           | 3, (F143|F413|F142|F412|F321|F231|F324|F234) -> (p1, p2)
           | 3, (F314|F341|F214|F241|F132|F123|F432|F423) -> (p2, p3)
           | 3, (F134|F431|F124|F421|F312|F213|F342|F243) -> (p1, p3)
           | 4, (F143|F413|F142|F412|F321|F231|F324|F234) -> (p1, p2)
           | 4, (F314|F341|F214|F241|F132|F123|F432|F423) -> (p2, p3)
           | 4, (F134|F431|F124|F421|F312|F213|F342|F243) -> (p1, p3)
           | 5, (F143|F413|F142|F412|F321|F231|F324|F234) -> (p1, p2)
           | 5, (F314|F341|F214|F241|F132|F123|F432|F423) -> (p2, p3)
           | 5, (F134|F431|F124|F421|F312|F213|F342|F243) -> (p1, p3)
           | 6, (F134|F132|F314|F312|F241|F243|F421|F423) -> (p1, p2)
           | 6, (F213|F413|F231|F431|F124|F324|F142|F342) -> (p2, p3)
           | 6, (F143|F123|F341|F321|F412|F214|F432|F234) -> (p1, p3)
           | 7, (F134|F132|F314|F312|F241|F243|F421|F423) -> (p1, p2)
           | 7, (F213|F413|F231|F431|F124|F324|F142|F342) -> (p2, p3)
           | 7, (F143|F123|F341|F321|F412|F214|F432|F234) -> (p1, p3)
           | 8, (F134|F132|F314|F312|F241|F243|F421|F423) -> (p1, p2)
           | 8, (F213|F413|F231|F431|F124|F324|F142|F342) -> (p2, p3)
           | 8, (F143|F123|F341|F321|F412|F214|F432|F234) -> (p1, p3)
           | _, (F341|F431|F342|F432|F123|F213|F124|F214) -> (p1, p2)
           | _, (F134|F143|F234|F243|F312|F321|F412|F421) -> (p2, p3)
           | _, (F314|F413|F324|F423|F132|F231|F142|F241) -> (p1, p3)
           end in
         begin match contractions with
         | [] -> invalid_arg "Targets.print_current: DScalar2_Vector4_K_Matrix_cf_m0 []"
         | head :: tail ->
            printf "(";
            print_dscalar2_vector2_m_0_km c pa pb wf1 wf2 wf3 p1 p2 p3 fusion head;
            List.iter (print_add_dscalar2_vector2_m_0_km c pa pb wf1 wf2 wf3 p1 p2 p3 fusion) tail;
            printf ")"
         end
      | DScalar2_Vector2_m_1_K_Matrix_cf (disc, contractions) ->
         let pa, pb =
           begin match disc, fusion with
           | 3, (F143|F413|F142|F412|F321|F231|F324|F234) -> (p1, p2)
           | 3, (F314|F341|F214|F241|F132|F123|F432|F423) -> (p2, p3)
           | 3, (F134|F431|F124|F421|F312|F213|F342|F243) -> (p1, p3)
           | 4, (F143|F413|F142|F412|F321|F231|F324|F234) -> (p1, p2)
           | 4, (F314|F341|F214|F241|F132|F123|F432|F423) -> (p2, p3)
           | 4, (F134|F431|F124|F421|F312|F213|F342|F243) -> (p1, p3)
           | 5, (F143|F413|F142|F412|F321|F231|F324|F234) -> (p1, p2)
           | 5, (F314|F341|F214|F241|F132|F123|F432|F423) -> (p2, p3)
           | 5, (F134|F431|F124|F421|F312|F213|F342|F243) -> (p1, p3)
           | 6, (F134|F132|F314|F312|F241|F243|F421|F423) -> (p1, p2)
           | 6, (F213|F413|F231|F431|F124|F324|F142|F342) -> (p2, p3)
           | 6, (F143|F123|F341|F321|F412|F214|F432|F234) -> (p1, p3)
           | 7, (F134|F132|F314|F312|F241|F243|F421|F423) -> (p1, p2)
           | 7, (F213|F413|F231|F431|F124|F324|F142|F342) -> (p2, p3)
           | 7, (F143|F123|F341|F321|F412|F214|F432|F234) -> (p1, p3)
           | 8, (F134|F132|F314|F312|F241|F243|F421|F423) -> (p1, p2)
           | 8, (F213|F413|F231|F431|F124|F324|F142|F342) -> (p2, p3)
           | 8, (F143|F123|F341|F321|F412|F214|F432|F234) -> (p1, p3)
           | _, (F341|F431|F342|F432|F123|F213|F124|F214) -> (p1, p2)
           | _, (F134|F143|F234|F243|F312|F321|F412|F421) -> (p2, p3)
           | _, (F314|F413|F324|F423|F132|F231|F142|F241) -> (p1, p3)
           end in
         begin match contractions with
         | [] -> invalid_arg "Targets.print_current: DScalar2_Vector4_K_Matrix_cf_m1 []"
         | head :: tail ->
            printf "(";
            print_dscalar2_vector2_m_1_km c pa pb wf1 wf2 wf3 p1 p2 p3 fusion head;
            List.iter (print_add_dscalar2_vector2_m_1_km c pa pb wf1 wf2 wf3 p1 p2 p3 fusion) tail;
            printf ")"
         end
      | DScalar2_Vector2_m_7_K_Matrix_cf (disc, contractions) ->
         let pa, pb =
           begin match disc, fusion with
           | 3, (F143|F413|F142|F412|F321|F231|F324|F234) -> (p1, p2)
           | 3, (F314|F341|F214|F241|F132|F123|F432|F423) -> (p2, p3)
           | 3, (F134|F431|F124|F421|F312|F213|F342|F243) -> (p1, p3)
           | 4, (F143|F413|F142|F412|F321|F231|F324|F234) -> (p1, p2)
           | 4, (F314|F341|F214|F241|F132|F123|F432|F423) -> (p2, p3)
           | 4, (F134|F431|F124|F421|F312|F213|F342|F243) -> (p1, p3)
           | 5, (F143|F413|F142|F412|F321|F231|F324|F234) -> (p1, p2)
           | 5, (F314|F341|F214|F241|F132|F123|F432|F423) -> (p2, p3)
           | 5, (F134|F431|F124|F421|F312|F213|F342|F243) -> (p1, p3)
           | 6, (F134|F132|F314|F312|F241|F243|F421|F423) -> (p1, p2)
           | 6, (F213|F413|F231|F431|F124|F324|F142|F342) -> (p2, p3)
           | 6, (F143|F123|F341|F321|F412|F214|F432|F234) -> (p1, p3)
           | 7, (F134|F132|F314|F312|F241|F243|F421|F423) -> (p1, p2)
           | 7, (F213|F413|F231|F431|F124|F324|F142|F342) -> (p2, p3)
           | 7, (F143|F123|F341|F321|F412|F214|F432|F234) -> (p1, p3)
           | 8, (F134|F132|F314|F312|F241|F243|F421|F423) -> (p1, p2)
           | 8, (F213|F413|F231|F431|F124|F324|F142|F342) -> (p2, p3)
           | 8, (F143|F123|F341|F321|F412|F214|F432|F234) -> (p1, p3)
           | _, (F341|F431|F342|F432|F123|F213|F124|F214) -> (p1, p2)
           | _, (F134|F143|F234|F243|F312|F321|F412|F421) -> (p2, p3)
           | _, (F314|F413|F324|F423|F132|F231|F142|F241) -> (p1, p3)
           end in
         begin match contractions with
         | [] -> invalid_arg "Targets.print_current: DScalar2_Vector4_K_Matrix_cf_m7 []"
         | head :: tail ->
            printf "(";
            print_dscalar2_vector2_m_7_km c pa pb wf1 wf2 wf3 p1 p2 p3 fusion head;
            List.iter (print_add_dscalar2_vector2_m_7_km c pa pb wf1 wf2 wf3 p1 p2 p3 fusion) tail;
            printf ")"
         end    
      | DScalar4_K_Matrix_ms (disc, contractions) ->
         let p123 = Printf.sprintf "(-%s-%s-%s)" p1 p2 p3 in
         let pa, pb =
           begin match disc, fusion with
           | 3, (F143|F413|F142|F412|F321|F231|F324|F234) -> (p1, p2)
           | 3, (F314|F341|F214|F241|F132|F123|F432|F423) -> (p2, p3)
           | 3, (F134|F431|F124|F421|F312|F213|F342|F243) -> (p1, p3)
           | _, (F341|F431|F342|F432|F123|F213|F124|F214) -> (p1, p2)
           | _, (F134|F143|F234|F243|F312|F321|F412|F421) -> (p2, p3)
           | _, (F314|F413|F324|F423|F132|F231|F142|F241) -> (p1, p3)
           end in
         begin match contractions with
         | [] -> invalid_arg "Targets.print_current: DScalar4_K_Matrix_ms []"
         | head :: tail ->
            printf "(";
            print_dscalar4_km c pa pb wf1 wf2 wf3 p1 p2 p3 p123 fusion head; 
            List.iter (print_add_dscalar4_km c pa pb wf1 wf2 wf3 p1 p2 p3 p123 fusion) tail;
            printf ")"
         end
      | Dim8_Scalar2_Vector2_1 coeff ->
         let c = format_coupling coeff c in
         begin match fusion with
         | F134 | F143 | F234 | F243 ->
            printf "phi_phi2v_1(%s,%s,%s,%s,%s,%s,%s)" c wf1 p1 wf2 p2 wf3 p3
         | F314 | F413 | F324 | F423 ->
            printf "phi_phi2v_1(%s,%s,%s,%s,%s,%s,%s)" c wf2 p2 wf1 p1 wf3 p3
         | F341 | F431 | F342 | F432 ->
            printf "phi_phi2v_1(%s,%s,%s,%s,%s,%s,%s)" c wf3 p3 wf2 p2 wf1 p1
         | F312 | F321 | F412 | F421 ->
	    printf "v_phi2v_1(%s,%s,%s,%s,%s,%s)" c wf3 p3 wf2 p2 wf1
         | F231 | F132 | F241 | F142 ->
	    printf "v_phi2v_1(%s,%s,%s,%s,%s,%s)" c wf1 p1 wf3 p3 wf2
         | F123 | F213 | F124 | F214 ->
	    printf "v_phi2v_1(%s,%s,%s,%s,%s,%s)" c wf1 p1 wf2 p2 wf3
         end
      | Dim8_Scalar2_Vector2_2 coeff ->
         let c = format_coupling coeff c in
         begin match fusion with
         | F134 | F143 | F234 | F243 ->
            printf "phi_phi2v_2(%s,%s,%s,%s,%s,%s,%s)" c wf1 p1 wf2 p2 wf3 p3
         | F314 | F413 | F324 | F423 ->
            printf "phi_phi2v_2(%s,%s,%s,%s,%s,%s,%s)" c wf2 p2 wf1 p1 wf3 p3
         | F341 | F431 | F342 | F432 ->
            printf "phi_phi2v_2(%s,%s,%s,%s,%s,%s,%s)" c wf3 p3 wf2 p2 wf1 p1
         | F312 | F321 | F412 | F421 ->
	    printf "v_phi2v_2(%s,%s,%s,%s,%s,%s)" c wf3 p3 wf2 p2 wf1
         | F231 | F132 | F241 | F142 ->
	    printf "v_phi2v_2(%s,%s,%s,%s,%s,%s)" c wf1 p1 wf3 p3 wf2
         | F123 | F213 | F124 | F214 ->
	    printf "v_phi2v_2(%s,%s,%s,%s,%s,%s)" c wf1 p1 wf2 p2 wf3
         end
      | Dim8_Scalar2_Vector2_m_0 coeff ->
         let c = format_coupling coeff c in
         begin match fusion with
         | F134 | F143 | F234 | F243 ->
            printf "phi_phi2v_m_0(%s,%s,%s,%s,%s,%s,%s)" c wf1 p1 wf2 p2 wf3 p3
         | F314 | F413 | F324 | F423 ->
            printf "phi_phi2v_m_0(%s,%s,%s,%s,%s,%s,%s)" c wf2 p2 wf1 p1 wf3 p3
         | F341 | F431 | F342 | F432 ->
            printf "phi_phi2v_m_0(%s,%s,%s,%s,%s,%s,%s)" c wf3 p3 wf2 p2 wf1 p1
         | F312 | F321 | F412 | F421 ->
            printf "v_phi2v_m_0(%s,%s,%s,%s,%s,%s,%s)" c wf3 p3 wf2 p2 wf1 p1
         | F231 | F132 | F241 | F142 ->
            printf "v_phi2v_m_0(%s,%s,%s,%s,%s,%s,%s)" c wf1 p1 wf3 p3 wf2 p2
         | F123 | F213 | F124 | F214 ->
            printf "v_phi2v_m_0(%s,%s,%s,%s,%s,%s,%s)" c wf1 p1 wf2 p2 wf3 p3
         end
      | Dim8_Scalar2_Vector2_m_1 coeff ->
         let c = format_coupling coeff c in
         begin match fusion with
         | F134 | F143 | F234 | F243 ->
            printf "phi_phi2v_m_1(%s,%s,%s,%s,%s,%s,%s)" c wf1 p1 wf2 p2 wf3 p3
         | F314 | F413 | F324 | F423 ->
            printf "phi_phi2v_m_1(%s,%s,%s,%s,%s,%s,%s)" c wf2 p2 wf1 p1 wf3 p3
         | F341 | F431 | F342 | F432 ->
            printf "phi_phi2v_m_1(%s,%s,%s,%s,%s,%s,%s)" c wf3 p3 wf2 p2 wf1 p1
         | F312 | F321 | F412 | F421 ->
            printf "v_phi2v_m_1(%s,%s,%s,%s,%s,%s,%s)" c wf3 p3 wf2 p2 wf1 p1
         | F231 | F132 | F241 | F142 ->
            printf "v_phi2v_m_1(%s,%s,%s,%s,%s,%s,%s)" c wf1 p1 wf3 p3 wf2 p2
         | F123 | F213 | F124 | F214 ->
            printf "v_phi2v_m_1(%s,%s,%s,%s,%s,%s,%s)" c wf1 p1 wf2 p2 wf3 p3
         end
      | Dim8_Scalar2_Vector2_m_7 coeff ->
         let c = format_coupling coeff c in
         begin match fusion with
         | F134 | F143 | F234 | F243 ->
            printf "phi_phi2v_m_7(%s,%s,%s,%s,%s,%s,%s)" c wf1 p1 wf2 p2 wf3 p3
         | F314 | F413 | F324 | F423 ->
            printf "phi_phi2v_m_7(%s,%s,%s,%s,%s,%s,%s)" c wf2 p2 wf1 p1 wf3 p3
         | F341 | F431 | F342 | F432 ->
            printf "phi_phi2v_m_7(%s,%s,%s,%s,%s,%s,%s)" c wf3 p3 wf2 p2 wf1 p1
         | F312 | F321 | F412 | F421 ->
            printf "v_phi2v_m_7(%s,%s,%s,%s,%s,%s,%s)" c wf3 p3 wf2 p2 wf1 p1
         | F231 | F132 | F241 | F142 ->
            printf "v_phi2v_m_7(%s,%s,%s,%s,%s,%s,%s)" c wf1 p1 wf3 p3 wf2 p2
         | F123 | F213 | F124 | F214 ->
            printf "v_phi2v_m_7(%s,%s,%s,%s,%s,%s,%s)" c wf1 p1 wf2 p2 wf3 p3
         end        
      | Dim8_Scalar4 coeff ->
         let c = format_coupling coeff c in
         begin match fusion with
         | F134 | F143 | F234 | F243 | F314 | F413 | F324 | F423
         | F341 | F431 | F342 | F432 | F312 | F321 | F412 | F421
         | F231 | F132 | F241 | F142 | F123 | F213 | F124 | F214 ->
	    printf "s_dim8s3 (%s,%s,%s,%s,%s,%s,%s)" c wf1 p1 wf2 p2 wf3 p3
         end
      | GBBG (coeff, fb, b, f) ->
         Fermions.print_current_g4 (coeff, fb, b, f) c wf1 wf2 wf3 fusion

      | Dim6_H4_P2 coeff ->
         let c = format_coupling coeff c in
         begin match fusion with
         | F134 | F143 | F234 | F243 | F314 | F413 | F324 | F423
         | F341 | F431 | F342 | F432 | F312 | F321 | F412 | F421
         | F231 | F132 | F241 | F142 | F123 | F213 | F124 | F214 ->
	    printf "hhhh_p2 (%s,%s,%s,%s,%s,%s,%s)" c wf1 p1 wf2 p2 wf3 p3
         end

      | Dim6_AHWW_DPB coeff ->
         let c = format_coupling coeff c in
         begin match fusion with
         | F234 -> printf "a_hww_DPB(%s,%s,%s,%s,%s,%s,%s)" c wf1 p1 wf2 p2 wf3 p3
         | F243 -> printf "a_hww_DPB(%s,%s,%s,%s,%s,%s,%s)" c wf1 p1 wf3 p3 wf2 p2
         | F342 -> printf "a_hww_DPB(%s,%s,%s,%s,%s,%s,%s)" c wf3 p3 wf1 p1 wf2 p2
         | F324 -> printf "a_hww_DPB(%s,%s,%s,%s,%s,%s,%s)" c wf2 p2 wf1 p1 wf3 p3
         | F423 -> printf "a_hww_DPB(%s,%s,%s,%s,%s,%s,%s)" c wf2 p2 wf3 p3 wf1 p1
         | F432 -> printf "a_hww_DPB(%s,%s,%s,%s,%s,%s,%s)" c wf3 p3 wf2 p2 wf1 p1
         | F134 -> printf "h_aww_DPB(%s,%s,%s,%s,%s,%s,%s)" c wf1 p1 wf2 p2 wf3 p3
         | F143 -> printf "h_aww_DPB(%s,%s,%s,%s,%s,%s,%s)" c wf1 p1 wf3 p3 wf2 p2
         | F341 -> printf "h_aww_DPB(%s,%s,%s,%s,%s,%s,%s)" c wf3 p3 wf1 p1 wf2 p2
         | F314 -> printf "h_aww_DPB(%s,%s,%s,%s,%s,%s,%s)" c wf2 p2 wf1 p1 wf3 p3
         | F413 -> printf "h_aww_DPB(%s,%s,%s,%s,%s,%s,%s)" c wf2 p2 wf3 p3 wf1 p1
         | F431 -> printf "h_aww_DPB(%s,%s,%s,%s,%s,%s,%s)" c wf3 p3 wf2 p2 wf1 p1
         | F124 -> printf "w_ahw_DPB(%s,%s,%s,%s,%s,%s,%s)" c wf1 p1 wf2 p2 wf3 p3
         | F142 -> printf "w_ahw_DPB(%s,%s,%s,%s,%s,%s,%s)" c wf1 p1 wf3 p3 wf2 p2
         | F241 -> printf "w_ahw_DPB(%s,%s,%s,%s,%s,%s,%s)" c wf3 p3 wf1 p1 wf2 p2
         | F214 -> printf "w_ahw_DPB(%s,%s,%s,%s,%s,%s,%s)" c wf2 p2 wf1 p1 wf3 p3
         | F412 -> printf "w_ahw_DPB(%s,%s,%s,%s,%s,%s,%s)" c wf2 p2 wf3 p3 wf1 p1
         | F421 -> printf "w_ahw_DPB(%s,%s,%s,%s,%s,%s,%s)" c wf3 p3 wf2 p2 wf1 p1
         | F123 -> printf "(-1)*w_ahw_DPB(%s,%s,%s,%s,%s,%s,%s)" c wf1 p1 wf2 p2 wf3 p3
         | F132 -> printf "(-1)*w_ahw_DPB(%s,%s,%s,%s,%s,%s,%s)" c wf1 p1 wf3 p3 wf2 p2
         | F231 -> printf "(-1)*w_ahw_DPB(%s,%s,%s,%s,%s,%s,%s)" c wf3 p3 wf1 p1 wf2 p2
         | F213 -> printf "(-1)*w_ahw_DPB(%s,%s,%s,%s,%s,%s,%s)" c wf2 p2 wf1 p1 wf3 p3
         | F312 -> printf "(-1)*w_ahw_DPB(%s,%s,%s,%s,%s,%s,%s)" c wf2 p2 wf3 p3 wf1 p1
         | F321 -> printf "(-1)*w_ahw_DPB(%s,%s,%s,%s,%s,%s,%s)" c wf3 p3 wf2 p2 wf1 p1
	 end
      | Dim6_AHWW_DPW coeff ->
         let c = format_coupling coeff c in
         begin match fusion with
         | F234 -> printf "a_hww_DPW(%s,%s,%s,%s,%s,%s,%s)" c wf1 p1 wf2 p2 wf3 p3
         | F243 -> printf "a_hww_DPW(%s,%s,%s,%s,%s,%s,%s)" c wf1 p1 wf3 p3 wf2 p2
         | F342 -> printf "a_hww_DPW(%s,%s,%s,%s,%s,%s,%s)" c wf3 p3 wf1 p1 wf2 p2
         | F324 -> printf "a_hww_DPW(%s,%s,%s,%s,%s,%s,%s)" c wf2 p2 wf1 p1 wf3 p3
         | F423 -> printf "a_hww_DPW(%s,%s,%s,%s,%s,%s,%s)" c wf2 p2 wf3 p3 wf1 p1
         | F432 -> printf "a_hww_DPW(%s,%s,%s,%s,%s,%s,%s)" c wf3 p3 wf2 p2 wf1 p1
         | F134 -> printf "h_aww_DPW(%s,%s,%s,%s,%s,%s,%s)" c wf1 p1 wf2 p2 wf3 p3
         | F143 -> printf "h_aww_DPW(%s,%s,%s,%s,%s,%s,%s)" c wf1 p1 wf3 p3 wf2 p2
         | F341 -> printf "h_aww_DPW(%s,%s,%s,%s,%s,%s,%s)" c wf3 p3 wf1 p1 wf2 p2
         | F314 -> printf "h_aww_DPW(%s,%s,%s,%s,%s,%s,%s)" c wf2 p2 wf1 p1 wf3 p3
         | F413 -> printf "h_aww_DPW(%s,%s,%s,%s,%s,%s,%s)" c wf2 p2 wf3 p3 wf1 p1
         | F431 -> printf "h_aww_DPW(%s,%s,%s,%s,%s,%s,%s)" c wf3 p3 wf2 p2 wf1 p1
         | F124 -> printf "w_ahw_DPW(%s,%s,%s,%s,%s,%s,%s)" c wf1 p1 wf2 p2 wf3 p3
         | F142 -> printf "w_ahw_DPW(%s,%s,%s,%s,%s,%s,%s)" c wf1 p1 wf3 p3 wf2 p2
         | F241 -> printf "w_ahw_DPW(%s,%s,%s,%s,%s,%s,%s)" c wf3 p3 wf1 p1 wf2 p2
         | F214 -> printf "w_ahw_DPW(%s,%s,%s,%s,%s,%s,%s)" c wf2 p2 wf1 p1 wf3 p3
         | F412 -> printf "w_ahw_DPW(%s,%s,%s,%s,%s,%s,%s)" c wf2 p2 wf3 p3 wf1 p1
         | F421 -> printf "w_ahw_DPW(%s,%s,%s,%s,%s,%s,%s)" c wf3 p3 wf2 p2 wf1 p1
         | F123 -> printf "(-1)*w_ahw_DPW(%s,%s,%s,%s,%s,%s,%s)" c wf1 p1 wf2 p2 wf3 p3
         | F132 -> printf "(-1)*w_ahw_DPW(%s,%s,%s,%s,%s,%s,%s)" c wf1 p1 wf3 p3 wf2 p2
         | F231 -> printf "(-1)*w_ahw_DPW(%s,%s,%s,%s,%s,%s,%s)" c wf3 p3 wf1 p1 wf2 p2
         | F213 -> printf "(-1)*w_ahw_DPW(%s,%s,%s,%s,%s,%s,%s)" c wf2 p2 wf1 p1 wf3 p3
         | F312 -> printf "(-1)*w_ahw_DPW(%s,%s,%s,%s,%s,%s,%s)" c wf2 p2 wf3 p3 wf1 p1
         | F321 -> printf "(-1)*w_ahw_DPW(%s,%s,%s,%s,%s,%s,%s)" c wf3 p3 wf2 p2 wf1 p1
	 end

      | Dim6_AHWW_DW coeff ->
         let c = format_coupling coeff c in
         begin match fusion with
         | F234 -> printf "a_hww_DW(%s,%s,%s,%s,%s,%s,%s)" c wf1 p1 wf2 p2 wf3 p3
         | F243 -> printf "a_hww_DW(%s,%s,%s,%s,%s,%s,%s)" c wf1 p1 wf3 p3 wf2 p2
         | F342 -> printf "a_hww_DW(%s,%s,%s,%s,%s,%s,%s)" c wf3 p3 wf1 p1 wf2 p2
         | F324 -> printf "a_hww_DW(%s,%s,%s,%s,%s,%s,%s)" c wf2 p2 wf1 p1 wf3 p3
         | F423 -> printf "a_hww_DW(%s,%s,%s,%s,%s,%s,%s)" c wf2 p2 wf3 p3 wf1 p1
         | F432 -> printf "a_hww_DW(%s,%s,%s,%s,%s,%s,%s)" c wf3 p3 wf2 p2 wf1 p1
         | F134 -> printf "h_aww_DW(%s,%s,%s,%s,%s,%s,%s)" c wf1 p1 wf2 p2 wf3 p3
         | F143 -> printf "h_aww_DW(%s,%s,%s,%s,%s,%s,%s)" c wf1 p1 wf3 p3 wf2 p2
         | F341 -> printf "h_aww_DW(%s,%s,%s,%s,%s,%s,%s)" c wf3 p3 wf1 p1 wf2 p2
         | F314 -> printf "h_aww_DW(%s,%s,%s,%s,%s,%s,%s)" c wf2 p2 wf1 p1 wf3 p3
         | F413 -> printf "h_aww_DW(%s,%s,%s,%s,%s,%s,%s)" c wf2 p2 wf3 p3 wf1 p1
         | F431 -> printf "h_aww_DW(%s,%s,%s,%s,%s,%s,%s)" c wf3 p3 wf2 p2 wf1 p1
         | F124 -> printf "w3_ahw_DW(%s,%s,%s,%s,%s,%s,%s)" c wf1 p1 wf2 p2 wf3 p3
         | F142 -> printf "w3_ahw_DW(%s,%s,%s,%s,%s,%s,%s)" c wf1 p1 wf3 p3 wf2 p2
         | F241 -> printf "w3_ahw_DW(%s,%s,%s,%s,%s,%s,%s)" c wf3 p3 wf1 p1 wf2 p2
         | F214 -> printf "w3_ahw_DW(%s,%s,%s,%s,%s,%s,%s)" c wf2 p2 wf1 p1 wf3 p3
         | F412 -> printf "w3_ahw_DW(%s,%s,%s,%s,%s,%s,%s)" c wf2 p2 wf3 p3 wf1 p1
         | F421 -> printf "w3_ahw_DW(%s,%s,%s,%s,%s,%s,%s)" c wf3 p3 wf2 p2 wf1 p1
         | F123 -> printf "w4_ahw_DW(%s,%s,%s,%s,%s,%s,%s)" c wf1 p1 wf2 p2 wf3 p3
         | F132 -> printf "w4_ahw_DW(%s,%s,%s,%s,%s,%s,%s)" c wf1 p1 wf3 p3 wf2 p2
         | F231 -> printf "w4_ahw_DW(%s,%s,%s,%s,%s,%s,%s)" c wf3 p3 wf1 p1 wf2 p2
         | F213 -> printf "w4_ahw_DW(%s,%s,%s,%s,%s,%s,%s)" c wf2 p2 wf1 p1 wf3 p3
         | F312 -> printf "w4_ahw_DW(%s,%s,%s,%s,%s,%s,%s)" c wf2 p2 wf3 p3 wf1 p1
         | F321 -> printf "w4_ahw_DW(%s,%s,%s,%s,%s,%s,%s)" c wf3 p3 wf2 p2 wf1 p1
         (*i               | F234 | F134 | F124 | F123 -> 
                      printf "a_hww_DW(%s,%s,%s,%s,%s,%s,%s)"
                          c wf1 p1 wf2 p2 wf3 p3
                  | F243 | F143 | F142 | F132 -> 
                      printf "a_hww_DW(%s,%s,%s,%s,%s,%s,%s)"
                          c wf1 p1 wf3 p3 wf2 p2
                  | F342 | F341 | F241 | F231 -> 
                      printf "a_hww_DW(%s,%s,%s,%s,%s,%s,%s)"
                          c wf3 p3 wf1 p1 wf2 p2
                  | F324 | F314 | F214 | F213 -> 
                      printf "a_hww_DW(%s,%s,%s,%s,%s,%s,%s)"
                          c wf2 p2 wf1 p1 wf3 p3
                  | F423 | F413 | F412 | F312 -> 
                      printf "a_hww_DW(%s,%s,%s,%s,%s,%s,%s)"
                          c wf2 p2 wf3 p3 wf1 p1
                  | F432 | F431 | F421 | F321 -> 
                      printf "a_hww_DW(%s,%s,%s,%s,%s,%s,%s)"
                          c wf3 p3 wf2 p2 wf1 p1 i*)
	 end

      | Dim6_Scalar2_Vector2_D coeff ->
         let c = format_coupling coeff c in
         begin match fusion with 
         | F234 | F134 -> printf "h_hww_D(%s,%s,%s,%s,%s,%s,%s)" c wf1 p1 wf2 p2 wf3 p3
         | F243 | F143 -> printf "h_hww_D(%s,%s,%s,%s,%s,%s,%s)" c wf1 p1 wf3 p3 wf2 p2
         | F342 | F341 -> printf "h_hww_D(%s,%s,%s,%s,%s,%s,%s)" c wf3 p3 wf1 p1 wf2 p2
         | F324 | F314 -> printf "h_hww_D(%s,%s,%s,%s,%s,%s,%s)" c wf2 p2 wf1 p1 wf3 p3
         | F423 | F413 -> printf "h_hww_D(%s,%s,%s,%s,%s,%s,%s)" c wf2 p2 wf3 p3 wf1 p1
         | F432 | F431 -> printf "h_hww_D(%s,%s,%s,%s,%s,%s,%s)" c wf3 p3 wf2 p2 wf1 p1
         | F124 | F123 -> printf "w_hhw_D(%s,%s,%s,%s,%s,%s,%s)" c wf1 p1 wf2 p2 wf3 p3
         | F142 | F132 -> printf "w_hhw_D(%s,%s,%s,%s,%s,%s,%s)" c wf1 p1 wf3 p3 wf2 p2
         | F241 | F231 -> printf "w_hhw_D(%s,%s,%s,%s,%s,%s,%s)" c wf3 p3 wf1 p1 wf2 p2
         | F214 | F213 -> printf "w_hhw_D(%s,%s,%s,%s,%s,%s,%s)" c wf2 p2 wf1 p1 wf3 p3
         | F412 | F312 -> printf "w_hhw_D(%s,%s,%s,%s,%s,%s,%s)" c wf2 p2 wf3 p3 wf1 p1
         | F421 | F321 -> printf "w_hhw_D(%s,%s,%s,%s,%s,%s,%s)" c wf3 p3 wf2 p2 wf1 p1
         end

      | Dim6_Scalar2_Vector2_DP coeff ->
         let c = format_coupling coeff c in
         begin match fusion with
         | F234 | F134 -> printf "h_hww_DP(%s,%s,%s,%s,%s,%s,%s)" c wf1 p1 wf2 p2 wf3 p3
         | F342 | F341 -> printf "h_hww_DP(%s,%s,%s,%s,%s,%s,%s)" c wf3 p3 wf1 p1 wf2 p2
         | F423 | F413 -> printf "h_hww_DP(%s,%s,%s,%s,%s,%s,%s)" c wf2 p2 wf3 p3 wf1 p1
         | F243 | F143 -> printf "h_hww_DP(%s,%s,%s,%s,%s,%s,%s)" c wf1 p1 wf3 p3 wf2 p2
         | F324 | F314 -> printf "h_hww_DP(%s,%s,%s,%s,%s,%s,%s)" c wf2 p2 wf1 p1 wf3 p3
         | F432 | F431 -> printf "h_hww_DP(%s,%s,%s,%s,%s,%s,%s)" c wf3 p3 wf2 p2 wf1 p1
         | F123 | F124 -> printf "w_hhw_DP(%s,%s,%s,%s,%s,%s,%s)" c wf1 p1 wf2 p2 wf3 p3
         | F231 | F241 -> printf "w_hhw_DP(%s,%s,%s,%s,%s,%s,%s)" c wf3 p3 wf1 p1 wf2 p2
         | F312 | F412 -> printf "w_hhw_DP(%s,%s,%s,%s,%s,%s,%s)" c wf2 p2 wf3 p3 wf1 p1
         | F132 | F142 -> printf "w_hhw_DP(%s,%s,%s,%s,%s,%s,%s)" c wf1 p1 wf3 p3 wf2 p2
         | F213 | F214 -> printf "w_hhw_DP(%s,%s,%s,%s,%s,%s,%s)" c wf2 p2 wf1 p1 wf3 p3
         | F321 | F421 -> printf "w_hhw_DP(%s,%s,%s,%s,%s,%s,%s)" c wf3 p3 wf2 p2 wf1 p1
         (*i               | F234 ->
                      printf "h_hww_DPW(%s,%s,%s,%s,%s,%s,%s)"
                          c wf1 p1 wf2 p2 wf3 p3
                  | F243 ->
                      printf "h_hww_DPW(%s,%s,%s,%s,%s,%s,%s)"
                          c wf1 p1 wf3 p3 wf2 p2
                  | F342  -> 
                      printf "h_hww_DPW(%s,%s,%s,%s,%s,%s,%s)"
                          c wf3 p3 wf1 p1 wf2 p2
                  | F324 ->
                      printf "h_hww_DPW(%s,%s,%s,%s,%s,%s,%s)"
                          c wf2 p2 wf1 p1 wf3 p3
                  | F423 ->
                      printf "h_hww_DPW(%s,%s,%s,%s,%s,%s,%s)"
                          c wf2 p2 wf3 p3 wf1 p1
                  | F432 ->
                      printf "h_hww_DPW(%s,%s,%s,%s,%s,%s,%s)"
                          c wf3 p3 wf2 p2 wf1 p1
                  | F124 ->
                      printf "w_hhw_DPW(%s,%s,%s,%s,%s,%s,%s)"
                          c wf1 p1 wf2 p2 wf3 p3
                  | F142 ->
                      printf "w_hhw_DPW(%s,%s,%s,%s,%s,%s,%s)"
                          c wf1 p1 wf3 p3 wf2 p2
                  | F241 ->
                      printf "w_hhw_DPW(%s,%s,%s,%s,%s,%s,%s)"
                          c wf3 p3 wf1 p1 wf2 p2
                  | F214 ->
                      printf "w_hhw_DPW(%s,%s,%s,%s,%s,%s,%s)"
                          c wf2 p2 wf1 p1 wf3 p3
                  | F412 -> 
                      printf "w_hhw_DPW(%s,%s,%s,%s,%s,%s,%s)"
                          c wf2 p2 wf3 p3 wf1 p1
                  | F421 ->
                      printf "w_hhw_DPW(%s,%s,%s,%s,%s,%s,%s)"
                          c wf3 p3 wf2 p2 wf1 p1
                  | F134 ->
                      printf "h_hww_DPW(%s,%s,%s,%s,%s,%s,%s)"
                          c wf1 p1 wf2 p2 wf3 p3
                  | F143 ->
                      printf "h_hww_DPW(%s,%s,%s,%s,%s,%s,%s)"
                          c wf1 p1 wf3 p3 wf2 p2
                  | F341 -> 
                      printf "h_hww_DPW(%s,%s,%s,%s,%s,%s,%s)"
                          c wf3 p3 wf1 p1 wf2 p2
                  | F314 ->
                      printf "h_hww_DPW(%s,%s,%s,%s,%s,%s,%s)"
                          c wf2 p2 wf1 p1 wf3 p3
                  | F413 ->
                      printf "h_hww_DPW(%s,%s,%s,%s,%s,%s,%s)"
                          c wf2 p2 wf3 p3 wf1 p1
                  | F431 ->
                      printf "h_hww_DPW(%s,%s,%s,%s,%s,%s,%s)"
                          c wf3 p3 wf2 p2 wf1 p1
                  | F123  ->
                      printf "w_hhw_DPW(%s,%s,%s,%s,%s,%s,%s)"
                          c wf1 p1 wf2 p2 wf3 p3
                  | F132  ->
                      printf "w_hhw_DPW(%s,%s,%s,%s,%s,%s,%s)"
                          c wf1 p1 wf3 p3 wf2 p2
                  | F231  ->
                      printf "w_hhw_DPW(%s,%s,%s,%s,%s,%s,%s)"
                          c wf3 p3 wf1 p1 wf2 p2
                  | F213  ->
                      printf "w_hhw_DPW(%s,%s,%s,%s,%s,%s,%s)"
                          c wf2 p2 wf1 p1 wf3 p3
                  | F312 -> 
                      printf "w_hhw_DPW(%s,%s,%s,%s,%s,%s,%s)"
                          c wf2 p2 wf3 p3 wf1 p1
                  | F321  ->
                      printf "w_hhw_DPW(%s,%s,%s,%s,%s,%s,%s)"
                          c wf3 p3 wf2 p2 wf1 p1   i*)
         end

      | Dim6_Scalar2_Vector2_PB coeff ->
         let c = format_coupling coeff c in
         begin match fusion with
         | F234 | F134 -> printf "h_hvv_PB(%s,%s,%s,%s,%s,%s,%s)" c wf1 p1 wf2 p2 wf3 p3
         | F342 | F341 -> printf "h_hvv_PB(%s,%s,%s,%s,%s,%s,%s)" c wf3 p3 wf1 p1 wf2 p2
         | F423 | F413 -> printf "h_hvv_PB(%s,%s,%s,%s,%s,%s,%s)" c wf2 p2 wf3 p3 wf1 p1
         | F243 | F143 -> printf "h_hvv_PB(%s,%s,%s,%s,%s,%s,%s)" c wf1 p1 wf3 p3 wf2 p2
         | F324 | F314 -> printf "h_hvv_PB(%s,%s,%s,%s,%s,%s,%s)" c wf2 p2 wf1 p1 wf3 p3
         | F432 | F431 -> printf "h_hvv_PB(%s,%s,%s,%s,%s,%s,%s)" c wf3 p3 wf2 p2 wf1 p1
         | F123 | F124 -> printf "v_hhv_PB(%s,%s,%s,%s,%s,%s,%s)" c wf1 p1 wf2 p2 wf3 p3
         | F231 | F241 -> printf "v_hhv_PB(%s,%s,%s,%s,%s,%s,%s)" c wf3 p3 wf1 p1 wf2 p2
         | F312 | F412 -> printf "v_hhv_PB(%s,%s,%s,%s,%s,%s,%s)" c wf2 p2 wf3 p3 wf1 p1
         | F132 | F142 -> printf "v_hhv_PB(%s,%s,%s,%s,%s,%s,%s)" c wf1 p1 wf3 p3 wf2 p2
         | F213 | F214 -> printf "v_hhv_PB(%s,%s,%s,%s,%s,%s,%s)" c wf2 p2 wf1 p1 wf3 p3
         | F321 | F421 -> printf "v_hhv_PB(%s,%s,%s,%s,%s,%s,%s)" c wf3 p3 wf2 p2 wf1 p1
         end  

      | Dim6_HHZZ_T coeff ->
         let c = format_coupling coeff c in
         begin match fusion with
         | F234 | F134 -> printf "(%s)*(%s)*(%s)*(%s)"  c wf1 wf2 wf3
         | F342 | F341 -> printf "(%s)*(%s)*(%s)*(%s)"  c wf3 wf1 wf2
         | F423 | F413 -> printf "(%s)*(%s)*(%s)*(%s)"  c wf2 wf3 wf1
         | F243 | F143 -> printf "(%s)*(%s)*(%s)*(%s)"  c wf1 wf3 wf2
         | F324 | F314 -> printf "(%s)*(%s)*(%s)*(%s)"  c wf2 wf1 wf3 
         | F432 | F431 -> printf "(%s)*(%s)*(%s)*(%s)"  c wf3 wf2 wf1
         | F123 | F124 | F231 | F241 | F312 | F412 -> printf "(%s)*(%s)*(%s)*(%s)"  c wf1 wf2 wf3
         | F132 | F142 | F213 | F214 | F321 | F421 -> printf "(%s)*(%s)*(%s)*(%s)"  c wf1 wf2 wf3
         end  

      | Dim6_Vector4_DW coeff ->
         let c = format_coupling coeff c in
         begin match fusion with
         | F234 | F134 -> printf "a_aww_DW(%s,%s,%s,%s,%s,%s,%s)" c wf1 p1 wf2 p2 wf3 p3
         | F342 | F341 -> printf "a_aww_DW(%s,%s,%s,%s,%s,%s,%s)" c wf3 p3 wf1 p1 wf2 p2
         | F423 | F413 -> printf "a_aww_DW(%s,%s,%s,%s,%s,%s,%s)" c wf2 p2 wf3 p3 wf1 p1
         | F243 | F143 -> printf "a_aww_DW(%s,%s,%s,%s,%s,%s,%s)" c wf1 p1 wf3 p3 wf2 p2
         | F324 | F314 -> printf "a_aww_DW(%s,%s,%s,%s,%s,%s,%s)" c wf2 p2 wf1 p1 wf3 p3 
         | F432 | F431 -> printf "a_aww_DW(%s,%s,%s,%s,%s,%s,%s)" c wf3 p3 wf2 p2 wf1 p1
         | F124 | F123 -> printf "w_aaw_DW(%s,%s,%s,%s,%s,%s,%s)" c wf1 p1 wf2 p2 wf3 p3
         | F241 | F231 -> printf "w_aaw_DW(%s,%s,%s,%s,%s,%s,%s)" c wf3 p3 wf1 p1 wf2 p2
         | F412 | F312 -> printf "w_aaw_DW(%s,%s,%s,%s,%s,%s,%s)" c wf2 p2 wf3 p3 wf1 p1
         | F142 | F132 -> printf "w_aaw_DW(%s,%s,%s,%s,%s,%s,%s)" c wf1 p1 wf3 p3 wf2 p2
         | F214 | F213 -> printf "w_aaw_DW(%s,%s,%s,%s,%s,%s,%s)" c wf2 p2 wf1 p1 wf3 p3
         | F421 | F321 -> printf "w_aaw_DW(%s,%s,%s,%s,%s,%s,%s)" c wf3 p3 wf2 p2 wf1 p1
         end

      | Dim6_Vector4_W coeff ->
         let c = format_coupling coeff c in
         begin match fusion with
         | F234 | F134 -> printf "a_aww_W(%s,%s,%s,%s,%s,%s,%s)" c wf1 p1 wf2 p2 wf3 p3
         | F342 | F341 -> printf "a_aww_W(%s,%s,%s,%s,%s,%s,%s)" c wf3 p3 wf1 p1 wf2 p2
         | F423 | F413 -> printf "a_aww_W(%s,%s,%s,%s,%s,%s,%s)" c wf2 p2 wf3 p3 wf1 p1
         | F243 | F143 -> printf "a_aww_W(%s,%s,%s,%s,%s,%s,%s)" c wf1 p1 wf3 p3 wf2 p2
         | F324 | F314 -> printf "a_aww_W(%s,%s,%s,%s,%s,%s,%s)" c wf2 p2 wf1 p1 wf3 p3
         | F432 | F431 -> printf "a_aww_W(%s,%s,%s,%s,%s,%s,%s)" c wf3 p3 wf2 p2 wf1 p1
         | F123 | F124 -> printf "w_aaw_W(%s,%s,%s,%s,%s,%s,%s)" c wf1 p1 wf2 p2 wf3 p3
         | F231 | F241 -> printf "w_aaw_W(%s,%s,%s,%s,%s,%s,%s)" c wf3 p3 wf1 p1 wf2 p2
         | F312 | F412 -> printf "w_aaw_W(%s,%s,%s,%s,%s,%s,%s)" c wf2 p2 wf3 p3 wf1 p1
         | F132 | F142 -> printf "w_aaw_W(%s,%s,%s,%s,%s,%s,%s)" c wf1 p1 wf3 p3 wf2 p2
         | F213 | F214 -> printf "w_aaw_W(%s,%s,%s,%s,%s,%s,%s)" c wf2 p2 wf1 p1 wf3 p3
         | F321 | F421 -> printf "w_aaw_W(%s,%s,%s,%s,%s,%s,%s)" c wf3 p3 wf2 p2 wf1 p1
         end  

      | Dim6_HWWZ_DW coeff ->
         let c = format_coupling coeff c in
         begin match fusion with 
         | F234 -> printf "h_wwz_DW(%s,%s,%s,%s,%s,%s,%s)" c wf1 p1 wf2 p2 wf3 p3
         | F243 -> printf "h_wwz_DW(%s,%s,%s,%s,%s,%s,%s)" c wf1 p1 wf3 p3 wf2 p2
         | F342 -> printf "h_wwz_DW(%s,%s,%s,%s,%s,%s,%s)" c wf3 p3 wf1 p1 wf2 p2
         | F324 -> printf "h_wwz_DW(%s,%s,%s,%s,%s,%s,%s)" c wf2 p2 wf1 p1 wf3 p3
         | F423 -> printf "h_wwz_DW(%s,%s,%s,%s,%s,%s,%s)" c wf2 p2 wf3 p3 wf1 p1
         | F432 -> printf "h_wwz_DW(%s,%s,%s,%s,%s,%s,%s)" c wf3 p3 wf2 p2 wf1 p1
         | F124 -> printf "(-1)*w_hwz_DW(%s,%s,%s,%s,%s,%s,%s)" c wf1 p1 wf2 p2 wf3 p3
         | F142 -> printf "(-1)*w_hwz_DW(%s,%s,%s,%s,%s,%s,%s)" c wf1 p1 wf3 p3 wf2 p2
         | F241 -> printf "(-1)*w_hwz_DW(%s,%s,%s,%s,%s,%s,%s)" c wf3 p3 wf1 p1 wf2 p2
         | F214 -> printf "(-1)*w_hwz_DW(%s,%s,%s,%s,%s,%s,%s)" c wf2 p2 wf1 p1 wf3 p3
         | F412 -> printf "(-1)*w_hwz_DW(%s,%s,%s,%s,%s,%s,%s)" c wf2 p2 wf3 p3 wf1 p1
         | F421 -> printf "(-1)*w_hwz_DW(%s,%s,%s,%s,%s,%s,%s)" c wf3 p3 wf2 p2 wf1 p1
         | F134 -> printf "w_hwz_DW(%s,%s,%s,%s,%s,%s,%s)" c wf1 p1 wf2 p2 wf3 p3
         | F143 -> printf "w_hwz_DW(%s,%s,%s,%s,%s,%s,%s)" c wf1 p1 wf3 p3 wf2 p2
         | F341 -> printf "w_hwz_DW(%s,%s,%s,%s,%s,%s,%s)" c wf3 p3 wf1 p1 wf2 p2
         | F314 -> printf "w_hwz_DW(%s,%s,%s,%s,%s,%s,%s)" c wf2 p2 wf1 p1 wf3 p3
         | F413 -> printf "w_hwz_DW(%s,%s,%s,%s,%s,%s,%s)" c wf2 p2 wf3 p3 wf1 p1
         | F431 -> printf "w_hwz_DW(%s,%s,%s,%s,%s,%s,%s)" c wf3 p3 wf2 p2 wf1 p1
         | F123 -> printf "z_hww_DW(%s,%s,%s,%s,%s,%s,%s)" c wf1 p1 wf2 p2 wf3 p3
         | F132 -> printf "z_hww_DW(%s,%s,%s,%s,%s,%s,%s)" c wf1 p1 wf3 p3 wf2 p2
         | F231 -> printf "z_hww_DW(%s,%s,%s,%s,%s,%s,%s)" c wf3 p3 wf1 p1 wf2 p2
         | F213 -> printf "z_hww_DW(%s,%s,%s,%s,%s,%s,%s)" c wf2 p2 wf1 p1 wf3 p3
         | F312 -> printf "z_hww_DW(%s,%s,%s,%s,%s,%s,%s)" c wf2 p2 wf3 p3 wf1 p1
         | F321 -> printf "z_hww_DW(%s,%s,%s,%s,%s,%s,%s)" c wf3 p3 wf2 p2 wf1 p1
         end

      | Dim6_HWWZ_DPB coeff ->
         let c = format_coupling coeff c in
         begin match fusion with 
         | F234 -> printf "h_wwz_DPB(%s,%s,%s,%s,%s,%s,%s)" c wf1 p1 wf2 p2 wf3 p3
         | F243 -> printf "h_wwz_DPB(%s,%s,%s,%s,%s,%s,%s)" c wf1 p1 wf3 p3 wf2 p2
         | F342 -> printf "h_wwz_DPB(%s,%s,%s,%s,%s,%s,%s)" c wf3 p3 wf1 p1 wf2 p2
         | F324 -> printf "h_wwz_DPB(%s,%s,%s,%s,%s,%s,%s)" c wf2 p2 wf1 p1 wf3 p3
         | F423 -> printf "h_wwz_DPB(%s,%s,%s,%s,%s,%s,%s)" c wf2 p2 wf3 p3 wf1 p1
         | F432 -> printf "h_wwz_DPB(%s,%s,%s,%s,%s,%s,%s)" c wf3 p3 wf2 p2 wf1 p1
         | F124 -> printf "(-1)*w_hwz_DPB(%s,%s,%s,%s,%s,%s,%s)" c wf1 p1 wf2 p2 wf3 p3
         | F142 -> printf "(-1)*w_hwz_DPB(%s,%s,%s,%s,%s,%s,%s)" c wf1 p1 wf3 p3 wf2 p2
         | F241 -> printf "(-1)*w_hwz_DPB(%s,%s,%s,%s,%s,%s,%s)" c wf3 p3 wf1 p1 wf2 p2
         | F214 -> printf "(-1)*w_hwz_DPB(%s,%s,%s,%s,%s,%s,%s)" c wf2 p2 wf1 p1 wf3 p3
         | F412 -> printf "(-1)*w_hwz_DPB(%s,%s,%s,%s,%s,%s,%s)" c wf2 p2 wf3 p3 wf1 p1
         | F421 -> printf "(-1)*w_hwz_DPB(%s,%s,%s,%s,%s,%s,%s)" c wf3 p3 wf2 p2 wf1 p1
         | F134 -> printf "w_hwz_DPB(%s,%s,%s,%s,%s,%s,%s)" c wf1 p1 wf2 p2 wf3 p3
         | F143 -> printf "w_hwz_DPB(%s,%s,%s,%s,%s,%s,%s)" c wf1 p1 wf3 p3 wf2 p2
         | F341 -> printf "w_hwz_DPB(%s,%s,%s,%s,%s,%s,%s)" c wf3 p3 wf1 p1 wf2 p2
         | F314 -> printf "w_hwz_DPB(%s,%s,%s,%s,%s,%s,%s)" c wf2 p2 wf1 p1 wf3 p3
         | F413 -> printf "w_hwz_DPB(%s,%s,%s,%s,%s,%s,%s)" c wf2 p2 wf3 p3 wf1 p1
         | F431 -> printf "w_hwz_DPB(%s,%s,%s,%s,%s,%s,%s)" c wf3 p3 wf2 p2 wf1 p1
         | F123 -> printf "z_hww_DPB(%s,%s,%s,%s,%s,%s,%s)" c wf1 p1 wf2 p2 wf3 p3
         | F132 -> printf "z_hww_DPB(%s,%s,%s,%s,%s,%s,%s)" c wf1 p1 wf3 p3 wf2 p2
         | F231 -> printf "z_hww_DPB(%s,%s,%s,%s,%s,%s,%s)" c wf3 p3 wf1 p1 wf2 p2
         | F213 -> printf "z_hww_DPB(%s,%s,%s,%s,%s,%s,%s)" c wf2 p2 wf1 p1 wf3 p3
         | F312 -> printf "z_hww_DPB(%s,%s,%s,%s,%s,%s,%s)" c wf2 p2 wf3 p3 wf1 p1
         | F321 -> printf "z_hww_DPB(%s,%s,%s,%s,%s,%s,%s)" c wf3 p3 wf2 p2 wf1 p1
         end

      | Dim6_HWWZ_DDPW coeff ->
         let c = format_coupling coeff c in
         begin match fusion with 
         | F234 -> printf "h_wwz_DDPW(%s,%s,%s,%s,%s,%s,%s)" c wf1 p1 wf2 p2 wf3 p3
         | F243 -> printf "h_wwz_DDPW(%s,%s,%s,%s,%s,%s,%s)" c wf1 p1 wf3 p3 wf2 p2
         | F342 -> printf "h_wwz_DDPW(%s,%s,%s,%s,%s,%s,%s)" c wf3 p3 wf1 p1 wf2 p2
         | F324 -> printf "h_wwz_DDPW(%s,%s,%s,%s,%s,%s,%s)" c wf2 p2 wf1 p1 wf3 p3
         | F423 -> printf "h_wwz_DDPW(%s,%s,%s,%s,%s,%s,%s)" c wf2 p2 wf3 p3 wf1 p1
         | F432 -> printf "h_wwz_DDPW(%s,%s,%s,%s,%s,%s,%s)" c wf3 p3 wf2 p2 wf1 p1
         | F124 -> printf "(-1)*w_hwz_DDPW(%s,%s,%s,%s,%s,%s,%s)" c wf1 p1 wf2 p2 wf3 p3
         | F142 -> printf "(-1)*w_hwz_DDPW(%s,%s,%s,%s,%s,%s,%s)" c wf1 p1 wf3 p3 wf2 p2
         | F241 -> printf "(-1)*w_hwz_DDPW(%s,%s,%s,%s,%s,%s,%s)" c wf3 p3 wf1 p1 wf2 p2
         | F214 -> printf "(-1)*w_hwz_DDPW(%s,%s,%s,%s,%s,%s,%s)" c wf2 p2 wf1 p1 wf3 p3
         | F412 -> printf "(-1)*w_hwz_DDPW(%s,%s,%s,%s,%s,%s,%s)" c wf2 p2 wf3 p3 wf1 p1
         | F421 -> printf "(-1)*w_hwz_DDPW(%s,%s,%s,%s,%s,%s,%s)" c wf3 p3 wf2 p2 wf1 p1
         | F134 -> printf "w_hwz_DDPW(%s,%s,%s,%s,%s,%s,%s)" c wf1 p1 wf2 p2 wf3 p3
         | F143 -> printf "w_hwz_DDPW(%s,%s,%s,%s,%s,%s,%s)" c wf1 p1 wf3 p3 wf2 p2
         | F341 -> printf "w_hwz_DDPW(%s,%s,%s,%s,%s,%s,%s)" c wf3 p3 wf1 p1 wf2 p2
         | F314 -> printf "w_hwz_DDPW(%s,%s,%s,%s,%s,%s,%s)" c wf2 p2 wf1 p1 wf3 p3
         | F413 -> printf "w_hwz_DDPW(%s,%s,%s,%s,%s,%s,%s)" c wf2 p2 wf3 p3 wf1 p1
         | F431 -> printf "w_hwz_DDPW(%s,%s,%s,%s,%s,%s,%s)" c wf3 p3 wf2 p2 wf1 p1
         | F123 -> printf "z_hww_DDPW(%s,%s,%s,%s,%s,%s,%s)" c wf1 p1 wf2 p2 wf3 p3
         | F132 -> printf "z_hww_DDPW(%s,%s,%s,%s,%s,%s,%s)" c wf1 p1 wf3 p3 wf2 p2
         | F231 -> printf "z_hww_DDPW(%s,%s,%s,%s,%s,%s,%s)" c wf3 p3 wf1 p1 wf2 p2
         | F213 -> printf "z_hww_DDPW(%s,%s,%s,%s,%s,%s,%s)" c wf2 p2 wf1 p1 wf3 p3
         | F312 -> printf "z_hww_DDPW(%s,%s,%s,%s,%s,%s,%s)" c wf2 p2 wf3 p3 wf1 p1
         | F321 -> printf "z_hww_DDPW(%s,%s,%s,%s,%s,%s,%s)" c wf3 p3 wf2 p2 wf1 p1
         end

      | Dim6_HWWZ_DPW coeff ->
         let c = format_coupling coeff c in
         begin match fusion with 
         | F234 -> printf "h_wwz_DPW(%s,%s,%s,%s,%s,%s,%s)" c wf1 p1 wf2 p2 wf3 p3
         | F243 -> printf "h_wwz_DPW(%s,%s,%s,%s,%s,%s,%s)" c wf1 p1 wf3 p3 wf2 p2
         | F342 -> printf "h_wwz_DPW(%s,%s,%s,%s,%s,%s,%s)" c wf3 p3 wf1 p1 wf2 p2
         | F324 -> printf "h_wwz_DPW(%s,%s,%s,%s,%s,%s,%s)" c wf2 p2 wf1 p1 wf3 p3
         | F423 -> printf "h_wwz_DPW(%s,%s,%s,%s,%s,%s,%s)" c wf2 p2 wf3 p3 wf1 p1
         | F432 -> printf "h_wwz_DPW(%s,%s,%s,%s,%s,%s,%s)" c wf3 p3 wf2 p2 wf1 p1
         | F124 -> printf "(-1)*w_hwz_DPW(%s,%s,%s,%s,%s,%s,%s)" c wf1 p1 wf2 p2 wf3 p3
         | F142 -> printf "(-1)*w_hwz_DPW(%s,%s,%s,%s,%s,%s,%s)" c wf1 p1 wf3 p3 wf2 p2
         | F241 -> printf "(-1)*w_hwz_DPW(%s,%s,%s,%s,%s,%s,%s)" c wf3 p3 wf1 p1 wf2 p2
         | F214 -> printf "(-1)*w_hwz_DPW(%s,%s,%s,%s,%s,%s,%s)" c wf2 p2 wf1 p1 wf3 p3
         | F412 -> printf "(-1)*w_hwz_DPW(%s,%s,%s,%s,%s,%s,%s)" c wf2 p2 wf3 p3 wf1 p1
         | F421 -> printf "(-1)*w_hwz_DPW(%s,%s,%s,%s,%s,%s,%s)" c wf3 p3 wf2 p2 wf1 p1
         | F134 -> printf "w_hwz_DPW(%s,%s,%s,%s,%s,%s,%s)" c wf1 p1 wf2 p2 wf3 p3
         | F143 -> printf "w_hwz_DPW(%s,%s,%s,%s,%s,%s,%s)" c wf1 p1 wf3 p3 wf2 p2
         | F341 -> printf "w_hwz_DPW(%s,%s,%s,%s,%s,%s,%s)" c wf3 p3 wf1 p1 wf2 p2
         | F314 -> printf "w_hwz_DPW(%s,%s,%s,%s,%s,%s,%s)" c wf2 p2 wf1 p1 wf3 p3
         | F413 -> printf "w_hwz_DPW(%s,%s,%s,%s,%s,%s,%s)" c wf2 p2 wf3 p3 wf1 p1
         | F431 -> printf "w_hwz_DPW(%s,%s,%s,%s,%s,%s,%s)" c wf3 p3 wf2 p2 wf1 p1
         | F123 -> printf "z_hww_DPW(%s,%s,%s,%s,%s,%s,%s)" c wf1 p1 wf2 p2 wf3 p3
         | F132 -> printf "z_hww_DPW(%s,%s,%s,%s,%s,%s,%s)" c wf1 p1 wf3 p3 wf2 p2
         | F231 -> printf "z_hww_DPW(%s,%s,%s,%s,%s,%s,%s)" c wf3 p3 wf1 p1 wf2 p2
         | F213 -> printf "z_hww_DPW(%s,%s,%s,%s,%s,%s,%s)" c wf2 p2 wf1 p1 wf3 p3
         | F312 -> printf "z_hww_DPW(%s,%s,%s,%s,%s,%s,%s)" c wf2 p2 wf3 p3 wf1 p1
         | F321 -> printf "z_hww_DPW(%s,%s,%s,%s,%s,%s,%s)" c wf3 p3 wf2 p2 wf1 p1
         end

      | Dim6_AHHZ_D coeff ->
         let c = format_coupling coeff c in
         begin match fusion with 
         | F234 -> printf "a_hhz_D(%s,%s,%s,%s,%s,%s,%s)" c wf1 p1 wf2 p2 wf3 p3
         | F243 -> printf "a_hhz_D(%s,%s,%s,%s,%s,%s,%s)" c wf1 p1 wf3 p3 wf2 p2
         | F342 -> printf "a_hhz_D(%s,%s,%s,%s,%s,%s,%s)" c wf3 p3 wf1 p1 wf2 p2
         | F324 -> printf "a_hhz_D(%s,%s,%s,%s,%s,%s,%s)" c wf2 p2 wf1 p1 wf3 p3
         | F423 -> printf "a_hhz_D(%s,%s,%s,%s,%s,%s,%s)" c wf2 p2 wf3 p3 wf1 p1
         | F432 -> printf "a_hhz_D(%s,%s,%s,%s,%s,%s,%s)" c wf3 p3 wf2 p2 wf1 p1
         | F124 -> printf "h_ahz_D(%s,%s,%s,%s,%s,%s,%s)" c wf1 p1 wf2 p2 wf3 p3
         | F142 -> printf "h_ahz_D(%s,%s,%s,%s,%s,%s,%s)" c wf1 p1 wf3 p3 wf2 p2
         | F241 -> printf "h_ahz_D(%s,%s,%s,%s,%s,%s,%s)" c wf3 p3 wf1 p1 wf2 p2
         | F214 -> printf "h_ahz_D(%s,%s,%s,%s,%s,%s,%s)" c wf2 p2 wf1 p1 wf3 p3
         | F412 -> printf "h_ahz_D(%s,%s,%s,%s,%s,%s,%s)" c wf2 p2 wf3 p3 wf1 p1
         | F421 -> printf "h_ahz_D(%s,%s,%s,%s,%s,%s,%s)" c wf3 p3 wf2 p2 wf1 p1
         | F134 -> printf "h_ahz_D(%s,%s,%s,%s,%s,%s,%s)" c wf1 p1 wf2 p2 wf3 p3
         | F143 -> printf "h_ahz_D(%s,%s,%s,%s,%s,%s,%s)" c wf1 p1 wf3 p3 wf2 p2
         | F341 -> printf "h_ahz_D(%s,%s,%s,%s,%s,%s,%s)" c wf3 p3 wf1 p1 wf2 p2
         | F314 -> printf "h_ahz_D(%s,%s,%s,%s,%s,%s,%s)" c wf2 p2 wf1 p1 wf3 p3
         | F413 -> printf "h_ahz_D(%s,%s,%s,%s,%s,%s,%s)" c wf2 p2 wf3 p3 wf1 p1
         | F431 -> printf "h_ahz_D(%s,%s,%s,%s,%s,%s,%s)" c wf3 p3 wf2 p2 wf1 p1
         | F123 -> printf "z_ahh_D(%s,%s,%s,%s,%s,%s,%s)" c wf1 p1 wf2 p2 wf3 p3
         | F132 -> printf "z_ahh_D(%s,%s,%s,%s,%s,%s,%s)" c wf1 p1 wf3 p3 wf2 p2
         | F231 -> printf "z_ahh_D(%s,%s,%s,%s,%s,%s,%s)" c wf3 p3 wf1 p1 wf2 p2
         | F213 -> printf "z_ahh_D(%s,%s,%s,%s,%s,%s,%s)" c wf2 p2 wf1 p1 wf3 p3
         | F312 -> printf "z_ahh_D(%s,%s,%s,%s,%s,%s,%s)" c wf2 p2 wf3 p3 wf1 p1
         | F321 -> printf "z_ahh_D(%s,%s,%s,%s,%s,%s,%s)" c wf3 p3 wf2 p2 wf1 p1
         end

      | Dim6_AHHZ_DP coeff ->
         let c = format_coupling coeff c in
         begin match fusion with 
         | F234 -> printf "a_hhz_DP(%s,%s,%s,%s,%s,%s,%s)" c wf1 p1 wf2 p2 wf3 p3
         | F243 -> printf "a_hhz_DP(%s,%s,%s,%s,%s,%s,%s)" c wf1 p1 wf3 p3 wf2 p2
         | F342 -> printf "a_hhz_DP(%s,%s,%s,%s,%s,%s,%s)" c wf3 p3 wf1 p1 wf2 p2
         | F324 -> printf "a_hhz_DP(%s,%s,%s,%s,%s,%s,%s)" c wf2 p2 wf1 p1 wf3 p3
         | F423 -> printf "a_hhz_DP(%s,%s,%s,%s,%s,%s,%s)" c wf2 p2 wf3 p3 wf1 p1
         | F432 -> printf "a_hhz_DP(%s,%s,%s,%s,%s,%s,%s)" c wf3 p3 wf2 p2 wf1 p1
         | F124 -> printf "h_ahz_DP(%s,%s,%s,%s,%s,%s,%s)" c wf1 p1 wf2 p2 wf3 p3
         | F142 -> printf "h_ahz_DP(%s,%s,%s,%s,%s,%s,%s)" c wf1 p1 wf3 p3 wf2 p2
         | F241 -> printf "h_ahz_DP(%s,%s,%s,%s,%s,%s,%s)" c wf3 p3 wf1 p1 wf2 p2
         | F214 -> printf "h_ahz_DP(%s,%s,%s,%s,%s,%s,%s)" c wf2 p2 wf1 p1 wf3 p3
         | F412 -> printf "h_ahz_DP(%s,%s,%s,%s,%s,%s,%s)" c wf2 p2 wf3 p3 wf1 p1
         | F421 -> printf "h_ahz_DP(%s,%s,%s,%s,%s,%s,%s)" c wf3 p3 wf2 p2 wf1 p1
         | F134 -> printf "h_ahz_DP(%s,%s,%s,%s,%s,%s,%s)" c wf1 p1 wf2 p2 wf3 p3
         | F143 -> printf "h_ahz_DP(%s,%s,%s,%s,%s,%s,%s)" c wf1 p1 wf3 p3 wf2 p2
         | F341 -> printf "h_ahz_DP(%s,%s,%s,%s,%s,%s,%s)" c wf3 p3 wf1 p1 wf2 p2
         | F314 -> printf "h_ahz_DP(%s,%s,%s,%s,%s,%s,%s)" c wf2 p2 wf1 p1 wf3 p3
         | F413 -> printf "h_ahz_DP(%s,%s,%s,%s,%s,%s,%s)" c wf2 p2 wf3 p3 wf1 p1
         | F431 -> printf "h_ahz_DP(%s,%s,%s,%s,%s,%s,%s)" c wf3 p3 wf2 p2 wf1 p1
         | F123 -> printf "z_ahh_DP(%s,%s,%s,%s,%s,%s,%s)" c wf1 p1 wf2 p2 wf3 p3
         | F132 -> printf "z_ahh_DP(%s,%s,%s,%s,%s,%s,%s)" c wf1 p1 wf3 p3 wf2 p2
         | F231 -> printf "z_ahh_DP(%s,%s,%s,%s,%s,%s,%s)" c wf3 p3 wf1 p1 wf2 p2
         | F213 -> printf "z_ahh_DP(%s,%s,%s,%s,%s,%s,%s)" c wf2 p2 wf1 p1 wf3 p3
         | F312 -> printf "z_ahh_DP(%s,%s,%s,%s,%s,%s,%s)" c wf2 p2 wf3 p3 wf1 p1
         | F321 -> printf "z_ahh_DP(%s,%s,%s,%s,%s,%s,%s)" c wf3 p3 wf2 p2 wf1 p1
         end

      | Dim6_AHHZ_PB coeff ->
         let c = format_coupling coeff c in
         begin match fusion with 
         | F234 -> printf "a_hhz_PB(%s,%s,%s,%s,%s,%s,%s)" c wf1 p1 wf2 p2 wf3 p3
         | F243 -> printf "a_hhz_PB(%s,%s,%s,%s,%s,%s,%s)" c wf1 p1 wf3 p3 wf2 p2
         | F342 -> printf "a_hhz_PB(%s,%s,%s,%s,%s,%s,%s)" c wf3 p3 wf1 p1 wf2 p2
         | F324 -> printf "a_hhz_PB(%s,%s,%s,%s,%s,%s,%s)" c wf2 p2 wf1 p1 wf3 p3
         | F423 -> printf "a_hhz_PB(%s,%s,%s,%s,%s,%s,%s)" c wf2 p2 wf3 p3 wf1 p1
         | F432 -> printf "a_hhz_PB(%s,%s,%s,%s,%s,%s,%s)" c wf3 p3 wf2 p2 wf1 p1
         | F124 -> printf "h_ahz_PB(%s,%s,%s,%s,%s,%s,%s)" c wf1 p1 wf2 p2 wf3 p3
         | F142 -> printf "h_ahz_PB(%s,%s,%s,%s,%s,%s,%s)" c wf1 p1 wf3 p3 wf2 p2
         | F241 -> printf "h_ahz_PB(%s,%s,%s,%s,%s,%s,%s)" c wf3 p3 wf1 p1 wf2 p2
         | F214 -> printf "h_ahz_PB(%s,%s,%s,%s,%s,%s,%s)" c wf2 p2 wf1 p1 wf3 p3
         | F412 -> printf "h_ahz_PB(%s,%s,%s,%s,%s,%s,%s)" c wf2 p2 wf3 p3 wf1 p1
         | F421 -> printf "h_ahz_PB(%s,%s,%s,%s,%s,%s,%s)" c wf3 p3 wf2 p2 wf1 p1
         | F134 -> printf "h_ahz_PB(%s,%s,%s,%s,%s,%s,%s)" c wf1 p1 wf2 p2 wf3 p3
         | F143 -> printf "h_ahz_PB(%s,%s,%s,%s,%s,%s,%s)" c wf1 p1 wf3 p3 wf2 p2
         | F341 -> printf "h_ahz_PB(%s,%s,%s,%s,%s,%s,%s)" c wf3 p3 wf1 p1 wf2 p2
         | F314 -> printf "h_ahz_PB(%s,%s,%s,%s,%s,%s,%s)" c wf2 p2 wf1 p1 wf3 p3
         | F413 -> printf "h_ahz_PB(%s,%s,%s,%s,%s,%s,%s)" c wf2 p2 wf3 p3 wf1 p1
         | F431 -> printf "h_ahz_PB(%s,%s,%s,%s,%s,%s,%s)" c wf3 p3 wf2 p2 wf1 p1
         | F123 -> printf "z_ahh_PB(%s,%s,%s,%s,%s,%s,%s)" c wf1 p1 wf2 p2 wf3 p3
         | F132 -> printf "z_ahh_PB(%s,%s,%s,%s,%s,%s,%s)" c wf1 p1 wf3 p3 wf2 p2
         | F231 -> printf "z_ahh_PB(%s,%s,%s,%s,%s,%s,%s)" c wf3 p3 wf1 p1 wf2 p2
         | F213 -> printf "z_ahh_PB(%s,%s,%s,%s,%s,%s,%s)" c wf2 p2 wf1 p1 wf3 p3
         | F312 -> printf "z_ahh_PB(%s,%s,%s,%s,%s,%s,%s)" c wf2 p2 wf3 p3 wf1 p1
         | F321 -> printf "z_ahh_PB(%s,%s,%s,%s,%s,%s,%s)" c wf3 p3 wf2 p2 wf1 p1
         end  

      (* \begin{dubious}
     In principle, [p4] could be obtained from the left hand side \ldots
   \end{dubious} *)
      | DScalar4 contractions ->
         let p123 = Printf.sprintf "(-%s-%s-%s)" p1 p2 p3 in
         begin match contractions with
         | [] -> invalid_arg "Targets.print_current: DScalar4 []"
         | head :: tail ->
            printf "(";
            print_dscalar4 c wf1 wf2 wf3 p1 p2 p3 p123 fusion head;
            List.iter (print_add_dscalar4 c wf1 wf2 wf3 p1 p2 p3 p123 fusion) tail;
            printf ")"
         end

      | DScalar2_Vector2 contractions ->
         let p123 = Printf.sprintf "(-%s-%s-%s)" p1 p2 p3 in
         begin match contractions with
         | [] -> invalid_arg "Targets.print_current: DScalar4 []"
         | head :: tail ->
            printf "(";
            print_dscalar2_vector2 c wf1 wf2 wf3 p1 p2 p3 p123 fusion head;
            List.iter (print_add_dscalar2_vector2 c wf1 wf2 wf3 p1 p2 p3 p123 fusion) tail;
            printf ")"
         end

      end

  end
