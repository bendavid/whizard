(* target_VM.ml --

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

(* \thocwmodulesection{O'Mega Virtual Machine with \texttt{Fortran\;90/95}} *)

module Make (Fusion_Maker : Fusion.Maker) (P : Momentum.T) (M : Model.T) =
  struct

    open Coupling
    open Format

    module CM = Colorize.It(M)
    module SCM = Orders.Slice(Colorize.It(M))
    module F = Fusion_Maker(P)(M)
    module CF = Fusion.Multi(Fusion_Maker)(P)(M)
    module CFlow = Color.Flow
    type amplitudes = CF.amplitudes

(* Options. *)
    type diagnostic = All | Arguments | Momenta | Gauge

    let wrapper_module = ref "ovm_wrapper"
    let parameter_module_external = ref "some_external_module_with_model_info"
    let bytecode_file = ref "bytecode.hbc"
    let md5sum = ref None
    let openmp = ref false
    let kind = ref "default"
    let whizard = ref false

    let options = Options.create
      [ "wrapper_module", Arg.String (fun s -> wrapper_module := s),
        "name name of wrapper module";
        "bytecode_file", Arg.String (fun s -> bytecode_file := s),
        "name bytecode file to be used in wrapper";
        "parameter_module_external", Arg.String (fun s ->
                                     parameter_module_external := s),
        "name external parameter module to be used in wrapper";
        "md5sum", Arg.String (fun s -> md5sum := Some s),
        "checksum transfer MD5 checksum in wrapper";
        "whizard", Arg.Set whizard, " include WHIZARD interface in wrapper";
        "openmp", Arg.Set openmp,
        "activate parallel computation of amplitude with OpenMP"]

(* Integers encode the opcodes (operation codes). *)
    let ovm_ADD_MOMENTA = 1
    let ovm_CALC_BRAKET = 2

    let ovm_LOAD_SCALAR = 10
    let ovm_LOAD_SPINOR_INC = 11
    let ovm_LOAD_SPINOR_OUT = 12
    let ovm_LOAD_CONJSPINOR_INC = 13
    let ovm_LOAD_CONJSPINOR_OUT = 14
    let ovm_LOAD_MAJORANA_INC = 15
    let ovm_LOAD_MAJORANA_OUT = 16
    let ovm_LOAD_VECTOR_INC = 17
    let ovm_LOAD_VECTOR_OUT = 18
    let ovm_LOAD_VECTORSPINOR_INC = 19
    let ovm_LOAD_VECTORSPINOR_OUT = 20
    let ovm_LOAD_TENSOR2_INC = 21
    let ovm_LOAD_TENSOR2_OUT = 22
    let ovm_LOAD_BRS_SCALAR = 30
    let ovm_LOAD_BRS_SPINOR_INC = 31
    let ovm_LOAD_BRS_SPINOR_OUT = 32
    let ovm_LOAD_BRS_CONJSPINOR_INC = 33
    let ovm_LOAD_BRS_CONJSPINOR_OUT = 34
    let ovm_LOAD_BRS_VECTOR_INC = 37
    let ovm_LOAD_BRS_VECTOR_OUT = 38
    let ovm_LOAD_MAJORANA_GHOST_INC = 23
    let ovm_LOAD_MAJORANA_GHOST_OUT = 24
    let ovm_LOAD_BRS_MAJORANA_INC = 35
    let ovm_LOAD_BRS_MAJORANA_OUT = 36

    let ovm_PROPAGATE_SCALAR = 51
    let ovm_PROPAGATE_COL_SCALAR = 52
    let ovm_PROPAGATE_GHOST = 53
    let ovm_PROPAGATE_SPINOR = 54
    let ovm_PROPAGATE_CONJSPINOR = 55
    let ovm_PROPAGATE_MAJORANA = 56
    let ovm_PROPAGATE_COL_MAJORANA = 57
    let ovm_PROPAGATE_UNITARITY = 58
    let ovm_PROPAGATE_COL_UNITARITY = 59
    let ovm_PROPAGATE_FEYNMAN = 60
    let ovm_PROPAGATE_COL_FEYNMAN = 61
    let ovm_PROPAGATE_VECTORSPINOR = 62
    let ovm_PROPAGATE_TENSOR2 = 63

(* \begin{dubious}
    [ovm_PROPAGATE_NONE] has to be split up to different types to work
    in conjunction with color MC \dots
   \end{dubious} *)
    let ovm_PROPAGATE_NONE = 64

    let ovm_FUSE_V_FF = -1
    let ovm_FUSE_F_VF = -2
    let ovm_FUSE_F_FV = -3
    let ovm_FUSE_VA_FF = -4
    let ovm_FUSE_F_VAF = -5
    let ovm_FUSE_F_FVA = -6
    let ovm_FUSE_VA2_FF = -7
    let ovm_FUSE_F_VA2F = -8
    let ovm_FUSE_F_FVA2 = -9
    let ovm_FUSE_A_FF = -10
    let ovm_FUSE_F_AF = -11
    let ovm_FUSE_F_FA = -12
    let ovm_FUSE_VL_FF = -13
    let ovm_FUSE_F_VLF = -14
    let ovm_FUSE_F_FVL = -15
    let ovm_FUSE_VR_FF = -16
    let ovm_FUSE_F_VRF = -17
    let ovm_FUSE_F_FVR = -18
    let ovm_FUSE_VLR_FF = -19
    let ovm_FUSE_F_VLRF = -20
    let ovm_FUSE_F_FVLR = -21
    let ovm_FUSE_SP_FF = -22
    let ovm_FUSE_F_SPF = -23
    let ovm_FUSE_F_FSP = -24
    let ovm_FUSE_S_FF = -25
    let ovm_FUSE_F_SF = -26
    let ovm_FUSE_F_FS = -27
    let ovm_FUSE_P_FF = -28
    let ovm_FUSE_F_PF = -29
    let ovm_FUSE_F_FP = -30
    let ovm_FUSE_SL_FF = -31
    let ovm_FUSE_F_SLF = -32
    let ovm_FUSE_F_FSL = -33
    let ovm_FUSE_SR_FF = -34
    let ovm_FUSE_F_SRF = -35
    let ovm_FUSE_F_FSR = -36
    let ovm_FUSE_SLR_FF = -37
    let ovm_FUSE_F_SLRF = -38
    let ovm_FUSE_F_FSLR = -39

    let ovm_FUSE_G_GG = -40
    let ovm_FUSE_V_SS = -41
    let ovm_FUSE_S_VV = -42
    let ovm_FUSE_S_VS = -43
    let ovm_FUSE_V_SV = -44
    let ovm_FUSE_S_SS = -45
    let ovm_FUSE_S_SVV = -46
    let ovm_FUSE_V_SSV = -47
    let ovm_FUSE_S_SSS = -48
    let ovm_FUSE_V_VVV = -49

    let ovm_FUSE_S_G2 = -50
    let ovm_FUSE_G_SG = -51
    let ovm_FUSE_G_GS = -52
    let ovm_FUSE_S_G2_SKEW = -53
    let ovm_FUSE_G_SG_SKEW = -54
    let ovm_FUSE_G_GS_SKEW = -55

    let inst_length = 8

(* Some helper functions. *)
    let printi ~lhs:l ~rhs1:r1 ?coupl:(cp = 0) ?coeff:(co = 0)
               ?rhs2:(r2 = 0) ?rhs3:(r3 = 0) ?rhs4:(r4 = 0) code =
      printf "@\n%d %d %d %d %d %d %d %d" code cp co l r1 r2 r3 r4

    let nl () = printf "@\n"

    let print_int_lst lst = nl (); lst |> List.iter (printf "%d   ")

    let print_str_lst lst = nl (); lst |> List.iter (printf "%s ")

    let break () = printi ~lhs:0 ~rhs1:0 0

(* Copied from below. Needed for header. *)
(* \begin{dubious}
     Could be fused with [lorentz_ordering].
   \end{dubious} *)
    type declarations =
      { scalars : F.wf list;
        spinors : F.wf list;
        conjspinors : F.wf list;
        realspinors : F.wf list;
        ghostspinors : F.wf list;
        vectorspinors : F.wf list;
        vectors : F.wf list;
        ward_vectors : F.wf list;
        massive_vectors : F.wf list;
        tensors_1 : F.wf list;
        tensors_2 : F.wf list;
        brs_scalars : F.wf list;
        brs_spinors : F.wf list;
        brs_conjspinors : F.wf list;
        brs_realspinors : F.wf list;
        brs_vectorspinors : F.wf list;
        brs_vectors : F.wf list;
        brs_massive_vectors : F.wf list }

    let rec classify_wfs' acc = function
      | [] -> acc
      | wf :: rest ->
          classify_wfs'
            (match SCM.lorentz (F.flavor wf) with
            | Scalar -> {acc with scalars = wf :: acc.scalars}
            | Spinor -> {acc with spinors = wf :: acc.spinors}
            | ConjSpinor -> {acc with conjspinors = wf :: acc.conjspinors}
            | Majorana -> {acc with realspinors = wf :: acc.realspinors}
            | Maj_Ghost -> {acc with ghostspinors = wf :: acc.ghostspinors}
            | Vectorspinor ->
                {acc with vectorspinors = wf :: acc.vectorspinors}
            | Vector -> {acc with vectors = wf :: acc.vectors}
            | Massive_Vector ->
                {acc with massive_vectors = wf :: acc.massive_vectors}
            | Tensor_1 -> {acc with tensors_1 = wf :: acc.tensors_1}
            | Tensor_2 -> {acc with tensors_2 = wf :: acc.tensors_2}
            | BRS Scalar -> {acc with brs_scalars = wf :: acc.brs_scalars}
            | BRS Spinor -> {acc with brs_spinors = wf :: acc.brs_spinors}
            | BRS ConjSpinor -> {acc with brs_conjspinors =
                                 wf :: acc.brs_conjspinors}
            | BRS Majorana -> {acc with brs_realspinors =
                               wf :: acc.brs_realspinors}
            | BRS Vectorspinor -> {acc with brs_vectorspinors =
                                   wf :: acc.brs_vectorspinors}
            | BRS Vector -> {acc with brs_vectors = wf :: acc.brs_vectors}
            | BRS Massive_Vector -> {acc with brs_massive_vectors =
                                     wf :: acc.brs_massive_vectors}
            | BRS _ -> invalid_arg "Targets.classify_wfs': not needed here")
            rest

    let classify_wfs wfs = classify_wfs'
      { scalars = [];
        spinors = [];
        conjspinors = [];
        realspinors = [];
        ghostspinors = [];
        vectorspinors = [];
        vectors = [];
        ward_vectors = [];
        massive_vectors = [];
        tensors_1 = [];
        tensors_2 = [];
        brs_scalars = [];
        brs_spinors = [];
        brs_conjspinors = [];
        brs_realspinors = [];
        brs_vectorspinors = [];
        brs_vectors = [];
        brs_massive_vectors = [] } wfs

(* \thocwmodulesubsection{Sets and maps} *)

(* The OVM identifies all objects via integers. Therefore, we need maps
   which assign the abstract object a unique ID. *)

(* I want [int list]s with less elements to come first. Used in conjunction
   with the int list representation of momenta, this will set the outer
   particles at first position and allows the OVM to set them without further
   instructions. *)

(* \begin{dubious}
      Using the Momentum module might give better performance than integer lists?
   \end{dubious} *)
    let rec int_lst_compare (e1 : int list) (e2 : int list) =
      match e1,e2 with
      | [], []  -> 0
      | _, [] -> +1
      | [], _ -> -1
      | [_;_], [_] -> +1
      | [_], [_;_] -> -1
      | hd1 :: tl1, hd2 :: tl2 ->
          let c = compare hd1 hd2 in
          if (c != 0 && List.length tl1 = List.length tl2) then
            c
          else
            int_lst_compare tl1 tl2

(* We need a canonical ordering for the different types
   of wfs. Copied, and slightly modified to order [wf]s, from
   \texttt{fusion.ml}. *)

    let lorentz_ordering wf =
      match SCM.lorentz (F.flavor wf) with
      | Scalar -> 0
      | Spinor -> 1
      | ConjSpinor -> 2
      | Majorana -> 3
      | Vector -> 4
      | Massive_Vector -> 5
      | Tensor_2 -> 6
      | Tensor_1 -> 7
      | Vectorspinor -> 8
      | BRS Scalar -> 9
      | BRS Spinor -> 10
      | BRS ConjSpinor -> 11
      | BRS Majorana -> 12
      | BRS Vector -> 13
      | BRS Massive_Vector -> 14
      | BRS Tensor_2 -> 15
      | BRS Tensor_1 -> 16
      | BRS Vectorspinor -> 17
      | Maj_Ghost -> invalid_arg "lorentz_ordering: not implemented"
      | BRS _ -> invalid_arg "lorentz_ordering: not needed"

    let wf_compare (wf1, mult1) (wf2, mult2) =
      let c1 = compare (lorentz_ordering wf1) (lorentz_ordering wf2) in
      if c1 <> 0 then
        c1
      else
        let c2 = compare wf1 wf2 in
        if c2 <> 0 then
          c2
        else
          compare mult1 mult2

    let amp_compare amp1 amp2 =
      let cflow a = SCM.flow (F.incoming a) (F.outgoing a) in
      let c1 = compare (cflow amp1) (cflow amp2) in
      if c1 <> 0 then
        c1
      else
        let process_sans_color a =
          (List.map SCM.flavor_sans_color (F.incoming a),
           List.map SCM.flavor_sans_color (F.outgoing a)) in
        compare (process_sans_color amp1) (process_sans_color amp2)

    let level_compare (f1, amp1) (f2, amp2) =
      let p1 = F.momentum_list (F.lhs f1)
      and p2 = F.momentum_list (F.lhs f2) in
      let c1 = int_lst_compare p1 p2 in
      if c1 <> 0 then
        c1
      else
        let c2 = compare f1 f2 in
        if c2 <> 0 then
          c2
        else
          amp_compare amp1 amp2

    module ISet = Set.Make (struct type t = int list
                            let compare = int_lst_compare end)

    module WFSet = Set.Make (struct type t = CF.wf * int
                             let compare = wf_compare end)

    module CSet = Set.Make (struct type t = CM.constant
                            let compare = compare end)

    module FSet = Set.Make (struct type t = F.fusion * F.amplitude
                            let compare = level_compare end)

(* \begin{dubious}
     It might be preferable to use a [PMap] which maps mom to int, instead of
     this way. More standard functions like [mem] could be used. Also, [get_ID]
     would be faster, $\mathcal{O}(\log N)$ instead of $\mathcal{O}(N)$, and
     simpler.  For 8 gluons: N=127 momenta. Minor performance issue.
   \end{dubious} *)

    module IMap = Map.Make(Int)

(* For [wf]s it is crucial for the performance to use a different type of
   [Map]s. *)

    module WFMap = Map.Make (struct type t = CF.wf * int
                             let compare = wf_compare end)

    type lookups = { pmap : int list IMap.t;
                     wfmap : int WFMap.t;
                     cmap : CM.constant IMap.t * CM.constant IMap.t;
                     amap : F.amplitude IMap.t;
                     n_wfs : int list;
                     amplitudes : CF.amplitudes;
                     dict : F.amplitude -> F.wf -> int }

    let largest_key imap =
      if (IMap.is_empty imap) then
        failwith "largest_key: Map is empty!"
      else
        fst (IMap.max_binding imap)

(* OCaml's [compare] from pervasives cannot compare functional types, e.g.
   for type [amplitude], if no specific equality function is given ("equal:
   functional value"). Therefore, we allow to specify the ordering. *)

    let get_ID' comp map elt : int =
      let smallmap = IMap.filter (fun _ x -> (comp x elt) = 0 ) map in
      if IMap.is_empty smallmap then
        raise Not_found
      else
        fst (IMap.min_binding smallmap)

(* \begin{dubious}
     Trying to curry [map] here leads to type errors of the
     polymorphic function [get_ID]?
   \end{dubious} *)

    let get_ID map = match map with
      | map -> get_ID' compare map

    let get_const_ID map x = match map with
      | (map1, map2) -> try get_ID' compare map1 x with
                       _ -> try get_ID' compare map2 x with
                       _ -> failwith "Impossible"

(* Creating an integer map of a list with an optional argument that
   indicates where the map should start counting. *)

    let map_of_list ?start:(st=1) lst =
      let g (ind, map) wf = (succ ind, IMap.add ind wf map) in
      lst |> List.fold_left g (st, IMap.empty) |> snd

    let wf_map_of_list ?start:(st=1) lst =
      let g (ind, map) wf = (succ ind, WFMap.add wf ind map) in
      lst |> List.fold_left g (st, WFMap.empty) |> snd

(* \thocwmodulesubsection{Header} *)

(* \begin{dubious}
     [Bijan:]
     It would be nice to save the creation date as comment. However, the Unix
     module doesn't seem to be loaded on default.
   \end{dubious} *)

    let version =
      String.concat " " [Config.version; Config.status; Config.date]
    let model_name =
      let basename = Filename.basename Sys.executable_name in
      try
        Filename.chop_extension basename
      with
      | _ -> basename


    let print_description cmdline  =
      printf "Model %s\n" model_name;
      printf "OVM %s\n" version;
      printf "@\nBytecode file generated automatically by O'Mega for OVM";
      printf "@\nDo not delete any lines. You called O'Mega with";
      printf "@\n  %s" cmdline;
      (*i
      let t = Unix.localtime (Unix.time() ) in
        printf "@\n on %5d %5d %5d" (succ t.Unix.tm_mon) t.Unix.tm_mday
               t.Unix.tm_year;
       i*)
      printf "@\n"

    let num_classified_wfs wfs =
      let wfs' = classify_wfs wfs in
      List.map List.length
        [ wfs'.scalars @ wfs'.brs_scalars;
          wfs'.spinors @ wfs'.brs_spinors;
          wfs'.conjspinors @ wfs'.brs_conjspinors;
          wfs'.realspinors @ wfs'.brs_realspinors @ wfs'.ghostspinors;
          wfs'.vectors @ wfs'.massive_vectors @ wfs'.brs_vectors
            @ wfs'.brs_massive_vectors @ wfs'.ward_vectors;
          wfs'.tensors_2;
          wfs'.tensors_1;
          wfs'.vectorspinors ]

    let description_classified_wfs =
      [ "N_scalars";
        "N_spinors";
        "N_conjspinors";
        "N_bispinors";
        "N_vectors";
        "N_tensors_2";
        "N_tensors_1";
        "N_vectorspinors" ]

    let num_particles_in amp =
      match CF.flavors amp with
      | [] -> 0
      | (fin, _) :: _ -> List.length fin

    let num_particles_out amp =
      match CF.flavors amp with
      | [] -> 0
      | (_, fout) :: _ -> List.length fout

    let num_particles amp =
      match CF.flavors amp with
      | [] -> 0
      | (fin, fout) :: _ -> List.length fin + List.length fout

    let num_color_indices_default = 2 (* Standard model and non-color-exotica *)

    let num_color_indices amp =
      try CFlow.rank (List.hd (CF.color_flows amp)) with
      _ -> num_color_indices_default

    let num_color_factors amp =
      let table = CF.color_factors amp in
      let n_cflow = Array.length table
      and n_cfactors = ref 0 in
      for c1 = 0 to pred n_cflow do
        for c2 = 0 to pred n_cflow do
          if c1 <= c2 then begin
            match table.(c1).(c2) with
            | [] -> ()
            | _ -> incr n_cfactors
          end
        done
      done;
      !n_cfactors

    let num_helicities amp = amp |> CF.helicities |> List.length

    let num_flavors amp = amp |> CF.flavors |> List.length

    let num_ks amp = amp |> CF.processes |> List.length

    let num_color_flows amp = amp |> CF.color_flows |> List.length

(* Use [fst] since [WFSet.t = F.wf * int]. *)
    let num_wfs wfset = wfset |> WFSet.elements |> List.map fst
                              |> num_classified_wfs

(* [largest_key] gives the number of momenta if applied to [pmap]. *)

    let num_lst lookups wfset =
      [ largest_key lookups.pmap;
        num_particles lookups.amplitudes;
        num_particles_in lookups.amplitudes;
        num_particles_out lookups.amplitudes;
        num_ks lookups.amplitudes;
        num_helicities lookups.amplitudes;
        num_color_flows lookups.amplitudes;
        num_color_indices lookups.amplitudes;
        num_flavors lookups.amplitudes;
        num_color_factors lookups.amplitudes ] @ num_wfs wfset

    let description_lst =
      [ "N_momenta";
        "N_particles";
        "N_prt_in";
        "N_prt_out";
        "N_amplitudes";
        "N_helicities";
        "N_col_flows";
        "N_col_indices";
        "N_flavors";
        "N_col_factors" ] @ description_classified_wfs

    let print_header' numbers =
      let chopped_num_lst = ThoList.chopn inst_length numbers
      and chopped_desc_lst = ThoList.chopn inst_length description_lst
      and printer a b = print_str_lst a; print_int_lst b in
      List.iter2 printer chopped_desc_lst chopped_num_lst

    let print_header lookups wfset = print_header' (num_lst lookups wfset)

    let print_zero_header () =
      let rec zero_list' j =
        if j < 1 then []
        else 0 :: zero_list' (j - 1) in
      let zero_list i = zero_list' (i + 1) in
      description_lst |> List.length |> zero_list |> print_header'

(* \thocwmodulesubsection{Tables} *)

    let print_spin_table' tuples =
      match tuples with
      | [] -> ()
      | _ -> tuples |> List.iter ( fun (tuple1, tuple2) ->
          tuple1 @ tuple2 |> List.map (Printf.sprintf "%d ")
                          |> String.concat "" |> printf "@\n%s" )

    let print_spin_table amplitudes =
      printf "@\nSpin states table";
      print_spin_table' @@ CF.helicities amplitudes

    let print_flavor_table tuples =
      match tuples with
      | [] -> ()
      | _ -> List.iter ( fun tuple -> tuple
                        |> List.map (fun f -> Printf.sprintf "%d " @@ M.pdg f)
                        |> String.concat "" |> printf "@\n%s"
                       ) tuples

    let print_flavor_tables amplitudes =
      printf "@\nFlavor states table";
      print_flavor_table @@ List.map (fun (fin, fout) -> fin @ fout)
                         @@ CF.flavors amplitudes

    let print_color_flows_table' tuple =
        match CFlow.to_lists tuple with
        | [] -> ()
        | cfs -> printf "@\n%s" @@ String.concat "" @@ List.map
                  ( fun cf -> cf |> List.map (Printf.sprintf "%d ")
                                 |> String.concat ""
                  ) cfs

    let print_color_flows_table tuples =
      match tuples with
      | [] -> ()
      | _ -> List.iter print_color_flows_table' tuples

    let print_ghost_flags_table tuples =
      match tuples with
      | [] -> ()
      | _ ->
        List.iter (fun tuple ->
        match CFlow.ghost_flags tuple with
            | [] -> ()
            | gfs -> printf "@\n"; List.iter (fun gf -> printf "%s "
              (if gf then "1" else "0") ) gfs
        ) tuples

    let format_power
      { CFlow.num = num; CFlow.den = den; CFlow.power = pwr } =
      match num, den, pwr with
      | _, 0, _ -> invalid_arg "targets.format_power: zero denominator"
      | n, d, p -> [n; d; p]

    let format_powers = function
      | [] -> [0]
      | powers -> List.flatten (List.map format_power powers)

    (*i
    (* We go through the array line by line and collect all colorfactors which
     * are nonzero because their corresponding color flows match.
     * With the gained intset, we would be able to print only the necessary
     * coefficients of the symmetric matrix and indicate from where the OVM
     * can copy the rest. However, this approach gets really slow for many
     * gluons and we can save at most 3 numbers per line.*)

    let print_color_factor_table_funct table =
      let n_cflow = Array.length table in
      let (intset, _, _ ) =
        let rec fold_array (set, cf1, cf2) =
          if cf1 > pred n_cflow then (set, 0, 0)
          else
              let returnset =
              match table.(cf1).(cf2) with
                  | [] -> set
                  | cf ->
                      ISet.add ([succ cf1; succ cf2] @ (format_powers cf)) set
              in
              if cf2 < pred n_cflow then
                fold_array (returnset, cf1, succ cf2) else
                fold_array (returnset, succ cf1, 0)
        in
        fold_array (ISet.empty, 0, 0)
      in
      let map = map_of_list (ISet.elements intset) in
      List.iter (fun x -> printf "@\n"; let xth = List.nth x in
      if (xth 0 <= xth 1) then List.iter (printf "%d ") x
      else printf "%d %d" 0 (get_ID map x))
        (ISet.elements intset)

    let print_color_factor_table_old table =
      let n_cflow = Array.length table in
      let (intlsts, _, _ ) =
        let rec fold_array (lsts, cf1, cf2) =
          if cf1 > pred n_cflow then (lsts, 0, 0)
          else
              let returnlsts =
              match table.(cf1).(cf2) with
                  | [] -> lsts
                  | cf -> ([succ cf1; succ cf2] @ (format_powers cf)) :: lsts
              in
              if cf2 < pred n_cflow then
                fold_array (returnlsts, cf1, succ cf2) else
                fold_array (returnlsts, succ cf1, 0)
        in
        fold_array ([], 0, 0)
      in
      let intlsts = List.rev intlsts in
      List.iter (fun x -> printf "@\n"; List.iter (printf "%d ") x ) intlsts
      i*)

(* Straightforward iteration gives a great speedup compared to the fancier
   approach which only collects nonzero colorfactors. *)

    let print_color_factor_table table =
      let n_cflow = Array.length table in
      if n_cflow > 0 then begin
        for c1 = 0 to pred n_cflow do
          for c2 = 0 to pred n_cflow do
            if c1 <= c2 then begin
              match table.(c1).(c2) with
              | [] -> ()
              | cf -> printf "@\n"; List.iter (printf "%9d")
                ([succ c1; succ c2] @ (format_powers cf));
            end
          done
        done
      end

    let option_to_binary = function
      | Some _ -> "1"
      | None -> "0"

    let print_flavor_color_table n_flv n_cflow table =
      if n_flv > 0 then begin
        for c = 0 to pred n_cflow do
          printf "@\n";
          for f = 0 to pred n_flv do
            printf "%s " (option_to_binary table.(f).(c))
          done;
        done;
      end

    let print_color_tables amplitudes =
      let cflows =  CF.color_flows amplitudes
      and cfactors = CF.color_factors amplitudes in
      printf "@\nColor flows table: [ (i, j) (k, l) -> (m, n) ...]";
      print_color_flows_table cflows;
      printf "@\nColor ghost flags table:";
      print_ghost_flags_table cflows;
      printf "@\nColor factors table: [ i, j: num den power], %s"
        "i, j are indexed color flows";
      print_color_factor_table cfactors;
      printf "@\nFlavor color combination is allowed:";
      print_flavor_color_table (num_flavors amplitudes) (List.length
        (CF.color_flows amplitudes)) (CF.process_table amplitudes)

(* \thocwmodulesubsection{Momenta} *)

(* Add the momenta of a WFSet to a Iset. For now, we are throwing away the
   information to which amplitude the momentum belongs. This could be optimized
   for random color flow computations. *)

    let momenta_set wfset =
      let get_mom wf = wf |> fst |> F.momentum_list in
      let momenta = List.map get_mom (WFSet.elements wfset) in
      momenta |> List.fold_left (fun set x -> set |> ISet.add x) ISet.empty

    let chop_in_3 lst =
      let ceil_div i j = if (i mod j = 0) then i/j else i/j + 1 in
      ThoList.chopn (ceil_div (List.length lst) 3) lst

(* Assign momenta via instruction code. External momenta [[_]] are already
   set by the OVM. To avoid unnecessary look-ups of IDs we seperate two cases.
   If we have more, we split up in two or three parts. *)

    let add_mom p pmap =
      let print_mom lhs rhs1 rhs2 rhs3 = if (rhs1!= 0) then
        printi ~lhs:lhs ~rhs1:rhs1 ~rhs2:rhs2 ~rhs3:rhs3 ovm_ADD_MOMENTA in
      let get_p_ID = get_ID pmap in
      match p with
      | [] | [_] -> print_mom 0 0 0 0
      | [rhs1;rhs2] -> print_mom (get_p_ID [rhs1;rhs2]) rhs1 rhs2 0
      | [rhs1;rhs2;rhs3] -> print_mom (get_p_ID [rhs1;rhs2;rhs3]) rhs1 rhs2 rhs3
      | more ->
          let ids = List.map get_p_ID (chop_in_3 more) in
          if (List.length ids = 3) then
            print_mom (get_p_ID more) (List.nth ids 0) (List.nth ids 1)
              (List.nth ids 2)
          else
            print_mom (get_p_ID more) (List.nth ids 0) (List.nth ids 1) 0

(* Hand through the current level and print level seperators if necessary. *)

    let add_all_mom lookups pset =
      let add_all' level p =
        let level' = List.length p in
        if (level' > level && level' > 3) then break ();
        add_mom p lookups.pmap; level'
      in
      ignore (pset |> ISet.elements |> List.fold_left add_all' 1)

(* Expand a set of momenta to contain all needed momenta for the computation
   in the OVM. For this, we create a list of sets which contains the chopped
   momenta and unify them afterwards. If the set has become larger, we
   expand again. *)

    let rec expand_pset p =
      let momlst = ISet.elements p in
      let pset_of lst = List.fold_left (fun s x -> ISet.add x s) ISet.empty
        lst in
      let sets = List.map (fun x -> pset_of (chop_in_3 x) ) momlst in
      let bigset = List.fold_left ISet.union ISet.empty sets in
      let biggerset = ISet.union bigset p in
      if (List.length momlst < List.length (ISet.elements biggerset) ) then
        expand_pset biggerset
      else
        biggerset

    let mom_ID pmap wf = get_ID pmap (F.momentum_list wf)

(* \thocwmodulesubsection{Wavefunctions and externals} *)

(* [mult_wf] is needed because the [wf] with same combination of flavor and
   momentum can have different dependencies and content. *)

    let mult_wf dict amplitude wf =
      try
        wf, dict amplitude wf
      with
        | Not_found -> wf, 0

(* Build the union of all [wf]s of all amplitudes and a map of the amplitudes. *)

    let wfset_amps amplitudes =
      let amap = amplitudes |> CF.processes |> List.sort amp_compare
                            |> map_of_list
      and dict = CF.dictionary amplitudes in
      let wfset_amp amp =
        let f = mult_wf dict amp in
        let lst = List.map f ((F.externals amp) @ (F.variables amp)) in
        lst |> List.fold_left (fun s x -> WFSet.add x s) WFSet.empty in
      let list_of_sets = amplitudes |> CF.processes |> List.map wfset_amp in
        List.fold_left WFSet.union WFSet.empty list_of_sets, amap

(* To obtain the Fortran index, we substract the number of precedent wave
   functions. *)

    let lorentz_ordering_reduced wf =
      match SCM.lorentz (F.flavor wf) with
      | Scalar | BRS Scalar -> 0
      | Spinor | BRS Spinor -> 1
      | ConjSpinor | BRS ConjSpinor -> 2
      | Majorana | BRS Majorana -> 3
      | Vector | BRS Vector | Massive_Vector | BRS Massive_Vector -> 4
      | Tensor_2 | BRS Tensor_2 -> 5
      | Tensor_1 | BRS Tensor_1 -> 6
      | Vectorspinor | BRS Vectorspinor -> 7
      | Maj_Ghost -> invalid_arg "lorentz_ordering: not implemented"
      | BRS _ -> invalid_arg "lorentz_ordering: not needed"

    let wf_index wfmap num_lst (wf, i) =
      let wf_ID = WFMap.find (wf, i) wfmap
      and sum lst = List.fold_left (fun x y -> x+y) 0 lst in
        wf_ID - sum (ThoList.hdn (lorentz_ordering_reduced wf) num_lst)

    let print_ext lookups amp_ID inc (wf, i) =
      let mom = (F.momentum_list wf) in
      let outer_index = if List.length mom = 1 then List.hd mom else
        failwith "targets.print_ext: called with non-external particle"
      and f = F.flavor wf in
      let pdg = SCM.pdg f
      and wf_code =
        match SCM.lorentz f with
        | Scalar -> ovm_LOAD_SCALAR
        | BRS Scalar -> ovm_LOAD_BRS_SCALAR
        | Spinor ->
            if inc then ovm_LOAD_SPINOR_INC
            else ovm_LOAD_SPINOR_OUT
        | BRS Spinor ->
            if inc then ovm_LOAD_BRS_SPINOR_INC
            else ovm_LOAD_BRS_SPINOR_OUT
        | ConjSpinor ->
            if inc then ovm_LOAD_CONJSPINOR_INC
            else ovm_LOAD_CONJSPINOR_OUT
        | BRS ConjSpinor ->
            if inc then ovm_LOAD_BRS_CONJSPINOR_INC
            else ovm_LOAD_BRS_CONJSPINOR_OUT
        | Vector | Massive_Vector ->
            if inc then ovm_LOAD_VECTOR_INC
            else ovm_LOAD_VECTOR_OUT
        | BRS Vector | BRS Massive_Vector ->
            if inc then ovm_LOAD_BRS_VECTOR_INC
            else ovm_LOAD_BRS_VECTOR_OUT
        | Tensor_2 ->
            if inc then ovm_LOAD_TENSOR2_INC
            else ovm_LOAD_TENSOR2_OUT
        | Vectorspinor | BRS Vectorspinor ->
            if inc then ovm_LOAD_VECTORSPINOR_INC
            else ovm_LOAD_VECTORSPINOR_OUT
        | Majorana ->
            if inc then ovm_LOAD_MAJORANA_INC
            else ovm_LOAD_MAJORANA_OUT
        | BRS Majorana ->
            if inc then ovm_LOAD_BRS_MAJORANA_INC
            else ovm_LOAD_BRS_MAJORANA_OUT
        | Maj_Ghost ->
            if inc then ovm_LOAD_MAJORANA_GHOST_INC
            else ovm_LOAD_MAJORANA_GHOST_OUT
        | Tensor_1 ->
            invalid_arg "targets.print_ext: Tensor_1 only internal"
        | BRS _ ->
            failwith "targets.print_ext: Not implemented"
      and wf_ind = wf_index lookups.wfmap lookups.n_wfs (wf, i)
      in
        printi wf_code ~lhs:wf_ind ~coupl:(abs(pdg)) ~rhs1:outer_index ~rhs4:amp_ID

    let print_ext_amp lookups amplitude =
      let incoming = (List.map (fun _ -> true) (F.incoming amplitude) @
                      List.map (fun _ -> false) (F.outgoing amplitude))
      and amp_ID = get_ID' amp_compare lookups.amap amplitude in
      let wf_tpl wf = mult_wf lookups.dict amplitude wf in
      let print_ext_wf inc wf = wf |> wf_tpl |> print_ext lookups amp_ID inc in
        List.iter2 print_ext_wf incoming (F.externals amplitude)

    let print_externals lookups seen_wfs amplitude =
      let externals =
        List.combine
          (F.externals amplitude)
          (List.map (fun _ -> true) (F.incoming amplitude) @
           List.map (fun _ -> false) (F.outgoing amplitude)) in
      List.fold_left (fun seen (wf, incoming) ->
        let amp_ID = get_ID' amp_compare lookups.amap amplitude in
        let wf_tpl = mult_wf lookups.dict amplitude wf in
        if not (WFSet.mem wf_tpl seen) then begin
          wf_tpl |> print_ext lookups amp_ID incoming
        end;
        WFSet.add wf_tpl seen) seen_wfs externals

(* [print_externals] and [print_ext_amp] do in principle the same thing but
   [print_externals] filters out dublicate external wave functions. Even with
   [print_externals] the same (numerically) external wave function will be
   loaded if it belongs to a different color flow, just as in the native Fortran
   code.  For color MC, [print_ext_amp] has to be used (redundant instructions
   but only one flow is computed) and the filtering of duplicate fusions has to
   be disabled. *)

    let print_ext_amps lookups =
      let print_external_amp s x = print_externals lookups s x in
      ignore (
        List.fold_left print_external_amp WFSet.empty
          (CF.processes lookups.amplitudes)
        )
      (*i
      List.iter (print_ext_amp lookups) (CF.processes lookups.amplitudes)
      i*)

(* \thocwmodulesubsection{Currents} *)

(* Parallelization issues: All fusions have to be completed before the
   propagation takes place. Preferably each fusion and propagation is done
   by one thread.  Solution: All fusions are subinstructions, i.e. if
   they are read by the main loop they are skipped. If a propagation
   occurs, all fusions have to be computed first. The additional control
   bit is the sign of the first int of an instruction. *)

    (*i TODO: (bcn 2014-07-21) Majorana support will come some day maybe i*)
    let print_fermion_current code_a code_b code_c coeff lhs c wf1 wf2 fusion =
      let printc code r1 r2 = printi code ~lhs:lhs ~coupl:c ~coeff:coeff
        ~rhs1:r1 ~rhs2:r2 in
      match fusion with
      | F13 -> printc code_a wf1 wf2
      | F31 -> printc code_a wf2 wf1
      | F23 -> printc code_b wf1 wf2
      | F32 -> printc code_b wf2 wf1
      | F12 -> printc code_c wf1 wf2
      | F21 -> printc code_c wf2 wf1

      let ferm_print_current = function
        | coeff, Psibar, V, Psi -> print_fermion_current
          ovm_FUSE_V_FF ovm_FUSE_F_VF ovm_FUSE_F_FV coeff
        | coeff, Psibar, VA, Psi -> print_fermion_current
          ovm_FUSE_VA_FF ovm_FUSE_F_VAF ovm_FUSE_F_FVA coeff
        | coeff, Psibar, VA2, Psi -> print_fermion_current
          ovm_FUSE_VA2_FF ovm_FUSE_F_VA2F ovm_FUSE_F_FVA2 coeff
        | coeff, Psibar, A, Psi -> print_fermion_current
          ovm_FUSE_A_FF ovm_FUSE_F_AF ovm_FUSE_F_FA coeff
        | coeff, Psibar, VL, Psi -> print_fermion_current
          ovm_FUSE_VL_FF ovm_FUSE_F_VLF ovm_FUSE_F_FVL coeff
        | coeff, Psibar, VR, Psi -> print_fermion_current
          ovm_FUSE_VR_FF ovm_FUSE_F_VRF ovm_FUSE_F_FVR coeff
        | coeff, Psibar, VLR, Psi -> print_fermion_current
          ovm_FUSE_VLR_FF ovm_FUSE_F_VLRF ovm_FUSE_F_FVLR coeff
        | coeff, Psibar, SP, Psi -> print_fermion_current
          ovm_FUSE_SP_FF ovm_FUSE_F_SPF ovm_FUSE_F_FSP coeff
        | coeff, Psibar, S, Psi -> print_fermion_current
          ovm_FUSE_S_FF ovm_FUSE_F_SF ovm_FUSE_F_FS coeff
        | coeff, Psibar, P, Psi -> print_fermion_current
          ovm_FUSE_P_FF ovm_FUSE_F_PF ovm_FUSE_F_FP coeff
        | coeff, Psibar, SL, Psi -> print_fermion_current
          ovm_FUSE_SL_FF ovm_FUSE_F_SLF ovm_FUSE_F_FSL coeff
        | coeff, Psibar, SR, Psi -> print_fermion_current
          ovm_FUSE_SR_FF ovm_FUSE_F_SRF ovm_FUSE_F_FSR coeff
        | coeff, Psibar, SLR, Psi -> print_fermion_current
          ovm_FUSE_SLR_FF ovm_FUSE_F_SLRF ovm_FUSE_F_FSLR coeff
        | _, Psibar, _, Psi -> invalid_arg
          "Targets.Fortran.VM: no superpotential here"
        | _, Chibar, _, _ | _, _, _, Chi -> invalid_arg
          "Targets.Fortran.VM: Majorana spinors not handled"
        | _, Gravbar, _, _ | _, _, _, Grav -> invalid_arg
          "Targets.Fortran.VM: Gravitinos not handled"

    let children2 rhs =
      match F.children rhs with
      | [wf1; wf2] -> (wf1, wf2)
      | _ -> failwith "Targets.children2: can't happen"

    let children3 rhs =
      match F.children rhs with
      | [wf1; wf2; wf3] -> (wf1, wf2, wf3)
      | _ -> invalid_arg "Targets.children3: can't happen"

    let print_vector4 c lhs wf1 wf2 wf3 fusion (coeff, contraction) =
      let printc r1 r2 r3 = printi ovm_FUSE_V_VVV ~lhs:lhs ~coupl:c
        ~coeff:coeff ~rhs1:r1 ~rhs2:r2 ~rhs3:r3 in
      match contraction, fusion with
      | C_12_34, (F341|F431|F342|F432|F123|F213|F124|F214)
      | C_13_42, (F241|F421|F243|F423|F132|F312|F134|F314)
      | C_14_23, (F231|F321|F234|F324|F142|F412|F143|F413) ->
          printc wf1 wf2 wf3
      | C_12_34, (F134|F143|F234|F243|F312|F321|F412|F421)
      | C_13_42, (F124|F142|F324|F342|F213|F231|F413|F431)
      | C_14_23, (F123|F132|F423|F432|F214|F241|F314|F341) ->
          printc wf2 wf3 wf1
      | C_12_34, (F314|F413|F324|F423|F132|F231|F142|F241)
      | C_13_42, (F214|F412|F234|F432|F123|F321|F143|F341)
      | C_14_23, (F213|F312|F243|F342|F124|F421|F134|F431) ->
          printc wf1 wf3 wf2

    let print_current lookups lhs amplitude rhs =
      let f = mult_wf lookups.dict amplitude in
      match F.coupling rhs with
      | V3 (vertex, fusion, constant) ->
          let ch1, ch2 = children2 rhs in
          let wf1 = wf_index lookups.wfmap lookups.n_wfs (f ch1)
          and wf2 = wf_index lookups.wfmap lookups.n_wfs (f ch2)
          and p1 = mom_ID lookups.pmap ch1
          and p2 = mom_ID lookups.pmap ch2
          and const_ID = get_const_ID lookups.cmap constant in
          let c = if (F.sign rhs) < 0 then - const_ID else const_ID in
          begin match vertex with
          | FBF (coeff, fb, b, f) ->
              begin match coeff, fb, b, f with
              | _, Psibar, VLRM, Psi | _, Psibar, SPM, Psi
              | _, Psibar, TVA, Psi | _, Psibar, TVAM, Psi
              | _, Psibar, TLR, Psi | _, Psibar, TLRM, Psi
              | _, Psibar, TRL, Psi | _, Psibar, TRLM, Psi -> failwith
       "print_current: V3: Momentum dependent fermion couplings not implemented"
              | _, _, _, _ ->
                  ferm_print_current (coeff, fb, b, f) lhs c wf1 wf2 fusion
              end
          | PBP (_, _, _, _) ->
              failwith "print_current: V3: PBP not implemented"
          | BBB (_, _, _, _) ->
              failwith "print_current: V3: BBB not implemented"
          | GBG (_, _, _, _) ->
              failwith "print_current: V3: GBG not implemented"

          | Gauge_Gauge_Gauge coeff ->
              let printc r1 r2 r3 r4 = printi ovm_FUSE_G_GG
                ~lhs:lhs ~coupl:c ~coeff:coeff ~rhs1:r1 ~rhs2:r2 ~rhs3:r3
                ~rhs4:r4 in
              begin match fusion with
              | (F23|F31|F12) -> printc wf1 p1 wf2 p2
              | (F32|F13|F21) -> printc wf2 p2 wf1 p1
              end

          | I_Gauge_Gauge_Gauge _ ->
              failwith "print_current: I_Gauge_Gauge_Gauge: not implemented"

          | Scalar_Vector_Vector coeff ->
              let printc code r1 r2 = printi code
                ~lhs:lhs ~coupl:c ~coeff:coeff ~rhs1:r1 ~rhs2:r2 in
              begin match fusion with
              | (F23|F32) -> printc ovm_FUSE_S_VV wf1 wf2
              | (F12|F13) -> printc ovm_FUSE_V_SV wf1 wf2
              | (F21|F31) -> printc ovm_FUSE_V_SV wf2 wf1
              end

          | Scalar_Scalar_Scalar coeff ->
              printi ovm_FUSE_S_SS ~lhs:lhs ~coupl:c ~coeff:coeff ~rhs1:wf1 ~rhs2:wf2

          | Vector_Scalar_Scalar coeff ->
              let printc code ?flip:(f = 1) r1 r2 r3 r4 = printi code
                ~lhs:lhs ~coupl:(c*f) ~coeff:coeff ~rhs1:r1 ~rhs2:r2 ~rhs3:r3
                ~rhs4:r4 in
              begin match fusion with
              | F23 -> printc ovm_FUSE_V_SS wf1 p1 wf2 p2
              | F32 -> printc ovm_FUSE_V_SS wf2 p2 wf1 p1
              | F12 -> printc ovm_FUSE_S_VS wf1 p1 wf2 p2
              | F21 -> printc ovm_FUSE_S_VS wf2 p2 wf1 p1
              | F13 -> printc ovm_FUSE_S_VS wf1 p1 wf2 p2 ~flip:(-1)
              | F31 -> printc ovm_FUSE_S_VS wf2 p2 wf1 p1 ~flip:(-1)
              end

          | Aux_Vector_Vector _ ->
              failwith "print_current: V3: not implemented"

          | Aux_Scalar_Scalar _ ->
              failwith "print_current: V3: not implemented"

          | Aux_Scalar_Vector _ ->
              failwith "print_current: V3: not implemented"

          | Graviton_Scalar_Scalar _ ->
              failwith "print_current: V3: not implemented"

          | Graviton_Vector_Vector _ ->
              failwith "print_current: V3: not implemented"

          | Graviton_Spinor_Spinor _ ->
              failwith "print_current: V3: not implemented"

          | Dim4_Vector_Vector_Vector_T _ ->
              failwith "print_current: V3: not implemented"

          | Dim4_Vector_Vector_Vector_L _ ->
              failwith "print_current: V3: not implemented"

          | Dim6_Gauge_Gauge_Gauge _ ->
              failwith "print_current: V3: not implemented"

          | Dim4_Vector_Vector_Vector_T5 _ ->
              failwith "print_current: V3: not implemented"

          | Dim4_Vector_Vector_Vector_L5 _ ->
              failwith "print_current: V3: not implemented"

          | Dim6_Gauge_Gauge_Gauge_5 _ ->
              failwith "print_current: V3: not implemented"

          | Aux_DScalar_DScalar _ ->
              failwith "print_current: V3: not implemented"

          | Aux_Vector_DScalar _ ->
              failwith "print_current: V3: not implemented"

          | Dim5_Scalar_Gauge2 coeff ->
              let printc code r1 r2 r3 r4 =  printi code
                ~lhs:lhs ~coupl:c ~coeff:coeff ~rhs1:r1 ~rhs2:r2 ~rhs3:r3
                ~rhs4:r4  in
              begin match fusion with
              | (F23|F32) -> printc ovm_FUSE_S_G2 wf1 p1 wf2 p2
              | (F12|F13) -> printc ovm_FUSE_G_SG wf1 p1 wf2 p2
              | (F21|F31) -> printc ovm_FUSE_G_GS wf2 p2 wf1 p1
              end

          | Dim5_Scalar_Gauge2_Skew coeff ->
              let printc code ?flip:(f = 1) r1 r2 r3 r4 =  printi code
                ~lhs:lhs ~coupl:(c*f) ~coeff:coeff ~rhs1:r1 ~rhs2:r2 ~rhs3:r3
                ~rhs4:r4  in
              begin match fusion with
              | (F23|F32) -> printc ovm_FUSE_S_G2_SKEW wf1 p1 wf2 p2
              | (F12|F13) -> printc ovm_FUSE_G_SG_SKEW wf1 p1 wf2 p2
              | (F21|F31) -> printc ovm_FUSE_G_GS_SKEW wf2 p1 wf1 p2 ~flip:(-1)
              end

          | Dim5_Scalar_Vector_Vector_T _ ->
              failwith "print_current: V3: not implemented"

          | Dim5_Scalar_Vector_Vector_U _ ->
              failwith "print_current: V3: not implemented"

          | Dim5_Scalar_Scalar2 _ ->
              failwith "print_current: V3: not implemented"

          | Dim6_Vector_Vector_Vector_T _ ->
              failwith "print_current: V3: not implemented"

          | Tensor_2_Vector_Vector _ ->
              failwith "print_current: V3: not implemented"

          | Tensor_2_Scalar_Scalar _ ->
              failwith "print_current: V3: not implemented"

          | Dim5_Tensor_2_Vector_Vector_1 _ ->
              failwith "print_current: V3: not implemented"

          | Dim5_Tensor_2_Vector_Vector_2 _ ->
              failwith "print_current: V3: not implemented"

          | Dim7_Tensor_2_Vector_Vector_T _ ->
              failwith "print_current: V3: not implemented"

          | Dim5_Scalar_Vector_Vector_TU _ ->
              failwith "print_current: V3: not implemented"

          | Scalar_Vector_Vector_t _ ->
              failwith "print_current: V3: not implemented"

          | Tensor_2_Vector_Vector_cf _ ->
              failwith "print_current: V3: not implemented"

          | Tensor_2_Scalar_Scalar_cf _ ->
              failwith "print_current: V3: not implemented"

          | Tensor_2_Vector_Vector_1 _ ->
              failwith "print_current: V3: not implemented"

          | Tensor_2_Vector_Vector_t _ ->
              failwith "print_current: V3: not implemented"

          | TensorVector_Vector_Vector _ ->
              failwith "print_current: V3: not implemented"

          | TensorVector_Vector_Vector_cf _ ->
              failwith "print_current: V3: not implemented"

          | TensorVector_Scalar_Scalar _ ->
              failwith "print_current: V3: not implemented"

          | TensorVector_Scalar_Scalar_cf _ ->
              failwith "print_current: V3: not implemented"

          | TensorScalar_Vector_Vector _ ->
              failwith "print_current: V3: not implemented"

          | TensorScalar_Vector_Vector_cf _ ->
              failwith "print_current: V3: not implemented"

          | TensorScalar_Scalar_Scalar _ ->
              failwith "print_current: V3: not implemented"

          | TensorScalar_Scalar_Scalar_cf _ ->
              failwith "print_current: V3: not implemented"

          | Dim6_Scalar_Vector_Vector_D _ ->
              failwith "print_current: V3: not implemented"

          | Dim6_Scalar_Vector_Vector_DP _ ->
              failwith "print_current: V3: not implemented"

          | Dim6_HAZ_D _ ->
              failwith "print_current: V3: not implemented"

          | Dim6_HAZ_DP _ ->
              failwith "print_current: V3: not implemented"

          | Dim6_HHH _ ->
              failwith "print_current: V3: not implemented"  

          | Dim6_Gauge_Gauge_Gauge_i _ ->
              failwith "print_current: V3: not implemented"  

          | Gauge_Gauge_Gauge_i _ ->
              failwith "print_current: V3: not implemented"

          | Dim6_GGG _ ->
              failwith "print_current: V3: not implemented"	

          | Dim6_AWW_DP _ ->
              failwith "print_current: V3: not implemented"

          | Dim6_AWW_DW _ ->
              failwith "print_current: V3: not implemented"

          | Dim6_WWZ_DPWDW _ ->
              failwith "print_current: V3: not implemented"
 
          | Dim6_WWZ_DW _ ->
              failwith "print_current: V3: not implemented"
 
          | Dim6_WWZ_D _ ->
              failwith "print_current: V3: not implemented"

          | Aux_Gauge_Gauge _ ->
              failwith "print_current: V3 (Aux_Gauge_Gauge): not implemented"

   end

(* Flip the sign in [c] to account for the~$\mathrm{i}^2$ relative to diagrams
   with only cubic couplings. *)
      | V4 (vertex, fusion, constant) ->
          let ch1, ch2, ch3 = children3 rhs in
          let wf1 = wf_index lookups.wfmap lookups.n_wfs (f ch1)
          and wf2 = wf_index lookups.wfmap lookups.n_wfs (f ch2)
          and wf3 = wf_index lookups.wfmap lookups.n_wfs (f ch3)
          (*i
          (*and p1 = mom_ID lookups.pmap ch1*)
          (*and p2 = mom_ID lookups.pmap ch2*)
          (*and p3 = mom_ID lookups.pmap ch2*)
          i*)
          and const_ID = get_const_ID lookups.cmap constant in
          let c =
            if (F.sign rhs) < 0 then const_ID else - const_ID in
          begin match vertex with
          | Scalar4 coeff ->
              printi ovm_FUSE_S_SSS ~lhs:lhs ~coupl:c ~coeff:coeff ~rhs1:wf1
                ~rhs2:wf2 ~rhs3:wf3
          | Scalar2_Vector2 coeff ->
              let printc code r1 r2 r3 = printi code
                ~lhs:lhs ~coupl:c ~coeff:coeff ~rhs1:r1 ~rhs2:r2 ~rhs3:r3 in
              begin match fusion with
              | F134 | F143 | F234 | F243 ->
                  printc ovm_FUSE_S_SVV wf1 wf2 wf3
              | F314 | F413 | F324 | F423 ->
                  printc ovm_FUSE_S_SVV wf2 wf1 wf3
              | F341 | F431 | F342 | F432 ->
                  printc ovm_FUSE_S_SVV wf3 wf1 wf2
              | F312 | F321 | F412 | F421 ->
                  printc ovm_FUSE_V_SSV wf2 wf3 wf1
              | F231 | F132 | F241 | F142 ->
                  printc ovm_FUSE_V_SSV wf1 wf3 wf2
              | F123 | F213 | F124 | F214 ->
                  printc ovm_FUSE_V_SSV wf1 wf2 wf3
              end

          | Vector4 contractions ->
              List.iter (print_vector4 c lhs wf1 wf2 wf3 fusion) contractions

          | Vector4_K_Matrix_tho _
          | Vector4_K_Matrix_jr _
          | Vector4_K_Matrix_cf_t0 _          
          | Vector4_K_Matrix_cf_t1 _
          | Vector4_K_Matrix_cf_t2 _
          | Vector4_K_Matrix_cf_t_rsi _
          | Vector4_K_Matrix_cf_m0 _
          | Vector4_K_Matrix_cf_m1 _
          | Vector4_K_Matrix_cf_m7 _          
          | DScalar2_Vector2_K_Matrix_ms _
          | DScalar2_Vector2_m_0_K_Matrix_cf _
          | DScalar2_Vector2_m_1_K_Matrix_cf _
          | DScalar2_Vector2_m_7_K_Matrix_cf _
          | DScalar4_K_Matrix_ms _ ->
              failwith "print_current: V4: K_Matrix not implemented"
          | Dim8_Scalar2_Vector2_1 _ 
          | Dim8_Scalar2_Vector2_2 _
          | Dim8_Scalar2_Vector2_m_0 _
          | Dim8_Scalar2_Vector2_m_1 _
          | Dim8_Scalar2_Vector2_m_7 _
          | Dim8_Scalar4 _ ->
              failwith "print_current: V4: not implemented"
          | Dim8_Vector4_t_0 _ ->
              failwith "print_current: V4: not implemented"
          | Dim8_Vector4_t_1 _ ->
              failwith "print_current: V4: not implemented"              
          | Dim8_Vector4_t_2 _ ->
              failwith "print_current: V4: not implemented"
          | Dim8_Vector4_m_0 _ ->
              failwith "print_current: V4: not implemented"
          | Dim8_Vector4_m_1 _ ->
              failwith "print_current: V4: not implemented"
          | Dim8_Vector4_m_7 _ ->
              failwith "print_current: V4: not implemented"    
          | GBBG _ ->
              failwith "print_current: V4: GBBG not implemented"
          | DScalar4 _
          | DScalar2_Vector2 _ ->
              failwith "print_current: V4: DScalars not implemented"
          | Dim6_H4_P2 _ ->  
              failwith "print_current: V4: not implemented"
          | Dim6_AHWW_DPB _ ->
              failwith "print_current: V4: not implemented"
          | Dim6_AHWW_DPW _ ->
              failwith "print_current: V4: not implemented"
          | Dim6_AHWW_DW _ ->
              failwith "print_current: V4: not implemented"
          | Dim6_Vector4_DW _ ->
              failwith "print_current: V4: not implemented"
          | Dim6_Vector4_W _ ->
              failwith "print_current: V4: not implemented"
          | Dim6_Scalar2_Vector2_D _ ->
              failwith "print_current: V4: not implemented"
          | Dim6_Scalar2_Vector2_DP _ ->
              failwith "print_current: V4: not implemented"
          | Dim6_HWWZ_DW _ ->
              failwith "print_current: V4: not implemented"
          | Dim6_HWWZ_DPB _ ->
              failwith "print_current: V4: not implemented"
          | Dim6_HWWZ_DDPW _ ->
              failwith "print_current: V4: not implemented"
          | Dim6_HWWZ_DPW _ ->
              failwith "print_current: V4: not implemented"
          | Dim6_AHHZ_D _ ->
              failwith "print_current: V4: not implemented"
          | Dim6_AHHZ_DP _ ->
              failwith "print_current: V4: not implemented"
          | Dim6_AHHZ_PB _ ->
              failwith "print_current: V4: not implemented"
          | Dim6_Scalar2_Vector2_PB _ ->           
              failwith "print_current: V4: not implemented"
          | Dim6_HHZZ_T _ ->   
              failwith "print_current: V4: not implemented"

          end

      | Vn (_, _, _) -> invalid_arg "Targets.print_current: n-ary fusion."

(* \thocwmodulesubsection{Fusions} *)

    let print_fusion lookups lhs_momID fusion amplitude =
      if F.on_shell amplitude (F.lhs fusion) then
        failwith "print_fusion: on_shell projectors not implemented!";
      if F.is_gauss amplitude (F.lhs fusion) then
        failwith "print_fusion: gauss amplitudes not implemented!";
      let lhs_wf = mult_wf lookups.dict amplitude (F.lhs fusion) in
      let lhs_wfID = wf_index lookups.wfmap lookups.n_wfs lhs_wf in
      let f = F.flavor (F.lhs fusion) in
      let pdg = SCM.pdg f in
      let w =
        begin match SCM.width f with
        | Vanishing | Fudged -> 0
        | Constant -> 1
        | Timelike -> 2
        | Complex_Mass -> 3
        | Running -> 4
        | Custom _ -> failwith "Targets.VM: custom width not available"
        end
      in
      let propagate code = printi code ~lhs:lhs_wfID ~rhs1:lhs_momID
        ~coupl:(abs(pdg)) ~coeff:w ~rhs4:(get_ID' amp_compare lookups.amap amplitude)
      in
      begin match SCM.propagator f with
      | Prop_Scalar ->
          propagate ovm_PROPAGATE_SCALAR
      | Prop_Col_Scalar ->
          propagate ovm_PROPAGATE_COL_SCALAR
      | Prop_Ghost ->
          propagate ovm_PROPAGATE_GHOST
      | Prop_Spinor ->
          propagate ovm_PROPAGATE_SPINOR
      | Prop_ConjSpinor ->
          propagate ovm_PROPAGATE_CONJSPINOR
      | Prop_Majorana ->
          propagate ovm_PROPAGATE_MAJORANA
      | Prop_Col_Majorana ->
          propagate ovm_PROPAGATE_COL_MAJORANA
      | Prop_Unitarity ->
          propagate ovm_PROPAGATE_UNITARITY
      | Prop_Col_Unitarity ->
          propagate ovm_PROPAGATE_COL_UNITARITY
      | Prop_Feynman ->
          propagate ovm_PROPAGATE_FEYNMAN
      | Prop_Col_Feynman ->
          propagate ovm_PROPAGATE_COL_FEYNMAN
      | Prop_Vectorspinor ->
          propagate ovm_PROPAGATE_VECTORSPINOR
      | Prop_Tensor_2 ->
          propagate ovm_PROPAGATE_TENSOR2
      | Aux_Col_Scalar | Aux_Col_Vector | Aux_Col_Tensor_1 ->
          failwith "print_fusion: Aux_Col_* not implemented!"
      | Aux_Vector | Aux_Tensor_1 | Aux_Scalar | Aux_Spinor | Aux_ConjSpinor
      | Aux_Majorana | Only_Insertion ->
          propagate ovm_PROPAGATE_NONE
      | Prop_Gauge _ ->
          failwith "print_fusion: Prop_Gauge not implemented!"
      | Prop_Tensor_pure ->
          failwith "print_fusion: Prop_Tensor_pure not implemented!"
      | Prop_Vector_pure ->
          failwith "print_fusion: Prop_Vector_pure not implemented!"
      | Prop_Rxi _ ->
          failwith "print_fusion: Prop_Rxi not implemented!"
      | Prop_UFO _ ->
          failwith "print_fusion: Prop_UFO not implemented!"
      end;

(* Since the OVM knows that we want to propagate a wf, we can send the
   necessary fusions now. *)

      List.iter (print_current lookups lhs_wfID amplitude) (F.rhs fusion)

    let print_all_fusions lookups =
      let fusions = CF.fusions lookups.amplitudes in
      let fset = List.fold_left (fun s x -> FSet.add x s) FSet.empty fusions in
      ignore (List.fold_left (fun level (f, amplitude) ->
        let wf = F.lhs f in
        let lhs_momID = mom_ID lookups.pmap wf in
        let level' = List.length (F.momentum_list wf) in
        if (level' > level && level' > 2) then break ();
        print_fusion lookups lhs_momID f amplitude;
        level')
      1 (FSet.elements fset) )

(* \thocwmodulesubsection{Brakets} *)

    let print_braket lookups amplitude braket =
      let bra = F.bra braket
      and ket = F.ket braket in
      let braID = wf_index lookups.wfmap lookups.n_wfs
        (mult_wf lookups.dict amplitude bra) in
      List.iter (print_current lookups braID amplitude) ket

(* \begin{equation}
   \ii T = \ii^{\#\text{vertices}}\ii^{\#\text{propagators}} \cdots
         = \ii^{n-2}\ii^{n-3} \cdots
         = -\ii(-1)^n \cdots
   \end{equation} *)

(* All brakets for one cflow amplitude should be calculated by one
   thread to avoid multiple access on the same memory (amplitude).*)

    let print_brakets lookups (amplitude, i) =
      let n = List.length (F.externals amplitude) in
      let sign = if n mod 2 = 0 then -1 else 1
      and sym = F.symmetry amplitude in
      printi ovm_CALC_BRAKET ~lhs:i ~rhs1:sym ~coupl:sign;
      match F.brakets amplitude with
      |[([], brakets)] -> List.iter (print_braket lookups amplitude) brakets
      | _ -> failwith "Targets.VM().print_brakets: coupling order slices not supported yet"

(* Fortran arrays/OCaml lists start on 1/0. The amplitude list is sorted by
   [amp_compare] according to their color flows. In this way the amp array
   is sorted in the same way as [table_color_factors]. *)

    let print_all_brakets lookups =
      let g i elt = print_brakets lookups (elt, i+1) in
      lookups.amplitudes |> CF.processes |> List.sort amp_compare
                         |> ThoList.iteri g 0

(* \thocwmodulesubsection{Couplings} *)

(* For now we only care to catch the arrays [gncneu], [gnclep], [gncup] and
   [gncdown] of the SM. This will need an overhaul when it is clear how we store
   the type information of coupling constants. *)

    let strip_array_tag = function
      | Real_Array x -> x
      | Complex_Array x -> x

    let array_constants_list =
      let params = M.parameters()
      and strip_to_constant (lhs, _) = strip_array_tag lhs in
        List.map strip_to_constant params.derived_arrays

    let is_array x = List.mem x array_constants_list

    let constants_map =
      let first = fun (x, _, _) -> x in
      let second = fun (_, y, _) -> y in
      let third = fun (_, _, z) -> z in
      let v3 = List.map third (first (M.vertices () ))
      and v4 = List.map third (second (M.vertices () )) in
      let set = List.fold_left (fun s x -> CSet.add x s) CSet.empty (v3 @ v4) in
      let (arrays, singles) = CSet.partition is_array set in
        (singles |> CSet.elements |> map_of_list,
         arrays  |> CSet.elements |> map_of_list)

(* \thocwmodulesubsection{Output calls} *)

    let amplitudes_to_channel (cmdline : string) (oc : out_channel)
      (diagnostics : (diagnostic * bool) list ) (amplitudes : CF.amplitudes) =

      set_formatter_out_channel oc;
      if (num_particles amplitudes = 0) then begin
        print_description cmdline;
        print_zero_header (); nl ()
      end else begin
        let (wfset, amap) = wfset_amps amplitudes in
        let pset = expand_pset (momenta_set wfset)
        and n_wfs = num_wfs wfset in
        let wfmap = wf_map_of_list (WFSet.elements wfset)
        and pmap = map_of_list (ISet.elements pset)
        and cmap = constants_map in

        let lookups = {pmap = pmap; wfmap = wfmap; cmap = cmap; amap = amap;
          n_wfs = n_wfs; amplitudes = amplitudes;
          dict = CF.dictionary amplitudes} in

        print_description cmdline;
        print_header lookups wfset;
        print_spin_table amplitudes;
        print_flavor_tables amplitudes;
        print_color_tables amplitudes;
        printf "@\n%s" ("OVM instructions for momenta addition," ^
                        " fusions and brakets start here: ");
        break ();
        add_all_mom lookups pset;
        print_ext_amps lookups;
        break ();
        print_all_fusions lookups;
        break ();
        print_all_brakets lookups;
        break (); nl ();
        print_flush ()
      end

    let parameters_to_fortran oc _ =
     (*i The -params options is used as wrapper between OVM and Whizard. Most
       * trouble for the OVM comes from the array dimensionalities of couplings
       * but O'Mega should also know whether a constant is real or complex.
       * Hopefully all will be clearer with the fully general Lorentz structures
       * and UFO support. For now, we stick with this brute-force solution. i*)
      set_formatter_out_channel oc;
      let arrays_to_set = not (IMap.is_empty (snd constants_map)) in
      let set_coupl ty dim cmap = IMap.iter (fun key elt ->
        printf "    %s(%s%d) = %s" ty dim key (M.constant_symbol elt);
        nl () ) cmap in
      let declarations () =
        printf "  complex(%s), dimension(%d) :: ovm_coupl_cmplx"
          !kind (constants_map |> fst |> largest_key); nl ();
        if arrays_to_set then
          printf "  complex(%s), dimension(2, %d) :: ovm_coupl_cmplx2"
            !kind (constants_map |> snd |> largest_key); nl () in
      let print_line str = printf "%s" str; nl() in
      let print_md5sum = function
          | Some s ->
            print_line "  function md5sum ()";
            print_line "    character(len=32) :: md5sum";
            print_line ("    bytecode_file = '" ^ !bytecode_file ^ "'");
            print_line "    call initialize_vm (vm, bytecode_file)";
            print_line "    ! DON'T EVEN THINK of modifying the following line!";
            print_line ("    md5sum = '" ^ s ^ "'");
            print_line "  end function md5sum";
          | None -> ()
      in
      let print_inquiry_function_openmp () = begin
        print_line "  pure function openmp_supported () result (status)";
        print_line "    logical :: status";
        print_line ("    status = " ^ (if !openmp then ".true." else ".false."));
        print_line "  end function openmp_supported";
        nl ()
      end in
      let print_interface whizard =
      if whizard then begin
        print_line "  subroutine init (par, scheme)";
        print_line "    real(kind=default), dimension(*), intent(in) :: par";
        print_line "    integer, intent(in) :: scheme";
        print_line ("     bytecode_file = '" ^ !bytecode_file ^ "'");
        print_line "    call import_from_whizard (par, scheme)";
        print_line "    call initialize_vm (vm, bytecode_file)";
        print_line "  end subroutine init";
        nl ();
        print_line "  subroutine final ()";
        print_line "    call vm%final ()";
        print_line "  end subroutine final";
        nl ();
        print_line "  subroutine update_alpha_s (alpha_s)";
        print_line ("    real(kind=" ^ !kind ^ "), intent(in) :: alpha_s");
        print_line "    call model_update_alpha_s (alpha_s)";
        print_line "  end subroutine update_alpha_s";
        nl ()
      end
      else begin
        print_line "  subroutine init ()";
        print_line ("     bytecode_file = '" ^ !bytecode_file ^ "'");
        print_line "     call init_parameters ()";
        print_line "     call initialize_vm (vm, bytecode_file)";
        print_line "  end subroutine"
      end in
      let print_lookup_functions () = begin
        print_line "  pure function number_particles_in () result (n)";
        print_line "    integer :: n";
        print_line "    n = vm%number_particles_in ()";
        print_line "  end function number_particles_in";
        nl();
        print_line "  pure function number_particles_out () result (n)";
        print_line "    integer :: n";
        print_line "    n = vm%number_particles_out ()";
        print_line "  end function number_particles_out";
        nl();
        print_line "  pure function number_spin_states () result (n)";
        print_line "    integer :: n";
        print_line "    n = vm%number_spin_states ()";
        print_line "  end function number_spin_states";
        nl();
        print_line "  pure subroutine spin_states (a)";
        print_line "    integer, dimension(:,:), intent(out) :: a";
        print_line "    call vm%spin_states (a)";
        print_line "  end subroutine spin_states";
        nl();
        print_line "  pure function number_flavor_states () result (n)";
        print_line "    integer :: n";
        print_line "    n = vm%number_flavor_states ()";
        print_line "  end function number_flavor_states";
        nl();
        print_line "  pure subroutine flavor_states (a)";
        print_line "    integer, dimension(:,:), intent(out) :: a";
        print_line "    call vm%flavor_states (a)";
        print_line "  end subroutine flavor_states";
        nl();
        print_line "  pure function number_color_indices () result (n)";
        print_line "    integer :: n";
        print_line "    n = vm%number_color_indices ()";
        print_line "  end function number_color_indices";
        nl();
        print_line "  pure function number_color_flows () result (n)";
        print_line "    integer :: n";
        print_line "    n = vm%number_color_flows ()";
        print_line "  end function number_color_flows";
        nl();
        print_line "  pure subroutine color_flows (a, g)";
        print_line "    integer, dimension(:,:,:), intent(out) :: a";
        print_line "    logical, dimension(:,:), intent(out) :: g";
        print_line "    call vm%color_flows (a, g)";
        print_line "  end subroutine color_flows";
        nl();
        print_line "  pure function number_color_factors () result (n)";
        print_line "    integer :: n";
        print_line "    n = vm%number_color_factors ()";
        print_line "  end function number_color_factors";
        nl();
        print_line "  pure subroutine color_factors (cf)";
        print_line "    use omega_color";
        print_line "    type(omega_color_factor), dimension(:), intent(out) :: cf";
        print_line "    call vm%color_factors (cf)";
        print_line "  end subroutine color_factors";
        nl();
        print_line "  !pure unless OpenMP";
        print_line "  !pure function color_sum (flv, hel) result (amp2)";
        print_line "  function color_sum (flv, hel) result (amp2)";
        print_line "    use kinds";
        print_line "    integer, intent(in) :: flv, hel";
        print_line "    real(kind=default) :: amp2";
        print_line "    amp2 = vm%color_sum (flv, hel)";
        print_line "  end function color_sum";
        nl();
        print_line "  subroutine new_event (p)";
        print_line "    use kinds";
        print_line "    real(kind=default), dimension(0:3,*), intent(in) :: p";
        print_line "    call vm%new_event (p)";
        print_line "  end subroutine new_event";
        nl();
        print_line "  subroutine reset_helicity_selection (threshold, cutoff)";
        print_line "    use kinds";
        print_line "    real(kind=default), intent(in) :: threshold";
        print_line "    integer, intent(in) :: cutoff";
        print_line "    call vm%reset_helicity_selection (threshold, cutoff)";
        print_line "  end subroutine reset_helicity_selection";
        nl();
        print_line "  pure function is_allowed (flv, hel, col) result (yorn)";
        print_line "    logical :: yorn";
        print_line "    integer, intent(in) :: flv, hel, col";
        print_line "    yorn = vm%is_allowed (flv, hel, col)";
        print_line "  end function is_allowed";
        nl();
        print_line "  pure function get_amplitude (flv, hel, col) result (amp_result)";
        print_line "    use kinds";
        print_line "    complex(kind=default) :: amp_result";
        print_line "    integer, intent(in) :: flv, hel, col";
        print_line "    amp_result = vm%get_amplitude(flv, hel, col)";
        print_line "  end function get_amplitude";
        nl();
      end in
      print_line ("module " ^ !wrapper_module);
      print_line ("  use " ^ !parameter_module_external);
      print_line "  use iso_varying_string, string_t => varying_string";
      print_line "  use kinds";
      print_line "  use omegavm95";
      print_line "  implicit none";
      print_line "  private";
      print_line "  type(vm_t) :: vm";
      print_line "  type(string_t) :: bytecode_file";
      print_line ("  public :: number_particles_in, number_particles_out," ^
          " number_spin_states, &");
      print_line ("    spin_states, number_flavor_states, flavor_states," ^
          " number_color_indices, &");
      print_line ("    number_color_flows, color_flows," ^
          " number_color_factors, color_factors, &");
      print_line ("    color_sum, new_event, reset_helicity_selection," ^
          " is_allowed, get_amplitude, &");
      print_line ("    init, " ^ 
          (match !md5sum with Some _ -> "md5sum, "
                            | None -> "") ^ "openmp_supported");
      if !whizard then
        print_line ("  public :: final, update_alpha_s")
      else
        print_line ("  public :: initialize_vm");
      declarations ();
      print_line "contains";

      print_line "  subroutine setup_couplings ()";
      set_coupl "ovm_coupl_cmplx" "" (fst constants_map);
      if arrays_to_set then
        set_coupl "ovm_coupl_cmplx2" ":," (snd constants_map);
      print_line "  end subroutine setup_couplings";
      print_line "  subroutine initialize_vm (vm, bytecode_file)";
      print_line "    class(vm_t), intent(out) :: vm";
      print_line "    type(string_t), intent(in) :: bytecode_file";
      print_line "    type(string_t) :: version";
      print_line "    type(string_t) :: model";
      print_line ("    version = 'OVM " ^ version ^ "'");
      print_line ("    model = 'Model " ^ model_name ^ "'");
      print_line "    call setup_couplings ()";
      print_line "    call vm%init (bytecode_file, version, model, verbose=.False., &";
      print_line "      coupl_cmplx=ovm_coupl_cmplx, &";
      if arrays_to_set then
        print_line "      coupl_cmplx2=ovm_coupl_cmplx2, &";
      print_line ("      mass=mass, width=width, openmp=" ^ (if !openmp then
        ".true." else ".false.") ^ ")");
      print_line "  end subroutine initialize_vm";
      nl();
      print_md5sum !md5sum;
      print_inquiry_function_openmp ();
      print_interface !whizard;
      print_lookup_functions ();

      print_line ("end module " ^ !wrapper_module)

    let parameters_to_channel oc =
      parameters_to_fortran oc (SCM.parameters ())

  end
