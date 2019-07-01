(* $Id: targets.ml 3832 2012-05-04 02:12:59Z jr_reuter $

   Copyright (C) 1999-2012 by

       Wolfgang Kilian <kilian@physik.uni-siegen.de>
       Thorsten Ohl <ohl@physik.uni-wuerzburg.de>
       Juergen Reuter <juergen.reuter@physik.uni-freiburg.de>
       Christian Speckner <christian.speckner@physik.uni-freiburg.de>
       Fabian Bach <fabian.bach@cern.ch> (only parts of this file)

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

let rcs_file = RCS.parse "Targets" ["Code Generation"]
    { RCS.revision = "$Revision: 3832 $";
      RCS.date = "$Date: 2012-05-04 04:12:59 +0200 (Fri, 04 May 2012) $";
      RCS.author = "$Author: jr_reuter $";
      RCS.source
        = "$URL: svn+ssh://jr_reuter@login.hepforge.org/hepforge/svn/whizard/trunk/src/omega/src/targets.ml $" }

module Dummy (F : Fusion.Maker) (P : Momentum.T) (M : Model.T) =
  struct
    let rcs_list = []
    type amplitudes = Fusion.Multi(F)(P)(M).amplitudes
    type diagnostic = All | Arguments | Momenta | Gauge
    let options = Options.empty
    let amplitudes_to_channel cmdline oc amplitudes = failwith "Targets.Dummy"
    let parameters_to_channel oc = failwith "Targets.Dummy"
  end

(* \thocwmodulesection{\texttt{Fortran\,90/95}} *)

(* \thocwmodulesubsection{Dirac Fermions}
   We factor out the code for fermions so that we can use the simpler
   implementation for Dirac fermions if the model contains no Majorana
   fermions. *) 

module type Fermions =
  sig
    open Coupling
    val psi_type : string
    val psibar_type : string
    val chi_type : string
    val grav_type : string
    val psi_incoming : string
    val brs_psi_incoming : string
    val psibar_incoming : string
    val brs_psibar_incoming : string
    val chi_incoming : string
    val brs_chi_incoming : string
    val grav_incoming : string
    val psi_outgoing : string
    val brs_psi_outgoing : string
    val psibar_outgoing : string
    val brs_psibar_outgoing : string 
    val chi_outgoing : string
    val brs_chi_outgoing : string 
    val grav_outgoing : string
    val psi_propagator : string
    val psibar_propagator : string
    val chi_propagator : string
    val grav_propagator : string
    val psi_projector : string
    val psibar_projector : string
    val chi_projector : string
    val grav_projector : string
    val psi_gauss : string
    val psibar_gauss : string
    val chi_gauss : string
    val grav_gauss : string
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
    val reverse_braket : lorentz -> bool
    val use_module : string
    val require_library : string list
    val rcs : RCS.t
   end

module Fortran_Fermions : Fermions =
  struct
    let rcs = RCS.rename rcs_file "Targets.Fortran_Fermions()"
        [ "generates Fortran95 code for Dirac fermions";
          "using revision 2000_10_A of module omega95" ]

    open Coupling
    open Format

    let psi_type = "spinor"
    let psibar_type = "conjspinor"
    let chi_type = "???"
    let grav_type = "???"

    let psi_incoming = "u"            
    let brs_psi_incoming = "brs_u"
    let psibar_incoming = "vbar"
    let brs_psibar_incoming = "brs_vbar"
    let chi_incoming = "???"
    let brs_chi_incoming = "???"
    let grav_incoming = "???"
    let psi_outgoing = "v"
    let brs_psi_outgoing = "brs_v"
    let psibar_outgoing = "ubar"
    let brs_psibar_outgoing = "brs_ubar"
    let chi_outgoing = "???"
    let brs_chi_outgoing = "???"
    let grav_outgoing = "???"

    let psi_propagator = "pr_psi"
    let psibar_propagator = "pr_psibar"
    let chi_propagator = "???"
    let grav_propagator = "???"

    let psi_projector = "pj_psi"
    let psibar_projector = "pj_psibar"
    let chi_projector = "???"
    let grav_projector = "???"

    let psi_gauss = "pg_psi"
    let psibar_gauss = "pg_psibar"
    let chi_gauss = "???"
    let grav_gauss = "???"

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

    let fastener s i ?p () = 
      try 
        let offset = (String.index s '(') in
        if ((String.get s (String.length s - 1)) != ')') then
          failwith "fastener: wrong usage of parentheses"
        else
          let func_name = (String.sub s 0 offset) and
	      tail =
	    (String.sub s (succ offset) (String.length s - offset - 2)) in 
          if (String.contains func_name ')') or 
	    (String.contains tail '(') or 
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
            | None -> s ^ "(" ^ string_of_int i ^ ")"
            | Some p -> s ^ "(" ^ p ^ "*" ^ p ^ "," ^ string_of_int i ^ ")"

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

    let print_fermion_current_mom1 coeff f c wf1 wf2 p1 p2 p12 fusion =
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

    let print_fermion_current_mom2 coeff f c wf1 wf2 p1 p2 p12 fusion =
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
  
    let print_current = function
      | coeff, Psibar, VA, Psi -> print_fermion_current2 coeff "va"
      | coeff, Psibar, VA2, Psi -> print_fermion_current coeff "va2"
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
      | coeff, Psibar, _, Psi -> invalid_arg
            "Targets.Fortran_Fermions: no superpotential here"
      | _, Chibar, _, _ | _, _, _, Chi -> invalid_arg
            "Targets.Fortran_Fermions: Majorana spinors not handled"
      | _, Gravbar, _, _ | _, _, _, Grav -> invalid_arg
            "Targets.Fortran_Fermions: Gravitinos not handled"

    let print_current_mom = function
      | coeff, Psibar, VLRM, Psi -> print_fermion_current_mom1 coeff "vlr"
      | coeff, Psibar, SPM, Psi -> print_fermion_current_mom1 coeff "sp"
      | coeff, Psibar, TVA, Psi -> print_fermion_current_mom1 coeff "tva"
      | coeff, Psibar, TVAM, Psi -> print_fermion_current_mom2 coeff "tvam"
      | coeff, Psibar, TLR, Psi -> print_fermion_current_mom1 coeff "tlr"
      | coeff, Psibar, TLRM, Psi -> print_fermion_current_mom2 coeff "tlrm"
      | coeff, Psibar, TRL, Psi -> print_fermion_current_mom1 coeff "trl"
      | coeff, Psibar, TRLM, Psi -> print_fermion_current_mom2 coeff "trlm"
      | coeff, Psibar, _, Psi -> invalid_arg
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

    let reverse_braket= function
      | Spinor -> true
      | _ -> false

    let use_module = "omega95"
    let require_library =
      ["omega_spinors_2010_01_A"; "omega_spinor_cpls_2010_01_A"]
  end
      
(* \thocwmodulesubsection{Main Functor} *)

module Make_Fortran (Fermions : Fermions)
    (Fusion_Maker : Fusion.Maker) (P : Momentum.T) (M : Model.T) =
  struct
    let rcs_list =
      [ RCS.rename rcs_file "Targets.Make_Fortran()"
          [ "Interface for Whizard 2.X";
            "NB: non-gauge vector couplings are not available yet" ];
        Fermions.rcs ] 

    let require_library =
      Fermions.require_library @
      [ "omega_vectors_2010_01_A"; "omega_polarizations_2010_01_A";
        "omega_couplings_2010_01_A"; "omega_color_2010_01_A";
        "omega_utils_2010_01_A" ]
        
    module CM = Colorize.It(M)
    module F = Fusion_Maker(P)(M)
    type amplitude = F.amplitude

    module CF = Fusion.Multi(Fusion_Maker)(P)(M)
    type amplitudes = CF.amplitudes

    open Coupling
    open Format

    type output_mode =
      | Single_Function
      | Single_Module of int
      | Single_File of int
      | Multi_File of int

    let line_length = ref 80
    let continuation_lines = ref (-1) (* 255 *)
    let kind = ref "default"
    let fortran95 = ref true
    let module_name = ref "omega_amplitude"
    let output_mode = ref (Single_Module 10)
    let use_modules = ref []
    let whizard = ref false
    let parameter_module = ref ""
    let md5sum = ref None
    let no_write = ref false        
    let km_write = ref false
    let km_pure = ref false
    let openmp = ref false

    let options = Options.create
      [ "90", Arg.Clear fortran95,
        "don't use Fortran95 features that are not in Fortran90";
        "kind", Arg.String (fun s -> kind := s),
        "real and complex kind (default: " ^ !kind ^ ")";
        "width", Arg.Int (fun w -> line_length := w), "maximum line length";
        "continuation", Arg.Int (fun l -> continuation_lines := l),
        "maximum # of continuation lines";
        "module", Arg.String (fun s -> module_name := s), "module name";
        "single_function", Arg.Unit (fun () -> output_mode := Single_Function),
        "compute the matrix element(s) in a monolithis function";
        "split_function", Arg.Int (fun n -> output_mode := Single_Module n),
        "split the matrix element(s) into small functions [default, size = 10]";
        "split_module", Arg.Int (fun n -> output_mode := Single_File n),
        "split the matrix element(s) into small modules";
        "split_file", Arg.Int (fun n -> output_mode := Multi_File n),
        "split the matrix element(s) into small files";
        "use", Arg.String (fun s -> use_modules := s :: !use_modules),
        "use module";
        "parameter_module", Arg.String (fun s -> parameter_module := s),
        "parameter_module";
        "md5sum", Arg.String (fun s -> md5sum := Some s),
        "transfer MD5 checksum";
        "whizard", Arg.Set whizard, "include WHIZARD interface";
        "no_write", Arg.Set no_write, "no 'write' statements";
        "kmatrix_write", Arg.Set km_write, "write K matrix functions";
        "kmatrix_write_pure", Arg.Set km_pure, "write K matrix pure functions";
        "openmp", Arg.Set openmp, "activate OpenMP support in generated code"]

(* Fortran style line continuation: *)

(* Default function to output spaces (copied from \texttt{format.ml}). *)
    let blank_line = String.make 80 ' '
    let rec display_blanks oc n =
      if n > 0 then
        if n <= 80 then
          output oc blank_line 0 n
        else begin
          output oc blank_line 0 80;
          display_blanks oc (n - 80)
        end

(* Default function to output new lines (copied from \texttt{format.ml}). *)
    let display_newline oc () =
      output oc "\n" 0  1

(* [current_continuation_line]
   \begin{itemize}
     \item $\le0$: not continuing: print a straight newline,
     \item $>0$: continuing: append [" &"] until we run up to [!continuation_lines].
       NB: [!continuation_lines < 0] means \emph{unlimited} continuation lines.
   \end{itemize} *)

    let current_continuation_line = ref 1
    exception Continuation_Lines of int

    let fortran_newline oc () =
      if !current_continuation_line > 0 then begin
        if !continuation_lines >= 0 && !current_continuation_line > !continuation_lines then
          raise (Continuation_Lines !current_continuation_line)
        else begin
          output oc " &" 0 2;
          incr current_continuation_line
        end
      end;
      display_newline oc ()
      
    let nl () =
      current_continuation_line := 0;
      print_newline ();
      current_continuation_line := 1

(* Make a formatter with default functions to output spaces and new lines. *)
    let setup_fortran_formatter width oc =
      set_all_formatter_output_functions
        ~out:(output oc)
        ~flush:(fun () -> flush oc)
        ~newline:(fortran_newline oc)
        ~spaces:(display_blanks oc);
      set_margin (width - 2)

    let print_list = function 
      | [] -> ()
      | a :: rest ->
          print_string a;
          List.iter (fun s -> printf ",@ %s" s) rest

(* \thocwmodulesubsection{Variables and Declarations} *)

    (* ["NC"] is already used up in the module ["constants"]: *)
    let nc_parameter = "N_"
    let omega_color_factor_abbrev = "OCF"
    let openmp_tld_type = "thread_local_data"
    let openmp_tld = "tld"

    let flavors_symbol ?(decl = false) flavors =
      (if !openmp & not decl then openmp_tld ^ "%" else "" ) ^
      "oks_" ^ String.concat "" (List.map CM.flavor_symbol flavors)

    let p2s p =
      if p >= 0 && p <= 9 then
        string_of_int p
      else if p <= 36 then
        String.make 1 (Char.chr (Char.code 'A' + p - 10))
      else
        "_"

    let format_momentum p =
      "p" ^ String.concat "" (List.map p2s p)

    let format_p wf =
      String.concat "" (List.map p2s (F.momentum_list wf))

    let ext_momentum wf =
      match F.momentum_list wf with
      | [n] -> n
      | _ -> invalid_arg "Targets.Fortran.ext_momentum"

    module PSet = Set.Make (struct type t = int list let compare = compare end)
    module WFSet = Set.Make (struct type t = F.wf let compare = compare end)

    let add_tag wf name =
      match F.wf_tag wf with
      | None -> name
      | Some tag -> name ^ "_" ^ tag

    let variable ?(decl = false) wf =
      (if !openmp & not decl then openmp_tld ^ "%" else "")
      ^ add_tag wf ("owf_" ^ CM.flavor_symbol (F.flavor wf) ^ "_" ^ format_p wf)

    let momentum wf = "p" ^ format_p wf
    let spin wf = "s(" ^ string_of_int (ext_momentum wf) ^ ")"

    let format_multiple_variable ?(decl = false) wf i =
      variable ~decl:decl wf ^ "_X" ^ string_of_int i

    let multiple_variable ?(decl = false) amplitude dictionary wf =
      try
        format_multiple_variable ~decl:decl wf (dictionary amplitude wf)
      with
      | Not_found -> variable wf

    let multiple_variables ?(decl = false) multiplicity wf =
      try
        List.map
          (format_multiple_variable ~decl:decl wf)
          (ThoList.range 1 (multiplicity wf))
      with
      | Not_found -> [variable ~decl:decl wf]

    let declaration_chunk_size = 64

    let declare_list_chunk multiplicity t = function
      | [] -> ()
      | wfs ->
          printf "    @[<2>%s :: " t;
          print_list (ThoList.flatmap (multiple_variables ~decl:true multiplicity) wfs); nl ()

    let declare_list multiplicity t = function
      | [] -> ()
      | wfs ->
          List.iter
            (declare_list_chunk multiplicity t)
            (ThoList.chopn declaration_chunk_size wfs)

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
            (match CM.lorentz (F.flavor wf) with
            | Scalar -> {acc with scalars = wf :: acc.scalars}
            | Spinor -> {acc with spinors = wf :: acc.spinors}
            | ConjSpinor -> {acc with conjspinors = wf :: acc.conjspinors}
            | Majorana -> {acc with realspinors = wf :: acc.realspinors}
            | Maj_Ghost -> {acc with ghostspinors = wf :: acc.ghostspinors}
            | Vectorspinor -> 
                {acc with vectorspinors = wf :: acc.vectorspinors}
            | Vector -> {acc with vectors = wf :: acc.vectors}
(*i            | Ward_Vector -> {acc with ward_vectors = wf :: acc.ward_vectors}
i*)  
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
            | BRS _ -> invalid_arg "Targets.wfs_classify': not needed here")
            rest

    let classify_wfs wfs = classify_wfs'
        { scalars = []; spinors = []; conjspinors = []; realspinors = [];
          ghostspinors = []; vectorspinors = []; vectors = []; 
          ward_vectors = [];
          massive_vectors = []; tensors_1 = []; tensors_2 = []; 
          brs_scalars = [] ; brs_spinors = []; brs_conjspinors = []; 
          brs_realspinors = []; brs_vectorspinors = [];
          brs_vectors = []; brs_massive_vectors = []}
        wfs

(* \thocwmodulesubsection{Parameters} *)

    type 'a parameters =
        { real_singles : 'a list;
          real_arrays : ('a * int) list;
          complex_singles : 'a list;
          complex_arrays : ('a * int) list }

    let rec classify_singles acc = function
      | [] -> acc
      | Real p :: rest -> classify_singles
            { acc with real_singles = p :: acc.real_singles } rest
      | Complex p :: rest -> classify_singles
            { acc with complex_singles = p :: acc.complex_singles } rest

    let rec classify_arrays acc = function
      | [] -> acc
      | (Real_Array p, rhs) :: rest -> classify_arrays
            { acc with real_arrays =
              (p, List.length rhs) :: acc.real_arrays } rest
      | (Complex_Array p, rhs) :: rest -> classify_arrays
            { acc with complex_arrays =
              (p, List.length rhs) :: acc.complex_arrays } rest

    let classify_parameters params =
      classify_arrays
        (classify_singles
           { real_singles = [];
             real_arrays = [];
             complex_singles = [];
             complex_arrays = [] }
           (List.map fst params.derived)) params.derived_arrays

(* \begin{dubious}
     Unify this with the other code using [ThoList.chopn].
   \end{dubious} *)

    let rec schisma n l = 
      if List.length l <= n then
        [l]
      else
        let a, b = ThoList.splitn n l in
        [a] @ (schisma n b)

    let rec schisma_num i n l = 
      if List.length l <= n then
        [(i,l)]
      else
        let a, b = ThoList.splitn n l in
        [(i,a)] @ (schisma_num (i+1) n b)

    let declare_parameters' t = function        
      | [] -> ()
      | plist -> 
          printf "  @[<2>%s(kind=%s), public, save :: " t !kind;
          print_list (List.map CM.constant_symbol plist); nl ()

    let declare_parameters t plist = 
      List.iter (declare_parameters' t) plist 

    let declare_parameter_array t (p, n) =
      printf "  @[<2>%s(kind=%s), dimension(%d), public, save :: %s"
        t !kind n (CM.constant_symbol p); nl ()

    let default_parameter (x, v) =
      printf "@ %s = %g_%s" (CM.constant_symbol x) v !kind

    let declare_default_parameters t = function
      | [] -> ()
      | p :: plist ->
          printf "  @[<2>%s(kind=%s), public, save ::" t !kind;
          default_parameter p;
          List.iter (fun p' -> printf ","; default_parameter p') plist;
          nl ()

    let rec format_constant = function
      | I -> sprintf "cmplx (0.0_%s, 1.0_%s)" !kind !kind
      | Const c when c < 0 -> sprintf "(%d.0_%s)" c !kind
      | Const c -> sprintf "%d.0_%s" c !kind
      | _ -> invalid_arg "format_constant"

    let rec eval_parameter' = function
      | I -> printf "cmplx (0.0_%s, 1.0_%s)" !kind !kind
      | Const c when c < 0 -> printf "(%d.0_%s)" c !kind
      | Const c -> printf "%d.0_%s" c !kind
      | Atom x -> printf "%s" (CM.constant_symbol x)
      | Sum [] -> printf "0.0_%s" !kind
      | Sum [x] -> eval_parameter' x
      | Sum (x :: xs) ->
          printf "@,("; eval_parameter' x;
          List.iter (fun x -> printf "@, + "; eval_parameter' x) xs;
          printf ")"
      | Diff (x, y) ->
          printf "@,("; eval_parameter' x;
          printf " - "; eval_parameter' y; printf ")"
      | Neg x -> printf "@,( - "; eval_parameter' x; printf ")"
      | Prod [] -> printf "1.0_%s" !kind
      | Prod [x] -> eval_parameter' x
      | Prod (x :: xs) ->
          printf "@,("; eval_parameter' x;
          List.iter (fun x -> printf " * "; eval_parameter' x) xs;
          printf ")"
      | Quot (x, y) ->
          printf "@,("; eval_parameter' x;
          printf " / "; eval_parameter' y; printf ")"
      | Rec x ->
          printf "@, (1.0_%s / " !kind; eval_parameter' x; printf ")"
      | Pow (x, n) ->
          printf "@,("; eval_parameter' x; printf "**%d" n; printf ")"
      | Sqrt x -> printf "@,sqrt ("; eval_parameter' x; printf ")"
      | Sin x -> printf "@,sin ("; eval_parameter' x; printf ")"
      | Cos x -> printf "@,cos ("; eval_parameter' x; printf ")"
      | Tan x -> printf "@,tan ("; eval_parameter' x; printf ")"
      | Cot x -> printf "@,cot ("; eval_parameter' x; printf ")"
      | Atan2 (y, x) -> printf "@,atan2 ("; eval_parameter' y;
          printf ",@ "; eval_parameter' x; printf ")"
      | Conj x -> printf "@,conjg ("; eval_parameter' x; printf ")"

    let strip_single_tag = function
      | Real x -> x
      | Complex x -> x

    let strip_array_tag = function
      | Real_Array x -> x
      | Complex_Array x -> x

    let eval_parameter (lhs, rhs) =
      let x = CM.constant_symbol (strip_single_tag lhs) in
      printf "    @[<2>%s = " x; eval_parameter' rhs; nl ()

    let eval_para_list n l = 
      printf " subroutine setup_parameters%s ()" (string_of_int n); nl();
      List.iter eval_parameter l;
      printf " end subroutine setup_parameters%s" (string_of_int n); nl()

    let eval_parameter_pair (lhs, rhs) =
      let x = CM.constant_symbol (strip_array_tag lhs) in
      let _ = List.fold_left (fun i rhs' ->
        printf "    @[<2>%s(%d) = " x i; eval_parameter' rhs'; nl ();
        succ i) 1 rhs in
      ()

    let eval_para_pair_list n l =
      printf " subroutine setup_parameters%s ()" (string_of_int n); nl();
      List.iter eval_parameter_pair l;
      printf " end subroutine setup_parameters%s" (string_of_int n); nl()

    let print_echo fmt p =
      let s = CM.constant_symbol p in
      printf "    write (unit = *, fmt = fmt_%s) \"%s\", %s"
        fmt s s; nl ()
        
    let print_echo_array fmt (p, n) =
      let s = CM.constant_symbol p in
      for i = 1 to n do
        printf "    write (unit = *, fmt = fmt_%s_array) " fmt ;
        printf "\"%s\", %d, %s(%d)" s i s i; nl ()
      done

    let parameters_to_fortran oc params =
      setup_fortran_formatter !line_length oc;
      let declarations = classify_parameters params in
      printf "module %s" !parameter_module; nl ();
      printf "  use kinds"; nl ();
      printf "  use constants"; nl ();
      printf "  implicit none"; nl ();
      printf "  private"; nl ();
      printf "  @[<2>public :: setup_parameters";
      if !no_write then begin 
        printf "! No print_parameters"; nl();
      end else begin 
        printf "@,, print_parameters"; nl ();
      end;  
      declare_default_parameters "real" params.input;
      declare_parameters "real" (schisma 69 declarations.real_singles);
      List.iter (declare_parameter_array "real") declarations.real_arrays;
      declare_parameters "complex" (schisma 69 declarations.complex_singles);
      List.iter (declare_parameter_array "complex") declarations.complex_arrays;
      printf "contains"; nl ();
      printf "    ! derived parameters:"; nl ();
      let shredded = schisma_num 1 120 params.derived in 
      let shredded_arrays = schisma_num 1 120 params.derived_arrays in 
         let num_sub = List.length shredded in 
         let num_sub_arrays = List.length shredded_arrays in 
      printf "     !length: %s" (string_of_int (List.length params.derived)); 
         nl(); 
      printf "     !Num_Sub: %s" (string_of_int num_sub); nl();
      List.iter (fun (i,l) -> eval_para_list i l) shredded;
      List.iter (fun (i,l) -> eval_para_pair_list (num_sub + i) l) 
        shredded_arrays; 
      printf "  subroutine setup_parameters ()"; nl();
      let sum_sub = num_sub + num_sub_arrays in
      for i = 1 to sum_sub do
        printf "    call setup_parameters%s" (string_of_int i); nl();
      done;
      printf "  end subroutine setup_parameters"; nl();
      if !no_write then begin
        printf "! No print_parameters"; nl();
      end else begin
        printf "  subroutine print_parameters ()"; nl();
        printf "    @[<2>character(len=*), parameter ::";
        printf "@ fmt_real = \"(A12,4X,' = ',E25.18)\",";
        printf "@ fmt_complex = \"(A12,4X,' = ',E25.18,' + i*',E25.18)\",";
        printf "@ fmt_real_array = \"(A12,'(',I2.2,')',' = ',E25.18)\",";
        printf "@ fmt_complex_array = ";
        printf "\"(A12,'(',I2.2,')',' = ',E25.18,' + i*',E25.18)\""; nl ();
        printf "    @[<2>write (unit = *, fmt = \"(A)\") @,";
        printf "\"default values for the input parameters:\""; nl ();
        List.iter (fun (p, _) -> print_echo "real" p) params.input;
        printf "    @[<2>write (unit = *, fmt = \"(A)\") @,";
        printf "\"derived parameters:\""; nl ();
        List.iter (print_echo "real") declarations.real_singles;
        List.iter (print_echo "complex") declarations.complex_singles;
        List.iter (print_echo_array "real") declarations.real_arrays;
        List.iter (print_echo_array "complex") declarations.complex_arrays;
        printf "  end subroutine print_parameters"; nl();
      end;  
      printf "end module %s" !parameter_module; nl ();
      printf "! O'Mega revision control information:"; nl ();
      List.iter (fun s -> printf "!    %s" s; nl ())
        (ThoList.flatmap RCS.summary (M.rcs :: rcs_list));
      printf "!!! program test_parameters"; nl();
      printf "!!!   use %s" !parameter_module; nl();
      printf "!!!   call setup_parameters ()"; nl();
      printf "!!!   call print_parameters ()"; nl();
      printf "!!! end program test_parameters"; nl()

(* \thocwmodulesubsection{Run-Time Diagnostics} *)

    type diagnostic = All | Arguments | Momenta | Gauge

    type diagnostic_mode = Off | Warn | Panic

    let warn mode =
      match !mode with
      | Off -> false
      | Warn -> true
      | Panic -> true

    let panic mode =
      match !mode with
      | Off -> false
      | Warn -> false
      | Panic -> true

    let suffix mode =
      if panic mode then
        "panic"
      else
        "warn"

    let diagnose_arguments = ref Off
    let diagnose_momenta = ref Off
    let diagnose_gauge = ref Off

    let rec parse_diagnostic = function
      | All, panic ->
          parse_diagnostic (Arguments, panic);
          parse_diagnostic (Momenta, panic);
          parse_diagnostic (Gauge, panic)
      | Arguments, panic ->
          diagnose_arguments := if panic then Panic else Warn
      | Momenta, panic ->
          diagnose_momenta := if panic then Panic else Warn
      | Gauge, panic ->
          diagnose_gauge := if panic then Panic else Warn

(* If diagnostics are required, we have to switch off
   Fortran95 features like pure functions. *)

    let parse_diagnostics = function
      | [] -> ()
      | diagnostics ->
          fortran95 := false;
          List.iter parse_diagnostic diagnostics

(* \thocwmodulesubsection{Amplitude} *)

    let declare_momenta_chunk = function
      | [] -> ()
      | momenta ->
          printf "    @[<2>type(momentum) :: ";
          print_list (List.map format_momentum momenta); nl ()

    let declare_momenta = function
      | [] -> ()
      | momenta ->
          List.iter
            declare_momenta_chunk
            (ThoList.chopn declaration_chunk_size momenta)

    let declare_wavefunctions multiplicity wfs =
      let wfs' = classify_wfs wfs in
      declare_list multiplicity ("complex(kind=" ^ !kind ^ ")") 
        (wfs'.scalars @ wfs'.brs_scalars);
      declare_list multiplicity ("type(" ^ Fermions.psi_type ^ ")")
        (wfs'.spinors @ wfs'.brs_spinors);
      declare_list multiplicity ("type(" ^ Fermions.psibar_type ^ ")") 
        (wfs'.conjspinors @ wfs'.brs_conjspinors);
      declare_list multiplicity ("type(" ^ Fermions.chi_type ^ ")") 
        (wfs'.realspinors @ wfs'.brs_realspinors @ wfs'.ghostspinors);
      declare_list multiplicity ("type(" ^ Fermions.grav_type ^ ")") wfs'.vectorspinors;
      declare_list multiplicity "type(vector)" (wfs'.vectors @ wfs'.massive_vectors @
         wfs'.brs_vectors @ wfs'.brs_massive_vectors @ wfs'.ward_vectors);
      declare_list multiplicity "type(tensor2odd)" wfs'.tensors_1;
      declare_list multiplicity "type(tensor)" wfs'.tensors_2

    let flavors a = F.incoming a @ F.outgoing a

    let declare_brakets_chunk = function
      | [] -> ()
      | amplitudes ->
          printf "    @[<2>complex(kind=%s) :: " !kind;
          print_list (List.map (fun a -> flavors_symbol ~decl:true (flavors a)) amplitudes); nl ()

    let declare_brakets = function
      | [] -> ()
      | amplitudes ->
          List.iter
            declare_brakets_chunk
            (ThoList.chopn declaration_chunk_size amplitudes)

    let print_variable_declarations amplitudes =
      let multiplicity = CF.multiplicity amplitudes
      and processes = CF.processes amplitudes in
      declare_momenta
        (PSet.elements
           (List.fold_left
              (fun set a ->
                PSet.union set (List.fold_right
                                  (fun wf -> PSet.add (F.momentum_list wf))
                                  (F.externals a) PSet.empty))
              PSet.empty processes));
      declare_momenta
        (PSet.elements
           (List.fold_left
              (fun set a ->
                PSet.union set (List.fold_right
                                  (fun wf -> PSet.add (F.momentum_list wf))
                                  (F.variables a) PSet.empty))
              PSet.empty processes));
      if !openmp then begin
         printf "  type %s@[<2>" openmp_tld_type;
         nl ();
      end ;
      declare_wavefunctions multiplicity
        (WFSet.elements
           (List.fold_left
              (fun set a ->
                WFSet.union set (List.fold_right WFSet.add (F.externals a) WFSet.empty))
              WFSet.empty processes));
      declare_wavefunctions multiplicity
        (WFSet.elements
           (List.fold_left
              (fun set a ->
                WFSet.union set (List.fold_right WFSet.add (F.variables a) WFSet.empty))
              WFSet.empty processes));
      declare_brakets processes;
      if !openmp then begin
         printf "@]  end type %s\n" openmp_tld_type;
         printf "  type(%s) :: %s" openmp_tld_type openmp_tld;
         nl ();
      end

(* [print_current] is the most important function that has to match the functions
   in \verb+omega95+ (see appendix~\ref{sec:fortran}).  It offers plentiful
   opportunities for making mistakes, in particular those related to signs.
   We start with a few auxiliary functions:  *)

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

    let print_dscalar2_vector2 c wf1 wf2 wf3 p1 p2 p3 p123
        fusion (coeff, contraction) =
      failwith "Targets.Fortran.print_dscalar2_vector2: incomplete!";
      match contraction, fusion with
      | C_12_34, (F134|F143|F234|F243) ->
          printf "((%s%s)*(%s*%s)*(%s*%s)*%s)"
            (format_coeff coeff) c p123 p1 wf2 wf3 wf1
      | C_12_34, (F312|F321|F412|F421) ->
          printf "((%s%s)*((%s*%s)*%s*%s)*%s)"
            (format_coeff coeff) c p2 p3 wf2 wf3 wf1
      | C_12_34, (F341|F431|F342|F432|F123|F213|F124|F214)
      | C_13_42, (F241|F421|F243|F423|F132|F312|F134|F314)
      | C_14_23, (F231|F321|F234|F324|F142|F412|F143|F413) ->
          printf "((%s%s)*(%s*%s)*(%s*%s)*%s*%s*%s)"
            (format_coeff coeff) c p1 p2 p3 p123 wf1 wf2 wf3
      | C_13_42, (F124|F142|F324|F342|F213|F231|F413|F431)
      | C_14_23, (F123|F132|F423|F432|F214|F241|F314|F341) ->
          printf "((%s%s)*(%s*%s)*(%s*%s)*%s*%s*%s)"
            (format_coeff coeff) c p2 p3 p1 p123 wf1 wf2 wf3
      | C_12_34, (F314|F413|F324|F423|F132|F231|F142|F241)
      | C_13_42, (F214|F412|F234|F432|F123|F321|F143|F341)
      | C_14_23, (F213|F312|F243|F342|F124|F421|F134|F431) ->
          printf "((%s%s)*(%s*%s)*(%s*%s)*%s*%s*%s)"
            (format_coeff coeff) c p1 p3 p2 p123 wf1 wf2 wf3

    let print_add_dscalar2_vector2 c wf1 wf2 wf3 p1 p2 p3 p123
        fusion (coeff, contraction) =
      printf "@ + ";
      print_dscalar2_vector2 c wf1 wf2 wf3 p1 p2 p3 p123
        fusion (coeff, contraction)

    let print_current amplitude dictionary rhs =
      match F.coupling rhs with
      | V3 (vertex, fusion, constant) ->
          let ch1, ch2 = children2 rhs in
          let wf1 = multiple_variable amplitude dictionary ch1
          and wf2 = multiple_variable amplitude dictionary ch2
          and p1 = momentum ch1
          and p2 = momentum ch2 
          and m1 = CM.mass_symbol (F.flavor ch1)
          and m2 = CM.mass_symbol (F.flavor ch2) in
          let c = CM.constant_symbol constant in
          printf "@, %s " (if (F.sign rhs) < 0 then "-" else "+");
          begin match vertex with

(* Fermionic currents $\bar\psi\fmslash{A}\psi$ and $\bar\psi\phi\psi$
   are handled by the [Fermions] module, since they depend on the
   choice of Feynman rules: Dirac or Majorana. *)

          | FBF (coeff, fb, b, f) ->
              begin match coeff, fb, b, f with
              | _, Psibar, VLRM, Psi | _, Psibar, SPM, Psi
              | _, Psibar, TVA, Psi | _, Psibar, TVAM, Psi 
              | _, Psibar, TLR, Psi | _, Psibar, TLRM, Psi
              | _, Psibar, TRL, Psi | _, Psibar, TRLM, Psi ->
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
          | GBG (coeff, fb, b, f) ->  let p12 =
              Printf.sprintf "(-%s-%s)" p1 p2 in
              Fermions.print_current_g (coeff, fb, b, f) c wf1 wf2 p1 p2 
                   p12 fusion

(* Table~\ref{tab:dim4-bosons} is a bit misleading, since if includes
   totally antisymmetric structure constants.  The space-time part alone
   is also totally antisymmetric: *)

          | Gauge_Gauge_Gauge coeff ->
              let c = format_coupling coeff c in
              begin match fusion with
              | (F23|F31|F12) ->
                  printf "g_gg(%s,%s,%s,%s,%s)" c wf1 p1 wf2 p2
              | (F32|F13|F21) ->
                  printf "g_gg(%s,%s,%s,%s,%s)" c wf2 p2 wf1 p1
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
              | F23 | F31 | F12 ->
                  printf "kg_kgkg(%s,%s,%s,%s,%s)" c wf1 p1 wf2 p2
              | F32 | F13 | F21 ->
                  printf "kg_kgkg(%s,%s,%s,%s,%s)" c wf2 p2 wf1 p1
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
              | (F23|F32) ->
                  printf "%s*(%s*%s)*(%s*%s)" c p1 p2 wf1 wf2
              | (F12|F13) ->
                  printf "%s*(-((%s+%s)*%s))*(%s*%s)" c p1 p2 p2 wf1 wf2
              | (F21|F31) ->
                  printf "%s*(-((%s+%s)*%s))*(%s*%s)" c p1 p2 p1 wf1 wf2
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
              | (F23|F32) -> printf "(%s)*((%s*%s)*(%s*%s) - (%s*%s)*(%s*%s))" 
                    c p1 wf2 p2 wf1 p1 p2 wf2 wf1 
              | (F12|F13) -> printf "(%s)*%s*((-((%s+%s)*%s))*%s - ((-(%s+%s)*%s))*%s)" 
                    c wf1 p1 p2 wf2 p2 p1 p2 p2 wf2
              | (F21|F31) -> printf "(%s)*%s*((-((%s+%s)*%s))*%s - ((-(%s+%s)*%s))*%s)" 
                    c wf2 p2 p1 wf1 p1 p1 p2 p1 wf1
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
              | F23 -> printf "(%s)*((%s*%s)*(-(%s+%s)*%s) - (%s*%s)*(-(%s+%s)*%s))" 
                    c p1 wf2 p1 p2 wf1 wf1 wf2 p1 p2 p1
              | F32 -> printf "(%s)*((%s*%s)*(-(%s+%s)*%s) - (%s*%s)*(-(%s+%s)*%s))" 
                    c p2 wf1 p2 p1 wf2 wf2 wf1 p2 p1 p2
              | F12 -> printf "(%s)*%s*((%s*%s)*%s - (%s*%s)*%s)" 
                    c wf1 p1 wf2 p2 p1 p2 wf2
              | F21 -> printf "(%s)*%s*((%s*%s)*%s - (%s*%s)*%s)" 
                    c wf2 p2 wf1 p1 p2 p1 wf1
              | F13 -> printf "(%s)*%s*((-((%s+%s)*%s))*%s - (-(%s+%s)*%s)*%s)" 
                    c wf1 p2 p1 wf2 p1 p1 p2 p1 wf2
              | F31 -> printf "(%s)*%s*((-((%s+%s)*%s))*%s - (-(%s+%s)*%s)*%s)" 
                    c wf2 p1 p2 wf1 p2 p2 p1 p2 wf1
              end

          | Dim6_Vector_Vector_Vector_T coeff ->
              let c = format_coupling coeff c in
              begin match fusion with
              | F23 -> printf "(%s)*(%s*%s)*(%s*%s)*(%s-%s)" c p2 wf1 p1 wf2 p1 p2
              | F32 -> printf "(%s)*(%s*%s)*(%s*%s)*(%s-%s)" c p1 wf2 p2 wf1 p2 p1
              | (F12|F13) -> printf "(%s)*((%s+2*%s)*%s)*(-((%s+%s)*%s))*%s"
                    c p1 p2 wf1 p1 p2 wf2 p2
              | (F21|F31) -> printf "(%s)*((-((%s+%s)*%s))*(%s+2*%s)*%s)*%s"
                    c p2 p1 wf1 p2 p1 wf2 p1
              end

          | Tensor_2_Vector_Vector coeff ->
              let c = format_coupling coeff c in
              begin match fusion with
              | (F23|F32) -> printf "t2_vv(%s,%s,%s)" c wf1 wf2
              | (F12|F13) -> printf "v_t2v(%s,%s,%s)" c wf1 wf2
              | (F21|F31) -> printf "v_t2v(%s,%s,%s)" c wf2 wf1
              end

          | Dim5_Tensor_2_Vector_Vector_1 coeff ->
              let c = format_coupling coeff c in
              begin match fusion with
              | (F23|F32) -> printf "t2_vv_d5_1(%s,%s,%s,%s,%s)" c wf1 p1 wf2 p2
              | (F12|F13) -> printf "v_t2v_d5_1(%s,%s,%s,%s,%s)" c wf1 p1 wf2 p2
              | (F21|F31) -> printf "v_t2v_d5_1(%s,%s,%s,%s,%s)" c wf2 p2 wf1 p1
              end

          | Dim5_Tensor_2_Vector_Vector_2 coeff ->
              let c = format_coupling coeff c in
              begin match fusion with
              | F23 -> printf "t2_vv_d5_2(%s,%s,%s,%s,%s)" c wf1 p1 wf2 p2
              | F32 -> printf "t2_vv_d5_2(%s,%s,%s,%s,%s)" c wf2 p2 wf1 p1
              | (F12|F13) -> printf "v_t2v_d5_2(%s,%s,%s,%s,%s)" c wf1 p1 wf2 p2
              | (F21|F31) -> printf "v_t2v_d5_2(%s,%s,%s,%s,%s)" c wf2 p2 wf1 p1
              end

          | Dim7_Tensor_2_Vector_Vector_T coeff ->
              let c = format_coupling coeff c in
              begin match fusion with
              | F23 -> printf "t2_vv_d7(%s,%s,%s,%s,%s)" c wf1 p1 wf2 p2
              | F32 -> printf "t2_vv_d7(%s,%s,%s,%s,%s)" c wf2 p2 wf1 p1
              | (F12|F13) -> printf "v_t2v_d7(%s,%s,%s,%s,%s)" c wf1 p1 wf2 p2
              | (F21|F31) -> printf "v_t2v_d7(%s,%s,%s,%s,%s)" c wf2 p2 wf1 p1
              end

          end

(* Flip the sign to account for the~$\mathrm{i}^2$ relative to diagrams
   with only cubic couplings.  *)

      | V4 (vertex, fusion, constant) ->
          let c = CM.constant_symbol constant
          and ch1, ch2, ch3 = children3 rhs in
          let wf1 = multiple_variable amplitude dictionary ch1
          and wf2 = multiple_variable amplitude dictionary ch2
          and wf3 = multiple_variable amplitude dictionary ch3
          and p1 = momentum ch1
          and p2 = momentum ch2
          and p3 = momentum ch3 in
          printf "@, %s " (if (F.sign rhs) < 0 then "+" else "-");
          begin match vertex with
          | Scalar4 coeff ->
              printf "(%s*%s*%s*%s)" (format_coupling coeff c) wf1 wf2 wf3
          | Scalar2_Vector2 coeff ->
              let c = format_coupling coeff c in
              begin match fusion with
              | F134 | F143 | F234 | F243 ->
                  printf "%s*%s*(%s*%s)" c wf1 wf2 wf3
              | F314 | F413 | F324 | F423 ->
                  printf "%s*%s*(%s*%s)" c wf2 wf1 wf3
              | F341 | F431 | F342 | F432 ->
                  printf "%s*%s*(%s*%s)" c wf3 wf1 wf2
              | F312 | F321 | F412 | F421 ->
                  printf "(%s*%s*%s)*%s" c wf2 wf3 wf1
              | F231 | F132 | F241 | F142 ->
                  printf "(%s*%s*%s)*%s" c wf1 wf3 wf2
              | F123 | F213 | F124 | F214 ->
                  printf "(%s*%s*%s)*%s" c wf1 wf2 wf3
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
          | Vector4_K_Matrix_tho (disc, poles) ->
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
                  (CM.constant_symbol coeff) pa pb pa pb
                  (CM.constant_symbol pole))
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
          | GBBG (coeff, fb, b, f) -> 
              Fermions.print_current_g4 (coeff, fb, b, f) c wf1 wf2 wf3 
                   fusion

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
                  List.iter (print_add_dscalar4
                               c wf1 wf2 wf3 p1 p2 p3 p123 fusion) tail;
                  printf ")"
              end

          | DScalar2_Vector2 contractions ->
              let p123 = Printf.sprintf "(-%s-%s-%s)" p1 p2 p3 in
              begin match contractions with
              | [] -> invalid_arg "Targets.print_current: DScalar4 []"
              | head :: tail ->
                  printf "(";
                  print_dscalar2_vector2
                    c wf1 wf2 wf3 p1 p2 p3 p123 fusion head;
                  List.iter (print_add_dscalar2_vector2
                               c wf1 wf2 wf3 p1 p2 p3 p123 fusion) tail;
                  printf ")"
              end
          end

      | Vn (_, _, _) ->
          invalid_arg "Targets.print_current: n-ary fusion"

    let print_propagator f p m gamma =
      let minus_third = "(-1.0_" ^ !kind ^ "/3.0_" ^ !kind ^ ")" in
      let w =
        begin match CM.width f with
        | Vanishing | Fudged -> "0.0_" ^ !kind
        | Constant -> gamma
        | Timelike -> "wd_tl(" ^ p ^ "," ^ gamma ^ ")"
        | Running ->
            failwith "Targets.Fortran: running width not yet available"
        | Custom f -> f ^ "(" ^ p ^ "," ^ gamma ^ ")"
        end in
      match CM.propagator f with
      | Prop_Scalar ->
          printf "pr_phi(%s,%s,%s," p m w
      | Prop_Col_Scalar -> 
          printf "%s * pr_phi(%s,%s,%s," minus_third p m w
      | Prop_Ghost -> printf "(0,1) * pr_phi(%s, %s, %s," p m w
      | Prop_Spinor ->
          printf "%s(%s,%s,%s," Fermions.psi_propagator p m w
      | Prop_ConjSpinor ->
          printf "%s(%s,%s,%s," Fermions.psibar_propagator p m w
      | Prop_Majorana ->
          printf "%s(%s,%s,%s," Fermions.chi_propagator p m w
      | Prop_Col_Majorana ->
          printf "%s * %s(%s,%s,%s," minus_third Fermions.chi_propagator p m w 
      | Prop_Unitarity ->
          printf "pr_unitarity(%s,%s,%s," p m w
      | Prop_Col_Unitarity -> 
          printf "%s * pr_unitarity(%s,%s,%s," minus_third p m w
      | Prop_Feynman ->
          printf "pr_feynman(%s," p
      | Prop_Col_Feynman -> 
          printf "%s * pr_feynman(%s," minus_third p
      | Prop_Gauge xi ->
          printf "pr_gauge(%s,%s," p (CM.gauge_symbol xi)
      | Prop_Rxi xi ->
          printf "pr_rxi(%s,%s,%s,%s," p m w (CM.gauge_symbol xi)
      | Prop_Tensor_2 ->
          printf "pr_tensor(%s,%s,%s," p m w
      | Prop_Vectorspinor ->
          printf "pr_grav(%s,%s,%s," p m w
      | Aux_Scalar | Aux_Spinor | Aux_ConjSpinor | Aux_Majorana
      | Aux_Vector | Aux_Tensor_1 -> printf "("
      | Aux_Col_Vector | Aux_Col_Tensor_1 -> printf "%s * (" minus_third
      | Only_Insertion -> printf "("

    let print_projector f p m gamma =
      let minus_third = "(-1.0_" ^ !kind ^ "/3.0_" ^ !kind ^ ")" in
      match CM.propagator f with
      | Prop_Scalar ->
          printf "pj_phi(%s,%s," m gamma
      | Prop_Col_Scalar -> 
          printf "%s * pj_phi(%s,%s," minus_third m gamma
      | Prop_Ghost ->
          printf "(0,1) * pj_phi(%s,%s," m gamma
      | Prop_Spinor ->
          printf "%s(%s,%s,%s," Fermions.psi_projector p m gamma
      | Prop_ConjSpinor ->
          printf "%s(%s,%s,%s," Fermions.psibar_projector p m gamma
      | Prop_Majorana ->
          printf "%s(%s,%s,%s," Fermions.chi_projector p m gamma
      | Prop_Col_Majorana ->
          printf "%s * %s(%s,%s,%s," minus_third Fermions.chi_projector p m gamma
      | Prop_Unitarity ->
          printf "pj_unitarity(%s,%s,%s," p m gamma
      | Prop_Col_Unitarity -> 
          printf "%s * pj_unitarity(%s,%s,%s," minus_third p m gamma
      | Prop_Feynman | Prop_Col_Feynman -> 
          invalid_arg "no on-shell Feynman propagator!" 
      | Prop_Gauge xi ->
          invalid_arg "no on-shell massless gauge propagator!"
      | Prop_Rxi xi ->
          invalid_arg "no on-shell Rxi propagator!"
      | Prop_Vectorspinor ->
          printf "pj_grav(%s,%s,%s," p m gamma
      | Prop_Tensor_2 ->
          printf "pj_tensor(%s,%s,%s," p m gamma
      | Aux_Scalar | Aux_Spinor | Aux_ConjSpinor | Aux_Majorana
      | Aux_Vector | Aux_Tensor_1 -> printf "("
      | Aux_Col_Vector | Aux_Col_Tensor_1 -> printf "%s * (" minus_third
      | Only_Insertion -> printf "("

    let print_gauss f p m gamma =
      let minus_third = "(-1.0_" ^ !kind ^ "/3.0_" ^ !kind ^ ")" in
      match CM.propagator f with
      | Prop_Scalar ->
          printf "pg_phi(%s,%s,%s," p m gamma
      | Prop_Ghost ->
          printf "(0,1) * pg_phi(%s,%s,%s," p m gamma
      | Prop_Spinor ->
          printf "%s(%s,%s,%s," Fermions.psi_projector p m gamma
      | Prop_ConjSpinor ->
          printf "%s(%s,%s,%s," Fermions.psibar_projector p m gamma
      | Prop_Majorana ->
          printf "%s(%s,%s,%s," Fermions.chi_projector p m gamma
      | Prop_Col_Majorana ->
          printf "%s * %s(%s,%s,%s," minus_third Fermions.chi_projector p m gamma
      | Prop_Unitarity ->
          printf "pg_unitarity(%s,%s,%s," p m gamma
      | Prop_Feynman | Prop_Col_Feynman -> 
          invalid_arg "no on-shell Feynman propagator!" 
      | Prop_Gauge xi ->
          invalid_arg "no on-shell massless gauge propagator!"
      | Prop_Rxi xi ->
          invalid_arg "no on-shell Rxi propagator!"
      | Prop_Tensor_2 ->
          printf "pg_tensor(%s,%s,%s," p m gamma
      | Aux_Scalar | Aux_Spinor | Aux_ConjSpinor | Aux_Majorana
      | Aux_Vector | Aux_Tensor_1 -> printf "("
      | Only_Insertion -> printf "("
      | _ -> invalid_arg "targets:print_gauss: not available"

    let print_fusion_diagnostics amplitude dictionary fusion =
      if warn diagnose_gauge then begin
        let lhs = F.lhs fusion in
        let f = F.flavor lhs
        and v = variable lhs
        and p = momentum lhs in
        let mass = CM.mass_symbol f in
        match CM.propagator f with
        | Prop_Gauge _ | Prop_Feynman
        | Prop_Rxi _ | Prop_Unitarity ->
            printf "      @[<2>%s =" v;
            List.iter (print_current amplitude dictionary) (F.rhs fusion); nl();
            begin match CM.goldstone f with
            | None ->
                printf "      call omega_ward_%s(\"%s\",%s,%s,%s)"
                  (suffix diagnose_gauge) v mass p v; nl ()
            | Some (g, phase) ->
                let gv = add_tag lhs (CM.flavor_symbol g ^ "_" ^ format_p lhs) in
                printf "      call omega_slavnov_%s"
                  (suffix diagnose_gauge);
                printf "(@[\"%s\",%s,%s,%s,@,%s*%s)"
                  v mass p v (format_constant phase) gv; nl ()
            end
        | _ -> ()
      end

    let print_fusion amplitude dictionary fusion =
      let lhs = F.lhs fusion in
      let f = F.flavor lhs in
      printf "      @[<2>%s =@, " (multiple_variable amplitude dictionary lhs);
      if F.on_shell amplitude lhs then
        print_projector f (momentum lhs)
          (CM.mass_symbol f) (CM.width_symbol f)
      else 
        if F.is_gauss amplitude lhs then
          print_gauss f (momentum lhs)
            (CM.mass_symbol f) (CM.width_symbol f)
        else
          print_propagator f (momentum lhs) 
            (CM.mass_symbol f) (CM.width_symbol f);
      List.iter (print_current amplitude dictionary) (F.rhs fusion);
      printf ")"; nl ()

    let print_momenta seen_momenta amplitude =
      List.fold_left (fun seen f ->
        let wf = F.lhs f in
        let p = F.momentum_list wf in
        if not (PSet.mem p seen) then begin
          let rhs1 = List.hd (F.rhs f) in
          printf "    %s = %s" (momentum wf)
            (String.concat " + "
               (List.map momentum (F.children rhs1))); nl ()
        end;
        PSet.add p seen)
        seen_momenta (F.fusions amplitude)

    let print_fusions dictionary fusions =
      List.iter
        (fun (f, amplitude) -> 
          print_fusion_diagnostics amplitude dictionary f;
          print_fusion amplitude dictionary f)
        fusions
        
    let print_braket amplitude dictionary name braket =
      let bra = F.bra braket
      and ket = F.ket braket in
      printf "      @[<2>%s = %s@, + " name name;
      begin match Fermions.reverse_braket (CM.lorentz (F.flavor bra)) with
      | false ->
          printf "%s*(@," (multiple_variable amplitude dictionary bra);
          List.iter (print_current amplitude dictionary) ket;
          printf ")"
      | true ->
          printf "(@,";
          List.iter (print_current amplitude dictionary) ket;
          printf ")*%s" (multiple_variable amplitude dictionary bra)
      end; nl ()

(* \begin{equation}
     \ii T = \ii^{\#\text{vertices}}\ii^{\#\text{propagators}} \cdots
           = \ii^{n-2}\ii^{n-3} \cdots
           = -\ii(-1)^n \cdots
   \end{equation} *)

(* \begin{dubious}
     [tho:] we write some brakets twice using different names.  Is it useful
     to cache them?
   \end{dubious} *)

    let print_brakets dictionary amplitude =
      let name = flavors_symbol (flavors amplitude) in
      printf "      %s = 0" name; nl ();
      List.iter (print_braket amplitude dictionary name) (F.brakets amplitude);
      let n = List.length (F.externals amplitude) in
      if n mod 2 = 0 then begin
        printf "      @[<2>%s =@, - %s ! %d vertices, %d propagators"
          name name (n - 2) (n - 3); nl ()
      end else begin
        printf "      ! %s = %s ! %d vertices, %d propagators"
          name name (n - 2) (n - 3); nl ()
      end;
      let s = F.symmetry amplitude in
      if s > 1 then
        printf "      @[<2>%s =@, %s@, / sqrt(%d.0_%s) ! symmetry factor" name name s !kind
      else
        printf "      ! unit symmetry factor";
      nl ()

    let print_incoming wf =
      let p = momentum wf
      and s = spin wf
      and f = F.flavor wf in
      let m = CM.mass_symbol f in
      match CM.lorentz f with
      | Scalar -> printf "1"
      | BRS Scalar -> printf "(0,-1) * (%s * %s - %s**2)" p p m 
      | Spinor ->
          printf "%s (%s, - %s, %s)" Fermions.psi_incoming m p s
      | BRS Spinor ->
          printf "%s (%s, - %s, %s)" Fermions.brs_psi_incoming m p s
      | ConjSpinor ->
          printf "%s (%s, - %s, %s)" Fermions.psibar_incoming m p s
      | BRS ConjSpinor ->
          printf "%s (%s, - %s, %s)" Fermions.brs_psibar_incoming m p s
      | Majorana ->
          printf "%s (%s, - %s, %s)" Fermions.chi_incoming m p s
      | Maj_Ghost -> printf "ghost (%s, - %s, %s)" m p s
      | BRS Majorana ->
          printf "%s (%s, - %s, %s)" Fermions.brs_chi_incoming m p s
      | Vector | Massive_Vector -> 
          printf "eps (%s, - %s, %s)" m p s
(*i   | Ward_Vector -> printf "%s" p   i*)
      | BRS Vector | BRS Massive_Vector -> printf 
            "(0,1) * (%s * %s - %s**2) * eps (%s, -%s, %s)" p p m m p s 
      | Vectorspinor | BRS Vectorspinor -> 
          printf "%s (%s, - %s, %s)" Fermions.grav_incoming m p s
      | Tensor_1 -> invalid_arg "Tensor_1 only internal"
      | Tensor_2 -> printf "eps2 (%s, - %s, %s)" m p s
      | _ -> invalid_arg "no such BRST transformations"

    let print_outgoing wf =
      let p = momentum wf
      and s = spin wf
      and f = F.flavor wf in
      let m = CM.mass_symbol f in
      match CM.lorentz f with
      | Scalar -> printf "1"
      | BRS Scalar -> printf "(0,-1) * (%s * %s - %s**2)" p p m 
      | Spinor ->
          printf "%s (%s, %s, %s)" Fermions.psi_outgoing m p s
      | BRS Spinor ->
          printf "%s (%s, %s, %s)" Fermions.brs_psi_outgoing m p s
      | ConjSpinor ->
          printf "%s (%s, %s, %s)" Fermions.psibar_outgoing m p s
      | BRS ConjSpinor ->
          printf "%s (%s, %s, %s)" Fermions.brs_psibar_outgoing m p s
      | Majorana ->
          printf "%s (%s, %s, %s)" Fermions.chi_outgoing m p s
      | BRS Majorana ->
          printf "%s (%s, %s, %s)" Fermions.brs_chi_outgoing m p s
      | Maj_Ghost -> printf "ghost (%s, %s, %s)" m p s 
      | Vector | Massive_Vector -> 
          printf "conjg (eps (%s, %s, %s))" m p s
(*i   | Ward_Vector -> printf "%s" p   i*)
      | BRS Vector | BRS Massive_Vector -> printf 
            "(0,1) * (%s*%s-%s**2) * (conjg (eps (%s, %s, %s)))" p p m m p s 
      | Vectorspinor | BRS Vectorspinor -> 
          printf "%s (%s, %s, %s)" Fermions.grav_incoming m p s
      | Tensor_1 -> invalid_arg "Tensor_1 only internal"
      | Tensor_2 -> printf "conjg (eps2 (%s, %s, %s))" m p s
      | BRS _ -> invalid_arg "no such BRST transformations"

    let twice_spin wf =
      match CM.lorentz (F.flavor wf) with
      | Scalar | BRS Scalar -> "0"
      | Spinor | ConjSpinor | Majorana | Maj_Ghost | Vectorspinor 
      | BRS Spinor | BRS ConjSpinor | BRS Majorana | BRS Vectorspinor -> "1"
      | Vector | BRS Vector | Massive_Vector | BRS Massive_Vector -> "2"
      | Tensor_1 -> "2"
      | Tensor_2 -> "4" 
      | BRS _ -> invalid_arg "Targets.twice_spin: no such BRST transformation"

    let print_argument_diagnostics amplitude =
      let externals = (F.externals amplitude) in
      let n = List.length externals
      and masses = List.map (fun wf -> CM.mass_symbol (F.flavor wf)) externals in
      if warn diagnose_arguments then begin
        printf "    call omega_check_arguments_%s (%d, k)"
          (suffix diagnose_arguments) n; nl ()
      end;
      if warn diagnose_momenta then begin
        printf "    @[<2>call omega_check_momenta_%s ((/ "
          (suffix diagnose_momenta);
        print_list masses;
        printf " /), k)"; nl ()
      end

    let print_external_momenta amplitude =
      let externals =
        List.combine
          (F.externals amplitude)
          (List.map (fun _ -> true) (F.incoming amplitude) @
           List.map (fun _ -> false) (F.outgoing amplitude)) in
      List.iter (fun (wf, incoming) ->
        if incoming then
          printf "    %s = - k(:,%d) ! incoming"
            (momentum wf) (ext_momentum wf)
        else
          printf "    %s =   k(:,%d) ! outgoing"
            (momentum wf) (ext_momentum wf); nl ()) externals

    let print_externals seen_wfs amplitude =
      let externals =
        List.combine
          (F.externals amplitude)
          (List.map (fun _ -> true) (F.incoming amplitude) @
           List.map (fun _ -> false) (F.outgoing amplitude)) in
      List.fold_left (fun seen (wf, incoming) ->
        if not (WFSet.mem wf seen) then begin
          printf "      @[<2>%s =@, " (variable wf);
          (if incoming then print_incoming else print_outgoing) wf; nl ()
        end;
        WFSet.add wf seen) seen_wfs externals

    let flavors_to_string flavors =
      String.concat " " (List.map CM.flavor_to_string flavors)

    let process_to_string amplitude =
      flavors_to_string (F.incoming amplitude) ^ " -> " ^
      flavors_to_string (F.outgoing amplitude)

    let flavors_sans_color_to_string flavors =
      String.concat " " (List.map M.flavor_to_string flavors)

    let process_sans_color_to_string (fin, fout) =
      flavors_sans_color_to_string fin ^ " -> " ^
      flavors_sans_color_to_string fout

    let print_fudge_factor amplitude =
      let name = flavors_symbol (flavors amplitude) in
      List.iter (fun wf ->
        let p = momentum wf
        and f = F.flavor wf in
        match CM.width f with
        | Fudged ->
            let m = CM.mass_symbol f
            and w = CM.width_symbol f in
            printf "      if (%s > 0.0_%s) then" w !kind; nl ();
            printf "        @[<2>%s = %s@ * (%s*%s - %s**2)"
              name name p p m;
            printf "@ / cmplx (%s*%s - %s**2, %s*%s, kind=%s)"
              p p m m w !kind; nl();
            printf "      end if"; nl ()
        | _ -> ()) (F.s_channel amplitude)

    let num_helicities amplitudes =
      List.length (CF.helicities amplitudes)

(* \thocwmodulesubsection{Spin, Flavor \&\ Color Tables} *)

(* The following abomination is required to keep the number of continuation
   lines as low as possible.  FORTRAN77-style \texttt{DATA} statements
   are actually a bit nicer here, but they are nor available for
   \emph{constant} arrays. *)

(* \begin{dubious}
     We used to have a more elegent design with a sentinel~0 added to each
     initializer, but some revisions of the Compaq/Digital Compiler have a
     bug that causes it to reject this variant.
   \end{dubious} *)

(* \begin{dubious}
     The actual table writing code using \texttt{reshape} should be factored,
     since it's the same algorithm every time.
   \end{dubious} *)

    let print_integer_parameter name value =
      printf "  @[<2>integer, parameter :: %s = %d" name value; nl ()

    let print_real_parameter name value =
      printf "  @[<2>real(kind=%s), parameter :: %s = %d"
        !kind name value; nl ()

    let print_logical_parameter name value =
      printf "  @[<2>logical, parameter :: %s = .%s."
        name (if value then "true" else "false"); nl ()

    let num_particles_in amplitudes =
      match CF.flavors amplitudes with
      | [] -> 0
      | (fin, _) :: _ -> List.length fin

    let num_particles_out amplitudes =
      match CF.flavors amplitudes with
      | [] -> 0
      | (_, fout) :: _ -> List.length fout

    let num_particles amplitudes =
      match CF.flavors amplitudes with
      | [] -> 0
      | (fin, fout) :: _ -> List.length fin + List.length fout

    module CFlow = Color.Flow

    let num_color_flows amplitudes =
      List.length (CF.color_flows amplitudes)

    let num_color_indices_default = 2 (* Standard model *)

    let num_color_indices amplitudes =
      try CFlow.rank (List.hd (CF.color_flows amplitudes)) with _ -> num_color_indices_default

    let color_to_string c =
      "(" ^ (String.concat "," (List.map (Printf.sprintf "%3d") c)) ^ ")"

    let cflow_to_string cflow =
      String.concat " " (List.map color_to_string (CFlow.in_to_lists cflow)) ^ " -> " ^
      String.concat " " (List.map color_to_string (CFlow.out_to_lists cflow))

    let protected = ""
    let protected = ", protected" (* Fortran 2003! *)

    let print_spin_table_old abbrev name = function
      | [] ->
          printf "  @[<2>integer, dimension(n_prt,0) ::";
          printf "@ table_spin_%s" name; nl ()
      | _ :: tuples' as tuples ->
          ignore (List.fold_left (fun i (tuple1, tuple2) ->
            printf "  @[<2>integer, dimension(n_prt), parameter, private ::";
            printf "@ %s%04d = (/ %s /)" abbrev i
              (String.concat ", " (List.map (Printf.sprintf "%2d") (tuple1 @ tuple2)));
            nl (); succ i) 1 tuples);
          printf
            "  @[<2>integer, dimension(n_prt,n_hel), parameter ::";
          printf "@ table_spin_%s =@ reshape ( (/" name;
          printf "@ %s%04d" abbrev 1;
          ignore (List.fold_left (fun i tuple ->
            printf ",@ %s%04d" abbrev i; succ i) 2 tuples');
          printf "@ /), (/ n_prt, n_hel /) )"; nl ()

    let print_spin_table name tuples =
      printf "  @[<2>integer, dimension(n_prt,n_hel), save%s :: table_spin_%s"
        protected name; nl();
      match tuples with
      | [] -> ()
      | _ ->
          ignore (List.fold_left (fun i (tuple1, tuple2) ->
            printf "  @[<2>data table_spin_%s(:,%4d) / %s /" name i
              (String.concat ", " (List.map (Printf.sprintf "%2d") (tuple1 @ tuple2)));
            nl (); succ i) 1 tuples)

    let print_spin_tables amplitudes =
      (* [print_spin_table_old "s" "states_old" (CF.helicities amplitudes);] *)
      print_spin_table "states" (CF.helicities amplitudes);
      nl ()

    let print_flavor_table_old n abbrev name = function
      | [] ->
          printf "  @[<2>integer, dimension(n_prt,0) ::";
          printf "@ table_flavor_%s" name; nl ()
      | _ :: tuples' as tuples ->
          ignore (List.fold_left (fun i tuple ->
            printf
              "  @[<2>integer, dimension(n_prt), parameter, private ::";
            printf "@ %s%04d = (/ %s /) ! %s" abbrev i
              (String.concat ", "
                 (List.map (fun f -> Printf.sprintf "%3d" (M.pdg f)) tuple))
              (String.concat " " (List.map M.flavor_to_string tuple));
            nl (); succ i) 1 tuples);
          printf
            "  @[<2>integer, dimension(n_prt,n_flv), parameter ::";
          printf "@ table_flavor_%s =@ reshape ( (/" name;
          printf "@ %s%04d" abbrev 1;
          ignore (List.fold_left (fun i tuple ->
            printf ",@ %s%04d" abbrev i; succ i) 2 tuples');
          printf "@ /), (/ n_prt, n_flv /) )"; nl ()

    let print_flavor_table n name tuples =
      printf "  @[<2>integer, dimension(n_prt,n_flv), save%s :: table_flavor_%s"
        protected name; nl();
      match tuples with
      | [] -> ()
      | _ ->
          ignore (List.fold_left (fun i tuple ->
            printf "  @[<2>data table_flavor_%s(:,%4d) / %s / ! %s" name i
              (String.concat ", "
                 (List.map (fun f -> Printf.sprintf "%3d" (M.pdg f)) tuple))
              (String.concat " " (List.map M.flavor_to_string tuple));
            nl (); succ i) 1 tuples)

    let print_flavor_tables amplitudes =
      let n = num_particles amplitudes in
      (* [print_flavor_table_old n "f" "states_old"
        (List.map (fun (fin, fout) -> fin @ fout) (CF.flavors amplitudes));] *)
      print_flavor_table n "states"
        (List.map (fun (fin, fout) -> fin @ fout) (CF.flavors amplitudes));
      nl ()

    let num_flavors amplitudes =
      List.length (CF.flavors amplitudes)

    let print_color_flows_table_old abbrev = function
      | [] ->
          printf "  @[<2>integer, dimension(n_cindex, n_prt, n_cflow) ::";
          printf "@ table_color_flows"; nl ()
      | _ :: tuples' as tuples ->
          ignore (List.fold_left (fun i tuple ->
            printf
              "  @[<2>integer, dimension(n_cindex, n_prt), parameter, private ::";
            printf "@ %s%04d = reshape ( (/ " abbrev i;
            begin match CFlow.to_lists tuple with
            | [] -> ()
            | cf1 :: cfn ->
                printf "@ %s" (String.concat "," (List.map string_of_int cf1));
                List.iter (function cf ->
                  printf ",@  %s" (String.concat "," (List.map string_of_int cf))) cfn
            end;
            printf "@ /),@ (/ n_cindex, n_prt /) )";
            nl (); succ i) 1 tuples);
          printf
            "  @[<2>integer, dimension(n_cindex, n_prt, n_cflow), parameter ::";
          printf "@ table_color_flows_old =@ reshape ( (/";
          printf "@ %s%04d" abbrev 1;
          ignore (List.fold_left (fun i tuple ->
            printf ",@ %s%04d" abbrev i; succ i) 2 tuples');
          printf "@ /),@ (/ n_cindex, n_prt, n_cflow /) )"; nl ()

    let print_ghost_flags_table_old abbrev = function
      | [] ->
          printf "  @[<2>logical, dimension(n_prt, n_cflow) ::";
          printf "@ table_ghost_flags"; nl ()
      | _ :: tuples' as tuples ->
          ignore (List.fold_left (fun i tuple ->
            printf
              "  @[<2>logical, dimension(n_prt), parameter, private ::";
            printf "@ %s%04d = (/ " abbrev i;
            begin match CFlow.ghost_flags tuple with
            | [] -> ()
            | gf1 :: gfn ->
                printf "@ %s" (if gf1 then "T" else "F");
                List.iter (function gf -> printf ",@  %s" (if gf then "T" else "F")) gfn
            end;
            printf "@ /)";
            nl (); succ i) 1 tuples);
          printf
            "  @[<2>logical, dimension(n_prt, n_cflow), parameter ::";
          printf "@ table_ghost_flags_old =@ reshape ( (/";
          printf "@ %s%04d" abbrev 1;
          ignore (List.fold_left (fun i tuple ->
            printf ",@ %s%04d" abbrev i; succ i) 2 tuples');
          printf "@ /),@ (/ n_prt, n_cflow /) )"; nl ()

    let print_color_flows_table tuples =
      printf
        "  @[<2>integer, dimension(n_cindex,n_prt,n_cflow), save%s :: table_color_flows"
        protected; nl ();
      match tuples with
      | [] -> ()
      | _ :: tuples' as tuples ->
          ignore (List.fold_left (fun i tuple ->
            begin match CFlow.to_lists tuple with
            | [] -> ()
            | cf1 :: cfn ->
                printf "  @[<2>data table_color_flows(:,:,%4d) /" i;
                printf "@ %s" (String.concat "," (List.map string_of_int cf1));
                List.iter (function cf ->
                  printf ",@  %s" (String.concat "," (List.map string_of_int cf))) cfn;
                printf "@ /"; nl ()
            end;
            succ i) 1 tuples)

    let print_ghost_flags_table tuples =
      printf
        "  @[<2>logical, dimension(n_prt,n_cflow), save%s :: table_ghost_flags"
        protected; nl ();
      match tuples with
      | [] -> ()
      | _ ->
          ignore (List.fold_left (fun i tuple ->
            begin match CFlow.ghost_flags tuple with
            | [] -> ()
            | gf1 :: gfn ->
                printf "  @[<2>data table_ghost_flags(:,%4d) /" i;
                printf "@ %s" (if gf1 then "T" else "F");
                List.iter (function gf -> printf ",@  %s" (if gf then "T" else "F")) gfn;
                printf " /";
                nl ()
            end;
            succ i) 1 tuples)

    let format_power_of x
        { Color.Flow.num = num; Color.Flow.den = den; Color.Flow.power = pwr } =
      match num, den, pwr with
      | _, 0, _ -> invalid_arg "format_power_of: zero denominator"
      | 0, _, _ -> "+zero"
      | 1, 1, 0 | -1, -1, 0 -> "+one"
      | -1, 1, 0 | 1, -1, 0 -> "-one"
      | 1, 1, 1 | -1, -1, 1 -> "+" ^ x
      | -1, 1, 1 | 1, -1, 1 -> "-" ^ x
      | 1, 1, -1 | -1, -1, -1 -> "+1/" ^ x
      | -1, 1, -1 | 1, -1, -1 -> "-1/" ^ x
      | 1, 1, p | -1, -1, p ->
          "+" ^ (if p > 0 then "" else "1/") ^ x ^ "**" ^ string_of_int (abs p)
      | -1, 1, p | 1, -1, p ->
          "-" ^ (if p > 0 then "" else "1/") ^ x ^ "**" ^ string_of_int (abs p)
      | n, 1, 0 ->
          (if n < 0 then "-" else "+") ^ string_of_int (abs n) ^ ".0_" ^ !kind
      | n, d, 0 ->
          (if n * d < 0 then "-" else "+") ^
          string_of_int (abs n) ^ ".0_" ^ !kind ^ "/" ^
          string_of_int (abs d)
      | n, 1, 1 ->
          (if n < 0 then "-" else "+") ^ string_of_int (abs n) ^ "*" ^ x
      | n, 1, -1 ->
          (if n < 0 then "-" else "+") ^ string_of_int (abs n) ^ "/" ^ x
      | n, d, 1 ->
          (if n * d < 0 then "-" else "+") ^
          string_of_int (abs n) ^ ".0_" ^ !kind ^ "/" ^
          string_of_int (abs d) ^ "*" ^ x
      | n, d, -1 ->
          (if n * d < 0 then "-" else "+") ^
          string_of_int (abs n) ^ ".0_" ^ !kind ^ "/" ^
          string_of_int (abs d) ^ "/" ^ x
      | n, 1, p ->
          (if n < 0 then "-" else "+") ^ string_of_int (abs n) ^
          (if p > 0 then "*" else "/") ^ x ^ "**" ^ string_of_int (abs p)
      | n, d, p ->
          (if n * d < 0 then "-" else "+") ^
          string_of_int (abs n) ^ ".0_" ^ !kind ^ "/" ^
          string_of_int (abs d) ^
          (if p > 0 then "*" else "/") ^ x ^ "**" ^ string_of_int (abs p)

    let format_powers_of x = function
      | [] -> "zero"
      | powers -> String.concat "" (List.map (format_power_of x) powers)

    let print_color_factor_table_old table =
      let n_cflow = Array.length table in
      let n_cfactors = ref 0 in
      for c1 = 0 to pred n_cflow do
        for c2 = 0 to pred n_cflow do
          match table.(c1).(c2) with
          | [] -> ()
          | _ -> incr n_cfactors
        done
      done;
      print_integer_parameter "n_cfactors"  !n_cfactors;
      if n_cflow <= 0 then begin
        printf "  @[<2>type(%s), dimension(n_cfactors) ::"
          omega_color_factor_abbrev;
        printf "@ table_color_factors"; nl ()
      end else begin
        printf
          "  @[<2>type(%s), dimension(n_cfactors), parameter ::"
          omega_color_factor_abbrev;
        printf "@ table_color_factors = (/@ ";
        let comma = ref "" in
        for c1 = 0 to pred n_cflow do
          for c2 = 0 to pred n_cflow do
            match table.(c1).(c2) with
            | [] -> ()
            | cf -> 
                printf "%s@ %s(%d,%d,%s)" !comma omega_color_factor_abbrev
                  (succ c1) (succ c2) (format_powers_of nc_parameter cf);
                comma := ","
          done
        done;
        printf "@ /)"; nl ()
      end

(* \begin{dubious}
     We can optimize the following slightly by reusing common color factor [parameter]s.
   \end{dubious} *)

    let print_color_factor_table table =
      let n_cflow = Array.length table in
      let n_cfactors = ref 0 in
      for c1 = 0 to pred n_cflow do
        for c2 = 0 to pred n_cflow do
          match table.(c1).(c2) with
          | [] -> ()
          | _ -> incr n_cfactors
        done
      done;
      print_integer_parameter "n_cfactors"  !n_cfactors;
      printf "  @[<2>type(%s), dimension(n_cfactors), save%s ::"
        omega_color_factor_abbrev protected;
      printf "@ table_color_factors"; nl ();
      let i = ref 1 in
      if n_cflow > 0 then begin
        for c1 = 0 to pred n_cflow do
          for c2 = 0 to pred n_cflow do
            match table.(c1).(c2) with
            | [] -> ()
            | cf -> 
                printf "  @[<2>real(kind=%s), parameter, private :: color_factor_%06d = %s"
                  !kind !i (format_powers_of nc_parameter cf);
                nl ();
                printf "  @[<2>data table_color_factors(%6d) / %s(%d,%d,color_factor_%06d) /"
                  !i omega_color_factor_abbrev (succ c1) (succ c2) !i;
                incr i;
                nl ();
          done
        done
      end

    let print_color_tables amplitudes =
      let cflows =  CF.color_flows amplitudes
      and cfactors = CF.color_factors amplitudes in
      (* [print_color_flows_table_old "c" cflows; nl ();] *)
      print_color_flows_table cflows; nl ();
      (* [print_ghost_flags_table_old "g" cflows; nl ();] *)
      print_ghost_flags_table cflows; nl ();
      (* [print_color_factor_table_old cfactors; nl ();] *)
      print_color_factor_table cfactors; nl ()

    let option_to_logical = function
      | Some _ -> "T"
      | None -> "F"

    let print_flavor_color_table_old abbrev n_flv n_cflow table =
      if n_flv <= 0 or n_cflow <= 0 then begin
        printf "  @[<2>logical, dimension(n_flv, n_cflow) ::";
        printf "@ flv_col_is_allowed"; nl ()
      end else begin
        for c = 0 to pred n_cflow do
          printf
            "  @[<2>logical, dimension(n_flv), parameter, private ::";
          printf "@ %s%04d = (/@ %s" abbrev (succ c) (option_to_logical table.(0).(c));
          for f = 1 to pred n_flv do
            printf ",@ %s" (option_to_logical table.(f).(c))
          done;
          printf "@ /)"; nl ()
        done;
        printf
          "  @[<2>logical, dimension(n_flv, n_cflow), parameter ::";
        printf "@ flv_col_is_allowed_old =@ reshape ( (/@ %s%04d" abbrev 1;
        for c = 1 to pred n_cflow do
          printf ",@ %s%04d" abbrev (succ c)
        done;
        printf "@ /),@ (/ n_flv, n_cflow /) )"; nl ()
      end

    let print_flavor_color_table n_flv n_cflow table =
      printf
        "  @[<2>logical, dimension(n_flv, n_cflow), save%s :: @ flv_col_is_allowed"
        protected; nl ();
      if n_flv > 0 then begin
        for c = 0 to pred n_cflow do
          printf
            "  @[<2>data flv_col_is_allowed(:,%4d) /" (succ c);
          printf "@ %s" (option_to_logical table.(0).(c));
          for f = 1 to pred n_flv do
            printf ",@ %s" (option_to_logical table.(f).(c))
          done;
          printf "@ /"; nl ()
        done;
      end

    let print_amplitude_table a =
      (* [print_flavor_color_table_old "a"
        (num_flavors a) (List.length (CF.color_flows a)) (CF.process_table a);
      nl ();] *)
      print_flavor_color_table
        (num_flavors a) (List.length (CF.color_flows a)) (CF.process_table a);
      nl ();
      printf
        "  @[<2>complex(kind=%s), dimension(n_flv, n_cflow, n_hel), save :: amp" !kind;
      nl ();
      nl ()

    let print_helicity_selection_table () =
      printf "  @[<2>logical, dimension(n_hel), save :: ";
      printf "hel_is_allowed = T"; nl();
      printf "  @[<2>real(kind=%s), dimension(n_hel), save :: " !kind;
      printf "hel_max_abs = 0"; nl ();
      printf "  @[<2>real(kind=%s), save :: " !kind;
      printf "hel_sum_abs = 0, ";
      printf "hel_threshold = 1E10"; nl ();
      printf "  @[<2>integer, save :: ";
      printf "hel_count = 0, ";
      printf "hel_cutoff = 100"; nl ();
      printf "  @[<2>integer :: ";
      printf "i"; nl ();
      printf "  @[<2>integer, save, dimension(n_hel) :: ";
      printf "hel_map = (/(i, i = 1, n_hel)/)"; nl ();
      printf "  @[<2>integer, save :: hel_finite = n_hel"; nl ();
      nl ()

(* \thocwmodulesubsection{Optional MD5 sum function} *)

    let print_md5sum_functions = function
      | Some s ->
          printf "  @[<5>"; if !fortran95 then printf "pure ";
          printf "function md5sum ()"; nl ();
          printf "    character(len=32) :: md5sum"; nl ();
          printf "    ! DON'T EVEN THINK of modifying the following line!"; nl ();
          printf "    md5sum = \"%s\"" s; nl (); 
          printf "  end function md5sum"; nl ();
          nl ()
      | None -> ()

(* \thocwmodulesubsection{Maintenance \&\ Inquiry Functions} *)

    let print_maintenance_functions amplitudes =
      if !whizard then begin
        printf "  subroutine init (par)"; nl ();
        printf "    real(kind=%s), dimension(*), intent(in) :: par" !kind; nl ();
        printf "    call import_from_whizard (par)"; nl ();
        printf "  end subroutine init"; nl ();
        nl ();
        printf "  subroutine final ()"; nl ();
        printf "  end subroutine final"; nl ();
        nl ();
        printf "  subroutine update_alpha_s (alpha_s)"; nl ();
        printf "    real(kind=%s), intent(in) :: alpha_s" !kind; nl ();
        printf "    call model_update_alpha_s (alpha_s)"; nl ();
        printf "  end subroutine update_alpha_s"; nl ();
        nl ()
      end

    let print_inquiry_function_openmp () = begin
      printf "  pure function openmp_supported () result (status)"; nl ();
      printf "    logical :: status"; nl ();
      printf "    status = %s" (if !openmp then ".true." else ".false."); nl ();
      printf "  end function openmp_supported"; nl ();
      nl ()
    end

    let print_inquiry_function_declarations name =
      printf "  @[<2>public :: number_%s,@ %s" name name;
      nl ()
      
    let print_numeric_inquiry_functions () = 
      printf "  @[<5>"; if !fortran95 then printf "pure ";
      printf "function number_particles_in () result (n)"; nl ();
      printf "    integer :: n"; nl ();
      printf "    n = n_in"; nl ();
      printf "  end function number_particles_in"; nl ();
      nl ();
      printf "  @[<5>"; if !fortran95 then printf "pure ";
      printf "function number_particles_out () result (n)"; nl ();
      printf "    integer :: n"; nl ();
      printf "    n = n_out"; nl ();
      printf "  end function number_particles_out"; nl ();
      nl ()

    let print_numeric_inquiry_functions (f, v) = 
      printf "  @[<5>"; if !fortran95 then printf "pure ";
      printf "function %s () result (n)" f; nl ();
      printf "    integer :: n"; nl ();
      printf "    n = %s" v; nl ();
      printf "  end function %s" f; nl ();
      nl ()

    let print_inquiry_functions name =
      printf "  @[<5>"; if !fortran95 then printf "pure ";
      printf "function number_%s () result (n)" name; nl ();
      printf "    integer :: n"; nl ();
      printf "    n = size (table_%s, dim=2)" name; nl ();
      printf "  end function number_%s" name; nl ();
      nl ();
      printf "  @[<5>"; if !fortran95 then printf "pure ";
      printf "subroutine %s (a)" name; nl ();
      printf "    integer, dimension(:,:), intent(out) :: a"; nl ();
      printf "    a = table_%s" name; nl ();
      printf "  end subroutine %s" name; nl ();
      nl ()

    let print_color_flows () =
      printf "  @[<5>"; if !fortran95 then printf "pure ";
      printf "function number_color_indices () result (n)"; nl ();
      printf "    integer :: n"; nl ();
      printf "    n = size (table_color_flows, dim=1)"; nl ();
      printf "  end function number_color_indices"; nl ();
      nl ();
      printf "  @[<5>"; if !fortran95 then printf "pure ";
      printf "function number_color_flows () result (n)"; nl ();
      printf "    integer :: n"; nl ();
      printf "    n = size (table_color_flows, dim=3)"; nl ();
      printf "  end function number_color_flows"; nl ();
      nl ();
      printf "  @[<5>"; if !fortran95 then printf "pure ";
      printf "subroutine color_flows (a, g)"; nl ();
      printf "    integer, dimension(:,:,:), intent(out) :: a"; nl ();
      printf "    logical, dimension(:,:), intent(out) :: g"; nl ();
      printf "    a = table_color_flows"; nl ();
      printf "    g = table_ghost_flags"; nl ();
      printf "  end subroutine color_flows"; nl ();
      nl ()

    let print_color_factors () =
      printf "  @[<5>"; if !fortran95 then printf "pure ";
      printf "function number_color_factors () result (n)"; nl ();
      printf "    integer :: n"; nl ();
      printf "    n = size (table_color_factors)"; nl ();
      printf "  end function number_color_factors"; nl ();
      nl ();
      printf "  @[<5>"; if !fortran95 then printf "pure ";
      printf "subroutine color_factors (cf)"; nl ();
      printf "    type(%s), dimension(:), intent(out) :: cf"
        omega_color_factor_abbrev; nl ();
      printf "    cf = table_color_factors"; nl ();
      printf "  end subroutine color_factors"; nl ();
      nl ();
      printf "  @[<5>"; if !fortran95 then printf "pure ";
      printf "function color_sum (flv, hel) result (amp2)"; nl ();
      printf "    integer, intent(in) :: flv, hel"; nl ();
      printf "    real(kind=%s) :: amp2" !kind; nl ();
      printf "    amp2 = real (omega_color_sum (flv, hel, amp, table_color_factors))"; nl ();
      printf "  end function color_sum"; nl ();
      nl ()

    let print_dispatch_functions () =
      printf "  @[<5>";
      printf "subroutine new_event (p)"; nl ();
      printf "    real(kind=%s), dimension(0:3,*), intent(in) :: p" !kind; nl ();
      printf "    logical :: mask_dirty"; nl ();
      printf "    integer :: hel"; nl ();
      printf "    call calculate_amplitudes (amp, p, hel_is_allowed)"; nl ();
      printf "    if ((hel_threshold .gt. 0) .and. (hel_count .le. hel_cutoff)) then"; nl ();
      printf "      call @[<3>omega_update_helicity_selection@ (hel_count,@ amp,@ ";
      printf "hel_max_abs,@ hel_sum_abs,@ hel_is_allowed,@ hel_threshold,@ hel_cutoff,@ mask_dirty)"; nl ();
      printf "      if (mask_dirty) then"; nl ();
      printf "        hel_finite = 0"; nl ();
      printf "        do hel = 1, n_hel"; nl ();
      printf "          if (hel_is_allowed(hel)) then"; nl ();
      printf "            hel_finite = hel_finite + 1"; nl ();
      printf "            hel_map(hel_finite) = hel"; nl ();
      printf "          end if"; nl ();
      printf "        end do"; nl ();
      printf "      end if"; nl ();
      printf "    end if"; nl();
      printf "  end subroutine new_event"; nl ();
      nl ();
      printf "  @[<5>";
      printf "subroutine reset_helicity_selection (threshold, cutoff)"; nl ();
      printf "    real(kind=%s), intent(in) :: threshold" !kind; nl ();
      printf "    integer, intent(in) :: cutoff"; nl ();
      printf "    integer :: i"; nl ();
      printf "    hel_is_allowed = T"; nl ();
      printf "    hel_max_abs = 0"; nl ();
      printf "    hel_sum_abs = 0"; nl ();
      printf "    hel_count = 0"; nl ();
      printf "    hel_threshold = threshold"; nl ();
      printf "    hel_cutoff = cutoff"; nl ();
      printf "    hel_map = (/(i, i = 1, n_hel)/)"; nl ();
      printf "    hel_finite = n_hel"; nl ();
      printf "  end subroutine reset_helicity_selection"; nl ();
      nl ();
      printf "  @[<5>"; if !fortran95 then printf "pure ";
      printf "function is_allowed (flv, hel, col) result (yorn)"; nl ();
      printf "    logical :: yorn"; nl ();
      printf "    integer, intent(in) :: flv, hel, col"; nl ();
      printf "    yorn = hel_is_allowed(hel) .and. ";
      printf "flv_col_is_allowed(flv,col)"; nl ();
      printf "  end function is_allowed"; nl ();
      nl ();
      printf "  @[<5>"; if !fortran95 then printf "pure ";
      printf "function get_amplitude (flv, hel, col) result (amp_result)"; nl ();
      printf "    complex(kind=%s) :: amp_result" !kind; nl ();
      printf "    integer, intent(in) :: flv, hel, col"; nl ();
      printf "    amp_result = amp(flv, col, hel)"; nl ();
      printf "  end function get_amplitude"; nl ();
      nl ()

(* \thocwmodulesubsection{Main Function} *)

    let format_power_of_nc
        { Color.Flow.num = num; Color.Flow.den = den; Color.Flow.power = pwr } =
      match num, den, pwr with
      | _, 0, _ -> invalid_arg "format_power_of_nc: zero denominator"
      | 0, _, _ -> ""
      | 1, 1, 0 | -1, -1, 0 -> "+ 1"
      | -1, 1, 0 | 1, -1, 0 -> "- 1"
      | 1, 1, 1 | -1, -1, 1 -> "+ N"
      | -1, 1, 1 | 1, -1, 1 -> "- N"
      | 1, 1, -1 | -1, -1, -1 -> "+ 1/N"
      | -1, 1, -1 | 1, -1, -1 -> "- 1/N"
      | 1, 1, p | -1, -1, p ->
          "+ " ^ (if p > 0 then "" else "1/") ^ "N^" ^ string_of_int (abs p)
      | -1, 1, p | 1, -1, p ->
          "- " ^ (if p > 0 then "" else "1/") ^ "N^" ^ string_of_int (abs p)
      | n, 1, 0 ->
          (if n < 0 then "- " else "+ ") ^ string_of_int (abs n)
      | n, d, 0 ->
          (if n * d < 0 then "- " else "+ ") ^
          string_of_int (abs n) ^ "/" ^ string_of_int (abs d)
      | n, 1, 1 ->
          (if n < 0 then "- " else "+ ") ^ string_of_int (abs n) ^ "N"
      | n, 1, -1 ->
          (if n < 0 then "- " else "+ ") ^ string_of_int (abs n) ^ "/N"
      | n, d, 1 ->
          (if n * d < 0 then "- " else "+ ") ^
          string_of_int (abs n) ^ "/" ^ string_of_int (abs d) ^ "N"
      | n, d, -1 ->
          (if n * d < 0 then "- " else "+ ") ^
          string_of_int (abs n) ^ "/" ^ string_of_int (abs d) ^ "/N"
      | n, 1, p ->
          (if n < 0 then "- " else "+ ") ^ string_of_int (abs n) ^
          (if p > 0 then "*" else "/") ^ "N^" ^ string_of_int (abs p)
      | n, d, p ->
          (if n * d < 0 then "- " else "+ ") ^ string_of_int (abs n) ^ "/" ^
          string_of_int (abs d) ^ (if p > 0 then "*" else "/") ^ "N^" ^ string_of_int (abs p)

    let format_powers_of_nc = function
      | [] -> "0"
      | powers -> String.concat " " (List.map format_power_of_nc powers)

    let print_description cmdline amplitudes () =
      printf "! File generated automatically by O'Mega"; nl();
      printf "!"; nl();
      printf "!   %s" cmdline; nl();
      printf "!"; nl();
      printf "! with all scattering amplitudes for the process(es)"; nl ();
      printf "!"; nl ();
      printf "!   flavor combinations:"; nl ();
      printf "!"; nl ();
      ThoList.iteri
        (fun i process ->
          printf "!     %3d: %s" i (process_sans_color_to_string process); nl ())
        1 (CF.flavors amplitudes);
      printf "!"; nl ();
      printf "!   color flows:"; nl ();
      printf "!"; nl ();
      ThoList.iteri
        (fun i cflow ->
          printf "!     %3d: %s" i (cflow_to_string cflow); nl ())
        1 (CF.color_flows amplitudes);
      printf "!"; nl ();
      printf "!     NB: i.g. not all color flows contribute to all flavor"; nl ();
      printf "!     combinations.  Consult the array FLV_COL_IS_ALLOWED"; nl ();
      printf "!     below for the allowed combinations."; nl ();
      printf "!"; nl ();
      printf "!   Color Factors:"; nl ();
      printf "!"; nl ();
      let cfactors = CF.color_factors amplitudes in
      for c1 = 0 to pred (Array.length cfactors) do
        for c2 = 0 to c1 do
          match cfactors.(c1).(c2) with
          | [] -> ()
          | cfactor ->
              printf "!     (%3d,%3d): %s"
                (succ c1) (succ c2) (format_powers_of_nc cfactor); nl ()
        done
      done;
      printf "!"; nl ();
      printf "!   vanishing or redundant flavor combinations:"; nl ();
      printf "!"; nl ();
      List.iter (fun process ->
        printf "!          %s" (process_sans_color_to_string process); nl ())
        (CF.vanishing_flavors amplitudes);
      printf "!"; nl ();
      begin
        match CF.constraints amplitudes with
        | None -> ()
        | Some s ->
            printf
              "!   diagram selection (MIGHT BREAK GAUGE INVARIANCE!!!):"; nl ();
            printf "!"; nl ();
            printf "!     %s" s; nl ();
            printf "!"; nl ()
      end;
      begin match RCS.description M.rcs with
      | line1 :: lines ->
          printf "! in %s" line1; nl ();
          List.iter (fun s -> printf "!    %s" s; nl ()) lines
      | [] -> printf "! in %s" (RCS.name M.rcs); nl ()
      end;
      printf "!"; nl ()

    let print_version () =
      printf "! O'Mega revision control information:"; nl ();
      List.iter (fun s -> printf "!    %s" s; nl ())
        (ThoList.flatmap RCS.summary (M.rcs :: rcs_list @ F.rcs_list))

(* \thocwmodulesubsection{Printing Modules} *)

    type accessibility =
      | Public
      | Private
      | Protected (* Fortran 2003 *)

    let accessibility_to_string = function
      | Public -> "public"
      | Private -> "private"
      | Protected -> "protected"

    type used_symbol =
      | As_Is of string
      | Aliased of string * string

    let print_used_symbol = function
      | As_Is name -> printf "%s" name
      | Aliased (orig, alias) -> printf "%s => %s" alias orig

    type used_module =
      | Full of string
      | Full_Aliased of string * (string * string) list
      | Subset of string * used_symbol list

    let print_used_module = function
      | Full name
      | Full_Aliased (name, [])
      | Subset (name, []) ->
          printf "  use %s" name;
          nl ()
      | Full_Aliased (name, aliases) ->
          printf "  @[<5>use %s" name;
          List.iter
            (fun (orig, alias) -> printf ", %s => %s" alias orig)
            aliases;
          nl ()
      | Subset (name, used_symbol :: used_symbols) ->
          printf "  @[<5>use %s, only: " name;
          print_used_symbol used_symbol;
          List.iter (fun s -> printf ", "; print_used_symbol s) used_symbols;
          nl ()
          
    type fortran_module =
        { module_name : string;
          default_accessibility : accessibility;
          used_modules : used_module list;
          public_symbols : string list;
          print_declarations : (unit -> unit) list;
          print_implementations : (unit -> unit) list }
          
    let print_public = function
      | name1 :: names ->
          printf "  @[<2>public :: %s" name1;
          List.iter (fun n -> printf ",@ %s" n) names; nl ()
      | [] -> ()

    let print_public_interface generic procedures =
      printf "  public :: %s" generic; nl ();
      begin match procedures with
      | name1 :: names ->
          printf "  interface %s" generic; nl ();
          printf "     @[<2>module procedure %s" name1;
          List.iter (fun n -> printf ",@ %s" n) names; nl ();
          printf "  end interface"; nl ();
          print_public procedures
      | [] -> ()
      end

    let print_module m =
      printf "module %s" m.module_name; nl ();
      List.iter print_used_module m.used_modules;
      printf "  implicit none"; nl ();
      printf "  %s" (accessibility_to_string m.default_accessibility); nl ();
      print_public m.public_symbols; nl ();
      begin match m.print_declarations with
      | [] -> ()
      | print_declarations ->
          List.iter (fun f -> f ()) print_declarations; nl ()
      end;
      begin match m.print_implementations with
      | [] -> ()
      | print_implementations ->
          printf "contains"; nl (); nl ();
          List.iter (fun f -> f ()) print_implementations; nl ();
      end;
      printf "end module %s" m.module_name; nl ()

    let print_modules modules =
      List.iter print_module modules;
      print_version ();
      print_flush () 

    let module_to_file line_length oc prelude m =
      output_string oc (m.module_name ^ "\n");
      let filename = m.module_name ^ ".f90" in
      let channel = open_out filename in
      setup_fortran_formatter line_length channel;
      prelude ();
      print_modules [m];
      close_out channel

    let modules_to_file line_length oc prelude = function
      | [] -> ()
      | m :: mlist ->
          module_to_file line_length oc prelude m;
          List.iter (module_to_file line_length oc (fun () -> ())) mlist

(* \thocwmodulesubsection{Chopping Up Amplitudes} *)

    let num_fusions_brakets size amplitudes =
      let num_fusions =
        max 1 size in
      let count_brakets =
        List.fold_left
          (fun sum process -> sum + List.length (F.brakets process))
          0 (CF.processes amplitudes)
      and count_processes = 
        List.length (CF.processes amplitudes) in
      if count_brakets > 0 then
        let num_brakets =
          max 1 ((num_fusions * count_processes) / count_brakets) in
        (num_fusions, num_brakets)
      else
        (num_fusions, 1)

    let chop_amplitudes size amplitudes =
      let num_fusions, num_brakets = num_fusions_brakets size amplitudes in
      (ThoList.enumerate 1 (ThoList.chopn num_fusions (CF.fusions amplitudes)),
       ThoList.enumerate 1 (ThoList.chopn num_brakets (CF.processes amplitudes)))

    let print_compute_fusions1 dictionary (n, fusions) =
      if !openmp then begin
        printf "  subroutine compute_fusions_%04d (%s)" n openmp_tld; nl ();
        printf "  @[<5>type(%s), intent(inout) :: %s" openmp_tld_type openmp_tld; nl ();
      end else begin
        printf "  @[<5>subroutine compute_fusions_%04d ()" n; nl ();
      end;
      print_fusions dictionary fusions;
      printf "  end subroutine compute_fusions_%04d" n; nl ()
        
    and print_compute_brakets1 dictionary (n, processes) =
      if !openmp then begin
        printf "  subroutine compute_brakets_%04d (%s)" n openmp_tld; nl ();
        printf "  @[<5>type(%s), intent(inout) :: %s" openmp_tld_type openmp_tld; nl ();
      end else begin
        printf "  @[<5>subroutine compute_brakets_%04d ()" n; nl ();
      end;
      List.iter (print_brakets dictionary) processes;
      printf "  end subroutine compute_brakets_%04d" n; nl ()
      
(* \thocwmodulesubsection{Common Stuff} *)

    let omega_public_symbols =
      ["number_particles_in"; "number_particles_out";
       "number_color_indices";
       "reset_helicity_selection"; "new_event";
       "is_allowed"; "get_amplitude"; "color_sum"; "openmp_supported"] @
      ThoList.flatmap
        (fun n -> ["number_" ^ n; n])
        ["spin_states"; "flavor_states"; "color_flows"; "color_factors"]
      
    let whizard_public_symbols md5sum =
      ["init"; "final"; "update_alpha_s"] @
      (match md5sum with Some _ -> ["md5sum"] | None -> [])

    let used_modules () =
      [Full "kinds";
       Full Fermions.use_module;
       Full_Aliased ("omega_color", ["omega_color_factor", omega_color_factor_abbrev])] @
      List.map
        (fun m -> Full m)
        (match !parameter_module with "" -> !use_modules | pm -> pm :: !use_modules)

    let public_symbols () =
      if !whizard then
        omega_public_symbols @ (whizard_public_symbols !md5sum)
      else
        omega_public_symbols

    let print_constants amplitudes =

      printf "  ! DON'T EVEN THINK of removing the following!"; nl ();
      printf "  ! If the compiler complains about undeclared"; nl ();
      printf "  ! or undefined variables, you are compiling"; nl ();
      printf "  ! against an incompatible omega95 module!"; nl ();
      printf "  @[<2>integer, dimension(%d), parameter, private :: "
        (List.length require_library);
      printf "require =@ (/ @[";
      print_list require_library;
      printf " /)"; nl(); nl ();
        
      (* Using these parameters makes sense for documentation, but in
         practice, there is no need to ever change them. *)
      List.iter
        (function name, value -> print_integer_parameter name (value amplitudes))
        [ ("n_prt", num_particles); 
          ("n_in", num_particles_in);
          ("n_out", num_particles_out);
          ("n_cflow", num_color_flows); (* Number of different color amplitudes. *)
          ("n_cindex", num_color_indices);  (* Maximum rank of color tensors. *)
          ("n_flv", num_flavors); (* Number of different flavor amplitudes. *)
          ("n_hel", num_helicities)  (* Number of different helicty amplitudes. *) ];
      nl ();

      (* Abbreviations.  *)
      printf "  ! NB: you MUST NOT change the value of %s here!!!" nc_parameter; nl();
      printf "  !     It is defined here for convenience only and must be"; nl ();
      printf "  !     compatible with hardcoded values in the amplitude!"; nl ();
      print_real_parameter nc_parameter (CM.nc ()); (* $N_C$ *)
      List.iter
        (function name, value -> print_logical_parameter name value)
        [ ("F", false); ("T", true) ]; nl ();

      print_spin_tables amplitudes;
      print_flavor_tables amplitudes;
      print_color_tables amplitudes;
      print_amplitude_table amplitudes;
      print_helicity_selection_table ()

    let print_interface amplitudes =
      print_md5sum_functions !md5sum;
      print_maintenance_functions amplitudes;
      List.iter print_numeric_inquiry_functions
        [("number_particles_in", "n_in");
         ("number_particles_out", "n_out")];
      List.iter print_inquiry_functions
        ["spin_states"; "flavor_states"];
      print_inquiry_function_openmp ();
      print_color_flows ();
      print_color_factors ();
      print_dispatch_functions ();
      nl();
      current_continuation_line := 0;
      if !km_write || !km_pure then (Targets_Kmatrix.Fortran.print !km_pure);
      current_continuation_line := 1;
      nl()

    let print_calculate_amplitudes declarations computations amplitudes =
      printf "  @[<5>subroutine calculate_amplitudes (amp, k, mask)"; nl ();
      printf "    complex(kind=%s), dimension(:,:,:), intent(out) :: amp" !kind; nl ();
      printf "    real(kind=%s), dimension(0:3,*), intent(in) :: k" !kind; nl ();
      printf "    logical, dimension(:), intent(in) :: mask"; nl ();
      printf "    integer, dimension(n_prt) :: s"; nl ();
      printf "    integer :: h, hi"; nl ();
      declarations ();
      begin match CF.processes amplitudes with
      | p :: _ -> print_external_momenta p
      |  _ -> ()
      end;
      ignore (List.fold_left print_momenta PSet.empty (CF.processes amplitudes));
      printf "    amp = 0"; nl ();
      if num_helicities amplitudes > 0 then begin
        printf "    if (hel_finite == 0) return"; nl ();
        if !openmp then begin
          printf "!$OMP PARALLEL DO DEFAULT(SHARED) PRIVATE(s, h, %s) SCHEDULE(STATIC)" openmp_tld; nl();
        end;
        printf "    do hi = 1, hel_finite"; nl ();
        printf "      h = hel_map(hi)"; nl ();
        printf "      s = table_spin_states(:,h)"; nl ();
        ignore (List.fold_left print_externals WFSet.empty (CF.processes amplitudes));
        computations ();
        List.iter print_fudge_factor (CF.processes amplitudes); 
        (* This sorting should slightly improve cache locality. *)
        let triple_snd = fun (_,  x, _) -> x
        in let triple_fst = fun (x, _, _) -> x
        in let rec builder1 flvi flowi flows = match flows with
          | (Some a) :: tl -> (flvi, flowi, flavors_symbol (flavors a)) :: (builder1 flvi (flowi + 1) tl)
          | None :: tl -> builder1 flvi (flowi + 1) tl
          | [] -> []
        in let rec builder2 flvi flvs = match flvs with
          | flv :: tl -> (builder1 flvi 1 flv) @ (builder2 (flvi + 1) tl)
          | [] -> []
        in let unsorted = builder2 1 (List.map Array.to_list (Array.to_list (CF.process_table amplitudes)))
        in let sorted = List.sort (fun a b -> 
            if (triple_snd a != triple_snd b) then triple_snd a - triple_snd b else (triple_fst a - triple_fst b))
          unsorted
        in List.iter (fun (flvi, flowi, flv) ->
          (printf "      amp(%d,%d,h) = %s" flvi flowi flv; nl ();)) sorted;

(*i     printf "     else"; nl ();
        printf "      amp(:,h,:) = 0"; nl (); i*)
        printf "    end do"; nl ();
        if !openmp then begin
          printf "!$OMP END PARALLEL DO"; nl ();
        end;
      end;
      printf "  end subroutine calculate_amplitudes"; nl ()

    let print_compute_chops chopped_fusions chopped_brakets () =
      List.iter
        (fun (i, _) -> printf "      call compute_fusions_%04d (%s)" i
           (if !openmp then openmp_tld else ""); nl ())
        chopped_fusions;
      List.iter
        (fun (i, _) -> printf "      call compute_brakets_%04d (%s)" i
           (if !openmp then openmp_tld else ""); nl ())
        chopped_brakets

(* \thocwmodulesubsection{Single Function} *)

    let amplitudes_to_channel_single_function cmdline oc amplitudes =

      let print_declarations () =
        print_constants amplitudes

      and print_implementations () =
        print_interface amplitudes;
        print_calculate_amplitudes
          (fun () -> print_variable_declarations amplitudes)
          (fun () ->
            print_fusions (CF.dictionary amplitudes) (CF.fusions amplitudes);
            List.iter
              (print_brakets (CF.dictionary amplitudes))
              (CF.processes amplitudes))
          amplitudes in

      let fortran_module = 
        { module_name = !module_name;
          used_modules = used_modules ();
          default_accessibility = Private;
          public_symbols = public_symbols ();
          print_declarations = [print_declarations];
          print_implementations = [print_implementations] } in

      setup_fortran_formatter !line_length oc;
      print_description cmdline amplitudes ();
      print_modules [fortran_module]

(* \thocwmodulesubsection{Single Module} *)

    let amplitudes_to_channel_single_module cmdline oc size amplitudes =

      let print_declarations () =
        print_constants amplitudes;
        print_variable_declarations amplitudes

      and print_implementations () =
        print_interface amplitudes in

      let chopped_fusions, chopped_brakets =
        chop_amplitudes size amplitudes in
      
      let dictionary = CF.dictionary amplitudes in

      let print_compute_amplitudes () =
        print_calculate_amplitudes
          (fun () -> ())
          (print_compute_chops chopped_fusions chopped_brakets)
          amplitudes

      and print_compute_fusions () =
        List.iter (print_compute_fusions1 dictionary) chopped_fusions

      and print_compute_brakets () =
        List.iter (print_compute_brakets1 dictionary) chopped_brakets in
        
      let fortran_module = 
        { module_name = !module_name;
          used_modules = used_modules ();
          default_accessibility = Private;
          public_symbols = public_symbols ();
          print_declarations = [print_declarations];
          print_implementations = [print_implementations;
                                   print_compute_amplitudes;
                                   print_compute_fusions;
                                   print_compute_brakets] } in

      setup_fortran_formatter !line_length oc;
      print_description cmdline amplitudes ();
      print_modules [fortran_module]

(* \thocwmodulesubsection{Multiple Modules} *)

    let modules_of_amplitudes cmdline oc size amplitudes =

      let name = !module_name in

      let print_declarations () =
        print_constants amplitudes
      and print_variables () =
        print_variable_declarations amplitudes in

      let constants_module = 
        { module_name = name ^ "_constants";
          used_modules = used_modules ();
          default_accessibility = Public;
          public_symbols = [];
          print_declarations = [print_declarations];
          print_implementations = [] } in

      let variables_module = 
        { module_name = name ^ "_variables";
          used_modules = used_modules ();
          default_accessibility = Public;
          public_symbols = [];
          print_declarations = [print_variables];
          print_implementations = [] } in

      let dictionary = CF.dictionary amplitudes in

      let print_compute_fusions (n, fusions) () =
        if !openmp then begin
          printf "  subroutine compute_fusions_%04d (%s)" n openmp_tld; nl ();
          printf "  @[<5>type(%s), intent(inout) :: %s" openmp_tld_type openmp_tld; nl ();
        end else begin
          printf "  @[<5>subroutine compute_fusions_%04d ()" n; nl ();
        end;
        print_fusions dictionary fusions;
        printf "  end subroutine compute_fusions_%04d" n; nl () in
        
      let print_compute_brakets (n, processes) () =
        if !openmp then begin
          printf "  subroutine compute_brakets_%04d (%s)" n openmp_tld; nl ();
          printf "  @[<5>type(%s), intent(inout) :: %s" openmp_tld_type openmp_tld; nl ();
        end else begin
          printf "  @[<5>subroutine compute_brakets_%04d ()" n; nl ();
        end;
        List.iter (print_brakets dictionary) processes;
        printf "  end subroutine compute_brakets_%04d" n; nl () in
        
      let fusions_module (n, _ as fusions) = 
        let tag = Printf.sprintf "_fusions_%04d" n in
        { module_name = name ^ tag;
          used_modules = (used_modules () @
                          [Full constants_module.module_name;
                           Full variables_module.module_name]);
          default_accessibility = Private;
          public_symbols = ["compute" ^ tag];
          print_declarations = [];
          print_implementations = [print_compute_fusions fusions] } in

      let brakets_module (n, _ as processes) = 
        let tag = Printf.sprintf "_brakets_%04d" n in
        { module_name = name ^ tag;
          used_modules = (used_modules () @
                          [Full constants_module.module_name;
                           Full variables_module.module_name]);
          default_accessibility = Private;
          public_symbols = ["compute" ^ tag];
          print_declarations = [];
          print_implementations = [print_compute_brakets processes] } in

      let chopped_fusions, chopped_brakets =
        chop_amplitudes size amplitudes in

      let fusions_modules = 
        List.map fusions_module chopped_fusions in

      let brakets_modules = 
        List.map brakets_module chopped_brakets in

      let print_implementations () =
        print_interface amplitudes;
        print_calculate_amplitudes
          (fun () -> ())
          (print_compute_chops chopped_fusions chopped_brakets)
          amplitudes in

      let public_module = 
        { module_name = name;
           used_modules = (used_modules () @
                           [Full constants_module.module_name;
                            Full variables_module.module_name ] @
                           List.map
                             (fun m -> Full m.module_name)
                             (fusions_modules @ brakets_modules));
          default_accessibility = Private;
          public_symbols = public_symbols ();
          print_declarations = [];
          print_implementations = [print_implementations] }
      and private_modules =
        [constants_module; variables_module] @ fusions_modules @ brakets_modules in

      (public_module, private_modules)

    let amplitudes_to_channel_single_file cmdline oc size amplitudes =
      let public_module, private_modules =
        modules_of_amplitudes cmdline oc size amplitudes in
      setup_fortran_formatter !line_length oc;
      print_description cmdline amplitudes ();
      print_modules (private_modules @ [public_module])

    let amplitudes_to_channel_multi_file cmdline oc size amplitudes =
      let public_module, private_modules =
        modules_of_amplitudes cmdline oc size amplitudes in
      modules_to_file !line_length oc
        (print_description cmdline amplitudes)
        (public_module :: private_modules)

(* \thocwmodulesubsection{Dispatch} *)

    let amplitudes_to_channel cmdline oc diagnostics amplitudes =
      parse_diagnostics diagnostics;
      match !output_mode with
      | Single_Function ->
          amplitudes_to_channel_single_function cmdline oc amplitudes
      | Single_Module size ->
          amplitudes_to_channel_single_module cmdline oc size amplitudes
      | Single_File size ->
          amplitudes_to_channel_single_file cmdline oc size amplitudes
      | Multi_File size ->
          amplitudes_to_channel_multi_file cmdline oc size amplitudes

    let parameters_to_channel oc =
      parameters_to_fortran oc (CM.parameters ())

  end

module Fortran = Make_Fortran(Fortran_Fermions)

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

module Fortran_Majorana_Fermions : Fermions =
  struct
    let rcs = RCS.rename rcs_file "Targets.Fortran_Majorana_Fermions()"
        [ "generates Fortran95 code for Dirac and Majorana fermions";
          " using revision 2003_03_A of module omega95_bispinors" ]

    open Coupling
    open Format

    let psi_type = "bispinor"
    let psibar_type = "bispinor"
    let chi_type = "bispinor"
    let grav_type = "vectorspinor"

(* \begin{JR}
   Because of our rules for fermions we are going to give all incoming fermions
   a [u] spinor and all outgoing fermions a [v] spinor, no matter whether they
   are Dirac fermions, antifermions or Majorana fermions.
   \end{JR} *)
        
    let psi_incoming = "u"
    let brs_psi_incoming = "brs_u"
    let psibar_incoming = "u"
    let brs_psibar_incoming = "brs_u"
    let chi_incoming = "u"
    let brs_chi_incoming = "brs_u"
    let grav_incoming = "ueps"

    let psi_outgoing = "v"
    let brs_psi_outgoing = "brs_v"    
    let psibar_outgoing = "v"
    let brs_psibar_outgoing = "brs_v"
    let chi_outgoing = "v"
    let brs_chi_outgoing = "brs_v"
    let grav_outgoing = "veps"

    let psi_propagator = "pr_psi"
    let psibar_propagator = "pr_psi"
    let chi_propagator = "pr_psi"
    let grav_propagator = "pr_grav"

    let psi_projector = "pj_psi"
    let psibar_projector = "pj_psi"
    let chi_projector = "pj_psi"
    let grav_projector = "pj_grav"

    let psi_gauss = "pg_psi"
    let psibar_gauss = "pg_psi"
    let chi_gauss = "pg_psi"
    let grav_gauss = "pg_grav"

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
          if (String.contains func_name ')') or 
	    (String.contains tail '(') or 
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
      | coeff, _, _, _ -> invalid_arg 
            "Targets.Fortran_Majorana_Fermions: Not needed in the models"

    let print_current_p = function
      | coeff, Psi, SL, Psi -> print_fermion_current coeff "sl"   
      | coeff, Psi, SR, Psi -> print_fermion_current coeff "sr"   
      | coeff, Psi, SLR, Psi -> print_fermion_current2 coeff "slr"       
      | coeff, _, _, _ -> invalid_arg 
            "Targets.Fortran_Majorana_Fermions: Not needed in the used models"

    let print_current_b = function
      | coeff, Psibar, SL, Psibar -> print_fermion_current coeff "sl"
      | coeff, Psibar, SR, Psibar -> print_fermion_current coeff "sr"
      | coeff, Psibar, SLR, Psibar -> print_fermion_current2 coeff "slr"
      | coeff, _, _, _  -> invalid_arg 
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

    let print_fermion_g_current_vector coeff f c wf1 wf2 p1 p2 p12 fusion =
      let c = format_coupling coeff c in
      match fusion with
      | F13 -> printf "%s_grf(%s,%s,%s)" f c wf1 wf2
      | F31 -> printf "%s_fgr(-%s,%s,%s)" f c wf1 wf2
      | F23 -> printf "gr_%sf(%s,%s,%s)" f c wf1 wf2
      | F32 -> printf "gr_%sf(%s,%s,%s)" f c wf2 wf1
      | F12 -> printf "f_%sgr(-%s,%s,%s)" f c wf2 wf1
      | F21 -> printf "f_%sgr(-%s,%s,%s)" f c wf1 wf2

    let print_fermion_g_current_vector_rev coeff f c wf1 wf2 p1 p2 p12 fusion =
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
      | coeff, _, _, _ -> invalid_arg
          "Targets.Fortran_Majorana_Fermions: not used in the models"

    let print_current_mom = function
      | coeff, _, _, _ -> invalid_arg
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

    let print_fermion_g4_svlr_current coeff f c wf1 wf2 wf3 fusion = 
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
      | coeff, Gravbar, _, _ -> invalid_arg "print_current_g4: not implemented"
      | coeff, _, S2, Grav -> print_fermion_g4_current_rev coeff "s2" 
      | coeff, _, SV, Grav -> print_fermion_g4_vector_current_rev (-coeff) "sv"
      | coeff, _, SLV, Grav -> print_fermion_g4_vector_current_rev (-coeff) "slv"
      | coeff, _, SRV, Grav -> print_fermion_g4_vector_current_rev (-coeff) "srv"
      | coeff, _, SLRV, Grav -> print_fermion_2_g4_vector_current_rev coeff "slrv"
      | coeff, _, PV, Grav -> print_fermion_g4_vector_current_rev coeff "pv" 
      | coeff, _, V2, Grav -> print_fermion_g4_vector_current_rev coeff "v2" 
      | coeff, _, V2LR, Grav -> print_fermion_2_g4_current_rev coeff "v2lr"
      | coeff, _, _, Grav -> invalid_arg "print_current_g4: not implemented"
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
      | coeff, _, V2LR, _ -> invalid_arg "Targets.print_current: not available"

    let reverse_braket _ = false

    let use_module = "omega95_bispinors"
    let require_library =
      ["omega_bispinors_2010_01_A"; "omega_bispinor_cpls_2010_01_A"]
   end

module Fortran_Majorana = Make_Fortran(Fortran_Majorana_Fermions)

(* \thocwmodulesubsection{\texttt{FORTRAN\,77}} *)

module Fortran77 = Dummy

(* \thocwmodulesection{O'Mega Virtual Machine} *)

module VM = Dummy

(* \thocwmodulesection{\texttt{C}} *)

module C = Dummy

(* \thocwmodulesubsection{\texttt{C++}} *)

module Cpp = Dummy

(* \thocwmodulesubsection{Java} *)

module Java = Dummy

(* \thocwmodulesection{O'Caml} *)

module Ocaml = Dummy

(* \thocwmodulesection{\LaTeX} *)

module LaTeX = Dummy

(*i
module VM_old (F : Fusion.T) (Make_MF : Fusion.MultiMaker)
    (M : Model.T with type flavor = F.flavor and type constant = F.constant) =
  struct
    let rcs_list =
      [ RCS.rename rcs_file "Targets.VM()"
          [ "Bytecode for the O'Mega Virtual Machine" ] ]

    module MF = Make_MF(F)
    type amplitude = F.amplitude
    type amplitudes = MF.amplitudes
    type diagnostic = All | Arguments | Momenta | Gauge
    let options = Options.empty

    let flavors_to_string flavors =
      String.concat " " (List.map M.flavor_to_string flavors)

    let format_process amplitude =
      flavors_to_string (F.incoming amplitude) ^ " -> " ^
      flavors_to_string (F.outgoing amplitude)

    open Format
    open Coupling

    let ovm_LOAD_SCALAR = 1
    let ovm_LOAD_U = 2
    let ovm_LOAD_UBAR = 3
    let ovm_LOAD_V = 4
    let ovm_LOAD_VBAR = 5
    let ovm_LOAD_VECTOR = 6

    let ovm_ADD_MOMENTA = 10

    let ovm_PROPAGATE_SCALAR = 11
    let ovm_PROPAGATE_SPINOR = 12
    let ovm_PROPAGATE_CONJSPINOR = 13
    let ovm_PROPAGATE_UNITARITY = 14
    let ovm_PROPAGATE_FEYNMAN = 15
    let ovm_PROPAGATE_TENSOR2 = 16

    let ovm_FUSE_VECTOR_PSIBAR_PSI = 21
    let ovm_FUSE_PSI_VECTOR_PSI = 22
    let ovm_FUSE_PSIBAR_PSIBAR_VECTOR = 23

    type instruction = 
        { code : int; sign : int; coupl : int;
          lhs : int; rhs1 : int; rhs2 : int }

    let printi i =
      printf "@\n%3d %3d %3d %3d %3d %3d"
        i.code i.sign i.coupl i.lhs i.rhs1 i.rhs2

    let load lhs f rhs =
      let code =
        match M.lorentz f with
        | Scalar -> ovm_LOAD_SCALAR
        | Spinor -> ovm_LOAD_U
        | ConjSpinor -> ovm_LOAD_UBAR
        | Majorana -> failwith "load: Majoranas not implemented yet"
        | Maj_Ghost -> failwith "load: SUSY ghosts not implemented yet"
        | Vector | Massive_Vector -> ovm_LOAD_VECTOR
        | Vectorspinor -> invalid_arg "external spin must be <=1"
        | Tensor_1 -> invalid_arg "Tensor_1 only internal"
        | Tensor_2 -> invalid_arg "external spin must be <= 1"
        | BRS _ -> invalid_arg "no BRST"
 in
      { code = code; sign = 0; coupl = M.pdg f;
        lhs = lhs; rhs1 = rhs; rhs2 = rhs }
      
    let print_external count flavor =
      printi (load count (F.flavor flavor) count);
      succ count

    let print_externals amplitude =
      printf "@\n@[<2>BEGIN EXTERNALS";
      ignore (List.fold_left print_external 1 (F.externals amplitude));
      printf "@]@\nEND EXTERNALS"

    let print_current rhs =
      match F.coupling rhs with
      | V3 (vertex, fusion, constant) -> printf "@\nV3"
      | V4 (vertex, fusion, constant) -> printf "@\nV4"
      | Vn (_, _, _) -> printf "@\nVn"

    let p2s p =
      if p >= 0 && p <= 9 then
        string_of_int p
      else if p <= 36 then
        String.make 1 (Char.chr (Char.code 'A' + p - 10))
      else
        "_"

    let format_p wf =
      String.concat "" (List.map p2s (F.momentum_list wf))

    let print_fusion fusion =
      let lhs = F.lhs fusion in
      let f = F.flavor lhs in
      (*i let momentum = format_p lhs in i*)
      List.iter print_current (F.rhs fusion);
      let propagate code =
        printi { code = code; sign = 0; coupl = 0;
                 lhs = int_of_string (format_p lhs);
                 rhs1 = abs (M.pdg f); rhs2 = abs (M.pdg f) } in
      match M.propagator f with
      | Prop_Scalar -> propagate ovm_PROPAGATE_SCALAR
      | Prop_Col_Scalar -> 
          failwith "print_fusion: Prop_Col_Scalar not implemented yet!" 
      | Prop_Ghost -> 
          failwith "print_fusion: Prop_Ghost not implemented yet!" 
      | Prop_Spinor -> propagate ovm_PROPAGATE_SPINOR
      | Prop_ConjSpinor -> propagate ovm_PROPAGATE_CONJSPINOR
      | Prop_Majorana | Prop_Col_Majorana ->
          failwith "print_fusion: Prop_Majorana not implemented yet!"
      | Prop_Unitarity -> propagate ovm_PROPAGATE_UNITARITY
      | Prop_Col_Unitarity -> 
          failwith "print_fusion: Prop_Col_Unitarity not implemented yet!" 
      | Prop_Feynman -> propagate ovm_PROPAGATE_FEYNMAN
      | Prop_Col_Feynman -> 
          failwith "print_fusion: Prop_Col_Feynman not implemented yet!"
      | Prop_Gauge xi ->
          failwith "print_fusion: Prop_Gauge not implemented yet!"
      | Prop_Rxi xi ->
          failwith "print_fusion: Prop_Rxi not implemented yet!"
      | Prop_Vectorspinor -> 
          failwith "print_fusion: Prop_Vectorspinor not implemented yet!"
      | Prop_Tensor_2 -> propagate ovm_PROPAGATE_TENSOR2
      | Aux_Scalar | Aux_Spinor | Aux_ConjSpinor | Aux_Majorana
      | Aux_Vector | Aux_Tensor_1 -> ()
      | Only_Insertion -> ()

    module P = Set.Make (struct type t = int list let compare = compare end)

    let rec add_momenta lhs = function
      | [] | [_] -> invalid_arg "add_momenta"
      | [rhs1; rhs2] ->
          printi { code = ovm_ADD_MOMENTA; sign = 0; coupl = 0;
                   lhs = int_of_string (format_p lhs);
                   rhs1 = int_of_string (format_p rhs1);
                   rhs2 = int_of_string (format_p rhs2) }
      | rhs1 :: rhs ->
          add_momenta lhs rhs;
          add_momenta lhs [lhs; rhs1]

    let print_fusions amplitude =
      printf "@\n@[<2>BEGIN FUSIONS";
      let momenta =
        List.fold_left (fun seen f ->
          let wf = F.lhs f in
          let p = F.momentum_list wf in
          let momentum = format_p wf in
          if not (P.mem p seen) then
            add_momenta wf (F.children (List.hd (F.rhs f)));
          print_fusion f;
          P.add p seen) P.empty (F.fusions amplitude)
      in
      printf "@]@\nEND FUSIONS"

    let print_brakets amplitude =
      printf "@\n@[<2>BEGIN BRAKETS";
      printf "@\n!!! not implemented yet !!!";
      printf "@]@\nEND BRAKETS"

    let print_fudge_factor amplitude = 
      printf "@\n@[<2>BEGIN FUDGE";
      printf "@\n!!! not implemented yet !!!";
      printf "@]@\nEND FUDGE"

    let amplitude_to_channel oc diagnostics amplitude =
      set_formatter_out_channel oc;
      printf "@\n@[<2>BEGIN AMPLITUDE %s" (format_process amplitude);
      print_externals amplitude;
      print_fusions amplitude;
      print_brakets amplitude;
      print_fudge_factor amplitude;
      printf "@]@\nEND AMPLITUDE"

    let amplitudes_to_channel oc diagnostics amplitudes =
      List.iter (amplitude_to_channel oc diagnostics) (MF.allowed amplitudes)

    let parameters_to_channel oc =
      set_formatter_out_channel oc;
      (*i let params = M.parameters () in i*)
      printf "@[<2>BEGIN PARAMETERS@\n";
      printf "!!! not implemented yet !!!@]@\n";
      printf "END PARAMETERS@\n"

  end

i*)

(*i
 *  Local Variables:
 *  mode:caml
 *  indent-tabs-mode:nil
 *  page-delimiter:"^(\\* .*\n"
 *  End:
i*)
