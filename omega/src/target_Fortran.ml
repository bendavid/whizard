(* target_Fortran.ml --

   Copyright (C) 1999-2023 by

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

module Make_Fortran (Names : Target_Fortran_Names.T)
    (Vintage_Fermions : Targets_vintage.Fermion_Maker)
    (Fusion_Maker : Fusion.Maker) (P : Momentum.T) (M : Model.T) =
  struct

    let require_library =
      Names.require_library @
      [ "omega_vectors_2010_01_A"; "omega_polarizations_2010_01_A";
        "omega_couplings_2010_01_A"; "omega_color_2010_01_A";
        "omega_utils_2010_01_A" ]

    module Fermions = Vintage_Fermions(Names)

    module CM = Colorize.It(M)
    module SCM = Orders.Slice(Colorize.It(M))
    module F = Fusion_Maker(P)(M)

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
    let amp_triv = ref false
    let parameter_module = ref ""
    let md5sum = ref None
    let no_write = ref false
    let km_write = ref false
    let km_pure = ref false
    let km_2_write = ref false
    let km_2_pure = ref false
    let openmp = ref false
    let pure_unless_openmp = false

    let options = Options.create
      [ "90", Arg.Clear fortran95, " use only Fortran90 features";
        "kind", Arg.String (fun s -> kind := s),
        "kind real and complex kind (default: '" ^ !kind ^ "')";
        "width", Arg.Int (fun w -> line_length := w), "n maximum line length";
        "continuation", Arg.Int (fun l -> continuation_lines := l),
        "n maximum # of continuation lines";
        "module", Arg.String (fun s -> module_name := s), "name module name";
        "single_function", Arg.Unit (fun () -> output_mode := Single_Function),
        " compute the matrix element in one function";
        "split_function", Arg.Int (fun n -> output_mode := Single_Module n),
        "size split the matrix element into small functions";
        "split_module", Arg.Int (fun n -> output_mode := Single_File n),
        "size split the matrix element into small modules";
        "split_file", Arg.Int (fun n -> output_mode := Multi_File n),
        "size split the matrix element into small files";
        "use", Arg.String (fun s -> use_modules := s :: !use_modules),
        "name use module";
        "parameter_module", Arg.String (fun s -> parameter_module := s),
        "name parameter_module";
        "md5sum", Arg.String (fun s -> md5sum := Some s),
        "sum transfer MD5 checksum";
        "whizard", Arg.Set whizard, " include WHIZARD interface";
        "amp_triv", Arg.Set amp_triv, " only print trivial amplitude";
        "no_write", Arg.Set no_write, " no 'write' statements";
        "kmatrix_write", Arg.Set km_2_write, " write K matrix functions";
        "kmatrix_2_write", Arg.Set km_write, " write K matrix 2 functions";
        "kmatrix_write_pure", Arg.Set km_pure, " write K matrix pure functions";
        "kmatrix_2_write_pure", Arg.Set km_2_pure, " write Kmatrix2pure functions";
        "openmp", Arg.Set openmp, " activate OpenMP support in generated code"]

(* Fortran style line continuation: *)
    let nl = Format_Fortran.newline

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

    let flavors_symbol ?(decl = false) ?orders flavors =
      let flavors_all_orders = List.map SCM.flavor_all_orders flavors in
      let orders_tag =
        match orders with
        | None -> ""
        | Some orders -> SCM.orders_symbol orders in
      (if !openmp && not decl then openmp_tld ^ "%" else "" ) ^
      "oks_" ^ String.concat "_" (List.map CM.flavor_symbol flavors_all_orders) ^ orders_tag

    let p2s p =
      if p >= 0 && p <= 9 then
        string_of_int p
      else if p <= 36 then
        String.make 1 (Char.chr (Char.code 'A' + p - 10))
      else
        "_"

    (* \begin{dubious}
         There many similar functions for formatting momenta.
         This is grown historically and should be cleaned up!
       \end{dubious} *)

    (* Prefix with a ["p"] to make a variable name holding a four momentum. *)
    let format_momentum : int list -> string =
      fun p ->
      "p" ^ String.concat "" (List.map p2s p)

    (* No prefix, to be used as part of a variable name holding a wavefunction. *)
    let format_p : F.wf -> string =
      fun wf ->
      String.concat "" (List.map p2s (F.momentum_list wf))

    let ext_momentum wf =
      match F.momentum_list wf with
      | [n] -> n
      | _ -> invalid_arg "Targets.Fortran.ext_momentum"

    module PSet = Set.Make (struct type t = int list let compare = compare end)
    module WFSet = Set.Make (struct type t = F.wf let compare = compare end)

    let variable ?(decl = false) wf =
      (if !openmp && not decl then openmp_tld ^ "%" else "")
      ^ "owf_" ^ SCM.flavor_symbol (F.flavor wf) ^ "_p" ^ format_p wf

    let momentum wf = "p" ^ format_p wf
    let spin wf = "s(" ^ string_of_int (ext_momentum wf) ^ ")"

    let format_multiple_variable ?(decl = false) wf i =
      variable ~decl wf ^ "_X" ^ string_of_int i

    let multiple_variable ?(decl = false) amplitude dictionary wf =
      try
        format_multiple_variable ~decl wf (dictionary amplitude wf)
      with
      | Not_found -> variable wf

    let multiple_variables ?(decl = false) multiplicity wf =
      try
        List.map
          (format_multiple_variable ~decl wf)
          (ThoList.range 1 (multiplicity wf))
      with
      | Not_found -> [variable ~decl wf]

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
            (match SCM.lorentz (F.flavor wf) with
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

    let schisma = ThoList.chopn

    let schisma_num i n l =
      ThoList.enumerate i (schisma n l)

    let declare_parameters' t = function
      | [] -> ()
      | plist ->
          printf "  @[<2>%s(kind=%s), public, save :: " t !kind;
          print_list (List.map SCM.constant_symbol plist); nl ()

    let declare_parameters t plist =
      List.iter (declare_parameters' t) plist

    let declare_parameter_array t (p, n) =
      printf "  @[<2>%s(kind=%s), dimension(%d), public, save :: %s"
        t !kind n (SCM.constant_symbol p); nl ()

    (* NB: we use [string_of_float] to make sure that a decimal
       point is included to make Fortran compilers happy. *)
    let default_parameter (x, v) =
      printf "@ %s = %s_%s" (SCM.constant_symbol x) (string_of_float v) !kind

    let declare_default_parameters t = function
      | [] -> ()
      | p :: plist ->
          printf "  @[<2>%s(kind=%s), public, save ::" t !kind;
          default_parameter p;
          List.iter (fun p' -> printf ","; default_parameter p') plist;
          nl ()

    let format_constant = function
      | I -> "(0,1)"
      | Integer c ->
         if c < 0 then
           sprintf "(%d.0_%s)" c !kind
         else
           sprintf "%d.0_%s" c !kind
      | Float x ->
         if x < 0. then
           "(" ^ string_of_float x ^ "_" ^ !kind ^ ")"
         else
           string_of_float x ^ "_" ^ !kind
      | _ -> invalid_arg "format_constant"

    let rec eval_parameter' = function
      | (I | Integer _ | Float _) as c ->
         printf "%s" (format_constant c)
      | Atom x -> printf "%s" (SCM.constant_symbol x)
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
         printf "@,("; eval_parameter' x; 
         if n < 0 then
           printf "**(%d)" n
         else
           printf "**%d" n;
         printf ")"
      | PowX (x, y) ->
          printf "@,("; eval_parameter' x;
           printf "**"; eval_parameter' y; printf ")"
      | Sqrt x -> printf "@,sqrt ("; eval_parameter' x; printf ")"
      | Sin x -> printf "@,sin ("; eval_parameter' x; printf ")"
      | Cos x -> printf "@,cos ("; eval_parameter' x; printf ")"
      | Tan x -> printf "@,tan ("; eval_parameter' x; printf ")"
      | Cot x -> printf "@,cot ("; eval_parameter' x; printf ")"
      | Asin x -> printf "@,asin ("; eval_parameter' x; printf ")"
      | Acos x -> printf "@,acos ("; eval_parameter' x; printf ")"
      | Atan x -> printf "@,atan ("; eval_parameter' x; printf ")"
      | Atan2 (y, x) -> printf "@,atan2 ("; eval_parameter' y;
          printf ",@ "; eval_parameter' x; printf ")"
      | Sinh x -> printf "@,sinh ("; eval_parameter' x; printf ")"
      | Cosh x -> printf "@,cosh ("; eval_parameter' x; printf ")"
      | Tanh x -> printf "@,tanh ("; eval_parameter' x; printf ")"
      | Exp x -> printf "@,exp ("; eval_parameter' x; printf ")"
      | Log x -> printf "@,log ("; eval_parameter' x; printf ")"
      | Log10 x -> printf "@,log10 ("; eval_parameter' x; printf ")"
      | Conj (Integer _ | Float _ as x) -> eval_parameter' x
      | Conj x -> printf "@,cconjg ("; eval_parameter' x; printf ")"
      | Abs x -> printf "@,abs ("; eval_parameter' x; printf ")"

    let strip_single_tag = function
      | Real x -> x
      | Complex x -> x

    let strip_array_tag = function
      | Real_Array x -> x
      | Complex_Array x -> x

    let eval_parameter (lhs, rhs) =
      let x = SCM.constant_symbol (strip_single_tag lhs) in
      printf "    @[<2>%s = " x; eval_parameter' rhs; nl ()

    let eval_para_list n l =
      printf "  subroutine setup_parameters_%03d ()" n; nl ();
      List.iter eval_parameter l;
      printf "  end subroutine setup_parameters_%03d" n; nl ()

    let eval_parameter_pair (lhs, rhs) =
      let x = SCM.constant_symbol (strip_array_tag lhs) in
      let _ = List.fold_left (fun i rhs' ->
        printf "    @[<2>%s(%d) = " x i; eval_parameter' rhs'; nl ();
        succ i) 1 rhs in
      ()

    let eval_para_pair_list n l =
      printf "  subroutine setup_parameters_%03d ()" n; nl ();
      List.iter eval_parameter_pair l;
      printf "  end subroutine setup_parameters_%03d" n; nl ()

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

    let contains params couplings =
      List.exists
        (fun (name, _) -> List.mem (SCM.constant_symbol name) params)
        couplings.input

    let rec depends_on params = function
      | I | Integer _ | Float _ -> false
      | Atom name -> List.mem (SCM.constant_symbol name) params
      | Sum es | Prod es ->
         List.exists (depends_on params) es
      | Diff (e1, e2) | Quot (e1, e2) | PowX (e1, e2) ->
         depends_on params e1 || depends_on params e2
      | Neg e | Rec e | Pow (e, _) ->
         depends_on params e
      | Sqrt e | Exp e | Log e | Log10 e
      | Sin e | Cos e | Tan e | Cot e
      | Asin e | Acos e | Atan e
      | Sinh e | Cosh e | Tanh e
      | Conj e | Abs e ->
         depends_on params e
      | Atan2 (e1, e2) ->
         depends_on params e1 || depends_on params e2

    let dependencies params couplings =
      if contains params couplings then
        List.rev
          (fst (List.fold_left
                  (fun (deps, plist) (param, v) ->
                    match param with
                    | Real name | Complex name ->
                       if depends_on plist v then
                         ((param, v) :: deps, CM.constant_symbol name :: plist)
                       else
                         (deps, plist))
                  ([], params) couplings.derived))
      else
        []

    let dependencies_arrays params couplings =
      if contains params couplings then
        List.rev
          (fst (List.fold_left
                  (fun (deps, plist) (param, vlist) ->
                    match param with
                    | Real_Array name | Complex_Array name ->
                       if List.exists (depends_on plist) vlist then
                         ((param, vlist) :: deps,
                          CM.constant_symbol name :: plist)
                       else
                         (deps, plist))
                  ([], params) couplings.derived_arrays))
      else
        []

    let parameters_to_fortran oc params =
      Format_Fortran.set_formatter_out_channel ~width:!line_length oc;
      let declarations = classify_parameters params in
      printf "module %s" !parameter_module; nl ();
      printf "  use kinds"; nl ();
      printf "  use constants"; nl ();
      printf "  implicit none"; nl ();
      printf "  private"; nl ();
      printf "  @[<2>public :: setup_parameters";
      printf ",@ import_from_whizard";
      printf ",@ model_update_alpha_s";
      if !no_write then begin
        printf "! No print_parameters";
      end else begin
        printf ",@ print_parameters";
      end; nl ();
      declare_default_parameters "real" params.input;
      declare_parameters "real" (schisma 69 declarations.real_singles);
      List.iter (declare_parameter_array "real") declarations.real_arrays;
      declare_parameters "complex" (schisma 69 declarations.complex_singles);
      List.iter (declare_parameter_array "complex") declarations.complex_arrays;
      printf "  interface cconjg"; nl ();
      printf "    module procedure cconjg_real, cconjg_complex"; nl ();
      printf "  end interface"; nl ();
      printf "  private :: cconjg_real, cconjg_complex"; nl ();
      printf "contains"; nl ();
      printf "  function cconjg_real (x) result (xc)"; nl ();
      printf "    real(kind=default), intent(in) :: x"; nl ();
      printf "    real(kind=default) :: xc"; nl ();
      printf "    xc = x"; nl ();
      printf "  end function cconjg_real"; nl ();
      printf "  function cconjg_complex (z) result (zc)"; nl ();
      printf "    complex(kind=default), intent(in) :: z"; nl ();
      printf "    complex(kind=default) :: zc"; nl ();
      printf "    zc = conjg (z)"; nl ();
      printf "  end function cconjg_complex"; nl ();
      printf "  ! derived parameters:"; nl ();
      let shredded = schisma_num 1 120 params.derived in
      let shredded_arrays = schisma_num 1 120 params.derived_arrays in
      let num_sub = List.length shredded in
      let num_sub_arrays = List.length shredded_arrays in
      List.iter (fun (i,l) -> eval_para_list i l) shredded;
      List.iter (fun (i,l) -> eval_para_pair_list (num_sub + i) l)
        shredded_arrays;
      printf "  subroutine setup_parameters ()"; nl ();
      for i = 1 to num_sub + num_sub_arrays do
        printf "    call setup_parameters_%03d ()" i; nl ();
      done;
      printf "  end subroutine setup_parameters"; nl ();
      printf "  subroutine import_from_whizard (par_array, scheme)"; nl ();
      printf
        "    real(%s), dimension(%d), intent(in) :: par_array"
        !kind (List.length params.input); nl ();
      printf "    integer, intent(in) :: scheme"; nl ();
      let i = ref 1 in
      List.iter
        (fun (p, _) ->
          printf "    %s = par_array(%d)" (SCM.constant_symbol p) !i; nl ();
          incr i)
        params.input;
      printf "    call setup_parameters ()"; nl ();
      printf "  end subroutine import_from_whizard"; nl ();
      printf "  subroutine model_update_alpha_s (alpha_s)"; nl ();
      printf "    real(%s), intent(in) :: alpha_s" !kind; nl ();
      begin match (dependencies ["aS"] params,
                   dependencies_arrays ["aS"] params) with
      | [], [] ->
         printf "    ! 'aS' not among the input parameters"; nl ();
      | deps, deps_arrays ->
         printf "    aS = alpha_s"; nl ();
         List.iter eval_parameter deps;
         List.iter eval_parameter_pair deps_arrays
      end;
      printf "  end subroutine model_update_alpha_s"; nl ();
      if !no_write then begin
        printf "! No print_parameters"; nl ();
      end else begin
        printf "  subroutine print_parameters ()"; nl ();
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
        printf "  end subroutine print_parameters"; nl ();
      end;
      printf "end module %s" !parameter_module; nl ()

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
      declare_list multiplicity ("type(" ^ Names.psi_type ^ ")")
        (wfs'.spinors @ wfs'.brs_spinors);
      declare_list multiplicity ("type(" ^ Names.psibar_type ^ ")")
        (wfs'.conjspinors @ wfs'.brs_conjspinors);
      declare_list multiplicity ("type(" ^ Names.chi_type ^ ")")
        (wfs'.realspinors @ wfs'.brs_realspinors @ wfs'.ghostspinors);
      declare_list multiplicity ("type(" ^ Names.grav_type ^ ")") wfs'.vectorspinors;
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
      if not !amp_triv then begin
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
	end;
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

    let print_current amplitude dictionary rhs =
      let module Vintage = Targets_vintage.Make_Fortran(Names)(Vintage_Fermions)(Fusion_Maker)(P)(M) in
      match F.coupling rhs with
      | V3 (vertex, fusion, constant) ->
         Vintage.print_current_V3 multiple_variable momentum amplitude dictionary rhs vertex fusion constant
      | V4 (vertex, fusion, constant) ->
         Vintage.print_current_V4 multiple_variable momentum amplitude dictionary rhs vertex fusion constant

      (* \begin{dubious}
           This reproduces the hack on page~\pageref{hack:sign(V4)}
           and gives the correct results up to quartic vertices.
           Make sure that it is also correct in light
           of~\eqref{eq:factors-of-i}, i.\,e.
           \begin{equation*}
             \ii T = \ii^{\#\text{vertices}}\ii^{\#\text{propagators}} \cdots
                   = \ii^{n-2}\ii^{n-3} \cdots
                   = -\ii(-1)^n \cdots
           \end{equation*}
         \end{dubious} *)
      | Vn (UFO (c, v, s, fl, color), fusion, constant) ->
         if Birdtracks.is_unit color then
           let g = CM.constant_symbol constant
           and chn = F.children rhs in
           let wfs = List.map (multiple_variable amplitude dictionary) chn
           and ps = List.map momentum chn in
           let n = List.length fusion in
           let eps = if n mod 2 = 0 then -1 else 1 in
           printf "@, %s " (if (eps * F.sign rhs) < 0 then "-" else "+");
           UFO.Targets.Fortran.fuse c v s fl g wfs ps fusion
         else
           failwith "print_current: nontrivial color structure"

    let print_propagator f p m gamma =
      let minus_third = "(-1.0_" ^ !kind ^ "/3.0_" ^ !kind ^ ")" in
      let w =
        begin match SCM.width f with
          | Vanishing | Fudged -> "0.0_" ^ !kind
          | Constant | Complex_Mass -> gamma
          | Timelike -> "wd_tl(" ^ p ^ "," ^ gamma ^ ")"
          | Running -> "wd_run(" ^ p ^ "," ^ m ^ "," ^ gamma ^ ")"
          | Custom f -> f ^ "(" ^ p ^ "," ^ gamma ^ ")"
        end in
      let cms =
	begin match SCM.width f with
	  | Complex_Mass -> ".true."
	  | _ -> ".false."
	end in
      match SCM.propagator f with
	| Prop_Scalar ->
          printf "pr_phi(%s,%s,%s," p m w
	| Prop_Col_Scalar ->
          printf "%s * pr_phi(%s,%s,%s," minus_third p m w
	| Prop_Ghost -> printf "(0,1) * pr_phi(%s, %s, %s," p m w
	| Prop_Spinor ->
          printf "%s(%s,%s,%s,%s," Names.psi_propagator p m w cms
	| Prop_ConjSpinor ->
          printf "%s(%s,%s,%s,%s," Names.psibar_propagator p m w cms
	| Prop_Majorana ->
          printf "%s(%s,%s,%s,%s," Names.chi_propagator p m w cms
	| Prop_Col_Majorana ->
          printf "%s * %s(%s,%s,%s,%s," minus_third Names.chi_propagator p m w cms
	| Prop_Unitarity ->
          printf "pr_unitarity(%s,%s,%s,%s," p m w cms
	| Prop_Col_Unitarity ->
          printf "%s * pr_unitarity(%s,%s,%s,%s," minus_third p m w cms
	| Prop_Feynman ->
          printf "pr_feynman(%s," p
	| Prop_Col_Feynman ->
          printf "%s * pr_feynman(%s," minus_third p
	| Prop_Gauge xi ->
          printf "pr_gauge(%s,%s," p (SCM.gauge_symbol xi)
	| Prop_Rxi xi ->
          printf "pr_rxi(%s,%s,%s,%s," p m w (SCM.gauge_symbol xi)
	| Prop_Tensor_2 ->
          printf "pr_tensor(%s,%s,%s," p m w
	| Prop_Tensor_pure ->
          printf "pr_tensor_pure(%s,%s,%s," p m w
	| Prop_Vector_pure ->
          printf "pr_vector_pure(%s,%s,%s," p m w
	| Prop_Vectorspinor ->
          printf "pr_grav(%s,%s,%s," p m w
	| Aux_Scalar | Aux_Spinor | Aux_ConjSpinor | Aux_Majorana
	| Aux_Vector | Aux_Tensor_1 -> printf "("
	| Aux_Col_Scalar | Aux_Col_Vector | Aux_Col_Tensor_1 -> printf "%s * (" minus_third
	| Only_Insertion -> printf "("
	| Prop_UFO name ->
          printf "pr_U_%s(%s,%s,%s," name p m w

    let print_projector f p m gamma =
      let minus_third = "(-1.0_" ^ !kind ^ "/3.0_" ^ !kind ^ ")" in
      match SCM.propagator f with
      | Prop_Scalar ->
          printf "pj_phi(%s,%s," m gamma
      | Prop_Col_Scalar ->
          printf "%s * pj_phi(%s,%s," minus_third m gamma
      | Prop_Ghost ->
          printf "(0,1) * pj_phi(%s,%s," m gamma
      | Prop_Spinor ->
          printf "%s(%s,%s,%s," Names.psi_projector p m gamma
      | Prop_ConjSpinor ->
          printf "%s(%s,%s,%s," Names.psibar_projector p m gamma
      | Prop_Majorana ->
          printf "%s(%s,%s,%s," Names.chi_projector p m gamma
      | Prop_Col_Majorana ->
          printf "%s * %s(%s,%s,%s," minus_third Names.chi_projector p m gamma
      | Prop_Unitarity ->
          printf "pj_unitarity(%s,%s,%s," p m gamma
      | Prop_Col_Unitarity ->
          printf "%s * pj_unitarity(%s,%s,%s," minus_third p m gamma
      | Prop_Feynman | Prop_Col_Feynman ->
          invalid_arg "no on-shell Feynman propagator!"
      | Prop_Gauge _ ->
          invalid_arg "no on-shell massless gauge propagator!"
      | Prop_Rxi _ ->
          invalid_arg "no on-shell Rxi propagator!"
      | Prop_Vectorspinor ->
          printf "pj_grav(%s,%s,%s," p m gamma
      | Prop_Tensor_2 ->
          printf "pj_tensor(%s,%s,%s," p m gamma
      | Prop_Tensor_pure ->
          invalid_arg "no on-shell pure Tensor propagator!"
      | Prop_Vector_pure ->
          invalid_arg "no on-shell pure Vector propagator!"
      | Aux_Scalar | Aux_Spinor | Aux_ConjSpinor | Aux_Majorana
      | Aux_Vector | Aux_Tensor_1 -> printf "("
      | Aux_Col_Scalar | Aux_Col_Vector | Aux_Col_Tensor_1 -> printf "%s * (" minus_third
      | Only_Insertion -> printf "("
      | Prop_UFO name ->
         invalid_arg "no on shell UFO propagator"

    let print_gauss f p m gamma =
      let minus_third = "(-1.0_" ^ !kind ^ "/3.0_" ^ !kind ^ ")" in
      match SCM.propagator f with
      | Prop_Scalar ->
          printf "pg_phi(%s,%s,%s," p m gamma
      | Prop_Ghost ->
          printf "(0,1) * pg_phi(%s,%s,%s," p m gamma
      | Prop_Spinor ->
          printf "%s(%s,%s,%s," Names.psi_projector p m gamma
      | Prop_ConjSpinor ->
          printf "%s(%s,%s,%s," Names.psibar_projector p m gamma
      | Prop_Majorana ->
          printf "%s(%s,%s,%s," Names.chi_projector p m gamma
      | Prop_Col_Majorana ->
          printf "%s * %s(%s,%s,%s," minus_third Names.chi_projector p m gamma
      | Prop_Unitarity ->
          printf "pg_unitarity(%s,%s,%s," p m gamma
      | Prop_Feynman | Prop_Col_Feynman ->
          invalid_arg "no on-shell Feynman propagator!"
      | Prop_Gauge _ ->
          invalid_arg "no on-shell massless gauge propagator!"
      | Prop_Rxi _ ->
          invalid_arg "no on-shell Rxi propagator!"
      | Prop_Tensor_2 ->
          printf "pg_tensor(%s,%s,%s," p m gamma
      | Prop_Tensor_pure ->
          invalid_arg "no pure tensor propagator!"
      | Prop_Vector_pure ->
          invalid_arg "no pure vector propagator!"
      | Aux_Scalar | Aux_Spinor | Aux_ConjSpinor | Aux_Majorana
      | Aux_Vector | Aux_Tensor_1 -> printf "("
      | Only_Insertion -> printf "("
      | Prop_UFO name ->
         invalid_arg "no UFO gauss insertion"
      | _ -> invalid_arg "targets:print_gauss: not available"

    let print_fusion_diagnostics amplitude dictionary fusion =
      if warn diagnose_gauge then begin
        let lhs = F.lhs fusion in
        let f = F.flavor lhs
        and v = variable lhs
        and p = momentum lhs in
        let mass = SCM.mass_symbol f in
        match SCM.propagator f with
        | Prop_Gauge _ | Prop_Feynman
        | Prop_Rxi _ | Prop_Unitarity ->
            printf "      @[<2>%s =" v;
            List.iter (print_current amplitude dictionary) (F.rhs fusion); nl ();
            begin match SCM.goldstone f with
            | None ->
                printf "      call omega_ward_%s(\"%s\",%s,%s,%s)"
                  (suffix diagnose_gauge) v mass p v; nl ()
            | Some (g, phase) ->
                let gv = SCM.flavor_symbol g ^ "_" ^ format_p lhs in
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
          (SCM.mass_symbol f) (SCM.width_symbol f)
      else
        if F.is_gauss amplitude lhs then
          print_gauss f (momentum lhs)
            (SCM.mass_symbol f) (SCM.width_symbol f)
        else
          print_propagator f (momentum lhs)
            (SCM.mass_symbol f) (SCM.width_symbol f);
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

(* \begin{dubious}
     The following will need a bit more work, because
     the decision when to [reverse_braket] for UFO models
     with Majorana fermions needs collaboration
     from [UFO.Targets.Fortran.fuse] which is called by
     [print_current].  See the function
     [UFO_targets.Fortran.jrr_print_majorana_current_transposing]
     for illustration (the function is never used and only for
     documentation).
   \end{dubious} *)

    let spins_of_rhs rhs =
      List.map (fun wf -> SCM.lorentz (F.flavor wf)) (F.children rhs)

    let spins_of_ket ket =
      match ThoList.uniq (List.map spins_of_rhs ket) with
      | [spins] -> spins
      | [] -> failwith "Targets.Fortran.spins_of_ket: empty"
      | _ -> [] (* HACK! *)

    let print_braket amplitude dictionary name braket =
      let bra = F.bra braket
      and ket = F.ket braket in
      let spin_bra = SCM.lorentz (F.flavor bra)
      and spins_ket = spins_of_ket ket in
      let vintage = true (* [F.vintage] *) in
      printf "      @[<2>%s =@ %s@, + " name name;
      if Fermions.reverse_braket vintage spin_bra spins_ket then
        begin
          printf "@,(";
          List.iter (print_current amplitude dictionary) ket;
          printf ")*%s" (multiple_variable amplitude dictionary bra)
        end
      else
        begin
          printf "%s*@,(" (multiple_variable amplitude dictionary bra);
          List.iter (print_current amplitude dictionary) ket;
          printf ")"
        end;
      nl ()

(* \begin{equation}
   \label{eq:factors-of-i}
     \ii T = \ii^{\#\text{vertices}}\ii^{\#\text{propagators}} \cdots
           = \ii^{n-2}\ii^{n-3} \cdots
           = -\ii(-1)^n \cdots
   \end{equation} *)

(* \begin{dubious}
     [tho:] we write some brakets twice using different names.  Is it useful
     to cache them?
   \end{dubious} *)

    let print_braket_slice ?orders dictionary amplitude brakets =
      let name = flavors_symbol ?orders (flavors amplitude) in
      printf "      %s = 0" name; nl ();
      List.iter (print_braket amplitude dictionary name) brakets;
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

    let print_brakets dictionary amplitude =
      match F.brakets amplitude with
      |[([], brakets)] -> print_braket_slice dictionary amplitude brakets
      |[(orders, brakets)] ->
        Printf.eprintf "omega: implementation of coupling order slices not complete yet!\n";
        print_braket_slice ~orders dictionary amplitude brakets
      | slices ->
         Printf.eprintf "omega: implementation of coupling order slices not complete yet!\n";
         List.iter
           (fun (orders, brakets) -> print_braket_slice ~orders dictionary amplitude brakets)
           slices

    let print_incoming wf =
      let p = momentum wf
      and s = spin wf
      and f = F.flavor wf in
      let m = SCM.mass_symbol f in
      match SCM.lorentz f with
      | Scalar -> printf "1"
      | BRS Scalar -> printf "(0,-1) * (%s * %s - %s**2)" p p m
      | Spinor ->
          printf "%s (%s, - %s, %s)" Names.psi_incoming m p s
      | BRS Spinor ->
          printf "%s (%s, - %s, %s)" Names.brs_psi_incoming m p s
      | ConjSpinor ->
          printf "%s (%s, - %s, %s)" Names.psibar_incoming m p s
      | BRS ConjSpinor ->
          printf "%s (%s, - %s, %s)" Names.brs_psibar_incoming m p s
      | Majorana ->
          printf "%s (%s, - %s, %s)" Names.chi_incoming m p s
      | Maj_Ghost -> printf "ghost (%s, - %s, %s)" m p s
      | BRS Majorana ->
          printf "%s (%s, - %s, %s)" Names.brs_chi_incoming m p s
      | Vector | Massive_Vector ->
          printf "eps (%s, - %s, %s)" m p s
(*i   | Ward_Vector -> printf "%s" p   i*)
      | BRS Vector | BRS Massive_Vector -> printf
            "(0,1) * (%s * %s - %s**2) * eps (%s, -%s, %s)" p p m m p s
      | Vectorspinor | BRS Vectorspinor ->
          printf "%s (%s, - %s, %s)" Names.grav_incoming m p s
      | Tensor_1 -> invalid_arg "Tensor_1 only internal"
      | Tensor_2 -> printf "eps2 (%s, - %s, %s)" m p s
      | _ -> invalid_arg "no such BRST transformations"

    let print_outgoing wf =
      let p = momentum wf
      and s = spin wf
      and f = F.flavor wf in
      let m = SCM.mass_symbol f in
      match SCM.lorentz f with
      | Scalar -> printf "1"
      | BRS Scalar -> printf "(0,-1) * (%s * %s - %s**2)" p p m
      | Spinor ->
          printf "%s (%s, %s, %s)" Names.psi_outgoing m p s
      | BRS Spinor ->
          printf "%s (%s, %s, %s)" Names.brs_psi_outgoing m p s
      | ConjSpinor ->
          printf "%s (%s, %s, %s)" Names.psibar_outgoing m p s
      | BRS ConjSpinor ->
          printf "%s (%s, %s, %s)" Names.brs_psibar_outgoing m p s
      | Majorana ->
          printf "%s (%s, %s, %s)" Names.chi_outgoing m p s
      | BRS Majorana ->
          printf "%s (%s, %s, %s)" Names.brs_chi_outgoing m p s
      | Maj_Ghost -> printf "ghost (%s, %s, %s)" m p s
      | Vector | Massive_Vector ->
          printf "conjg (eps (%s, %s, %s))" m p s
(*i   | Ward_Vector -> printf "%s" p   i*)
      | BRS Vector | BRS Massive_Vector -> printf
            "(0,1) * (%s*%s-%s**2) * (conjg (eps (%s, %s, %s)))" p p m m p s
      | Vectorspinor | BRS Vectorspinor ->
          printf "%s (%s, %s, %s)" Names.grav_incoming m p s
      | Tensor_1 -> invalid_arg "Tensor_1 only internal"
      | Tensor_2 -> printf "conjg (eps2 (%s, %s, %s))" m p s
      | BRS _ -> invalid_arg "no such BRST transformations"

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
      String.concat " " (List.map (fun f -> CM.flavor_to_string (SCM.flavor_all_orders f)) flavors)

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
        match SCM.width f with
        | Fudged ->
            let m = SCM.mass_symbol f
            and w = SCM.width_symbol f in
            printf "      if (%s > 0.0_%s) then" w !kind; nl ();
            printf "        @[<2>%s = %s@ * (%s*%s - %s**2)"
              name name p p m;
            printf "@ / cmplx (%s*%s - %s**2, %s*%s, kind=%s)"
              p p m m w !kind; nl ();
            printf "      end if"; nl ()
        | _ -> ()) (F.s_channel amplitude)

    let num_helicities amplitudes =
      List.length (CF.helicities amplitudes)

    let num_coupling_orders amplitudes =
      match CF.coupling_orders amplitudes with
      | None -> 0
      | Some (co_list, _) -> List.length co_list

    let num_coupling_order_powers amplitudes =
      match CF.coupling_orders amplitudes with
      | None -> 0
      | Some (_, powers) -> List.length powers

(* \thocwmodulesubsection{Spin, Flavor \&\ Color Tables} *)

(* The following abomination is required to keep the number of continuation
   lines as low as possible.  FORTRAN77-style \texttt{DATA} statements
   are actually a bit nicer here, but they are not available for
   \emph{constant} arrays. *)

(* \begin{dubious}
     We used to have a more elegant design with a sentinel~0 added to each
     initializer, but some revisions of the Compaq/Digital Compiler have a
     bug that causes them to reject this variant.
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
      if !amp_triv then
        1
      else
        List.length (CF.color_flows amplitudes)

    let num_color_indices_default = 2 (* Standard model *)

    let num_color_indices amplitudes =
      try CFlow.rank (List.hd (CF.color_flows amplitudes)) with _ -> num_color_indices_default

    let color_to_string c =
      "(" ^ (String.concat "," (List.map (Printf.sprintf "%3d") c)) ^ ")"

    let cflow_to_string cflow =
      String.concat " " (List.map color_to_string (CFlow.in_to_lists cflow)) ^ " -> " ^
      String.concat " " (List.map color_to_string (CFlow.out_to_lists cflow))

    let protected = ", protected" (* Fortran 2003! *)

    let print_coupling_orders_table amplitudes =
      printf "  @[<2>integer, dimension(n_co,n_cop), save%s :: table_coupling_orders" protected; nl ();
      begin match CF.coupling_orders amplitudes with
      | None | Some (_, []) -> ()
      | Some (_, powers) ->
         List.iteri
           (fun i powers ->
             printf "  @[<2>data table_coupling_orders(:,%4d) / %s /" (succ i)
               (String.concat ", " (List.map (Printf.sprintf "%2d") powers));
             nl ())
           powers
      end;
      nl ()

    let print_spin_table name tuples =
      printf "  @[<2>integer, dimension(n_prt,n_hel), save%s :: table_spin_%s"
        protected name; nl ();
      match tuples with
      | [] -> ()
      | _ ->
         List.iteri
           (fun i (tuple1, tuple2) ->
             printf "  @[<2>data table_spin_%s(:,%4d) / %s /" name (succ i)
               (String.concat ", " (List.map (Printf.sprintf "%2d") (tuple1 @ tuple2)));
             nl ())
           tuples

    let print_spin_tables amplitudes =
      print_spin_table "states" (CF.helicities amplitudes);
      nl ()

    let print_flavor_table name tuples =
      printf "  @[<2>integer, dimension(n_prt,n_flv), save%s :: table_flavor_%s"
        protected name; nl ();
      match tuples with
      | [] -> ()
      | _ ->
         List.iteri
           (fun i tuple ->
             printf "  @[<2>data table_flavor_%s(:,%4d) / %s / ! %s" name (succ i)
               (String.concat ", "
                  (List.map (fun f -> Printf.sprintf "%3d" (M.pdg f)) tuple))
               (String.concat " " (List.map M.flavor_to_string tuple));
             nl ())
           tuples

    let print_flavor_tables amplitudes =
      print_flavor_table "states"
        (List.map (fun (fin, fout) -> fin @ fout) (CF.flavors amplitudes));
      nl ()

    let num_flavors amplitudes =
      List.length (CF.flavors amplitudes)

    let print_color_flows_table tuples =
      if !amp_triv then begin
        printf
          "  @[<2>integer, dimension(n_cindex,n_prt,n_cflow), save%s :: table_color_flows = 0"
          protected; nl ();
	end
      else begin
        printf
          "  @[<2>integer, dimension(n_cindex,n_prt,n_cflow), save%s :: table_color_flows"
          protected; nl ();
      end;
      if not !amp_triv then begin
        match tuples with
        | [] -> ()
        | _ :: _ as tuples ->
           List.iteri
             (fun i tuple ->
               begin match CFlow.to_lists tuple with
               | [] -> ()
               | cf1 :: cfn ->
                  printf "  @[<2>data table_color_flows(:,:,%4d) /" (succ i);
                  printf "@ %s" (String.concat "," (List.map string_of_int cf1));
                  List.iter (fun cf -> printf ",@  %s" (String.concat "," (List.map string_of_int cf))) cfn;
                  printf "@ /"; nl ()
               end)
             tuples
      end

    let print_ghost_flags_table tuples =
      if !amp_triv then begin
        printf
          "  @[<2>logical, dimension(n_prt,n_cflow), save%s :: table_ghost_flags = F"
          protected; nl ();
	end
      else begin
        printf
          "  @[<2>logical, dimension(n_prt,n_cflow), save%s :: table_ghost_flags"
          protected; nl ();
        match tuples with
        | [] -> ()
        | _ ->
           List.iteri
             (fun i tuple ->
               begin match CFlow.ghost_flags tuple with
               | [] -> ()
               | gf1 :: gfn ->
                  printf "  @[<2>data table_ghost_flags(:,%4d) /" (succ i);
                  printf "@ %s" (if gf1 then "T" else "F");
                  List.iter (fun gf -> printf ",@  %s" (if gf then "T" else "F")) gfn;
                  printf " /";
                  nl ()
               end)
             tuples
      end

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

    (*i unused value
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
    i*)

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
      if not !amp_triv then begin
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
        end;
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

    (*i unused value
    let print_flavor_color_table_old abbrev n_flv n_cflow table =
      if n_flv <= 0 || n_cflow <= 0 then begin
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
    i*)

    let print_flavor_color_table n_flv n_cflow table =
      if !amp_triv then begin
        printf
          "  @[<2>logical, dimension(n_flv, n_cflow), save%s :: @ flv_col_is_allowed = T"
        protected; nl ();
	end
      else begin
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
	end;
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
      printf "hel_is_allowed = T"; nl ();
      printf "  @[<2>real(kind=%s), dimension(n_hel), save :: " !kind;
      printf "hel_max_abs = 0"; nl ();
      printf "  @[<2>real(kind=%s), save :: " !kind;
      printf "hel_sum_abs = 0, ";
      printf "hel_threshold = 1E10_%s" !kind; nl ();
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

    let print_maintenance_functions () =
      if !whizard then begin
        printf "  subroutine init (par, scheme)"; nl ();
        printf "    real(kind=%s), dimension(*), intent(in) :: par" !kind; nl ();
        printf "    integer, intent(in) :: scheme"; nl ();
        printf "    call import_from_whizard (par, scheme)"; nl ();
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

    (*i unused value
    let print_inquiry_function_declarations name =
      printf "  @[<2>public :: number_%s,@ %s" name name;
      nl ()
    i*)

    (*i unused value
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
    i*)

    let print_external_mass_case flv (fin, fout) =
      printf "    case (%3d)" (succ flv); nl ();
      List.iteri
        (fun i f ->
          printf "      m(%2d) = %s" (succ i) (M.mass_symbol f); nl ())
        (fin @ fout)

    let print_external_masses amplitudes =
      printf "  @[<5>"; if !fortran95 then printf "pure ";
      printf "subroutine external_masses (m, flv)"; nl ();
      printf "    real(kind=%s), dimension(:), intent(out) :: m" !kind; nl ();
      printf "    integer, intent(in) :: flv"; nl ();
      printf "    select case (flv)"; nl ();
      List.iteri print_external_mass_case (CF.flavors amplitudes);
      printf "    end select"; nl ();
      printf "  end subroutine external_masses"; nl ();
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
      if !amp_triv then begin
        printf "    n = n_cindex"; nl ();
	end
      else begin
        printf "    n = size (table_color_flows, dim=1)"; nl ();
      end;
      printf "  end function number_color_indices"; nl ();
      nl ();
      printf "  @[<5>"; if !fortran95 then printf "pure ";
      printf "function number_color_flows () result (n)"; nl ();
      printf "    integer :: n"; nl ();
      if !amp_triv then begin
        printf "    n = n_cflow"; nl ();
	end
      else begin
        printf "    n = size (table_color_flows, dim=3)"; nl ();
      end;
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
      printf "  @[<5>"; if !fortran95 && pure_unless_openmp then printf "pure ";
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
      printf "    end if"; nl ();
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
      if !amp_triv then begin
         printf "    ! print *, 'inside is_allowed'"; nl ();
      end;
      if not !amp_triv then begin
         printf "    yorn = hel_is_allowed(hel) .and. ";
         printf "flv_col_is_allowed(flv,col)"; nl ();
         end
      else begin
         printf "    yorn = .false."; nl ();
      end;
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

    let dump_amplitude_slices amplitudes =
      match CF.coupling_orders amplitudes with
      | None -> ()
      | Some (co_list, cop_list) ->
         printf "!   coupling orders:"; nl ();
         printf "!"; nl ();
         printf "!      %s" (String.concat ", " (List.map CM.coupling_order_to_string co_list)); nl ();
         List.iter
           (fun cop_list ->
             printf "!      %s" (String.concat ", " (List.map string_of_int cop_list)); nl ())
           cop_list;
         printf "!"; nl ();
         List.iter
           (fun amplitude ->
             printf "!     %s" (process_to_string amplitude); nl ();
             match F.brakets amplitude with
             | [] -> ()
             | lines ->
                let order_to_string (order, n) =
                  Printf.sprintf "%s = %d" (CM.coupling_order_to_string order) n in
                let orders_to_string orders =
                  String.concat ", " (List.map order_to_string orders) in
                List.iter (fun (orders, _) -> printf "!     %s" (orders_to_string orders); nl ()) lines;
                printf "!"; nl ())
           (CF.processes amplitudes);
         printf "!"; nl ()

    let print_description cmdline amplitudes () =
      printf
        "! File generated automatically by O'Mega %s %s %s"
        Config.version Config.status Config.date; nl ();
      List.iter (fun s -> printf "! %s" s; nl ()) (M.caveats ());
      printf "!"; nl ();
      printf "!   %s" cmdline; nl ();
      printf "!"; nl ();
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
      if not !amp_triv then begin
	printf "!"; nl ();
	ThoList.iteri
          (fun i cflow ->
            printf "!     %3d: %s" i (cflow_to_string cflow); nl ())
          1 (CF.color_flows amplitudes);
	printf "!"; nl ();
	printf "!     NB: i.g. not all color flows contribute to all flavor"; nl ();
	printf "!     combinations.  Consult the array FLV_COL_IS_ALLOWED"; nl ();
	printf "!     below for the allowed combinations."; nl ();
      end;
      printf "!"; nl ();
      printf "!   Color Factors:"; nl ();
      printf "!"; nl ();
      if not !amp_triv then begin
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
      end;
      if not !amp_triv then begin
         printf "!"; nl ();
         printf "!   vanishing or redundant flavor combinations:"; nl ();
         printf "!"; nl ();
         List.iter (fun process ->
           printf "!          %s" (process_sans_color_to_string process); nl ())
           (CF.vanishing_flavors amplitudes);
         printf "!"; nl ();
      end;
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
      begin
        match CF.slicings amplitudes with
        | [] -> ()
        | lines ->
            printf
              "!   coupling constant selections ('slicings'):"; nl ();
            printf "!"; nl ();
            List.iter (fun s -> printf "!     %s" s; nl ()) lines;
            printf "!"; nl ()
      end;
      dump_amplitude_slices amplitudes;
      printf "!"; nl ()

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

    (*i unused value
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
    i*)

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
      print_flush ()

    let module_to_file line_length oc prelude m =
      output_string oc (m.module_name ^ "\n");
      let filename = m.module_name ^ ".f90" in
      let channel = open_out filename in
      Format_Fortran.set_formatter_out_channel ~width:line_length channel;
      prelude ();
      print_modules [m];
      close_out channel

    let modules_to_file line_length oc prelude = function
      | [] -> ()
      | m :: mlist ->
          module_to_file line_length oc prelude m;
          List.iter (module_to_file line_length oc (fun () -> ())) mlist

(* \thocwmodulesubsection{Chopping Up Amplitudes} *)
    let all_brakets process =
      ThoList.flatmap snd (F.brakets process)

    let num_fusions_brakets size amplitudes =
      let num_fusions =
        max 1 size in
      let count_brakets =
        List.fold_left
          (fun sum process -> sum + List.length (all_brakets process))
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
      if not !amp_triv then begin
	if !openmp then begin
          printf "  subroutine compute_fusions_%04d (%s)" n openmp_tld; nl ();
          printf "  @[<5>type(%s), intent(inout) :: %s" openmp_tld_type openmp_tld; nl ();
	end else begin
          printf "  @[<5>subroutine compute_fusions_%04d ()" n; nl ();
	end;
	print_fusions dictionary fusions;
	printf "  end subroutine compute_fusions_%04d" n; nl ();
      end

    and print_compute_brakets1 dictionary (n, processes) =
      if not !amp_triv then begin
	if !openmp then begin
          printf "  subroutine compute_brakets_%04d (%s)" n openmp_tld; nl ();
          printf "  @[<5>type(%s), intent(inout) :: %s" openmp_tld_type openmp_tld; nl ();
	end else begin
          printf "  @[<5>subroutine compute_brakets_%04d ()" n; nl ();
	end;
	List.iter (print_brakets dictionary) processes;
	printf "  end subroutine compute_brakets_%04d" n; nl ();
      end

(* \thocwmodulesubsection{Common Stuff} *)

    let omega_public_symbols =
      ["number_particles_in"; "number_particles_out";
       "number_color_indices";
       "reset_helicity_selection"; "new_event";
       "is_allowed"; "get_amplitude"; "color_sum";
       "external_masses"; "openmp_supported"] @
      ThoList.flatmap
        (fun n -> ["number_" ^ n; n])
        ["spin_states"; "flavor_states"; "color_flows"; "color_factors"]

    let whizard_public_symbols md5sum =
      ["init"; "final"; "update_alpha_s"] @
      (match md5sum with Some _ -> ["md5sum"] | None -> [])

    let used_modules () =
      [Full "kinds";
       Full Names.use_module;
       Full_Aliased ("omega_color", ["omega_color_factor", omega_color_factor_abbrev])] @
      List.map
        (fun m -> Full m)
        (match !parameter_module with
         | "" -> !use_modules
         | pm -> pm :: !use_modules)

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
      printf " /)"; nl (); nl ();

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
          ("n_hel", num_helicities); (* Number of different helicity amplitudes. *)
          ("n_co", num_coupling_orders); (* Number of different coupling orders. *)
          ("n_cop", num_coupling_order_powers)  (* Number of different powers of coupling orders. *) ];
      nl ();

      (* Abbreviations.  *)
      printf "  ! NB: you MUST NOT change the value of %s here!!!" nc_parameter;
      nl ();
      printf "  !     It is defined here for convenience only and must be"; nl ();
      printf "  !     compatible with hardcoded values in the amplitude!"; nl ();
      print_real_parameter nc_parameter (SCM.nc ()); (* $N_C$ *)
      List.iter
        (function name, value -> print_logical_parameter name value)
        [ ("F", false); ("T", true) ]; nl ();

      print_coupling_orders_table amplitudes;
      print_spin_tables amplitudes;
      print_flavor_tables amplitudes;
      print_color_tables amplitudes;
      print_amplitude_table amplitudes;
      print_helicity_selection_table ()

    let print_interface amplitudes =
      print_md5sum_functions !md5sum;
      print_maintenance_functions ();
      List.iter print_numeric_inquiry_functions
        [("number_particles_in", "n_in");
         ("number_particles_out", "n_out")];
      List.iter print_inquiry_functions
        ["spin_states"; "flavor_states"];
      print_external_masses amplitudes;
      print_inquiry_function_openmp ();
      print_color_flows ();
      print_color_factors ();
      print_dispatch_functions ();
      nl ();
      (* Is this really necessary? *)
      Format_Fortran.switch_line_continuation false;
      if !km_write || !km_pure then (Targets_Kmatrix.Fortran.print !km_pure);
      if !km_2_write || !km_2_pure then (Targets_Kmatrix_2.Fortran.print !km_2_pure);
      Format_Fortran.switch_line_continuation true;
      nl ()

    let print_calculate_amplitudes declarations computations amplitudes =
      printf "  @[<5>subroutine calculate_amplitudes (amp, k, mask)"; nl ();
      printf "    complex(kind=%s), dimension(:,:,:), intent(out) :: amp" !kind; nl ();
      printf "    real(kind=%s), dimension(0:3,*), intent(in) :: k" !kind; nl ();
      printf "    logical, dimension(:), intent(in) :: mask"; nl ();
      printf "    integer, dimension(n_prt) :: s"; nl ();
      printf "    integer :: h, hi"; nl ();
      declarations ();
      if not !amp_triv then begin
	begin match CF.processes amplitudes with
	| p :: _ -> print_external_momenta p
	|  _ -> ()
	end;
	ignore (List.fold_left print_momenta PSet.empty (CF.processes amplitudes));
      end;
      printf "    amp = 0"; nl ();
      if not !amp_triv then begin
	if num_helicities amplitudes > 0 then begin
          printf "    if (hel_finite == 0) return"; nl ();
          if !openmp then begin
            printf "!$OMP PARALLEL DO DEFAULT(SHARED) PRIVATE(s, h, %s) SCHEDULE(STATIC)" openmp_tld; nl ();
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

    (* \thocwmodulesubsection{UFO Fusions} *)

    module VSet =
      Set.Make (struct type t = F.constant Coupling.t let compare = compare end)

    let ufo_fusions_used amplitudes =
      let couplings =
        List.fold_left
          (fun acc p ->
            let fusions = ThoList.flatmap F.rhs (F.fusions p)
            and brakets = ThoList.flatmap F.ket (all_brakets p) in
            let couplings =
              VSet.of_list (List.map F.coupling (fusions @ brakets)) in
            VSet.union acc couplings)
          VSet.empty (CF.processes amplitudes) in
      VSet.fold
        (fun v acc ->
          match v with
          | Coupling.Vn (Coupling.UFO (_, v, _, _, _), _, _) ->
             Sets.String.add v acc
          | _ -> acc)
        couplings Sets.String.empty

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

      Format_Fortran.set_formatter_out_channel ~width:!line_length oc;
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

      Format_Fortran.set_formatter_out_channel ~width:!line_length oc;
      print_description cmdline amplitudes ();
      print_modules [fortran_module]

(* \thocwmodulesubsection{Multiple Modules} *)

    let modules_of_amplitudes _ _ size amplitudes =

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
	if not !amp_triv then begin
          if !openmp then begin
            printf "  subroutine compute_fusions_%04d (%s)" n openmp_tld; nl ();
            printf "  @[<5>type(%s), intent(inout) :: %s" openmp_tld_type openmp_tld; nl ();
          end else begin
            printf "  @[<5>subroutine compute_fusions_%04d ()" n; nl ();
          end;
          print_fusions dictionary fusions;
          printf "  end subroutine compute_fusions_%04d" n; nl ();
	end in

      let print_compute_brakets (n, processes) () =
	if not !amp_triv then begin
          if !openmp then begin
            printf "  subroutine compute_brakets_%04d (%s)" n openmp_tld; nl ();
            printf "  @[<5>type(%s), intent(inout) :: %s" openmp_tld_type openmp_tld; nl ();
          end else begin
            printf "  @[<5>subroutine compute_brakets_%04d ()" n; nl ();
          end;
          List.iter (print_brakets dictionary) processes;
          printf "  end subroutine compute_brakets_%04d" n; nl ();
	end in

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
        [constants_module; variables_module] @
          fusions_modules @ brakets_modules in
      (public_module, private_modules)

    let amplitudes_to_channel_single_file cmdline oc size amplitudes =
      let public_module, private_modules =
        modules_of_amplitudes cmdline oc size amplitudes in
      Format_Fortran.set_formatter_out_channel ~width:!line_length oc;
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
      let ufo_fusions =
        let ufo_fusions_set = ufo_fusions_used amplitudes in
        if Sets.String.is_empty ufo_fusions_set then
          None
        else
          Some ufo_fusions_set in
      begin match ufo_fusions with
      | Some only ->
         let name = !module_name ^ "_ufo"
         and fortran_module = Names.use_module in
         use_modules := name :: !use_modules;
         UFO.Targets.Fortran.lorentz_module
           ~only ~name ~fortran_module ~parameter_module:!parameter_module
           (Format_Fortran.formatter_of_out_channel oc) ()
      | None -> ()
      end;
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

module Make =
  Make_Fortran(Target_Fortran_Names.Dirac)(Targets_vintage.Fortran_Fermions)
module Make_Majorana =
  Make_Fortran(Target_Fortran_Names.Majorana)(Targets_vintage.Fortran_Majorana_Fermions)
