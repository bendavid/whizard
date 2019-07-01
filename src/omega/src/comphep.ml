(* $Id: comphep.ml 5864 2014-06-02 13:12:47Z ohl $

   Copyright (C) 1999-2014 by

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

let rcs_file = RCS.parse "Comphep" ["Plagiarizing CompHEP models ..."]
    { RCS.revision = "$Revision: 5864 $";
      RCS.date = "$Date: 2014-06-02 15:12:47 +0200 (Mon, 02 Jun 2014) $";
      RCS.author = "$Author: ohl $";
      RCS.source
        = "$URL: svn+ssh://jr_reuter@login.hepforge.org/hepforge/svn/whizard/trunk/src/omega/src/comphep.ml $" }

(* A friendlier [String.sub] that returns an empty string instead of
   raising an exception.  Instead of the length, the second argument
   denotes the last position. *)

let substring buffer i1 i2 =
  let imax = String.length buffer - 1 in
  let i1 = max i1 0
  and i2 = min i2 imax in
  let len = i2 - i1 + 1 in
  if len > 0 then
    String.sub buffer i1 len
  else
    ""

let first_non_white buffer =
  let len = String.length buffer in
  let rec skip_white i =
    if i >= len then
      i
    else if buffer.[i] <> ' ' && buffer.[i] <> '\t' then
      i
    else
      skip_white (succ i) in
  skip_white 0
      
let last_non_white buffer =
  let len = String.length buffer in
  let rec skip_white i =
    if i < 0 then
      i
    else if buffer.[i] <> ' ' && buffer.[i] <> '\t' then
      i
    else
      skip_white (pred i) in
  skip_white (pred len)
      
let gobble_white buffer =
  substring buffer (first_non_white buffer) (last_non_white buffer)

let gobble_arrows buffer =
  let imax = String.length buffer - 1 in
  if imax >= 0 then
    gobble_white
      (substring buffer
         (if buffer.[0] = '>' then 1 else 0)
         (if buffer.[imax] = '<' then pred imax else imax))
  else
    ""

let fold_lines ic f init =
  let rec fold_lines' acc =
    let continue =
      try
        let acc' = f (input_line ic) acc in
        fun () -> fold_lines' acc'
      with
      | End_of_file -> fun () -> acc in
    continue () in
  fold_lines' init

let column_tabs line =
  let len = String.length line in
  let rec tabs' acc i =
    if i >= len then
      List.rev acc
    else if line.[i] = '|' then
      tabs' (i :: acc) (succ i)
    else
      tabs' acc (succ i)
  in
  tabs' [] 0

let columns tabs line =
  let imax = String.length line - 1 in
  let rec columns' acc i = function
    | [] -> List.rev_map gobble_white (substring line i imax :: acc)
    | tab :: rest ->
        if tab < i then
          invalid_arg "columns: clash"
        else if (match rest with [] -> false | _ -> true)
            && line.[tab] <> '|' then
          invalid_arg "columns: expecting '|'"
        else
          columns' (substring line i (pred tab) :: acc) (succ tab) rest
  in
  columns' [] 0 tabs

let input_table name =
  let ic = open_in name in
  let model = input_line ic in
  let table = input_line ic in
  let line = input_line ic in
  let tabs = column_tabs line in
  let titles = columns tabs line in
  let rows = fold_lines ic (fun line acc ->
    if String.length line > 0 && line.[0] = '=' then
      acc
    else
      columns tabs line :: acc) [] in
  close_in ic;
  (gobble_white model, gobble_white table, List.map gobble_arrows titles, rows)

let substitute_char (cold, cnew) s =
  for i = 0 to String.length s - 1 do
    if s.[i] = cold then
      s.[i] <- cnew
  done;
  s

let sanitize_symbol s =
  List.fold_right substitute_char [('+', 'p'); ('-', 'm')] (String.copy s)

(* \begin{dubious}
     Fodder for a future [Coupling] module \ldots
   \end{dubious} *)

let rec fermion_of_lorentz = function
  | Coupling.Spinor -> 1
  | Coupling.ConjSpinor -> -1
  | Coupling.Majorana -> 1
  | Coupling.Maj_Ghost -> 1
  | Coupling.Vectorspinor -> 1
  | Coupling.Vector | Coupling.Massive_Vector -> 0
  | Coupling.Scalar | Coupling.Tensor_1 | Coupling.Tensor_2 -> 0 
  | Coupling.BRS f -> fermion_of_lorentz f

let rec conjugate_lorentz = function
  | Coupling.Spinor -> Coupling.ConjSpinor
  | Coupling.ConjSpinor -> Coupling.Spinor
  | Coupling.BRS f -> Coupling.BRS (conjugate_lorentz f) 
  | f -> f

(* \begin{dubious}
     Currently, this operates on the sanitized symbol names.
   \end{dubious} *)

let pdg_heuristic name =
  match name with
  | "e1" -> 11 | "E1" -> -11 | "n1" -> 12 | "N1" -> -12
  | "e2" -> 13 | "E2" -> -13 | "n2" -> 14 | "N2" -> -14
  | "e3" -> 15 | "E3" -> -15 | "n3" -> 16 | "N3" -> -16
  | "u" -> 2 | "U" -> -2 | "d" -> 1 | "D" -> -1
  | "c" -> 4 | "C" -> -4 | "s" -> 3 | "S" -> -3
  | "t" -> 6 | "T" -> -6 | "b" -> 5 | "B" -> -5
  | "G" -> 21 | "A" -> 22 | "Z" -> 23
  | "Wp" -> 24 | "Wm" -> -24 | "H" -> 25
  | _ -> invalid_arg ("pdg_heuristic failed: " ^ name)

module Model =
  struct

    type flavor = int
    type constant = string
    type gauge = unit

    module M = Modeltools.Mutable
        (struct type f = flavor type g = gauge type c = constant end)

    let flavors = M.flavors
    let external_flavors = M.external_flavors
    let lorentz = M.lorentz
    let color = M.color
    let propagator = M.propagator
    let width = M.width
    let goldstone = M.goldstone
    let conjugate = M.conjugate
    let fermion = M.fermion
    let vertices = M.vertices
    let fuse2 = M.fuse2
    let fuse3 = M.fuse3
    let fuse = M.fuse
    let max_degree = M.max_degree
    let parameters = M.parameters
    let flavor_of_string = M.flavor_of_string
    let flavor_to_string = M.flavor_to_string
    let flavor_to_TeX = M.flavor_to_TeX
    let flavor_symbol = M.flavor_symbol
    let gauge_symbol = M.gauge_symbol
    let pdg = M.pdg
    let mass_symbol = M.mass_symbol
    let width_symbol = M.width_symbol
    let constant_symbol = M.constant_symbol
    module Ch = M.Ch
    let charges = M.charges

    let rcs = rcs_file

    type symbol =
      | Selfconjugate of string
      | Conjugates of string * string

    type particle =
        { p_name : string;
          p_symbol : symbol;
          p_spin : Coupling.lorentz;
          p_mass : Comphep_syntax.raw;
          p_width : Comphep_syntax.raw;
          p_color : Color.t;
          p_aux : string option }

    let count_flavors particles =
      List.fold_left (fun n p -> n +
        match p.p_symbol with
        | Selfconjugate _ -> 1
        | Conjugates _ -> 2) 0 particles

    type particle_flavor =
        { f_name : string;
          f_conjugate : int;
          f_symbol : string;
          f_pdg : int;
          f_spin : Coupling.lorentz;
          f_propagator : gauge Coupling.propagator;
          f_fermion : int;
          f_mass : string;
          f_width : string;
          f_color : Color.t;
          f_aux : string option }

    let real_variable = function
      | Comphep_syntax.Integer 0 -> "zero"
      | Comphep_syntax.Symbol s -> s
      | _ -> invalid_arg "real_variable"

    let dummy_flavor =
      { f_name = "";
        f_conjugate = -1;
        f_symbol = "";
        f_pdg = 0;
        f_spin = Coupling.Scalar;
        f_propagator = Coupling.Prop_Scalar;
        f_fermion = 0;
        f_mass = real_variable (Comphep_syntax.integer 0);
        f_width = real_variable (Comphep_syntax.integer 0);
        f_color = Color.Singlet;
        f_aux = None }

    let propagator_of_lorentz = function
      | Coupling.Scalar -> Coupling.Prop_Scalar
      | Coupling.Spinor -> Coupling.Prop_Spinor
      | Coupling.ConjSpinor -> Coupling.Prop_ConjSpinor
      | Coupling.Majorana -> Coupling.Prop_Majorana
      | Coupling.Maj_Ghost -> invalid_arg 
            "propagator_of_lorentz: SUSY ghosts do not propagate"
      | Coupling.Vector -> Coupling.Prop_Feynman
      | Coupling.Massive_Vector -> Coupling.Prop_Unitarity
      | Coupling.Vectorspinor -> 
          invalid_arg "propagator_of_lorentz: Vectorspinor"
      | Coupling.Tensor_1 -> invalid_arg "propagator_of_lorentz: Tensor_1"
      | Coupling.Tensor_2 -> invalid_arg "propagator_of_lorentz: Tensor_2"
      | Coupling.BRS _ -> invalid_arg "propagator_of_lorentz: no BRST"

    let flavor_of_particle symbol conjg particle =
      let spin = particle.p_spin in
      { f_name = particle.p_name;
        f_conjugate = conjg;
        f_symbol = symbol;
        f_pdg = pdg_heuristic symbol;
        f_spin = spin;
        f_propagator = propagator_of_lorentz spin;
        f_fermion = fermion_of_lorentz spin;
        f_mass = real_variable particle.p_mass;
        f_width = real_variable particle.p_width;
        f_color = particle.p_color;
        f_aux = particle.p_aux }

    let flavor_of_antiparticle symbol conjg particle =
      let spin = conjugate_lorentz particle.p_spin in
      { f_name = "anti-" ^ particle.p_name;
        f_conjugate = conjg;
        f_symbol = symbol;
        f_pdg = pdg_heuristic symbol;
        f_spin = spin;
        f_propagator = propagator_of_lorentz spin;
        f_fermion = fermion_of_lorentz spin;
        f_mass = real_variable particle.p_mass;
        f_width = real_variable particle.p_width;
        f_color = Color.conjugate particle.p_color;
        f_aux = particle.p_aux }
  
    let parse_expr text =
      try
        Comphep_parser.expr Comphep_lexer.token (Lexing.from_string text)
      with
      | Parsing.Parse_error -> invalid_arg ("parse error: " ^ text)

    let parse_function_row = function
      | name :: fct :: comment :: _ -> (name, parse_expr fct, comment)
      | _ -> invalid_arg "parse_function_row"

    let parse_lagragian_row = function
      | p1 :: p2 :: p3 :: p4 :: c :: t :: _ ->
          ((p1, p2, p3, p4), parse_expr c, parse_expr t)
      | _ -> invalid_arg "parse_lagragian_row"

    let parse_symbol s1 s2 =
      if s1 = s2 then
        Selfconjugate (sanitize_symbol s1)
      else
        Conjugates (sanitize_symbol s1, sanitize_symbol s2)

    let parse_spin spin =
      match int_of_string spin with
      | 0 -> Coupling.Scalar
      | 1 -> Coupling.Spinor
      | 2 -> Coupling.Vector
      | _ -> invalid_arg ("parse_spin: spin = " ^ spin)

    let parse_color color =
      match int_of_string color with
      | 1 -> Color.Singlet
      | 3 -> Color.SUN 3
      | 8 -> Color.AdjSUN 3
      | _ -> invalid_arg ("parse_color: color = " ^ color)

    let parse_particle_row = function
      | name :: symbol :: symbol_cc :: spin :: mass :: width :: color ::
        aux :: _ ->
          { p_name = name;
            p_symbol = parse_symbol symbol symbol_cc;
            p_spin = parse_spin spin;
            p_mass = parse_expr mass;
            p_width = parse_expr width;
            p_color = parse_color color;
            p_aux = match aux with "" -> None | _ -> Some aux }
      | _ -> invalid_arg "parse_particle_row"

    let parse_variable_row = function
      | name :: value :: comment :: _ ->
          (name, float_of_string value, comment)
      | _ -> invalid_arg "parse_variable_row"

    let parse_table parse_row name =
      let model, table, titles, rows = input_table name in
      (model, table, titles, List.rev_map parse_row rows)

    let input_functions = parse_table parse_function_row
    let input_lagrangian = parse_table parse_lagragian_row
    let input_particles = parse_table parse_particle_row
    let input_variables = parse_table parse_variable_row

    let input_model dir idx =
      let idx = string_of_int idx in
      (input_particles (dir ^ "/prtcls" ^ idx ^ ".mdl"),
       input_variables (dir ^ "/vars" ^ idx ^ ".mdl"),
       input_functions (dir ^ "/func" ^ idx ^ ".mdl"),
       input_lagrangian (dir ^ "/lgrng" ^ idx ^ ".mdl"))

    let flavors_of_particles particles =
      let flavors = Array.create (count_flavors particles) dummy_flavor in
      ignore (List.fold_left (fun n p ->
        match p.p_symbol with
        | Selfconjugate f ->
            flavors.(n) <- flavor_of_particle f n p;
            n + 1
        | Conjugates (f1, f2) ->
            flavors.(n) <- flavor_of_particle f1 (n + 1) p;
            flavors.(n+1) <- flavor_of_antiparticle f2 n p;
            n + 2) 0 particles);
      flavors

    module F = Modeltools.Fusions (struct
      type f = flavor
      type c = constant
      let compare = compare
      let conjugate = conjugate
    end)

    let translate_tensor3 _ = Coupling.Scalar_Scalar_Scalar 1
    let translate_tensor4 _ = Coupling.Scalar4 1
    let translate_constant _ = ""

    let init flavors variables functions vertices =
      let fmax = Array.length flavors - 1 in
      let flist = ThoList.range 0 fmax in
      let clamp_flavor msg f =
        if f >= 0 || f <= fmax then
          f
        else
          invalid_arg (msg ^ ": invalid flavor: " ^ string_of_int f) in
      let flavor_hash = Hashtbl.create 37 in
      let flavor_of_string s =
        try
          Hashtbl.find flavor_hash s
        with
        | Not_found -> invalid_arg ("flavor_of_string: " ^ s) in
      for f = 0 to fmax do
        Hashtbl.add flavor_hash flavors.(f).f_symbol f
      done;
      let vertices3, vertices4 =
        List.fold_left (fun (v3, v4) ((p1, p2, p3, p4), c, t) ->
          if p4 = "" then
            (((flavor_of_string p1, flavor_of_string p2, flavor_of_string p3),
              translate_tensor3 t, translate_constant c) :: v3, v4)
          else
            (v3, ((flavor_of_string p1, flavor_of_string p2,
                   flavor_of_string p3, flavor_of_string p4),
                  translate_tensor4 t, translate_constant c) :: v4))
          ([], []) vertices in
      let max_degree = match vertices4 with [] -> 3 | _ -> 4 in
      let all_vertices () = (vertices3, vertices4, []) in
      let table = F.of_vertices (all_vertices ()) in
      let input_parameters = 
        (real_variable (Comphep_syntax.integer 0), 0.0) ::
        (List.map (fun (n, v, _) -> (n, v)) variables) in
      let derived_parameters =
        List.map (fun (n, f, _) -> (Coupling.Real n, Coupling.Const 0))
          functions in
      M.setup
        ~color:(fun f -> flavors.(clamp_flavor "color" f).f_color)
        ~pdg:(fun f -> flavors.(clamp_flavor "pdg" f).f_pdg)
        ~lorentz:(fun f -> flavors.(clamp_flavor "spin" f).f_spin)
        ~propagator:(fun f ->
          flavors.(clamp_flavor "propagator" f).f_propagator)
        ~width:(fun f -> Coupling.Constant)
        ~goldstone:(fun f -> None)
        ~conjugate:(fun f -> flavors.(clamp_flavor "conjugate" f).f_conjugate)
        ~fermion:(fun f -> flavors.(clamp_flavor "fermion" f).f_fermion)
        ~max_degree
        ~vertices:all_vertices
        ~fuse:(F.fuse2 table, F.fuse3 table, F.fuse table)
        ~flavors:([("All Flavors", flist)])
        ~parameters:(fun () ->
          { Coupling.input = input_parameters;
            Coupling.derived = derived_parameters;
            Coupling.derived_arrays = [] })
        ~flavor_of_string
        ~flavor_to_string:(fun f ->
          flavors.(clamp_flavor "flavor_to_string" f).f_name)
        ~flavor_to_TeX:(fun f ->
          flavors.(clamp_flavor "flavor_to_TeX" f).f_name)
        ~flavor_symbol:(fun f ->
          flavors.(clamp_flavor "flavor_symbol" f).f_symbol)
        ~gauge_symbol:(fun () -> "")
        ~mass_symbol:(fun f ->
          flavors.(clamp_flavor "mass_symbol" f).f_mass)
        ~width_symbol:(fun f ->
          flavors.(clamp_flavor "width_symbol" f).f_width)
        ~constant_symbol:(fun c -> failwith "constant_symbol")

    let particles_file = ref "prtcls1.mdl"
    let variables_file = ref "vars1.mdl"
    let functions_file = ref "func1.mdl"
    let lagrangian_file = ref "lgrng1.mdl"

    let load () =
      let (_, _, _, p), v, f, l =
        (input_particles !particles_file, input_variables !variables_file,
         input_functions !functions_file, input_lagrangian !lagrangian_file) in
      init (flavors_of_particles p) [] [] []

    let options = Options.create
        [ ("p", Arg.String (fun name -> particles_file := name),
           "CompHEP particles file (default: " ^ !particles_file ^ ")");
          ("v", Arg.String (fun name -> variables_file := name),
           "CompHEP variables file (default: " ^ !variables_file ^ ")");
          ("f", Arg.String (fun name -> functions_file := name),
           "CompHEP functions file (default: " ^ !functions_file ^ ")");
          ("l", Arg.String (fun name -> lagrangian_file := name),
           "CompHEP lagrangian file (default: " ^ !lagrangian_file ^ ")");
          ("exec", Arg.Unit load,
           "load the model files (required _before_ any particle)");
          ("help", Arg.Unit (fun () ->
            print_endline
              ("[" ^ String.concat "|"
                       (List.map M.flavor_to_string (M.flavors ())) ^ "]")),
            "print information on the model")]

  end

(*i
 *  Local Variables:
 *  mode:caml
 *  indent-tabs-mode:nil
 *  page-delimiter:"^(\\* .*\n"
 *  End:
i*)
