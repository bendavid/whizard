(* omega_cli.ml --

   Copyright (C) 1999-2025 by

       Wolfgang Kilian <kilian@physik.uni-siegen.de>
       Thorsten Ohl <ohl@physik.uni-wuerzburg.de>
       Juergen Reuter <juergen.reuter@desy.de>

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

(* \thocwmodulesection{Model Collection} *)
module SMap = Map.Make(String)

module Models =
  struct

    type t = (string * string * (module Model.T)) SMap.t

    let normalize = String.lowercase_ascii

    let of_list model_list =
      List.fold_left
        (fun acc (name, _, _ as model) ->
          let key = normalize name in
          begin match SMap.find_opt key acc with
          | None -> ()
          | Some (clash, _, _) ->
             invalid_arg
               (Printf.sprintf "Omega_cli.Models.of_list: ambiguous model names '%s' ~ '%s'!" name clash)
          end;
          SMap.add key model acc)
        SMap.empty model_list

    let by_name_opt models name =
      match SMap.find_opt (normalize name) models with
      | None -> None
      | Some (_, _, model) -> Some model

    let names models =
      List.map (fun (_, (name, description, _)) -> (name, description)) (SMap.bindings models)

  end

(* \thocwmodulesection{Output Files} *)

type filename_components =
  { stem : string;
    extension : string }

type filename =
  | Components of filename_components
  | Full of string
  | Stdout

let open_output_channel name =
  let oc = open_out name in
  let close () = close_out oc in
  (oc, close, name)

let standard_output_channel =
  (stdout, (fun () -> flush stdout), "/dev/stdout")

let prefix_directory directory_opt name =
  match directory_opt with
  | None -> name
  | Some dir ->
     if Filename.is_relative name && Filename.is_implicit name then
       Filename.concat dir name
     else
       name

let output_channel directory_opt prefix = function
  | Stdout -> standard_output_channel
  | Full name -> open_output_channel (prefix_directory directory_opt name)
  | Components { stem; extension } ->
     begin match prefix, stem, extension with
     | "", "", "" -> standard_output_channel
     | _, _, _ ->
        let suffix =
          if stem = "" || extension = "" then
            stem ^ extension
          else
            stem ^ "." ^ extension in
        let name =
          if prefix = "" || suffix = "" then
            prefix ^ suffix
          else
            prefix ^ "_" ^ suffix in
        open_output_channel (prefix_directory directory_opt name)
     end

let with_output_channel ?logging directory_opt prefix file f =
  let channel, close, name = output_channel directory_opt prefix file in
  begin match logging with
  | None -> f channel
  | Some product  ->
     Printf.eprintf "Omega_cli: writing %s to '%s' ..." product name;
     f channel;
     Printf.eprintf " done.\n"
  end;
  close ()

(* \thocwmodulesection{Output File Options} *)

module type Output =
  sig
    type t = { write : bool; file : filename }
    val default : t
    val specs : t ref -> (Arg.key * Arg.spec * Arg.doc) list
  end

module type File =
  sig
    val write : bool
    val opt : string
    val stem : string
    val ext : string
  end

module Output (F : File) : Output =
  struct

    type t = { write : bool; file : filename }

    let default =
      { write = F.write;
        file = Components { stem = F.stem; extension = F.ext } }

    let write output yorn =
      output := { !output with write = yorn }

    let stdout output () =
      output := { write = true; file = Stdout }

    let name output file =
      output := { write = true; file = Full file }

    let warn_component component ignored name =
      Printf.eprintf
        "omega3: new %s file %s '%s' ignored, full name '%s' already set!\n"
        F.opt component ignored name

    let stem output stem =
      match !output.file with
      | Components components -> output := { !output with file = Components { components with stem } }
      | Full name -> warn_component "stem" stem name
      | _ -> ()
           
    let extension output extension =
      match !output.file with
      | Components components -> output := { !output with file = Components { components with extension } }
      | Full name ->  warn_component "extension" extension name
      | _ -> ()

    let specs output =
      let open Printf in
      [ ("--" ^ F.opt, Arg.Bool (write output),
         sprintf "true|false write %s file (default: %b)" F.opt F.write);
        ("--" ^ F.opt ^ "_stdout", Arg.Unit (stdout output),
         sprintf " write %s file to /dev/stdout" F.opt);
        ("--" ^ F.opt ^ "_name", Arg.String (name output),
         sprintf "name set %s file name" F.opt);
        ("--" ^ F.opt ^ "_stem", Arg.String (stem output),
         sprintf "stem set %s file stem (default='%s')" F.opt F.stem);
        ("--" ^ F.opt ^ "_extension", Arg.String (extension output),
         sprintf "ext set %s file extension (default='%s')" F.opt F.ext) ]

  end

module Amplitude = Output (struct let write = true let opt = "amplitude" let stem = opt let ext = "f90" end)
module Log = Output (struct let write = true let opt = "log" let stem = "amplitude" let ext = "log" end)
module Parameters = Output (struct let write = false let opt = "parameters" let stem = opt let ext = "f90" end)
module Phasespace = Output (struct let write = false let opt = "phasespace" let stem = opt let ext = "phs" end)
module Poles = Output (struct let write = false let opt = "poles" let stem = opt let ext = "poles" end)
module Whizard_Model = Output (struct let write = false let opt = "whizard" let stem = opt let ext = "mdl" end)
module Forest = Output (struct let write = false let opt = "forest" let stem = opt let ext = "out" end)
module Diagrams = Output (struct let write = false let opt = "diagrams" let stem = opt let ext = "tex" end)
module Colorflows = Output (struct let write = false let opt = "colorflows" let stem = opt let ext = "tex" end)
module DAG = Output (struct let write = false let opt = "dag" let stem = opt let ext = "dot" end)
module Full_DAG = Output (struct let write = false let opt = "full_dag" let stem = opt let ext = "dot" end)

(* \thocwmodulesection{Command Line Parsing} *)

type processes =
  | Scatterings of string list
  | Decays of string list

type command_ref =
  { processes_ref : processes ref;
    restrictions_rev_ref : string list ref;
    orders_rev_ref : string list ref;
    orders2_ref : bool ref;
    unphysical_ref : int option ref;
    directory_ref : string option ref;
    prefix_ref : string ref;
    amplitude_ref : Amplitude.t ref;
    log_ref : Log.t ref;
    parameters_ref : Parameters.t ref;
    phasespace_ref : Phasespace.t ref;
    poles_ref : Poles.t ref;
    whizard_ref : Whizard_Model.t ref;
    forest_ref : Forest.t ref;
    diagrams_ref : Diagrams.t ref;
    colorflows_ref : Colorflows.t ref;
    latex_ref : bool ref;
    dag_ref : DAG.t ref;
    full_dag_ref : Full_DAG.t ref;
    template_ref : bool ref }

let default_ref =
  { processes_ref = ref (Scatterings []);
    restrictions_rev_ref = ref [];
    orders_rev_ref = ref [];
    orders2_ref  = ref false;
    unphysical_ref = ref None;
    directory_ref = ref None;
    prefix_ref = ref "omega";
    amplitude_ref = ref Amplitude.default;
    log_ref = ref Log.default;
    parameters_ref = ref Parameters.default;
    phasespace_ref = ref Phasespace.default;
    poles_ref = ref Poles.default;
    whizard_ref = ref Whizard_Model.default;
    diagrams_ref = ref Diagrams.default;
    forest_ref = ref Forest.default;
    colorflows_ref = ref Colorflows.default;
    latex_ref = ref false;
    dag_ref = ref DAG.default;
    full_dag_ref = ref Full_DAG.default;
    template_ref = ref false }

let add_scatterings command lines =
  let processes =
    match !(command.processes_ref) with
    | Scatterings rev_lines -> Scatterings (lines @ rev_lines)
    | Decays [] -> Scatterings lines
    | Decays _ -> invalid_arg "Omega_cli.add_scattering: mixing -scatter and -decay" in
  command.processes_ref := processes

let add_decays command lines =
  let processes =
    match !(command.processes_ref) with
    | Decays rev_lines -> Decays (lines @ rev_lines)
    | Scatterings [] -> Decays lines
    | Scatterings _ -> invalid_arg "Omega_cli.add_decay: mixing -scatter and -decay" in
  command.processes_ref := processes

let add_restrictions command lines =
  command.restrictions_rev_ref := lines @ !(command.restrictions_rev_ref)

let add_orders command lines =
  command.orders_rev_ref := lines @ !(command.orders_rev_ref)

let set_orders2 command yorn =
  command.orders2_ref := yorn

let set_unphysical command n =
  command.unphysical_ref := Some n

let set_directory command directory =
  command.directory_ref := Some directory

let set_prefix command prefix =
  command.prefix_ref := prefix

type command =
  { processes : processes;
    restrictions_rev : string list;
    orders_rev : string list;
    orders2 : bool;
    unphysical : int option;
    directory : string option;
    prefix : string;
    amplitude : Amplitude.t;
    log : Log.t;
    parameters : Parameters.t;
    phasespace : Phasespace.t;
    poles : Poles.t;
    whizard : Whizard_Model.t;
    forest : Forest.t;
    diagrams : Diagrams.t;
    colorflows : Colorflows.t;
    latex : bool;
    dag : DAG.t;
    full_dag : Full_DAG.t;
    template : bool }

let command_of_ref command =
  { processes = !(command.processes_ref);
    restrictions_rev = !(command.restrictions_rev_ref);
    orders_rev = !(command.orders_rev_ref);
    orders2 = !(command.orders2_ref);
    unphysical = !(command.unphysical_ref);
    directory = !(command.directory_ref);
    prefix = !(command.prefix_ref);
    amplitude = !(command.amplitude_ref);
    log = !(command.log_ref);
    parameters = !(command.parameters_ref);
    poles = !(command.poles_ref);
    phasespace = !(command.phasespace_ref);
    whizard = !(command.whizard_ref);
    forest = !(command.forest_ref);
    diagrams = !(command.diagrams_ref);
    colorflows = !(command.colorflows_ref);
    latex = !(command.latex_ref);
    dag = !(command.dag_ref);
    full_dag = !(command.full_dag_ref);
    template = !(command.template_ref) }

(* \thocwmodulesection{Combining [Target], [Topology], and [Model.T]} *)

(* \begin{dubious}
     The [Target] module is not in the \verb+omega_core+ library
     and we can not reference implementations here, only interfaces
     like [Target_Maker].
   \end{dubious} *)

module type T =
  sig
    val main : ?current:int ref -> ?argv:string array -> unit -> unit
  end

module P = Momentum.Default
module P_Whizard = Momentum.DefaultW

let obsolete_UFO_options =
  Sets.String.of_list [ "UFO_dir"; "Majorana"; "dump"; "write_WHIZARD"; "exec" ]

let purge_ufo_options options =
  Options.exclude (fun o -> Sets.String.mem o obsolete_UFO_options) options

module Make (FM : Fusion.Maker) (PHS_Maker : Fusion.Maker) (TM : Target.Maker) (M : Model.Mutable) =
  struct

    type flavor = M.flavor

    module Proc = Process.Make(M)
    module C = Cascade.Make(M)(P)
    module Coupling_Orders = Orders.Conditions(Colorize.It(M))

    module CM = Colorize.It(M)
    module SCM = Orders.Slice(Colorize.It(M))

    module F = FM(P)(M)
    module CF = Fusion.Multi(FM)(P)(M)
    module T = TM(FM)(P)(M)

    module VSet = Set.Make (struct type t = F.constant Coupling.t let compare = compare end)
    module W = Whizard.Make(FM)(P)(P_Whizard)(M)
    module MT = Modeltools.Topology3(M)
    module PHS = PHS_Maker(P)(MT)
    module CT = Cascade.Make(MT)(P)
    module FMP = Feynmp.Make(FM)(P)(M)

    let parse_processes processes =
      try
        ThoList.uniq
          (List.sort compare
             (match processes with
              | Scatterings lines -> Proc.expand_scatterings (List.rev_map Proc.parse_scattering lines)
              | Decays lines -> Proc.expand_decays (List.rev_map Proc.parse_decay lines)))
      with
      | Invalid_argument s ->
         invalid_arg (Printf.sprintf "Omega_cli: invalid process specification: %s!\n" s)

    let parse_restrictions processes restrictions =
      match processes with
      | [] -> C.no_cascades
      | (fin, fout) :: _ ->
         begin match restrictions with
         | [] -> C.no_cascades
         | restrictions ->
            C.to_selectors (C.of_string_list (List.length fin + List.length fout) restrictions)
         end

    (* Once more with only triple vertices for the phasespace.
       This could be functorized over [CT]: *)
    let parse_restrictions_phs processes restrictions =
      match processes with
      | [] -> CT.no_cascades
      | (fin, fout) :: _ ->
         begin match restrictions with
         | [] -> CT.no_cascades
         | restrictions ->
            CT.to_selectors (CT.of_string_list (List.length fin + List.length fout) restrictions)
         end

    let parse_orders = function
      | [] -> None
      | lines -> Some (Coupling_Orders.of_strings lines)

    let flavors_to_string_all_orders flavors =
      String.concat " " (List.map (fun f -> CM.flavor_to_string (SCM.flavor_all_orders f)) flavors)

    let process_to_string_all_orders amplitude =
      flavors_to_string_all_orders (F.incoming amplitude) ^ " -> " ^
      flavors_to_string_all_orders (F.outgoing amplitude)

    let log_to_channel cmdline amplitudes channel =
      let open Printf in
      fprintf channel "%s\n" cmdline;
      List.iter
        (fun amplitude ->
          fprintf channel "%s: %d fusions, %d propagators, %d diagrams\n"
            (process_to_string_all_orders amplitude)
            (F.count_fusions amplitude)
            (F.count_propagators amplitude)
            (F.count_diagrams amplitude))
        (CF.processes amplitudes);
      let couplings =
        List.fold_left
          (fun acc p ->
            let brakets = ThoList.flatmap snd (F.brakets p) in
            let fusions = ThoList.flatmap F.rhs (F.fusions p)
            and brakets = ThoList.flatmap F.ket brakets in
            let couplings = VSet.of_list (List.map F.coupling (fusions @ brakets)) in
            VSet.union acc couplings)
          VSet.empty (CF.processes amplitudes) in
      fprintf channel "%d vertices\n" (VSet.cardinal couplings);
      let ufo_couplings =
        VSet.fold
          (fun v acc ->
            match v with
            | Coupling.Vn (Coupling.UFO (_, v, _, _, _), _, _) -> Sets.String.add v acc
            | _ -> acc)
          couplings Sets.String.empty in
      if not (Sets.String.is_empty ufo_couplings) then
        fprintf channel "%d UFO vertices: %s\n"
          (Sets.String.cardinal ufo_couplings)
          (String.concat ", " (Sets.String.elements ufo_couplings))

    let phasespace_to_channel restrictions processes channel =
      let selectors = parse_restrictions_phs processes restrictions in
      List.iter
        (fun (fin, fout) ->
          Printf.fprintf channel "%s -> %s ::\n"
            (String.concat " " (List.map M.flavor_to_string fin))
            (String.concat " " (List.map M.flavor_to_string fout));
          match fin with
          | [_] ->
             PHS.phase_space_channels channel (PHS.amplitude_sans_color false selectors fin fout)
          | [f1; f2] ->
             PHS.phase_space_channels channel (PHS.amplitude_sans_color false selectors fin fout);
             PHS.phase_space_channels_flipped channel (PHS.amplitude_sans_color false selectors [f2; f1] fout)
          | _ ->
             failwith (Printf.sprintf "Omega_cli.phasespace_to_channel: impossible: % incoming particles"
                         (List.length fin)))
        processes

    (* \begin{dubious}
         [Whizard.Make().write] has been disabled for a while now.  Don't show this option.
       \end{dubious} *)
    let poles_to_channel amplitudes channel =
      List.iter
        (fun amplitude -> W.write channel "omega" (W.merge (W.trees amplitude)))
        (CF.processes amplitudes)

    let p2s p =
      if p >= 0 && p <= 9 then
        string_of_int p
      else if p <= 36 then
        String.make 1 (Char.chr (Char.code 'A' + p - 10))
      else
        "_"

    let format_p wf =
      String.concat "" (List.map p2s (F.momentum_list wf))

    let variable wf =
      M.flavor_to_string (F.flavor_sans_color wf) ^ "[" ^ format_p wf ^ "]"

    let forest_to_channel amplitudes channel =
      List.iter
        (fun amplitude ->
          List.iter
            (fun tree ->
              Printf.fprintf channel "%s\n"
                (Tree.to_string (Tree.map (fun (wf, _) -> variable wf) (fun _ -> "") tree)))
            (F.forest (List.hd (F.externals amplitude)) amplitude))
        (CF.processes amplitudes)

    let debug (str, descr, opt, var) =
      [ "--warning:" ^ str, Arg.Unit (fun () -> var := (opt, false):: !var),
        " check " ^ descr ^ " and warn";
        "--error:" ^ str, Arg.Unit (fun () -> var := (opt, true):: !var),
        " check " ^ descr ^ " and terminate" ]

    let rec include_goldstones = function
      | [] -> false
      | (T.Gauge, _) :: _ -> true
      | _ :: rest -> include_goldstones rest

    let read_lines_rev file =
      let ic = open_in file in
      let rev_lines = ref [] in
      let rec slurp () =
        rev_lines := input_line ic :: !rev_lines;
        slurp () in
      try
        slurp ()
      with
      | End_of_file ->
          close_in ic;
          !rev_lines

    let read_lines file = 
      List.rev (read_lines_rev file)

    let list_flavors () =
      List.iter
        (fun (group, flavors) ->
          Printf.printf "%s:\n" group;
          List.iter (fun f -> Printf.printf "  %s\n" (M.flavor_to_string f)) flavors)
        (M.external_flavors ())

    let main ?current ?(argv=Sys.argv) () =
      let my_name = Filename.basename argv.(0)
      and cmdline = String.concat " " (List.map ThoString.quote (Array.to_list Sys.argv)) in
      let usage = Printf.sprintf "usage: %s [-help] [options]" my_name in
      let command = default_ref
      and arg_head_rev = ref [] in
      let checks = ref [] in

      let specs_lists =
        [ [ ("-f", Arg.Unit (fun () -> list_flavors (); exit 0),
             " list all flavors and exit");
            ("--flavors", Arg.Unit (fun () -> list_flavors (); exit 0),
             " list all flavors and exit");

            ("-s", Arg.String (fun s -> add_scatterings command [s]),
             "process add a scattering 'i1 i2 -> o1 o2 ...'");
            ("--scatter", Arg.String (fun s -> add_scatterings command [s]),
             "process add a scattering 'i1 i2 -> o1 o2 ...'");
            ("--scatter_file", Arg.String (fun s -> add_scatterings command (read_lines_rev s)),
             "name add scattering lines 'i1 i2 -> o1 o2 ...'");

            ("-d", Arg.String (fun s -> add_decays command [s]),
             "process add a decay 'i -> o1 o2 ...'");
            ("--decay", Arg.String (fun s -> add_decays command [s]),
             "process add a decay 'i -> o1 o2 ...'");
            ("--decay_file", Arg.String (fun s -> add_decays command (read_lines_rev s)),
             "name add decay lines 'i -> o1 o2 ...'");

            ("-r", Arg.String (fun s -> add_restrictions command [s]),
             "restriction add a restriction");
            ("--restrictions", Arg.String (fun s -> add_restrictions command [s]),
             "restriction add a restriction");
            ("--restrictions_file", Arg.String (fun s -> add_restrictions command (read_lines_rev s)),
             "name add restrictions");

            ("-o", Arg.String (fun s -> add_orders command [s]),
             "condition add a coupling order condition on amplitude");
            ("--orders", Arg.String (fun s -> add_orders command [s]),
             "condition add a coupling order condition on amplitude");
            ("--orders_file", Arg.String (fun s -> add_orders command (read_lines_rev s)),
             "name add coupling order conditions on amplitude");

            ("-O", Arg.Bool (set_orders2 command),
             "true|false coupling orders of |M|^2 (default=" ^ string_of_bool !(command.orders2_ref) ^ ")");
            ("--orders2", Arg.Bool (set_orders2 command),
             "true|false coupling orders of |M|^2 (default=" ^ string_of_bool !(command.orders2_ref) ^ ")");

            ("-u", Arg.Int (set_unphysical command),
             "n unphysical polarization vector for particle n");
            ("--unphysical", Arg.Int (set_unphysical command),
             "n unphysical polarization vector for particle n");

            ("-p", Arg.String (set_prefix command),
             "pfx prefix for output files (default='" ^ !(command.prefix_ref) ^ "')");
            ("--prefix", Arg.String (set_prefix command),
             "pfx prefix for output files (default='" ^ !(command.prefix_ref) ^ "')");
            ("--directory", Arg.String (set_directory command),
             "dir directory for output files (default='" ^ Filename.current_dir_name ^ "')") ];

          Amplitude.specs command.amplitude_ref;

          Options.cmdline "--model:" (purge_ufo_options M.options);
          Options.cmdline "--fusion:" CF.options;
          Options.cmdline "--target:" T.options;

          ThoList.flatmap debug
            [ ("a", "arguments", T.All, checks);
              ("n", "# of input arguments", T.Arguments, checks);
              ("m", "input momenta", T.Momenta, checks);
              ("g", "internal Ward identities", T.Gauge, checks) ];

          Log.specs command.log_ref;
          Parameters.specs command.parameters_ref;
          Phasespace.specs command.phasespace_ref;

          (* [Poles.specs command.poles_ref;] *)
          Whizard_Model.specs command.whizard_ref;
          Forest.specs command.forest_ref;
          Diagrams.specs command.diagrams_ref;
          Colorflows.specs command.colorflows_ref;
          [ ("--latex", Arg.Set command.latex_ref, " wrap diagrams in minimal LaTeX") ];

          DAG.specs command.dag_ref;
          Full_DAG.specs command.full_dag_ref;

          (* [ [ ("--template", Arg.Set command.template_ref,
                        " empty wrapper for a handcoded amplitudes")] ] *) ] in

      (* There is no default action if the command line is empty after
         [Omega3] has consumed the model loading options. *)
      if Array.length argv <= 1 then
        begin
          prerr_endline usage;
          exit 2
        end;

      (* Parse the command line. *)
      begin
        try
          Arg.parse_argv ?current argv
            (Arg.align (List.concat specs_lists))
            (fun s -> arg_head_rev := s :: !arg_head_rev)
            usage
        with
        | Arg.Bad msg ->
           prerr_endline msg;
           exit 2
        | Arg.Help msg ->
           print_endline msg;
           exit 0
      end;

      (* Collect options. *)
      let command = command_of_ref command in

      let to_output_channel ?logging write file f =
        if write then
          with_output_channel ?logging command.directory command.prefix file f in

      (* Process dependent outputs make only sense if the list of
         processes is not empty. *)
      begin match parse_processes command.processes with
      | [] -> ()
      | processes ->

         let selectors = parse_restrictions processes (List.rev command.restrictions_rev)
         and orders = parse_orders (List.rev command.orders_rev) in

         let amplitudes =
           CF.amplitudes (include_goldstones !checks) command.unphysical selectors orders processes in

         to_output_channel command.amplitude.write command.amplitude.file
           (fun channel -> T.amplitudes_to_channel cmdline channel !checks amplitudes);

         to_output_channel command.log.write command.log.file
           (log_to_channel cmdline amplitudes);

         to_output_channel command.phasespace.write command.phasespace.file
            (phasespace_to_channel (List.rev command.restrictions_rev) processes);

         to_output_channel command.poles.write command.poles.file
           (poles_to_channel amplitudes);

         to_output_channel command.forest.write command.forest.file
           (forest_to_channel amplitudes);
         
         to_output_channel command.diagrams.write command.diagrams.file
           (FMP.amplitudes_sans_color_to_channel command.latex amplitudes);

         to_output_channel command.colorflows.write command.colorflows.file
           (FMP.amplitudes_color_only_to_channel command.latex amplitudes);

         to_output_channel command.dag.write command.dag.file
           (fun channel -> List.iter (F.amplitude_to_dot channel) (CF.processes amplitudes));

         to_output_channel command.full_dag.write command.full_dag.file
           (fun channel -> List.iter (F.tower_to_dot channel) (CF.processes amplitudes))

      end;

      (* The model dependent outputs can be written in any case. *)
      to_output_channel command.parameters.write command.parameters.file T.parameters_to_channel;
      to_output_channel command.whizard.write command.whizard.file M.write_whizard;
      ()

  end

