(* $Id: omega.ml 3670 2012-01-21 19:33:07Z jr_reuter $

   Copyright (C) 1999-2012 by

       Wolfgang Kilian <kilian@physik.uni-siegen.de>
       Thorsten Ohl <ohl@physik.uni-wuerzburg.de>
       Juergen Reuter <juergen.reuter@desy.de>
       Christian Speckner <christian.speckner@physik.uni-freiburg.de>

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

module P = Momentum.Default
module P_Whizard = Momentum.DefaultW

module type T =
  sig
    val main : unit -> unit
    type flavor
    val diagrams : flavor -> flavor -> flavor list ->
      ((flavor * Momentum.Default.t) *
         (flavor * Momentum.Default.t,
          flavor * Momentum.Default.t) Tree.t) list
  end

module Make (Fusion_Maker : Fusion.Maker) (Target_Maker : Target.Maker) (M : Model.T) =
  struct

(* \begin{dubious}
     [max_lines = 8] is plenty, since amplitudes with 8 gluons still take
     several \emph{days} to construct.
   \end{dubious} *)
    module CM = Colorize.It(M)

    type flavor = M.flavor

    module Proc = Process.Make(M)

(* \begin{dubious}
     NB: this causes the constant initializers in [Fusion_Maker] more than once.
     Such side effects must be avoided if the initializers involve expensive
     computations.   \emph{Relying on the fact that the functor will be
     called only once is not a good idea!}
   \end{dubious} *)
    module F = Fusion_Maker(P)(M)
    module CF = Fusion.Multi(Fusion_Maker)(P)(M)
    module T = Target_Maker(Fusion_Maker)(P)(M)
    module W = Whizard.Make(Fusion_Maker)(P)(P_Whizard)(M)
    module C = Cascade.Make(M)(P)

    let version () =
      List.iter (fun s -> prerr_endline ("RCS: " ^ s))
        (ThoList.flatmap RCS.summary (CM.rcs :: T.rcs_list @ F.rcs_list))

    let debug (str, descr, opt, var) =
      [ "-warning:" ^ str, Arg.Unit (fun () -> var := (opt, false):: !var),
        "check " ^ descr ^ " and print warning on error";
        "-error:" ^ str, Arg.Unit (fun () -> var := (opt, true):: !var),
        "check " ^ descr ^ " and terminate on error" ]

    let rec include_goldstones = function
      | [] -> false
      | (T.Gauge, _) :: _ -> true
      | _ :: rest -> include_goldstones rest
      
    let p2s p =
      if p >= 0 && p <= 9 then
        string_of_int p
      else if p <= 36 then
        String.make 1 (Char.chr (Char.code 'A' + p - 10))
      else
        "_"

    let format_p wf =
      String.concat "" (List.map p2s (F.momentum_list wf))

    let variable wf = M.flavor_to_string (F.flavor_sans_color wf) ^ "[" ^ format_p wf ^ "]"
    let variable' wf = M.flavor_symbol (F.flavor_sans_color wf) ^ "[" ^ format_p wf ^ "]"

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

    type cache_mode =
      | Cache_Default
      | Cache_Initialize of string

    let cache_option =
      ref Cache_Default

    let unphysical_polarization = ref None
    
(* \thocwmodulesection{Main Program} *)

    let main () =
      let usage =
        "usage: " ^ Sys.argv.(0) ^
        " [options] [" ^ String.concat "|" (List.map M.flavor_to_string (M.flavors ())) ^ "]"
      and rev_scatterings = ref []
      and rev_decays = ref []
      and cascades = ref []
      and checks = ref []
      and output_file = ref None
      and print_forest = ref false
      and template = ref false
      and feynmf = ref None
      and feynmf_tex = ref false
      and quiet = ref false
      and write = ref true
      and params = ref false
      and poles = ref false
      and dag_out = ref None
      and dag0_out = ref None in
      Arg.parse
        (Options.cmdline "-target:" T.options @
         Options.cmdline "-model:" M.options @
         Options.cmdline "-fusion:" CF.options @
         ThoList.flatmap debug
           ["", "arguments", T.All, checks;
            "a", "# of input arguments", T.Arguments, checks;
            "m", "input momenta", T.Momenta, checks;
            "g", "internal Ward identities", T.Gauge, checks] @
         [("-o", Arg.String (fun s -> output_file := Some s),
           "write to given file instead of /dev/stdout");
          ("-scatter", Arg.String (fun s -> rev_scatterings := s :: !rev_scatterings),
           "in1 in2 -> out1 out2 ...");
          ("-scatter_file",
           Arg.String (fun s -> rev_scatterings := read_lines_rev s @ !rev_scatterings),
           "in1 in2 -> out1 out2 ...");
          ("-decay", Arg.String (fun s -> rev_decays := s :: !rev_decays),
           "in -> out1 out2 ...");
          ("-decay_file", Arg.String (fun s -> rev_decays := read_lines_rev s @ !rev_decays),
           "in -> out1 out2 ...");
           ("-cascade", Arg.String (fun s -> cascades := s :: !cascades),
            "select diagrams");
          ("-initialize", Arg.String (fun s -> cache_option := Cache_Initialize s),
           "precompute large lookup table(s) and store them in the directory");
          ("-unphysical", Arg.Int (fun i -> unphysical_polarization := Some i),
           "select unphysical polarization state for one particle to test Ward Identities");
          ("-template", Arg.Set template,
           "write a template for using handwritten amplitudes with WHIZARD");
          ("-forest", Arg.Set print_forest, "Diagrammatic expansion");
          ("-feynmf", Arg.String (fun s -> feynmf := Some s), "print feynmf/mp output");
          ("-feynmf_tex", Arg.Set feynmf_tex, "print feynmf/mp/LaTeX output");
          ("-revision", Arg.Unit version, "print revision control information");
          ("-quiet", Arg.Set quiet, "don't print a summary");
          ("-summary", Arg.Clear write, "print only a summary");
          ("-params", Arg.Set params, "print the model parameters");
          ("-poles", Arg.Set poles, "print the Monte Carlo poles");
          ("-dag", Arg.String (fun s -> dag_out := Some s), "print minimal DAG");
          ("-full_dag", Arg.String (fun s -> dag0_out := Some s), "print complete DAG")])
(*i       ("-T", Arg.Int Topology.Binary.debug_triplet, "");
          ("-P", Arg.Int Topology.Binary.debug_partition, "")])
i*)
        (fun _ -> prerr_endline usage; exit 1)
        usage;

      let cmdline =
        String.concat " " (List.map ThoString.quote (Array.to_list Sys.argv)) in
        
      let output_channel =
        match !output_file with
        | None -> stdout
        | Some name -> open_out name in

      let processes =
        try
          ThoList.uniq
            (List.sort compare 
               (match List.rev !rev_scatterings, List.rev !rev_decays with
               | [], [] -> []
               | scatterings, [] ->
                   Proc.expand_scatterings (List.map Proc.parse_scattering scatterings)
               | [], decays ->
                   Proc.expand_decays (List.map Proc.parse_decay decays)
               | scatterings, decays -> 
                   invalid_arg "mixed scattering and decay!"))
        with
        | Invalid_argument s ->
            begin 
              Printf.eprintf "O'Mega: invalid process specification: %s!\n" s;
              flush stderr;
              []
            end in

(* \begin{dubious}
     This is still crude.  Eventually, we want to catch \emph{all} exceptions
     and write an empty (but compilable) amplitude unless one of the special options
     is selected.
   \end{dubious} *)

      begin match processes, !cache_option, !params with
      | [], Cache_Initialize dir, false ->
          F.initialize_cache dir;
          exit 0
      | _, _, true ->
          T.parameters_to_channel output_channel;
          exit 0
      | [], _, false ->
          T.amplitudes_to_channel cmdline output_channel !checks CF.empty;
          exit 0
      | _, _, false ->

        let selectors =
          let fin, fout = List.hd processes in
          C.to_selectors (C.of_string_list (List.length fin + List.length fout) !cascades) in

        let amplitudes =
          try
            begin match F.check_charges () with
            | [] -> ()
            | violators ->
                let violator_strings =
                  String.concat ", "
                    (List.map
                       (fun flist ->
                         "(" ^ String.concat "," (List.map M.flavor_to_string flist) ^ ")")
                       violators) in
                failwith ("charge violating vertices: " ^ violator_strings)
            end;
            CF.amplitudes (include_goldstones !checks) !unphysical_polarization selectors processes
          with
          | exc ->
              begin 
                Printf.eprintf
                  "O'Mega: exception %s in amplitude construction!\n"
                  (Printexc.to_string exc);
                flush stderr;
                CF.empty;
              end in

        if !write then
          T.amplitudes_to_channel cmdline output_channel !checks amplitudes;

        if not !quiet then begin
          List.iter
            (fun amplitude ->
              Printf.eprintf "SUMMARY: %d fusions, %d propagators"
                (F.count_fusions amplitude) (F.count_propagators amplitude);
              flush stderr;
              Printf.eprintf ", %d diagrams" (F.count_diagrams amplitude);
              Printf.eprintf "\n")
            (CF.processes amplitudes);
        end;

        if !poles then begin
          List.iter
            (fun amplitude ->
              W.write output_channel "omega" (W.merge (W.trees amplitude)))
            (CF.processes amplitudes)
        end;

        begin match !dag0_out with
        | Some name ->
            let ch = open_out name in
            List.iter (F.tower_to_dot ch) (CF.processes amplitudes);
            close_out ch
        | None -> ()
        end;

        begin match !dag_out with
        | Some name ->
            let ch = open_out name in
            List.iter (F.amplitude_to_dot ch) (CF.processes amplitudes);
            close_out ch
        | None -> ()
        end;

        if !print_forest then
          List.iter
            (fun amplitude ->
              List.iter (fun t -> Printf.eprintf "%s\n"
                  (Tree.to_string
                     (Tree.map (fun (wf, _) -> variable wf) (fun _ -> "") t)))
                (F.forest (List.hd (F.externals amplitude)) amplitude))
            (CF.processes amplitudes);

(*i HACK: DIAGNOSTICS TEMPORARYLY DISABLED!!!
        begin match !feynmf with
        | Some name ->
            let fmf wf =
              { Tree.style =
                begin match M.propagator (F.flavor wf) with
                | Coupling.Prop_Feynman
                | Coupling.Prop_Gauge _ -> Some "photon"
                | Coupling.Prop_Unitarity
                | Coupling.Prop_Rxi _ -> Some "double"
                | Coupling.Prop_Spinor
                | Coupling.Prop_ConjSpinor -> Some "fermion"
                | _ -> None
                end;
                Tree.rev =
                begin match M.propagator (F.flavor wf) with
                | Coupling.Prop_Spinor -> false
                | Coupling.Prop_ConjSpinor -> true
                | _ -> false
                end;
                Tree.label = None;
                Tree.tension = None } in
            let a = CF.processes amplitudes in
            let wf1 = List.hd (F.externals a)
            and wf2 = List.hd (List.tl (F.externals a)) 
            in
            Tree.to_feynmf feynmf_tex name variable' wf2
              (List.map (Tree.map (fun (n, _) -> fmf n) (fun l -> l))
                 (F.forest wf1 a))
        | None -> ()
        end;
HACK: DIAGNOSTICS TEMPORARYLY DISABLED!!! i*)
        begin match !output_file with
        | None -> ()
        | Some name -> close_out output_channel
        end;
        exit 0

      end

(* \begin{dubious}
     This was only intended for debugging O'Giga \ldots
   \end{dubious} *)

    let decode wf =
      (F.flavor wf, (F.momentum wf : Momentum.Default.t))

    let diagrams in1 in2 out =
      match F.amplitudes false C.no_cascades [in1; in2] out with
      | a :: _ ->
          let wf1 = List.hd (F.externals a)
          and wf2 = List.hd (List.tl (F.externals a)) in
          let wf2 = decode wf2 in
          List.map (fun t ->
            (wf2,
             Tree.map (fun (wf, _) -> decode wf) decode t))
            (F.forest wf1 a)
      | [] -> []

    let diagrams in1 in2 out =
      failwith "Omega().diagrams: disabled"

  end

(*i
 *  Local Variables:
 *  mode:caml
 *  indent-tabs-mode:nil
 *  page-delimiter:"^(\\* .*\n"
 *  End:
i*)
