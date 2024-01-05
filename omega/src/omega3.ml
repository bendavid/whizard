(* omega3.ml --

   Copyright (C) 1999-2024 by

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

(* \begin{dubious}
     Next generation single executable.
     \begin{center}
       $\Omega^3$: only healthy fatty acids included!
     \end{center}
     Playground for first class modules.
   \end{dubious} *)

(* \begin{dubious}
     The following static models are still missing
     \begin{itemize}
       \item model defined in the compilation unit of the executable:
         \verb+CQED+, \verb+Littlest_Zprime+, \verb+SM_top+, \verb+SYM+.
     \end{itemize}
   \end{dubious} *)

let static_models_SM : (string * string * (module Model.T)) list =
  let open Modellib_SM in
  let pfx = "from Modellib_SM: " in
  [ ("QED", "Quantum Electro Dynamics", (module QED));
    ("QCD", "Quantum Chromo Dynamics", (module QCD));
    ("SM", "Standard Model (minimal, no CKM)", (module SM(SM_no_anomalous)));
    ("SM_CKM", pfx^"SM(SM_no_anomalous_ckm)", (module SM(SM_no_anomalous_ckm)));
    ("SM_Higgs", pfx^"SM(SM_Higgs)", (module SM(SM_Higgs)));
    ("SM_Higgs_CKM", pfx^"SM(SM_Higgs_CKM)", (module SM(SM_Higgs_CKM)));
    ("SM_ac", pfx^"SM(SM_anomalous)", (module SM(SM_anomalous)));
    ("SM_ac_CKM", pfx^"SM(SM_anomalous_ckm)", (module SM(SM_anomalous_ckm)));
    ("SM_top_anom", pfx^"SM(SM_anomalous_top)", (module SM(SM_anomalous_top)));
    ("SM_dim6", pfx^"SM(SM_dim6)", (module SM(SM_dim6)));
    ("SM_tt_threshold", pfx^"SM(SM_tt_threshold)", (module SM(SM_tt_threshold)));
    ("SM_ul", pfx^"SM(SM_k_matrix)", (module SM(SM_k_matrix)));
    ("SM_rx", pfx^"SM(SM_k_matrix) = SM_ul with fewer parameters in Whizard", (module SM(SM_k_matrix)));
    ("SM_Rxi", pfx^"SM_Rxi", (module SM_Rxi));
    ("SM_clones", pfx^"SM_clones", (module SM_clones));
    ("Phi3", "phi^3 toy model", (module Phi3));
    ("Phi4", "phi^3 + phi^4 toy model", (module Phi4)) ]

let static_models_BSM : (string * string * (module Model.T)) list =
  let open Modellib_BSM in
  let pfx = "from Modellib_BSM: " in
  [ ("THDM", pfx^"TwoHiggsDoublet(THDM)", (module TwoHiggsDoublet(THDM)));
    ("THDM_CKM", pfx^"TwoHiggsDoublet(THDM_CKM)", (module TwoHiggsDoublet(THDM_CKM)));
    ("GravTest", pfx^"GravTest(BSM_bsm)", (module GravTest(BSM_bsm)));
    ("HSExt", pfx^"HSExt(BSM_bsm)", (module HSExt(BSM_bsm)));
    ("Littlest", pfx^"Littlest(BSM_bsm)", (module Littlest(BSM_bsm)));
    ("Littlest_Eta", pfx^"Littlest(BSM_ungauged)", (module Littlest(BSM_ungauged)));
    ("Littlest_Tpar", pfx^"(Littlest_Tpar(BSM_bsm))", (module (Littlest_Tpar(BSM_bsm))));
    ("Simplest", pfx^"Simplest(BSM_bsm)", (module Simplest(BSM_bsm)));
    ("Simplest_univ", pfx^"Simplest(BSM_anom)", (module Simplest(BSM_anom)));
    ("SSC", pfx^"SSC(SSC_kmatrix)", (module SSC(SSC_kmatrix)));
    ("SSC_2", pfx^"SSC(SSC_kmatrix_2)", (module SSC(SSC_kmatrix_2)));
    ("SSC_AltT", pfx^"SSC_AltT(SSC_kmatrix_2)", (module SSC_AltT(SSC_kmatrix_2)));
    ("Template", pfx^"Template(BSM_bsm)", (module Template(BSM_bsm)));
    ("Threeshl", pfx^"Threeshl(Threeshl_no_ckm)", (module Threeshl(Threeshl_no_ckm)));
    ("Threeshl_nohf", pfx^"Threeshl(Threeshl_no_ckm_no_hf)", (module Threeshl(Threeshl_no_ckm_no_hf)));
    ("UED", pfx^"UED(BSM_bsm)", (module UED(BSM_bsm)));
    ("Xdim", pfx^"Xdim(BSM_bsm)", (module Xdim(BSM_bsm))) ]

let static_models_MSSM : (string * string * (module Model.T)) list =
  let open Modellib_MSSM in
  let pfx = "from Modellib_MSSM: " in
  [ ("MSSM", pfx^"MSSM(MSSM_no_4)", (module MSSM(MSSM_no_4)));
    ("MSSM_CKM", pfx^"MSSM(MSSM_no_4_ckm)", (module MSSM(MSSM_no_4_ckm)));
    ("MSSM_Hgg", pfx^"MSSM(MSSM_Hgg)", (module MSSM(MSSM_Hgg)));
    ("MSSM_Grav", pfx^"MSSM(MSSM_Grav)", (module MSSM(MSSM_Grav))) ]

let static_models_NMSSM : (string * string * (module Model.T)) list =
  let open Modellib_NMSSM in
  let pfx = "from Modellib_NMSSM: " in
  [ ("NMSSM", pfx^"NMSSM_func(NMSSM)", (module NMSSM_func(NMSSM)));
    ("NMSSM_CKM", pfx^"NMSSM_func(NMSSM_CKM)", (module NMSSM_func(NMSSM_CKM)));
    ("NMSSM_Hgg", pfx^"NMSSM_func(NMSSM_Hgg)", (module NMSSM_func(NMSSM_Hgg))) ]

let static_models_NoH : (string * string * (module Model.T)) list =
  let open Modellib_NoH in
  let pfx = "from Modellib_NoH: " in
  [ ("AltH", pfx^"AltH(NoH_k_matrix)", (module AltH(NoH_k_matrix)));
    ("NoH_rx", pfx^"NoH(NoH_k_matrix)", (module NoH(NoH_k_matrix))) ]

let static_models_other : (string * string * (module Model.T)) list =
  let module Zprime = Modellib_Zprime in
  let module PSSSM = Modellib_PSSSM in
  let module WZW = Modellib_WZW in
  let pfx s = "from Modellib_" ^ s ^ ": " in
  [ ("Zprime", pfx "Zprime"^"Zprime.Zprime(Zprime.SM_no_anomalous)", (module Zprime.Zprime(Zprime.SM_no_anomalous)));
    ("PSSSM", pfx "PSSSM"^"PSSSM.ExtMSSM(PSSSM.PSSSM)", (module PSSSM.ExtMSSM(PSSSM.PSSSM)));
    ("WZW", pfx "WZW"^"WZW.WZW(WZW.SM_no_anomalous)", (module WZW.WZW(WZW.SM_no_anomalous))) ]

let static_models =
  Omega_cli.Models.of_list
    (List.concat [ static_models_SM;
                   static_models_BSM;
                   static_models_MSSM;
                   static_models_NMSSM;
                   static_models_NoH;
                   static_models_other ])
  
let list_models () =
  List.iter
    (fun (name, description) -> Printf.printf "%s : %s\n" name description)
    (Omega_cli.Models.names static_models)

type model =
  | Static_Model of string
  | UFO_Model of string

let load_model ?(flags=[]) = function
  | Static_Model name ->
     begin match Omega_cli.Models.by_name_opt static_models name with
     | Some (module S) -> (module Modeltools.Static(S) : Model.Mutable)
     | None -> invalid_arg (Printf.sprintf "omega: static model '%s' not found!" name)
     end
  | UFO_Model directory ->
     let (module U) = (module UFO.Model : Model.Mutable with type init = string * string list) in
     U.init (directory, flags);
     (module U : Model.Mutable)

(* Check if the model [M] contains Majorana fermions.
   In the case of UFO, this can only be used \emph{after} the UFO model has
   been loaded with [M.init dir], of course! *)
let needs_majorana (module M : Model.T) =
  List.exists (fun f -> M.fermion f = 2) (M.flavors ())

(* Interface to the old CLI module [Omega] for testing the first class modules
   code before implementing the new [Omega_cli]. *)

module Legacy =
  struct

    (* Match a model without Majorana fermions and a target to a topology. *)
    let dirac (module T : Target.Maker) (module M : Model.Mutable) =
      let n = M.max_degree () in
      if n > 4 then
        (module (Omega.Nary(T)(M)) : Omega_cli.T)
      else if n = 4 then
        (module (Omega.Mixed23(T)(M)) : Omega_cli.T)
      else if n = 3 then
        (module (Omega.Binary(T)(M)) : Omega_cli.T)
      else
        invalid_arg "Omega3.Legacy.dirac: max_degree < 3"

    (* Match a model containing Majorana fermions and a target to a topology. *)
    let majorana (module T : Target.Maker) (module M : Model.Mutable) =
      let n = M.max_degree () in
      if n > 4 then
        (module (Omega.Nary_Majorana(T)(M)) : Omega_cli.T)
      else if n = 4 then
        (module (Omega.Mixed23_Majorana(T)(M)) : Omega_cli.T)
      else if n = 3 then
        (module (Omega.Binary_Majorana(T)(M)) : Omega_cli.T)
      else
        invalid_arg "Omega3.Legacy.majorana: max_degree < 3"

    (* Match a model containing Majorana fermions and a target to a topology
       using the old implementation. *)      
    let vintage_majorana (module T : Target.Maker) (module M : Model.Mutable) =
      let n = M.max_degree () in
      if n = 3 || n = 4 then
        (module (Omega.Mixed23_Majorana_vintage(T)(M)) : Omega_cli.T)
      else if n > 4 then
        invalid_arg "Omega3.Legacy.vintage_majorana: max_degree > 4"
      else
        invalid_arg "Omega3.Legacy.vintage_majorana: max_degree < 3"

    let fortran ?(force_vintage_majorana=false) ?(force_majorana=false) (module M : Model.Mutable) =
      if force_vintage_majorana then
        vintage_majorana (module Target_Fortran.Make_Majorana) (module M)
      else if force_majorana || needs_majorana (module M) then
        majorana (module Target_Fortran.Make_Majorana) (module M)
      else
        dirac (module Target_Fortran.Make) (module M)

    let vm ?(force_vintage_majorana=false) ?(force_majorana=false) (module M : Model.Mutable) =
      if force_vintage_majorana || force_majorana || needs_majorana (module M) then
        invalid_arg "Omega3.Legacy.vm: Majorana fermions not yet supported by the virtual machine"
      else
        dirac (module Target_VM.Make) (module M)

    let adjoin_target ?force_majorana ?force_vintage_majorana (module M : Model.Mutable) name =
      match String.lowercase_ascii name with
      | "fortran" -> fortran ?force_majorana ?force_vintage_majorana (module M)
      | "vm" -> vm ?force_majorana ?force_vintage_majorana (module M)
      | _ -> invalid_arg (Printf.sprintf "omega: target '%s' not found!" name)

    let load_omega ?flags ?force_majorana ?force_vintage_majorana target model =
      adjoin_target ?force_majorana ?force_vintage_majorana (load_model ?flags model) target

  end

module Bound (M : Model.T) : Tuple.Bound =
  struct
    let max_arity () = pred (M.max_degree ())
  end

module V3 =
  struct

    module CLI = Omega_cli.Make

    (* Match a model without Majorana fermions and a target to a topology. *)
    let dirac (module T : Target.Maker) (module M : Model.Mutable) =
      let open Fusion in
      let n = M.max_degree () in
      if n > 4 then
        (module (CLI(Nary(Bound(M)))(Helac(Bound(M)))(T)(M)) : Omega_cli.T)
      else if n = 4 then
        (module (CLI(Mixed23)(Helac_Mixed23)(T)(M)) : Omega_cli.T)
      else if n = 3 then
        (module (CLI(Binary)(Helac_Binary)(T)(M)) : Omega_cli.T)
      else
        invalid_arg "Omega3.V3.dirac: max_degree < 3"

    let dirac_helac (module T : Target.Maker) (module M : Model.Mutable) =
      let open Fusion in
      let n = M.max_degree () in
      if n > 4 then
        (module (CLI(Helac(Bound(M)))(Helac(Bound(M)))(T)(M)) : Omega_cli.T)
      else if n = 4 then
        (module (CLI(Helac_Mixed23)(Helac_Mixed23)(T)(M)) : Omega_cli.T)
      else if n = 3 then
        (module (CLI(Helac_Binary)(Helac_Binary)(T)(M)) : Omega_cli.T)
      else
        invalid_arg "Omega3.V3.dirac_helac: max_degree < 3"

    (* Match a model containing Majorana fermions and a target to a topology. *)
    let majorana (module T : Target.Maker) (module M : Model.Mutable) =
      let open Fusion in
      let n = M.max_degree () in
      if n > 4 then
        (module (CLI(Nary_Majorana(Bound(M)))(Helac_Majorana(Bound(M)))(T)(M)) : Omega_cli.T)
      else if n = 4 then
        (module (CLI(Mixed23_Majorana)(Helac_Mixed23_Majorana)(T)(M)) : Omega_cli.T)
      else if n = 3 then
        (module (CLI(Binary_Majorana)(Helac_Binary_Majorana)(T)(M)) : Omega_cli.T)
      else
        invalid_arg "Omega3.V3.majorana: max_degree < 3"

    let majorana_helac (module T : Target.Maker) (module M : Model.Mutable) =
      let open Fusion in
      let n = M.max_degree () in
      if n > 4 then
        (module (CLI(Helac_Majorana(Bound(M)))(Helac_Majorana(Bound(M)))(T)(M)) : Omega_cli.T)
      else if n = 4 then
        (module (CLI(Helac_Mixed23_Majorana)(Helac_Mixed23_Majorana)(T)(M)) : Omega_cli.T)
      else if n = 3 then
        (module (CLI(Helac_Binary_Majorana)(Helac_Binary_Majorana)(T)(M)) : Omega_cli.T)
      else
        invalid_arg "Omega3.V3.majorana_helac: max_degree < 3"

    (* Match a model containing Majorana fermions and a target to a topology
       using the old implementation. *)      
    let vintage_majorana (module T : Target.Maker) (module M : Model.Mutable) =
      let open Fusion_vintage in
      let n = M.max_degree () in
      if n > 4 then
        (module (CLI(Nary_Majorana(Bound(M)))(Helac_Majorana(Bound(M)))(T)(M)) : Omega_cli.T)
      else if n = 4 then
        (module (CLI(Mixed23_Majorana)(Helac_Majorana(Bound(M)))(T)(M)) : Omega_cli.T)
      else if n = 3 then
        (module (CLI(Binary_Majorana)(Helac_Majorana(Bound(M)))(T)(M)) : Omega_cli.T)
      else
        invalid_arg "Omega3.V3.vintage_majorana: max_degree < 3"

    let vintage_majorana_helac (module T : Target.Maker) (module M : Model.Mutable) =
      let open Fusion_vintage in
      let n = M.max_degree () in
      if n > 2 then
        (module (CLI(Nary_Majorana(Bound(M)))(Helac_Majorana(Bound(M)))(T)(M)) : Omega_cli.T)
      else
        invalid_arg "Omega3.V3.vintage_majorana_helac: max_degree < 3"

    let fortran ?(force_vintage_majorana=false) ?(force_majorana=false) (module M : Model.Mutable) =
      if force_vintage_majorana then
        vintage_majorana (module Target_Fortran.Make_Majorana) (module M)
      else if force_majorana || needs_majorana (module M) then
        majorana (module Target_Fortran.Make_Majorana) (module M)
      else
        dirac (module Target_Fortran.Make) (module M)

    let fortran_helac ?(force_vintage_majorana=false) ?(force_majorana=false) (module M : Model.Mutable) =
      if force_vintage_majorana then
        vintage_majorana_helac (module Target_Fortran.Make_Majorana) (module M)
      else if force_majorana || needs_majorana (module M) then
        majorana_helac (module Target_Fortran.Make_Majorana) (module M)
      else
        dirac_helac (module Target_Fortran.Make) (module M)

    let vm ?(force_vintage_majorana=false) ?(force_majorana=false) (module M : Model.Mutable) =
      if force_vintage_majorana || force_majorana || needs_majorana (module M) then
        invalid_arg "Omega3.V3.vm: Majorana fermions not yet supported by the virtual machine"
      else
        dirac (module Target_VM.Make) (module M)

    let vm_helac ?(force_vintage_majorana=false) ?(force_majorana=false) (module M : Model.Mutable) =
      if force_vintage_majorana || force_majorana || needs_majorana (module M) then
        invalid_arg "Omega3.V3.vm_helac: Majorana fermions not yet supported by the virtual machine"
      else
        dirac_helac (module Target_VM.Make) (module M)

    let adjoin_target ?(helac=false) ?force_majorana ?force_vintage_majorana (module M : Model.Mutable) name =
      match String.lowercase_ascii name with
      | "fortran" ->
         if helac then
           fortran_helac ?force_majorana ?force_vintage_majorana (module M)
         else
           fortran ?force_majorana ?force_vintage_majorana (module M)
      | "vm" ->
         if helac then
           vm_helac ?force_majorana ?force_vintage_majorana (module M)
         else
           vm ?force_majorana ?force_vintage_majorana (module M)
      | _ -> invalid_arg (Printf.sprintf "omega: target '%s' not found!" name)

    let load_omega ?helac ?flags ?force_majorana ?force_vintage_majorana target model =
      adjoin_target ?helac ?force_majorana ?force_vintage_majorana (load_model ?flags model) target

  end

(* This is the first part of the command line processing.
   Interpret the options up to ["--"] to load a model (static or UFO)
   and a target.  Then dispatch the rest of the command line to the old ([Omega.Make().main],
   selected by ["--legacy"]) main program or the new one ([Omega_cli.Make().main],
   selected by ["--v3"] or by default).

   For static models, the old command line interface should work in exactly the same way
   as the single executables.  For UFO models, some options in the old interface
   will not work, due to the new loading sequence. *)

let list_targets () =
  List.iter print_endline ["fortran"; "ovm"]

type mode =
  | V3
  | Legacy

let default_static_model_name = "SM"
let default_target_name = "fortran"

let _ =
  let argv0 = Sys.argv.(0) in
  let usage = "usage: " ^ argv0 ^ " [-help] [options]"
  and mode = ref V3
  and arg_head_rev = ref []
  and arg_tail_rev = ref []
  and ufo_debug = ref []
  and model = ref (Static_Model default_static_model_name)
  and target_name = ref default_target_name
  and force_majorana = ref None
  and force_vintage_majorana = ref None
  and helac = ref None in
  Arg.parse
    (Arg.align
       [ ( "-M", Arg.String (fun s -> model := Static_Model s),
           "model select static model (default='" ^ default_static_model_name ^ "')");
         ( "--model", Arg.String (fun s -> model := Static_Model s),
           "model select static model (default='" ^ default_static_model_name ^ "')");
         ( "--model_list", Arg.Unit list_models, " list all available static models");
         ( "-U", Arg.String (fun s -> model := UFO_Model s),
           "dir select UFO and read from directory");
         ( "--ufo_directory", Arg.String (fun s -> model := UFO_Model s),
           "dir select UFO and read from directory");
         ( "--ufo_debug", Arg.String (fun s -> ufo_debug := s :: !ufo_debug),
           "flag add UFO debug flags (undocumented)");
         ( "-T", Arg.String ((:=) target_name), "target select target (default='" ^ !target_name ^ "')");
         ( "--target", Arg.String ((:=) target_name), "target select target (default='" ^ !target_name ^ "')");
         ( "--target_list", Arg.Unit list_targets, " list all available targets");
         ( "--majorana", Arg.Unit (fun () -> force_majorana := Some true),
           " use Majorana spinors even if not needed");
         ( "--vintage_majorana", Arg.Unit (fun () -> force_vintage_majorana := Some true),
           " use the original implementation of Majorana spinors");
         ( "--helac", Arg.Unit (fun () -> helac := Some true),
           " use asymmetrical topologies like HELAC");
         ( "--v3", Arg.Unit (fun () -> mode := V3), " use the new omega CLI, version 3 (default)");
         ( "--legacy", Arg.Unit (fun () -> mode := Legacy), " use the historically grown omega CLI");
         ( "--", Arg.Rest (fun s -> arg_tail_rev := s :: !arg_tail_rev),
           " pass remaining options to the selected omega CLI") ])
    (fun s -> arg_head_rev := s :: !arg_head_rev)
    usage;
  let arg_head = List.rev !arg_head_rev
  and arg_tail = List.rev !arg_tail_rev in
  begin match arg_head with
  | [] -> ()
  | args -> Printf.eprintf "omega3: ignoring options before --: %s\n" (String.concat " " args)
  end;
  let force_majorana = !force_majorana
  and force_vintage_majorana = !force_vintage_majorana
  and helac = !helac
  and flags =
    match !ufo_debug with
    | [] -> None
    | flags -> Some flags in
  let (module O) =
    match !mode with
    | Legacy -> Legacy.load_omega ?flags ?force_majorana ?force_vintage_majorana !target_name !model
    | V3 -> V3.load_omega ?flags ?helac ?force_majorana ?force_vintage_majorana !target_name !model in
  let current = ref 0
  and argv = Array.of_list (argv0 :: arg_tail) in
  O.main ~current ~argv ()
