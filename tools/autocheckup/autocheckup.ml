(* $Id: autocheckup.ml 2499 2010-05-10 17:21:36Z ohl $ *)

let default_root = Filename.temp_dir_name

open Autocheckup_syntax

module Context = Map.Make (struct type t = string let compare = compare end)

type context = string list Context.t

let find key context =
  try Context.find key context with Not_found -> []

let replace key value context =
  Context.add key value context

type state =
    { context : context;
      prohibitions : context;
      terminals : context list}

let initial =
  { context = Context.empty;
    prohibitions = Context.empty;
    terminals = [] }

let dump_context prefix context =
  Context.iter
    (fun key value ->
      Printf.printf "%s %s = %s\n" prefix key
	(String.concat " " (List.map (fun s -> "\"" ^ s ^ "\"") value)))
    context

let dump_state state =
  dump_context "context>>" state.context;
  dump_context "prohibit>" state.prohibitions

let oset_add oset1 oset2 =
  oset1 @ List.filter (fun a -> not (List.mem a oset1)) oset2

let oset_sub oset1 oset2 =
  List.filter (fun a -> not (List.mem a oset2)) oset1

let parse_spec state spec =
  let context' =
    replace
      spec.key (oset_sub spec.value (find spec.key state.prohibitions))
      state.context in
  { state with context = context' }

let parse_addto state spec =
  let context' =
    replace
      spec.key (oset_sub
		  (oset_add (find spec.key state.context) spec.value)
		  (find spec.key state.prohibitions))
      state.context in
  { state with context = context' }

let parse_remove state spec  =
  let context' =
    replace
      spec.key (oset_sub
		  (oset_sub (find spec.key state.context) spec.value)
		  (find spec.key state.prohibitions))
      state.context in
  { state with context = context' }

let parse_prohibit state spec =
  let prohibitions' =
    replace
      spec.key (oset_add (find spec.key state.prohibitions) spec.value)
      state.prohibitions in
  { state with prohibitions = prohibitions' }

let is_group = function
  | Group _ -> true
  | _ -> false

let rec parse_elt state = function
  | Spec spec -> parse_spec state spec
  | Addto spec -> parse_addto state spec
  | Remove spec -> parse_remove state spec
  | Prohibit spec -> parse_prohibit state spec
  | Group elts -> parse_elts state elts

and parse_elts state elts =
  let state' = List.fold_left parse_elt state elts in
  let terminals' =
    if List.exists is_group elts then
      state'.terminals
    else
      state'.context :: state'.terminals in
  { state with terminals = terminals' }

module SSet = Set.Make (struct type t = string let compare = compare end)

let sset_of_list slist =
  List.fold_left (fun acc s -> SSet.add s acc) SSet.empty slist

let singletons =
  sset_of_list
    ["root"; "source"; "revision"]

let known_keys =
  sset_of_list
    ["root"; "source"; "revision"; "configuration"; "targets"; "mode" ]

exception Unknown_key of string
exception Missing_value of string
exception Multiple_values of string * string list

let filter_keys context =
  Context.fold 
    (fun key value acc ->
      if SSet.mem key known_keys then
	if SSet.mem key singletons then
	  match value with
	  | [] -> raise (Missing_value key)
	  | [_] -> Context.add key value acc
	  | _ -> raise (Multiple_values (key, value))
	else
	  Context.add key value acc
      else begin
	Printf.eprintf "dropping unknown key: %s\n" key;
	acc
      end)
    context Context.empty 

let parse elts =
  let final_state = parse_elts initial elts in
  List.rev_map filter_keys final_state.terminals

let scan_string s =
  Autocheckup_parser.main Autocheckup_lexer.token (Lexing.from_string s)

let scan_file name =
  let ic = open_in name in
  let config =
    Autocheckup_parser.main Autocheckup_lexer.token (Lexing.from_channel ic) in
  close_in ic;
  config

type svn_protocol = [ `HTTP | `HTTPS | `SVN | `SVN_SSH ]
type archive_protocol = [ `HTTP | `HTTPS | `File | `SCP | `RSYNC ]
type protocol = [ svn_protocol | archive_protocol ]

let protocol_to_string = function
  | `File -> "file"
  | `HTTP -> "http"
  | `HTTPS -> "https"
  | `SVN -> "svn"
  | `SVN_SSH -> "svn+ssh"
  | `SCP -> "scp"
  | `RSYNC -> "rsync"

exception Unknown_protocol of string

let protocol_of_string protocol =
  match String.lowercase protocol with
  | "file" -> `File
  | "http" -> `HTTP
  | "https" -> `HTTPS
  | "svn" -> `SVN
  | "svn+ssh" -> `SVN_SSH
  | "scp" -> `SCP
  | "rsync" -> `RSYNC
  | _ -> raise (Unknown_protocol protocol)

type 'a url =
    { protocol : 'a option;
      host : string option;
      path : string;
      url : string }

let url_regexp =
  Str.regexp "^\\(\\([^/:]+\\):\\)?\\(//\\([^/]*\\)\\)?\\(.*\\)$"

let url_of_string name =
  if Str.string_match url_regexp name 0 then
    { protocol = (try Some (protocol_of_string (Str.matched_group 2 name)) with Not_found -> None);
      host = (try Some (Str.matched_group 4 name) with Not_found -> None);
      path = (try Str.matched_group 5 name with Not_found -> "");
      url = name }
  else
    { protocol = None;
      host = None;
      path = name;
      url = name }

type revision =
  | Head
  | Revision of int

type subversion =
    { repository : svn_protocol url;
      revision : revision }

let subversion_to_string subversion =
  match subversion.revision with
  | Head -> subversion.repository.url
  | Revision r -> subversion.repository.url ^ "@" ^ string_of_int r

type archive =
  | Tar of archive_protocol url
  | Tar_gz of archive_protocol url
  | Tar_bz2 of archive_protocol url
  | Zip of archive_protocol url

let archive_to_string = function
  | Tar url | Tar_gz url | Tar_bz2 url | Zip url -> url.url

exception Unknown_archive of string

let archive_of_string s =
  let url = url_of_string s in
  match url.protocol with
  | None | Some (`HTTP | `HTTPS | `File | `SCP | `RSYNC) as p->
      begin try
	let _, archive =
	  List.find
	    (fun (suffix, cons) -> Filename.check_suffix url.path suffix)
	    [ (".tar", fun n -> Tar n);
	      (".tar.gz", fun n -> Tar_gz n);
	      (".tgz", fun n -> Tar_gz n);
	      (".tar.bz2", fun n -> Tar_bz2 n);
	      (".zip", fun n -> Zip n) ] in
	archive { url with protocol = p }
      with
      | Not_found -> raise (Unknown_archive s)
      end
  | Some _ -> raise (Unknown_archive s)

type source =
  | Subversion of subversion
  | Remote of archive
  | Local of archive

let source_to_string = function
  | Subversion svn -> subversion_to_string svn
  | Remote url -> archive_to_string url
  | Local name -> archive_to_string name

exception Missing_source
exception Multiple_sources of string list
exception Multiple_revisions of string list

let source_of_context context =
  let url =
    match find "source" context with
    | [] -> raise Missing_source
    | [s] -> url_of_string s
    | slist -> raise (Multiple_sources slist)
  and revision = 
    match find "revision" context with
    | [] -> Head
    | [s] -> Revision (int_of_string s)
    | slist -> raise (Multiple_revisions slist) in
  match url.protocol, url.host, url.path with
  | (None | Some `File), (None | Some "" | Some "localhost"), path ->
      Local (archive_of_string path)
  | Some (`SVN | `SVN_SSH as p), _, _ ->
      Subversion { repository = { url with protocol = Some p }; revision = revision }
  | Some (`HTTP | `HTTPS as p), _, path ->
      begin try
	Remote (archive_of_string path)
      with
      |	Unknown_archive _ ->
	  Subversion { repository = { url with protocol = Some p }; revision = revision }
      end
  | (None | Some `File), Some _, _ | Some (`RSYNC | `SCP), _, _ ->
      invalid_arg "source_of_string: rsync and scp not supported yet"


type mode =
  | SVN_in_tree
  | SVN_out_of_tree
  | In_tree
  | Out_of_tree

let mode_to_string = function
  | SVN_in_tree -> "svn-in-tree"
  | SVN_out_of_tree -> "svn-out-of-tree"
  | In_tree -> "in-tree"
  | Out_of_tree -> "out-of-tree"

exception Unknown_mode of string

let mode_of_string = function
  | "svn-in-tree" -> SVN_in_tree
  | "svn-out-of-tree" -> SVN_out_of_tree
  | "in-tree" -> In_tree
  | "out-of-tree" -> Out_of_tree
  | s -> raise (Unknown_mode s)

type configuration = string list

let quote_string s =
  if String.contains s ' ' then
    "\"" ^ s ^ "\""
  else
    s

let configuration_to_string options = 
  String.concat " " (List.map quote_string options)

type target = string

type 'a test =
    { source : source;
      configuration : configuration;
      root : string;
      mode : mode;
      targets : 'a list }

type 'a result =
  | Passed of 'a
  | Skipped
  | Failed of string * 'a

let result_to_string = function
  | Passed _ -> "PASSED"
  | Skipped -> "SKIPPED"
  | Failed (msg, _) -> "FAILED: " ^ msg

type unprocessed = target test
type 'a processed = (target * 'a result) test

let tests_of_context context =
  let configuration = find "configuration" context
  and targets = find "targets" context
  and modes = 
    match find "mode" context with
    | [] -> [SVN_out_of_tree]
    | slist -> List.map mode_of_string slist
  and root =
    match find "root" context with
    | [] -> default_root
    | [s] -> s
    | _ -> failwith "tests_of_context: multiple roots" in
  List.map
    (fun mode ->
      { source = source_of_context context;
	configuration = configuration;
	root = root ;
	mode = mode;
	targets = targets })
    modes

module Src_Projection =
  struct
    type elt = unprocessed
    type base = source
    let compare_elt = compare
    let compare_base = compare
    let pi test = test.source
  end

module Src_Bundle = Bundle.Make (Src_Projection)

module Cfg_Projection =
  struct
    type elt = unprocessed
    type base = source * configuration
    let compare_elt = compare
    let compare_base = compare
    let pi test = (test.source, test.configuration)
  end

module Cfg_Bundle = Bundle.Make (Cfg_Projection)

let src_cfg_bundle tests =
  List.map
    (fun (src, fiber) ->
      (src, Cfg_Bundle.fibers (Cfg_Bundle.of_list fiber)))
    (Src_Bundle.fibers (Src_Bundle.of_list tests))

open Printf

let fprint_test oc target_to_string test =
  fprintf oc "    root = %s / mode = %s / targets = %s\n"
    test.root
    (mode_to_string test.mode)
    (String.concat ", " (List.map target_to_string test.targets))

let fprint_test_configuration oc target_to_string ((source, configuration), tests) =
  fprintf oc "  configuration = %s\n"
    (String.concat " " configuration);
  List.iter (fprint_test oc target_to_string) tests

let fprint_test_source oc target_to_string (source, configurations) =
  fprintf oc "source = %s\n"
    (source_to_string source);
  List.iter (fprint_test_configuration oc target_to_string) configurations

let do_test prefix test =
  printf "    root = %s / mode = %s\n" test.root (mode_to_string test.mode);
  List.iter (fun t -> printf "      make %s\n" t) test.targets

let do_test_configuration prefix ((source, configuration), tests) =
  printf "  configuration = %s\n" (String.concat " " configuration);
  List.iter (do_test prefix) tests

let do_test_source prefix (source, configurations) =
  printf "source = %s\n" (source_to_string source);
  List.iter (do_test_configuration prefix) configurations

type input =
  | Input_File of string
  | Input_Expression of string
    
let scan_input = function
  | Input_File f -> scan_file f
  | Input_Expression s -> scan_string s

let _ =
  let usage = "usage: " ^ Sys.argv.(0) ^ " [options] [files]" in
  let rev_input = ref [] in
  Arg.parse
    ["-e", Arg.String (fun e -> rev_input := Input_Expression e :: !rev_input),
     "parse string as a complete prepended configuration file"]
    (fun f -> rev_input := Input_File f :: !rev_input)
    usage;
  let config = parse (List.concat (List.rev_map scan_input !rev_input)) in
  let prefix = ThoUnix.tm_to_string (Unix.gmtime (Unix.gettimeofday ())) in
  let tests = src_cfg_bundle (List.flatten (List.map tests_of_context config)) in
  Printf.printf "prefix = %s\n" prefix;
  List.iter (do_test_source prefix) tests
