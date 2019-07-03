(* $Id: SVN.ml 6004 2014-07-09 15:46:54Z cweiss $ *)

type pure_or_mixed =
  | Pure of int
  | Mixed of int * int

type version =
  | Plain of pure_or_mixed
  | Switched of pure_or_mixed
  | Modified of pure_or_mixed
  | Partial of pure_or_mixed

let pure_or_mixed_to_string = function
  | Pure v -> string_of_int v
  | Mixed (v1, v2) -> string_of_int v1 ^ ":" ^ string_of_int v2

let version_to_string = function
  | Plain rev -> pure_or_mixed_to_string rev
  | Switched rev -> pure_or_mixed_to_string rev ^ "S"
  | Modified rev -> pure_or_mixed_to_string rev ^ "M"
  | Partial rev -> pure_or_mixed_to_string rev ^ "P"

let version_flag v = function
  | "M" -> Modified v
  | "S" -> Switched v
  | "P" -> Partial v
  | s -> invalid_arg ("version flag '" ^ s ^ "'")

let version_of_string s =
  try Scanf.sscanf s "%d%!" (fun v -> Plain (Pure v)) with _ ->
  try Scanf.sscanf s "%d%[MSP]%!" (fun v -> version_flag (Pure v)) with _ ->
  try Scanf.sscanf s "%d:%d%!" (fun v v' -> Plain (Mixed (v, v'))) with _ ->
  try Scanf.sscanf s "%d:%d%[MSP]%!" (fun v v' -> version_flag (Mixed (v, v'))) with
  | Scanf.Scan_failure _ -> failwith "version_of_string"

let version ?(dir = ".") () =
  match ThoUnix.popen_lines ~search:true ["svnversion"; dir] with
  | [s] -> version_of_string s
  | [] -> failwith "svnversion produced no output"
  | _ -> failwith "svnversion produced too many lines of output"

type raw_info = string * string list

let split_string c s =
  try
    let i = String.index s c in
    (String.sub s 0 i, String.sub s (succ i) (String.length s - i - 1))
  with
  | Not_found -> (s, "")

let is_white c =
  c = ' '|| c = '\t'

let trim = function
  | "" -> ""
  | s ->
      let i = ref 0
      and j = ref (String.length s - 1) in
      while is_white s.[!i] do
	incr i
      done;
      while is_white s.[!j] do
	decr j
      done;
      if !i > !j then
	""
      else
	String.sub s !i (!j - !i + 1)

type info =
    { path : string;
      url : string;
      repository_root : string;
      repository_uuid : string;
      revision : int;
      node_kind : string;
      schedule : string;
      last_changed_author : string;
      last_changed_rev : int;
      last_changed_date : string }

exception Field_not_found of string

let info ?(path = ".") () =
  let lines =
    List.map
      (fun s ->
	let key, value = split_string ':' s in
	(trim key, trim value))
      (ThoUnix.popen_lines ~search:true ["svn"; "info"; path]) in
  let lookup key =
    try List.assoc key lines with Not_found -> raise (Field_not_found key) in
  { path = lookup "Path";
    url = lookup "URL";
    repository_root = lookup "Repository Root";
    repository_uuid = lookup "Repository UUID";
    revision = int_of_string (lookup "Revision");
    node_kind = lookup "Node Kind";
    schedule = lookup "Schedule";
    last_changed_author = lookup "Last Changed Author";
    last_changed_rev = int_of_string (lookup "Last Changed Rev");
    last_changed_date = lookup "Last Changed Date" }

let print_info info =
  Printf.printf "Path: %s\n" info.path;
  Printf.printf "URL: %s\n" info.url;
  Printf.printf "Revision: %d\n" info.revision;
  Printf.printf "Last Change: %d by %s at %s\n"
    info.last_changed_rev info.last_changed_author info.last_changed_date

type revision = Head | Revision of int

let checkout ?append ?log_file ?(revision = Head) url path =
  let status =
    let argv =
      ["svn"; "checkout"] @
      (match revision with Head -> [] | Revision r -> ["-r"; string_of_int r]) @
      [ url; path] in
    match log_file with
    | None ->
	ThoUnix.run_command ~in_file:"/dev/null" ~search:true argv
    | Some name ->
	ThoUnix.run_command ~in_file:"/dev/null"
	  ?append ~out_file:name ~err_file:name ~search:true argv
  in
  match status with
  | Unix.WEXITED 0 -> ()
  | Unix.WEXITED error ->
      failwith ("svn checkout exited w/code " ^ string_of_int error)
  | Unix.WSIGNALED signal ->
      failwith ("svn checkout caught signal #" ^ string_of_int signal)
  | Unix.WSTOPPED signal ->
      failwith ("svn checkout stopped by signal #" ^ string_of_int signal)

let update ?append ?log_file ?(revision = Head) path =
  let status =
    let argv =
      ["svn"; "update"] @
      (match revision with Head -> [] | Revision r -> ["-r"; string_of_int r]) @
      [ path] in
    match log_file with
    | None ->
	ThoUnix.run_command ~in_file:"/dev/null" ~search:true argv
    | Some name ->
	ThoUnix.run_command ~in_file:"/dev/null"
	  ?append ~out_file:name ~err_file:name ~search:true argv
  in
  match status with
  | Unix.WEXITED 0 -> ()
  | Unix.WEXITED error ->
      failwith ("svn update exited w/code " ^ string_of_int error)
  | Unix.WSIGNALED signal ->
      failwith ("svn update caught signal #" ^ string_of_int signal)
  | Unix.WSTOPPED signal ->
      failwith ("svn update stopped by signal #" ^ string_of_int signal)

let checkout_or_update ?append ?log_file ?revision url path =
  try
    let cur_info = info ~path () in
    if cur_info.url = url then
      update ?append ?log_file ?revision path
    else
      invalid_arg
	("checkout_or_update: " ^ path ^ " contains revision " ^
	 string_of_int cur_info.revision ^ " of " ^ cur_info.url ^ "!")
  with
  | _ -> checkout ?append ?log_file ?revision url path

let _ =
  Printf.printf "svnversion -> %s\n" (version_to_string (version ()));
  print_info (info ())
