(* $Id: SVN.mli 6004 2014-07-09 15:46:54Z cweiss $ *)

type pure_or_mixed = Pure of int | Mixed of int * int
type version =
  | Plain of pure_or_mixed
  | Switched of pure_or_mixed
  | Modified of pure_or_mixed
  | Partial of pure_or_mixed
val version_to_string : version -> string
val version_of_string : string -> version

val version : ?dir:string -> unit -> version

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
val print_info : info -> unit
val info : ?path:string -> unit -> info
exception Field_not_found of string

type revision = Head | Revision of int

val checkout : ?append:bool -> ?log_file:string -> ?revision:revision -> string -> string -> unit
val update : ?append:bool -> ?log_file:string -> ?revision:revision -> string -> unit


