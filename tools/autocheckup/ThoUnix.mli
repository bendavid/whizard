(* $Id: ThoUnix.mli 2496 2010-05-10 11:28:31Z ohl $ *)

val run_command :
    ?append:bool -> ?no_clobber:bool -> ?perm:Unix.file_perm ->
      ?in_file:string -> ?out_file:string -> ?err_file:string ->
	?search:bool -> ?env:string list ->
	  string list -> Unix.process_status

val reap_child : int -> int * Unix.process_status
val popen_in : ?search:bool -> ?env:string list -> string list -> int * in_channel
val popen_lines : ?search:bool -> ?env:string list -> string list -> string list

val tm_to_string : Unix.tm -> string
