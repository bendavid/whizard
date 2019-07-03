(* $Id: autocheckup_syntax.mli 2447 2010-04-29 17:05:47Z ohl $ *)

exception Syntax_Error of string * int * int

type spec =
    { key : string;
      value : string list }

type elt =
  | Spec of spec
  | Addto of spec
  | Remove of spec
  | Prohibit of spec
  | Group of elt list

type t = elt list

val spec : string -> string list -> elt
val addto : string -> string list -> elt
val remove : string -> string list -> elt
val prohibit : string -> string list -> elt
val group : elt list -> elt

val print : t -> unit

