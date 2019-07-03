(* $Id: autocheckup_syntax.ml 2447 2010-04-29 17:05:47Z ohl $ *)

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

let spec_raw key value =
  { key = String.lowercase key; value = value }

let spec key value =
  Spec (spec_raw key value)

let addto key value =
  Addto (spec_raw key value)

let remove key value =
  Remove (spec_raw key value)

let prohibit key value =
  Prohibit (spec_raw key value)

let group elts = Group elts

let print_spec op spec =
  Printf.printf "%s %s %s ;\n" spec.key op
    (String.concat " " (List.map (fun s -> "\"" ^ s ^ "\"") spec.value))

let rec print_elt = function
  | Spec spec -> print_spec "=" spec
  | Addto spec -> print_spec "+=" spec
  | Remove spec -> print_spec "-=" spec
  | Prohibit spec -> print_spec "!=" spec
  | Group elts -> print_group elts
  
and print_group elts =
  Printf.printf "{\n";
  List.iter print_elt elts;
  Printf.printf "}\n"

let print elts =
  List.iter print_elt elts

