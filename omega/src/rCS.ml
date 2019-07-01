(* $Id: rCS.ml 4926 2013-12-04 12:35:06Z jr_reuter $

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

type raw = { revision : string; date : string; author : string; source : string }

type t =
    { name : string;
      description : string list;
      rcs_revision : string;
      rcs_date : string;
      rcs_author : string;
      rcs_source : string }

let name r = r.name
let description r = r.description
let revision r = r.rcs_revision
let date r = r.rcs_date
let author r = r.rcs_author
let source r = r.rcs_source

module TS = ThoString

let strip_dollars s =
  TS.strip_from_last '$' (TS.strip_prefix "$" s)

let strip_keyword k s =
  TS.strip_prefix_star ' ' (TS.strip_prefix ":" (TS.strip_required_prefix k s))

let parse1 k s =
  strip_keyword k (strip_dollars s)

let strip_before_keyword k s =
  try
    let i = TS.index_string k s in
    String.sub s i (String.length s - i)
  with
  | Not_found -> s
      
let strip_before_a_keyword k_list s =
  let rec strip_before_a_keyword' = function
    | k :: k_rest ->
        begin try
          let i = TS.index_string k s in
          String.sub s i (String.length s - i)
        with
        | Not_found -> strip_before_a_keyword' k_rest
        end
    | [] -> s in
  strip_before_a_keyword' k_list
      
(* Required for the transition from CVS to Subversion, because the latter doesn't
   support the \texttt{Source} keyword.  \texttt{URL} is probably the way to go,
   but we leave in \texttt{Id} as a fallback option. *)

let parse_source s =
  let s = strip_dollars s in
  try strip_keyword "URL" s with Invalid_argument _ ->
    try strip_keyword "Source" s with Invalid_argument _ ->
      TS.strip_from_first ' ' (strip_keyword "Id" s)

(* Assume that the SVN repository follows the recommended layout and
   that all files can be found beneath ["/trunk/"], ["/branches/"] or
   ["/tags/"].  Strip everything before that. *)

let strip_svn_repos s =
  strip_before_a_keyword ["/trunk/"; "/branches/"; "/tags/"] s

let parse name description r =
  { name = name;
    description = description;
    rcs_revision = parse1 "Revision" r.revision;
    rcs_date = parse1 "Date" r.date;
    rcs_author = parse1 "Author" r.author;
    rcs_source = strip_svn_repos (parse_source r.source) }

let rename rcs name description =
  { rcs with name = name; description = description }

let summary rcs =
  [ name rcs ^ ":"] @
  List.map (fun s -> "  " ^ s) (description rcs) @
  [ "  Source: " ^ source rcs;
    "  revision: " ^ revision rcs ^ " checked in by " ^
    author rcs ^ " at " ^ date rcs ]

(*i
 *  Local Variables:
 *  mode:caml
 *  indent-tabs-mode:nil
 *  page-delimiter:"^(\\* .*\n"
 *  End:
i*)
