(* $Id: thoString.ml 6465 2015-01-10 15:22:31Z jr_reuter $

   Copyright (C) 1999-2015 by

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

let strip_prefix p s =
  let lp = String.length p
  and ls = String.length s in
  if lp > ls then
    s
  else
    let rec strip_prefix' i =
      if i >= lp then
	String.sub s i (ls - i)
      else if p.[i] <> s.[i] then
	s
      else
	strip_prefix' (succ i)
    in
    strip_prefix' 0

let strip_prefix_star p s =
  let ls = String.length s in
  if ls < 1 then
    s
  else
    let rec strip_prefix_star' i =
      if i < ls then begin
	if p <> s.[i] then
	  String.sub s i (ls - i)
	else
	  strip_prefix_star' (succ i)
      end else
	""
    in
    strip_prefix_star' 0

let strip_required_prefix p s =
  let lp = String.length p
  and ls = String.length s in
  if lp > ls then
    invalid_arg ("strip_required_prefix: expected `" ^ p ^ "' got `" ^ s ^ "'")
  else
    let rec strip_prefix' i =
      if i >= lp then
	String.sub s i (ls - i)
      else if p.[i] <> s.[i] then
	invalid_arg ("strip_required_prefix: expected `" ^ p ^ "' got `" ^ s ^ "'")
      else
	strip_prefix' (succ i)
    in
    strip_prefix' 0

let strip_from_first c s =
  try
    String.sub s 0 (String.index s c)
  with
  | Not_found -> s

let strip_from_last c s =
  try
    String.sub s 0 (String.rindex s c)
  with
  | Not_found -> s

let index_string pat s =
  let lpat = String.length pat
  and ls = String.length s in
  if lpat = 0 then
    0
  else
    let rec index_string' n =
      let i = String.index_from s n pat.[0] in
      if i + lpat > ls then
        raise Not_found
      else
        if String.compare pat (String.sub s i lpat) = 0 then
          i
        else
          index_string' (succ i)
    in
    index_string' 0

let quote s =
  if String.contains s ' ' || String.contains s '\n' then begin
    if String.contains s '"' then
      "'" ^ s ^ "'"
    else
      "\"" ^ s ^ "\""
  end else
    s

(*i
 *  Local Variables:
 *  mode:caml
 *  indent-tabs-mode:nil
 *  page-delimiter:"^(\\* .*\n"
 *  End:
i*)
