(* orders_syntax.ml --

   Copyright (C) 2023-2025 by

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

type co = string

type co_set =
  | Set of co list
  | Diff of co_set * co_set
  | Complement of co_set

type range =
  | Range of int * int
  | Min of int
  | Max of int

type atom =
  | Interval of co_set * range
  | Slices of co_set * range
  | Exact of co_set * int
  | Null of co_set

type t =
  | Atom of atom
  | And of t list
  | Or of t list

exception Syntax_Error of string * int * int
