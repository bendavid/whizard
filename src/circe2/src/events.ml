(* $Id: events.ml,v 1.5 2001/10/18 10:11:15 ohl Exp $ *)
(* Copyright (C) 2001-2011 by Thorsten Ohl <ohl@physik.uni-wuerzburg.de>
   Circe2 is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by 
   the Free Software Foundation; either version 2, or (at your option)
   any later version.
   Circe2 is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the 
   GNU General Public License for more details.
   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.  *)  

(* \subsubsection{Reading Bigarrays} *)

(* Reading big arrays efficiently is not trivial, if we don't know
   the size of the arrays beforehand.  Here we use the brute force
   approach of reading a list of not-so-big arrays and blitting them
   into the resulting array later.  This avoids a second reading
   of the file, but temporarily needs twice the memory. *)

open Bigarray
open Printf

type t = (float, float64_elt, fortran_layout) Array2.t

exception Incomplete of int * t

(* Read lines from a channel into the columns of a bigarray.  If the
   file turns out to be short, the exception [Incomplete (i2, array)]
   is raised with the number of columns actually read. *)
let read_lines ic reader array i2_first i2_last =
  let i2 = ref i2_first in
  try
    while !i2 <= i2_last do
      reader array !i2 (input_line ic);
      incr i2
    done
  with
  | End_of_file -> raise (Incomplete (pred !i2, array))

(* Decode a line of floating point numbers into a column of
   a bigarray. *)

(* Fortran allows ['d'] and ['D'] as exponent starter, but
   O'Caml's [Genlex] doesn't accept it.  *)

let normalize_ascii_floats orig =
  let normalized = String.copy orig in
  for i = 0 to String.length normalized - 1 do
    let c = normalized.[i] in
    if c = 'd' || c = 'D' then
      normalized.[i] <- 'E'
  done;
  normalized
  
let lexer = Genlex.make_lexer []

let next_float s =
  match Stream.next s with
  | Genlex.Int n -> float n
  | Genlex.Float x -> x
  | _ -> invalid_arg "Events.int_as_float"

let read_floats array i2 line =
  let tokens = lexer (Stream.of_string (normalize_ascii_floats line)) in
  for i1 = 1 to Array2.dim1 array do
    Array2.set array i1 i2 (next_float tokens)
  done

(*i
let read_floats array i2 line =
  let tokens = lexer (Stream.of_string (normalize_ascii_floats line)) in
  for i1 = 1 to Array2.dim1 array do
    array.{i1,i2} <- next_float tokens
  done
i*)

(* Try to read the columns of a bigarray from a channel.  If the file
   turns out to be short, the exception~[Incomplete (dim2, array)] is
   raised with the number of columns actually read. *)
let try_of_ascii_channel dim1 dim2 ic =
  let array = Array2.create float64 fortran_layout dim1 dim2 in
  read_lines ic read_floats array 1 dim2;
  (dim2, array)

(* Read a~[dim1] floating point numbers per line into the columns
   of a reverted list of bigarrays, each with a maximum of~[chunk]
   columns. *)
let rev_list_of_ascii_channel chunk dim1 ic =
  let rec rev_list_of_ascii_channel' acc =
    let continue =
      try
        let acc' = try_of_ascii_channel dim1 chunk ic :: acc in
        fun () -> rev_list_of_ascii_channel' acc'
      with
      | Incomplete (len, a) -> fun () -> (len, a) :: acc in
    continue () in
  rev_list_of_ascii_channel' []

(* Concatenate a list of bigarrays~$[(l_n,a_n);\ldots;(l_2,a_2);(l_1,a_1)]$
   in reverse order~$a_1a_2\ldots a_n$.  Of each array~$a_i$, only the
   first~$l_i$ columns are used. If the optional [file] name is
   present, map the corresponding file to the bigarray.
   We can close the file descriptor immediately, since \verb+close(2)+
   does \emph{not} \verb+munmap(2)+. *) 

let create_array ?file dim1 dim2 =
  match file with
  | None -> Array2.create float64 fortran_layout dim1 dim2
  | Some name ->
      let fd =
        Unix.openfile name
          [Unix.O_RDWR; Unix.O_CREAT; Unix.O_TRUNC] 0o644 in
      let a = Array2.map_file fd float64 fortran_layout true dim1 dim2 in
      Unix.close fd;
      a

let rev_concat ?file arrays =
  let sum_dim2 =
    List.fold_left (fun sum (dim2, _) -> sum + dim2) 0 arrays in
  if sum_dim2 <= 0 then
    invalid_arg "Events.rev_concat";
  let dim1 = Array2.dim1 (snd (List.hd arrays)) in
  let array = create_array ?file dim1 sum_dim2 in
  let _ = List.fold_right
      (fun (dim2, a) ofs ->
        Array2.blit
          (Array2.sub_right a 1 dim2) (Array2.sub_right array ofs dim2);
        ofs + dim2)
      arrays 1 in
  array

let of_ascii_channel ?file ?(chunk = 100000) dim1 ic =
  rev_concat ?file (rev_list_of_ascii_channel chunk dim1 ic)

let of_ascii_file ?file ?chunk dim1 name =
  let ic = open_in name in
  let a = of_ascii_channel ?file ?chunk dim1 ic in
  close_in ic;
  a

(* We can close the file descriptor immediately, since \verb+close(2)+
   does \emph{not} \verb+munmap(2)+. *) 
let of_binary_file dim1 file =
  let fd = Unix.openfile file [Unix.O_RDONLY] 0o644 in
  let a = Array2.map_file fd float64 fortran_layout false dim1 (-1) in
  Unix.close fd;
  a

let shared_map_binary_file dim1 file =
  let fd = Unix.openfile file [Unix.O_RDWR] 0o644 in
  let a = Array2.map_file fd float64 fortran_layout true dim1 (-1) in
  Unix.close fd;
  a

let to_ascii_channel oc a =
  let dim1 = Array2.dim1 a
  and dim2 = Array2.dim2 a in
  for i2 = 1 to dim2 do
    for i1 = 1 to dim1 do
      fprintf oc " %.17E" (Array2.get a i1 i2)
    done;
    fprintf oc "\n"
  done

(*i
let to_ascii_channel oc a =
  let dim1 = Array2.dim1 a
  and dim2 = Array2.dim2 a in
  for i2 = 1 to dim2 do
    for i1 = 1 to dim1 do
      fprintf oc " %.17E" a.{i1,i2}
    done;
    fprintf oc "\n"
  done
i*)

let to_ascii_file name a =
  let oc = open_out name in
  to_ascii_channel oc a;
  close_out oc

let to_binary_file file a =
  let fd =
    Unix.openfile file
      [Unix.O_RDWR; Unix.O_CREAT; Unix.O_TRUNC] 0o644 in
  let a' =
    Array2.map_file fd float64 fortran_layout true
      (Array2.dim1 a) (Array2.dim2 a) in
  Unix.close fd;
  Array2.blit a a'

(*i
 *  Local Variables:
 *  mode:caml
 *  indent-tabs-mode:nil
 *  page-delimiter:"^(\\* .*\n"
 *  End:
i*)
