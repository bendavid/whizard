(* $Id: thoArray.ml 4926 2013-12-04 12:35:06Z jr_reuter $

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

type 'a compressed = 
    { uniq : 'a array;
      embedding: int array }

let uniq a = a.uniq
let embedding a = a.embedding

type 'a compressed2 = 
    { uniq2 : 'a array array;
      embedding1: int array;
      embedding2: int array }

let uniq2 a = a.uniq2
let embedding1 a = a.embedding1
let embedding2 a = a.embedding2

module PMap = Pmap.Tree

let compress a =
  let last = Array.length a - 1 in
  let embedding = Array.make (succ last) (-1) in
  let rec scan num_uniq uniq elements n =
    if n > last then
      { uniq = Array.of_list (List.rev elements);
        embedding = embedding }
    else
      match PMap.find_opt compare a.(n) uniq with
      | Some n' ->
          embedding.(n) <- n';
          scan num_uniq uniq elements (succ n)
      | None ->
          embedding.(n) <- num_uniq;
          scan
            (succ num_uniq)
            (PMap.add compare a.(n) num_uniq uniq)
            (a.(n) :: elements)
            (succ n) in
  scan 0 PMap.empty [] 0
  
let uncompress a =
  Array.map (Array.get a.uniq) a.embedding

(* \begin{dubious}
     Using [transpose] simplifies the algorithms, but can be inefficient.
     If this turns out to be the case, we should add special treatments
     for symmetric matrices.
   \end{dubious} *)

let transpose a =
  let dim1 = Array.length a
  and dim2 = Array.length a.(0) in
  let a' = Array.make_matrix dim2 dim1 a.(0).(0) in
  for i1 = 0 to pred dim1 do
    for i2 = 0 to pred dim2 do
      a'.(i2).(i1) <- a.(i1).(i2)
    done
  done;
  a'

let compress2 a =
  let c2 = compress a in
  let c12_transposed = compress (transpose c2.uniq) in
  { uniq2 = transpose c12_transposed.uniq;
    embedding1 = c12_transposed.embedding;
    embedding2 = c2.embedding }

let uncompress2 a =
  let a2 = uncompress { uniq = a.uniq2; embedding = a.embedding2 } in
  transpose (uncompress { uniq = transpose a2; embedding = a.embedding1 })

(*i
 *  Local Variables:
 *  mode:caml
 *  indent-tabs-mode:nil
 *  page-delimiter:"^(\\* .*\n"
 *  End:
i*)





