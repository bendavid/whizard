(* $Id: float.ml,v 1.6 2002/08/05 16:54:52 ohl Exp $ *)
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

open Printf

module type T =
  sig
    type t
    val epsilon : t
    val to_string : t -> string
    val input_binary_float : in_channel -> float
    val input_binary_floats : in_channel -> float array -> unit
  end

module Double =
  struct

    type t = float

  (* Difference between~$1.0$ and the minimum float greater than~$1.0$
     \begin{dubious}
       This is the hard coded value for double precision on
       Linux/Intel.  We should determine this \emph{machine dependent}
       value during configuration.
     \end{dubious} *)
    let epsilon = 2.2204460492503131e-16
    let little_endian = true

    let to_string x =
      let s = sprintf "%.17E" x in
      for i = 0 to String.length s - 1 do
        let c = s.[i] in
        if c = 'e' || c = 'E' then
          s.[i] <- 'D'
      done;
      s

    (* Identity floatingpoint numbers that are indistinguishable from
       integers for more concise printing. *)

    type int_or_float =
      | Int of int
      | Float of float

    let float_min_int = float min_int
    let float_max_int = float max_int

    let soft_truncate x =
      let eps = 2.0 *. abs_float x *. epsilon in
      if x >= 0.0 then begin
        if x > float_max_int then
          Float x
        else if x -. floor x <= eps then
          Int (int_of_float x)
        else if ceil x -. x <= eps then
          Int (int_of_float x + 1)
        else
          Float x
      end else begin
        if x < float_min_int then
          Float x
        else if ceil x -. x <= eps then
          Int (int_of_float x)
        else if x -. floor x <= eps then
          Int (int_of_float x - 1)
        else
          Float x
      end

    let to_short_string x =
      match soft_truncate x with
      | Int i -> string_of_int i ^ "D0"
      | Float x -> to_string x


(* 
   Remark JRR:
   The function [float_of_string] was part of the \texttt{C}
   code of \texttt{O'Caml} from version 3.01 to version 3.07. 
   In the transition from version 3.06 to 3.07 it was decided
   to be obsolete and superseded by the routines from the 
   module [Int64]. There I make ThO's functions available 
   and I comment out the external [C] function. 

   The following code uses the external [C] function:
*)

(*i
    external float_of_bytes : string -> float = "float_of_bytes"

    let rev8 s =
      let swap i1 i2 =
        let tmp = s.[i1] in
        s.[i1] <- s.[i2];
        s.[i2] <- tmp in
      swap 0 7;
      swap 1 6;
      swap 2 5;
      swap 3 4

    let input_binary_float ic =
      let buf = String.create 8 in
      really_input ic buf 0 8;
      if little_endian then
        rev8 buf;
      float_of_bytes buf

    let input_binary_floats ic array =
      let n = Array.length array in
      let bytes = 8 * n in
      let buf = String.create bytes in
      really_input ic buf 0 bytes;
      for i = 0 to n - 1 do
        let s = String.sub buf (8 * i) 8 in
        if little_endian then
          rev8 s;
        array.(i) <- float_of_bytes s
      done

    let unsafe_rev8 s =
      let swap i1 i2 =
        let tmp = String.unsafe_get s i1 in
        String.unsafe_set s i1 (String.unsafe_get s i2);
        String.unsafe_set s i2 tmp in
      swap 0 7;
      swap 1 6;
      swap 2 5;
      swap 3 4

    let input_binary_float ic =
      let buf = String.create 8 in
      really_input ic buf 0 8;
      if little_endian then
        unsafe_rev8 buf;
      float_of_bytes buf

    let input_binary_floats ic array =
      let n = Array.length array in
      let bytes = 8 * n in
      let buf = String.create bytes in
      really_input ic buf 0 bytes;
      for i = 0 to n - 1 do
        let s = String.sub buf (8 * i) 8 in
        if little_endian then
          unsafe_rev8 s;
        array.(i) <- float_of_bytes s
      done

i*)

    let float_to_bytes x =
      let bytes = String.create 8 in
      let bits = Int64.bits_of_float x in
      let copy i j =
        String.unsafe_set bytes i
          (Char.chr (0xFF land (Int64.to_int (Int64.shift_right_logical bits (8*j))))) in
      copy 7 0;
      copy 6 1;
      copy 5 2;
      copy 4 3;
      copy 3 4;
      copy 2 5;
      copy 1 6;
      copy 0 7;
      bytes

(* The following three functions make only use of the [Int64] module. *)

    let float_of_bytes bytes =
      let copy i j =
        Int64.shift_left (Int64.of_int (Char.code (String.unsafe_get bytes j))) (8*i) in
      Int64.float_of_bits
        (Int64.logor (copy 7 0)
           (Int64.logor (copy 6 1)
              (Int64.logor (copy 5 2)
                 (Int64.logor (copy 4 3)
                    (Int64.logor (copy 3 4)
                       (Int64.logor (copy 2 5)
                          (Int64.logor (copy 1 6)
                             (copy 0 7))))))))

    let input_binary_float ic =
      let buf = String.create 8 in
      really_input ic buf 0 8;
      float_of_bytes buf

    let input_binary_floats ic array =
      let n = Array.length array in
      let bytes = 8 * n in
      let buf = String.create bytes in
      really_input ic buf 0 bytes;
      for i = 0 to n - 1 do
        let s = String.sub buf (8 * i) 8 in
        array.(i) <- float_of_bytes s
      done

    (* Suggested by Xavier Leroy: *)

    let output_float_big_endian oc f =
      let n = ref (Int64.bits_of_float f) in
      for i = 0 to 7 do
        output_byte oc (Int64.to_int (Int64.shift_right_logical !n 56));
        n := Int64.shift_left !n 8
      done

    let output_float_little_endian oc f =
      let n = ref (Int64.bits_of_float f) in
      for i = 0 to 7 do
        output_byte oc (Int64.to_int !n);
        n := Int64.shift_right_logical !n 8
      done
        
    let input_float_big_endian oc =
      let n = ref Int64.zero in
      for i = 0 to 7 do
        let b = input_byte oc in
        n := Int64.logor (Int64.shift_left !n 8) (Int64.of_int b)
      done;
      Int64.float_of_bits !n

    let input_float_little_endian oc =
      let n = ref Int64.zero in
      for i = 0 to 7 do
        let b = input_byte oc in
        n := Int64.logor !n (Int64.shift_left (Int64.of_int b) (i*8))
      done;
      Int64.float_of_bits !n

  end

(*i
 *  Local Variables:
 *  mode:caml
 *  indent-tabs-mode:nil
 *  page-delimiter:"^(\\* .*\n"
 *  End:
i*)
