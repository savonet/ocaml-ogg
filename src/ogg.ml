(*
 * Copyright 2007 Samuel Mimram
 *
 * This file is part of ocaml-ogg.
 *
 * ocaml-ogg is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * ocaml-ogg is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with ocaml-ogg; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 * 
 * As a special exception to the GNU Library General Public License, you may 
 * link, statically or dynamically, a "work that uses the Library" with a publicly 
 * distributed version of the Library to produce an executable file containing 
 * portions of the Library, and distribute that executable file under terms of 
 * your choice, without any of the additional requirements listed in clause 6 
 * of the GNU Library General Public License. 
 * By "a publicly distributed version of the Library", we mean either the unmodified 
 * Library as distributed by INRIA, or a modified version of the Library that is 
 * distributed under the conditions defined in clause 3 of the GNU Library General 
 * Public License. This exception does not however invalidate any other reasons why 
 * the executable file might be covered by the GNU Library General Public License.
 * 
 *)

(*
 * Functions for manipulating ogg streams files using libogg.
 *
 * @author Samuel Mimram
 *)

exception Not_enough_data
exception Bad_data
exception Out_of_sync

let () =
  Callback.register_exception "ogg_exn_not_enough_data" Not_enough_data ;
  Callback.register_exception "ogg_exn_bad_data" Bad_data ;
  Callback.register_exception "ogg_exn_out_of_sync" Out_of_sync 

module Page = 
struct
  type t = string*string

  external serialno : t -> nativeint = "ocaml_ogg_page_serialno"

  external eos : t -> bool = "ocaml_ogg_page_eos"

  external bos : t -> bool = "ocaml_ogg_page_bos"

  external packets : t -> int = "ocaml_ogg_page_packets"

  external continued : t -> bool = "ocaml_ogg_page_continued"

  external version : t -> int = "ocaml_ogg_page_version"

  external granulepos : t -> Int64.t = "ocaml_ogg_page_granulepos"

  external pageno : t -> nativeint = "ocaml_ogg_page_pageno"

  external set_checksum : t -> unit = "ocaml_ogg_page_checksum_set"

end

module Stream =
struct
  type stream

  type packet

  external create : nativeint -> stream = "ocaml_ogg_stream_init"

  let create ?(serial = Random.nativeint (Nativeint.of_int 0x3FFFFFFF)) () = create serial

  external eos : stream -> bool = "ocaml_ogg_stream_eos"

  external get_page : stream -> Page.t = "ocaml_ogg_stream_pageout"

  external get_packet : (unit -> unit) -> stream -> packet = "ocaml_ogg_stream_packetout"

  let get_packet ?(sync=ref false) s = 
    let callback () = sync := true in
    get_packet callback s

  external peek_packet : stream -> packet = "ocaml_ogg_stream_packetpeek"

  external put_packet : stream -> packet -> unit = "ocaml_ogg_stream_packetin"

  external put_page : stream -> Page.t -> unit = "ocaml_ogg_stream_pagein"

  external flush_page : stream -> Page.t = "ocaml_ogg_flush_stream"

  (** Backward compatibility *)
  type t = stream

  let s_o_f (h,b) = h ^ b

  let pageout s = s_o_f (get_page s)

  let pagesout s = 
    let rec f v = 
      try
        let n = pageout s in
        f (v ^ n)
      with
        | Not_enough_data -> v
    in
    f ""

  let flush s = 
    let rec f v = 
      try
        let v = v ^ (s_o_f (flush_page s)) in
        f v
      with
        | Not_enough_data -> v
    in
    f ""

  let pagesout_eos s = 
    let rec f v = 
      let p = flush_page s in
      let v = v ^ (s_o_f p) in
      if Page.eos p then 
       v
      else
       f v
    in
    f ""

end

module Sync =
struct
  (** Internal type for sync state *)
  type sync

  (** External type for sync state. References the C sync structure, and the read function *)
  type t = ((int -> string*int)*sync) ref

  external create : unit -> sync = "ocaml_ogg_sync_init"

  let create f = 
    ref (f,create ())

  let create_from_file f = 
    let fd = Unix.openfile f [Unix.O_RDONLY] 0o400 in
    try
      create (fun n -> 
                let s = String.create n in
                let r = Unix.read fd s 0 n in
                s,r),fd
    with
      | e -> Unix.close fd; raise e

  external read : (int -> string*int) -> sync -> Page.t = "ocaml_ogg_sync_read"

  let read s = 
    let (f,s) = !s in
    read f s

  external reset : sync -> unit = "ocaml_ogg_sync_reset"

  let reset ?read_func x = 
    let (f,s) = !x in
    reset s;
    match read_func with
      | None -> x := (f,s)
      | Some v -> x := (v,s)

end

