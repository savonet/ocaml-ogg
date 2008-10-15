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

(**
  * Functions for manipulating ogg streams files using libogg.
  *
  * @author Samuel Mimram, Romain Beauxis
  *)

exception Not_enough_data
exception Bad_data
exception Out_of_sync

(**
  * The [page] struct encapsulates the data for an Ogg page.
  *
  * Ogg pages are the fundamental unit of framing and interleave in an ogg
  * bitstream. They are made up of packet segments of 255 bytes each. There can
  * be as many as 255 packet segments per page, for a maximum page size of a
  * little under 64 kB. This is not a practical limitation as the segments can be
  * joined across page boundaries allowing packets of arbitrary size. In practice
  * pages are usually around 4 kB.
  *)
module Page :
sig
  (** A page is a header and a body *)
  type t = string*string

  (** 
    * Returns the unique serial number for the logical bitstream of this page. 
    * Each page contains the serial number for the logical bitstream that it belongs to.*)
  val serialno : t -> int

  (** 
    * Indicates whether this page is at the end of the logical bitstream. *)
  val eos : t -> bool

  (** 
    * Indicates whether this page is at the begining of the logical bitstream. *)
  val bos : t -> bool

  (**
    * Indicates whether this page contains packet data which has been 
    * continued from the previous page. *)
  val continued : t -> bool

  (** 
    * Returns the number of packets that are completed on this page. 
    * If the leading packet is begun on a previous page, but ends on this page, it's counted. 
    *
    * If a page consists of a packet begun on a previous page, and a new packet begun 
    * (but not completed) on this page, the return will be:
    *
    * [packets page] will return [1],
    * [continued paged] will return [true]
    *
    * If a page happens to be a single packet that was begun on a previous page, 
    * and spans to the next page (in the case of a three or more page packet), the return will be:
    *
    * [packets page] will return 0,
    * [continued page] will return [true].*)
  val packets : t -> int

  (** 
    * This function returns the version of ogg_page used in this page. 
    * In current versions of libogg, all ogg_page structs have the same version, 
    * so [0] should always be returned. *)
  val version : t -> int

  (**
    * Returns the exact granular position of the packet data contained at the end of this page. 
    *
    * This is useful for tracking location when seeking or decoding. 
    *
    * For example, in audio codecs this position is the pcm sample number and 
    * in video this is the frame number.*)
  val granulepos : t -> Int64.t

  (**
    * Returns the sequential page number. 
    * 
    * This is useful for ordering pages or determining when pages have been lost. *)
  val pageno : t -> int

  (**
    * Checksums an ogg_page. *)
  val set_checksum : t -> unit

end

module Sync :
sig
  type t

  (** 
    * This function is used to initialize a [Sync.t] to a known initial value 
    * in preparation for manipulation of an Ogg bitstream. 
    *
    * The function passed is used to fill the stream with new data. It receives a number of bytes to read
    * and returns a string read and its size. *)
  val create : (int -> string*int) -> t

  (**
    * Wrapper around [create] to open a file as the ogg stream. *)
  val create_from_file : string -> (t*Unix.file_descr)

  (** 
    * Read a page from [Sync.t] 
    *
    * Raises [Not_enough_data] if the reading function returned an empty string. *)
  val read : t -> Page.t

  (** 
    * This function is used to reset the internal counters of the [Sync.t] to initial values.
    *
    * [read_func] is optional and is a new function to read new data. *)
  val reset : ?read_func:(int -> string*int) -> t -> unit
end


module Stream :
sig
  (**
    * The [stream] values track the current encode/decode state of the
    * current logical bitstream.
    *)
  type stream

  (**
    * A data packet to pass to the decoder *)
  type packet

  (**
    * Create a [stream].
    *)
  val create : ?serial:int -> unit -> stream

  (** Returns true if the end of stream has been reached. *)
  val eos : stream -> bool

  (**
    * This function forms packets into pages. Internally,
    * it assembles the accumulated packet bodies into an Ogg page
    * suitable for writing to a stream.
    *
    * This function will only return a page when a "reasonable" amount of packet
    * data is available. Normally this is appropriate since it limits the overhead
    * of the Ogg page headers in the bitstream. Call
    * [flush_page] if immediate page generation is desired. This may be
    * occasionally necessary, for example, to limit the temporal latency of a
    * variable bitrate stream.
    *)
  val get_page : stream -> Page.t

  (** 
    * This function adds a complete page to the bitstream. 
    *
    * In a typical decoding situation, this function would be called after 
    * using [Sync.read] to create a valid [Page.t] 
    * 
    * Raises [Bad_data] if the serial number of the page did not match the 
    * serial number of the bitstream, or the page version was incorrect. *)
  val put_page : stream -> Page.t -> unit

  (**
    * This function assembles a data packet for output 
    * to the codec decoding engine.
    *
    * Each successive call returns the next complete packet built from those segments.
    * In a typical decoding situation, this should be used after calling 
    * [put_page] to submit a page of data to the bitstream. 
    *
    * This function should *not* be used. Because of ocaml's paradigm, it is necessary 
    * to copy each packet since they are only valid until this function is called again.
    * When dealing with many packets, this will lead to multiple unecessary memory allocation 
    * and desallocation.
    *
    * Raises [Not_enough_data] if more data is needed and another page should be submitted.
    *
    * Raises [Out_of_sync]  if we are out of sync and there is a gap in the data
    *
    * [sync] is a reference to a boolean set to true if the stream had a gap. *)
  val get_packet : ?sync:(bool ref) -> stream -> packet

  (**
    * This function assembles a data packet for output
    * to the codec decoding engine without advancing the stream.
    *
    * Raises [Not_enough_data] if more data is needed and another page should be submitted.
    *
    * Raises [Out_of_sync]  if we are out of sync and there is a gap in the data *)
  val peek_packet : stream -> packet

  (**
    * This function checks for remaining packets inside the stream and forces
    * remaining packets into a page, regardless of the size of the page.
    *
    * This should only be used when you want to flush an undersized page from the
    * middle of the stream. Otherwise, [get_page] should always be used.
    *
    * This function can be used to verify that all packets have been flushed.
    *
    * Raises [Not_enough_data] if  all packet data has already been flushed into pages,
    * and there are no packets to put into the page.
    *)
  val flush_page : stream -> Page.t

  (** Backward compatibility *)

  type t = stream

  val pageout : t -> string

  val pagesout : t -> string

  val flush : t -> string

  val pagesout_eos : t -> string

end

