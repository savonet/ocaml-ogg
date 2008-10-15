/*
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
 */


#include <caml/alloc.h>
#include <caml/callback.h>
#include <caml/custom.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>

#include <caml/signals.h>

#include <ogg/ogg.h>

#include <string.h>
#include <assert.h>
#include <stdio.h>

#include "ocaml-ogg.h"

/** Page manipulation **/

value value_of_page(ogg_page *op)
{
  CAMLparam0();
  CAMLlocal3(v,header,body);
  header = caml_alloc_string(op->header_len);
  memcpy(String_val(header),op->header,op->header_len);

  body = caml_alloc_string(op->body_len);
  memcpy(String_val(body),op->body,op->body_len);

  v = caml_alloc_tuple(2);
  Store_field(v,0,header);
  Store_field(v,1,body);

  CAMLreturn(v);
}

ogg_page *page_of_value(value v,ogg_page *page)
{
  page->header = (unsigned char *)String_val(Field(v,0));
  page->header_len = caml_string_length(Field(v,0));

  page->body = (unsigned char *)String_val(Field(v,1));
  page->body_len = caml_string_length(Field(v,1));

  return page;
}

CAMLprim value ocaml_ogg_page_serialno(value page)
{
  CAMLparam1(page);
  ogg_page op;
  CAMLreturn(Val_long(ogg_page_serialno(page_of_value(page,&op))));
}

CAMLprim value ocaml_ogg_page_eos(value page)
{
  CAMLparam1(page);
  ogg_page op;
  CAMLreturn(Val_bool(ogg_page_eos(page_of_value(page,&op))));
}

CAMLprim value ocaml_ogg_page_bos(value page)
{
  CAMLparam1(page);
  ogg_page op;
  CAMLreturn(Val_bool(ogg_page_bos(page_of_value(page,&op))));
}

CAMLprim value ocaml_ogg_page_packets(value page)
{
  CAMLparam1(page);
  ogg_page op;
  CAMLreturn(Val_int(ogg_page_packets(page_of_value(page,&op))));
}

CAMLprim value ocaml_ogg_page_continued(value page)
{
  CAMLparam1(page);
  ogg_page op;
  CAMLreturn(Val_bool(ogg_page_continued(page_of_value(page,&op))));
}

CAMLprim value ocaml_ogg_page_version(value page)
{
  CAMLparam1(page);
  ogg_page op;
  CAMLreturn(Val_int(ogg_page_version(page_of_value(page,&op))));
}

CAMLprim value ocaml_ogg_page_granulepos(value page)
{
  CAMLparam1(page);
  ogg_page op;
  CAMLreturn(Val_int(ogg_page_granulepos(page_of_value(page,&op))));
}

CAMLprim value ocaml_ogg_page_pageno(value page)
{
  CAMLparam1(page);
  ogg_page op;
  CAMLreturn(Val_long(ogg_page_pageno(page_of_value(page,&op))));
}

CAMLprim value ocaml_ogg_page_checksum_set(value page)
{
  CAMLparam1(page);
  ogg_page op;
  ogg_page_checksum_set(page_of_value(page,&op));
  CAMLreturn(Val_unit);
}

/***** Sync state *****/

static void finalize_sync_state(value s)
{
  ogg_sync_destroy(Sync_state_val(s));
}

static struct custom_operations sync_state_ops =
{
  "ocaml_ogg_sync_state",
  finalize_sync_state,
  custom_compare_default,
  custom_hash_default,
  custom_serialize_default,
  custom_deserialize_default
};

CAMLprim value ocaml_ogg_sync_init()
{
  CAMLparam0();
  CAMLlocal1(sync);
  ogg_sync_state *oy = malloc(sizeof(ogg_sync_state));

  ogg_sync_init(oy);
  sync = caml_alloc_custom(&sync_state_ops, sizeof(ogg_sync_state*), 1, 0);
  Sync_state_val(sync) = oy;

  CAMLreturn(sync);
}

CAMLprim value ocaml_ogg_sync_reset(value oy)
{
  CAMLparam1(oy);
  ogg_sync_reset(Sync_state_val(oy));

  CAMLreturn(Val_unit);
}

CAMLprim value ocaml_ogg_sync_read(value callback, value oy)
{
  CAMLparam2(callback,oy);
  CAMLlocal3(ret,s,bytes);
  ogg_sync_state *sync = Sync_state_val(oy);
  int len = 4096;
  ogg_page page;

  while (ogg_sync_pageout(sync, &page) != 1)
  {
    ret = caml_callback(callback,Val_int(len));
    s = Field(ret,0);
    bytes = Field(ret,1);
    if (Int_val(bytes) == 0)
      caml_raise_constant(*caml_named_value("ogg_exn_not_enough_data"));

    char *buffer = ogg_sync_buffer(sync,Int_val(bytes));
    memcpy(buffer,String_val(s),Int_val(bytes));
    ogg_sync_wrote(sync,Int_val(bytes));
  }

  CAMLreturn(value_of_page(&page));
}

/***** Stream state ******/

static void finalize_stream_state(value s)
{
  // This also free the argument
  ogg_stream_destroy(Stream_state_val(s));
}

static struct custom_operations stream_state_ops =
{
  "ocaml_ogg_stream_state",
  finalize_stream_state,
  custom_compare_default,
  custom_hash_default,
  custom_serialize_default,
  custom_deserialize_default
};

static void finalize_packet(value s)
{
  ogg_packet *op = Packet_val(s);
  free(op->packet);
  free(op);
}

static inline ogg_packet *copy_packet(ogg_packet *op)
{
  ogg_packet *nop = malloc(sizeof(ogg_packet));
  nop->packet = malloc(op->bytes);
  memcpy(nop->packet,op->packet,op->bytes);
  nop->bytes = op->bytes;
  nop->b_o_s = op->b_o_s;
  nop->e_o_s = op->e_o_s;
  nop->granulepos = op->granulepos;
  nop->packetno = op->packetno;

  return nop;
}

static struct custom_operations packet_ops =
{
  "ocaml_ogg_packet",
  finalize_packet,
  custom_compare_default,
  custom_hash_default,
  custom_serialize_default,
  custom_deserialize_default
};

CAMLprim value ocaml_ogg_stream_init(value serial)
{
  CAMLparam0();
  CAMLlocal1(ans);
  ogg_stream_state *os = malloc(sizeof(ogg_stream_state));

  ogg_stream_init(os, Int_val(serial));
  ans = caml_alloc_custom(&stream_state_ops, sizeof(ogg_stream_state*), 1, 0);
  Stream_state_val(ans) = os;

  CAMLreturn(ans);
}

CAMLprim value ocaml_ogg_stream_eos(value o_stream_state)
{
  CAMLparam1(o_stream_state);
  ogg_stream_state *os = Stream_state_val(o_stream_state);
  CAMLreturn(Val_bool(ogg_stream_eos(os)));
}

CAMLprim value ocaml_ogg_stream_pageout(value o_stream_state)
{
  CAMLparam1(o_stream_state);
  ogg_stream_state *os = Stream_state_val(o_stream_state);
  ogg_page og;

  if(!ogg_stream_pageout(os, &og))
    caml_raise_constant(*caml_named_value("ogg_exn_not_enough_data"));

  CAMLreturn(value_of_page(&og));
}

CAMLprim value ocaml_ogg_stream_pagein(value o_stream_state, value page)
{
  CAMLparam2(o_stream_state, page);
  ogg_stream_state *os = Stream_state_val(o_stream_state);
  ogg_page op;

  if (ogg_stream_pagein(os, page_of_value(page,&op)) != 0)
    caml_raise_constant(*caml_named_value("ogg_exn_bad_data"));

  CAMLreturn(Val_unit);
}

CAMLprim value ocaml_ogg_stream_packetout(value callback,value o_stream_state)
{
  CAMLparam2(callback,o_stream_state);
  CAMLlocal1(packet);
  ogg_stream_state *os = Stream_state_val(o_stream_state);
  ogg_packet op;
  int ret = ogg_stream_packetout(os,&op);

  if (ret == 0) 
    caml_raise_constant(*caml_named_value("ogg_exn_not_enough_data"));

  if (ret == -1)
    caml_callback(callback,Val_unit);

  packet = caml_alloc_custom(&packet_ops, sizeof(ogg_packet*), 1, 0);
  Packet_val(packet) = copy_packet(&op);

  CAMLreturn(packet);
}

CAMLprim value ocaml_ogg_stream_packetpeek(value o_stream_state)
{
  CAMLparam1(o_stream_state);
  CAMLlocal1(packet);
  ogg_stream_state *os = Stream_state_val(o_stream_state);
  ogg_packet op;
  int ret = ogg_stream_packetpeek(os,&op);

  if (ret == 0)
    caml_raise_constant(*caml_named_value("ogg_exn_not_enough_data"));

  packet = caml_alloc_custom(&packet_ops, sizeof(ogg_packet*), 1, 0);
  Packet_val(packet) = copy_packet(&op);

  CAMLreturn(packet);
}

CAMLprim value ocaml_ogg_flush_stream(value o_stream_state)
{
  CAMLparam1(o_stream_state);
  ogg_stream_state *os = Stream_state_val(o_stream_state);
  ogg_page og;

  if(!ogg_stream_flush(os, &og))
    caml_raise_constant(*caml_named_value("ogg_exn_not_enough_data"));

  CAMLreturn(value_of_page(&og));
}

