0.7.4 (2023-05-07)
=====
* Add `Stream.terminate`

0.7.3 (14-08-2022)
=====
* Fix build error with OCaml `4.14`

0.7.2 (28-06-2022)
=====
* Use `caml_alloc_custom_mem` for packet allocations (#2348)

0.7.1 (07-03-2022)
=====
* Added decoder API for audio big array.

0.7.0 (06-03-2021)
=====
* Switch to `dune`

0.6.1 (19-10-2020)
=====
* Revert back to autoconf (See: savonet/liquidsoap#1378)

0.6.0 (07-10-2020)
=====
* Switch to read callbacks with bytes in Ogg.Sync.

0.5.2 (07-10-2017)
=====
* Fix compilation with OCaml 4.06

0.5.1 (11-04-2017)
=====
* Install .cmx files

0.5.0 (03-08-2015)
=====
* Removed old Ogg.Stream backward compatibility functions.
* Switch to Bytes API.

0.4.5 (08-05-2013)
=====
* Added optional fill parameter to [get_page]
  to try to limit ogg logical pages size.

0.4.4 (18-02-2013)
=====
* Added Ogg.Internal_error exception.
* Updated configure.

0.4.3 (04-10-2011)
=====
* New Ogg_demuxer.End_of_stream exception, raised at the end of
  logical streams. The former incorrect behavior was to raise
  Ogg.End_of_stream, which is intended to signify the end of data.

0.4.2 (02.07.2011)
=====
* Added [Ogg_demuxer] module to
  decode ogg streams.
* Added the following functions:
  - [Sync.sync]
  - [Stream.{peek_granulepos,
             skip_packet,
             packet_granulepos}]
* Fixed incorrect mention of INRIA in 
  license headers.

0.4.1 (04-09-2010)
=====
* Raise Out_of_sync in Sync.read
  when data is not synced, e.g.
  ogg_sync_pageout returned -1.

0.4.0 (19-08-2010)
=====
* Removed sync reference in Stream.get_packet 
  and actually raise Out_of_sync exception. 
  No packet should be returned when the stream is 
  out of sync.

0.3.1 (12-10-2009)
=====
* Added support for --enable-debugging configure option
* Added NO_CUSTOM to build
  in standard mode.
* Added prefix to main compilation variables
  if passed to configure.
* Makefile now honnors LIBDIRS
  variable for linking against libraries
  located in other places than then standard
  ones.

0.3.0 (17-02-2009)
=====
* Added file descriptor to Ogg.Sync.create_from_file
  in order to be able to close it and let the Gc clean
  everything..
* Added Ogg.Stream.eos
* Added Ogg.Stream.peek_packet to peek a packet
  without advancing the stream. Usefull to test first
  packet when parsing ogg streams.

0.2.0 (16-04-2008
=====
* More portable invokation of make
* Now installs .cmx file
* Reworked API, added more functions.
  + Binding is now ready to decode theora 
    and multiplexed streams
  + Compatibility functions are available, 
    old code should compile on the new
    API

0.1.1 (05-11-2007)
=====
* Cleared out license headers.
  Now using LGPL + linking exception
Acked-by: smimram

0.1.0 (16-10-2007)
=====
* Initial release
