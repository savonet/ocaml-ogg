(lang dune 2.8)
(version 0.8.0)
(name ogg)
(source (github savonet/ocaml-ogg))
(license "LGPL-2.1-or-later WITH OCaml-LGPL-linking-exception")
(authors "The Savonet Team <savonet-users@lists.sourceforge.net>")
(maintainers "The Savonet Team <savonet-users@lists.sourceforge.net>")

(generate_opam_files true)
(use_standard_c_and_cxx_flags false)

(package
 (name ogg)
 (synopsis "Bindings to libogg")
 (depends
  conf-libogg
  conf-pkg-config
  (ocaml (>= 4.08.0))
  dune
  dune-configurator)
)
