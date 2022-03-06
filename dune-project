(lang dune 2.8)
(version 0.7.1)
(name ogg)
(source (github savonet/ocaml-ogg))
(license GPL-2.0)
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
  dune
  dune-configurator)
)
