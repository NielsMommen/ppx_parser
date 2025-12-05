(lang dune 2.9)

(name ppx_parser)

(generate_opam_files true)

(source
 (github NielsMommen/ppx_parser))

(authors "Niels Mommen <nielsmommen@hotmail.com>")

(maintainers "Niels Mommen <nielsmommen@hotmail.com>")

(license ISC)

(package
 (name ppx_parser)
 (synopsis "OCaml PPX extension for writing stream parsers")
 (description
  "\n This library is an OCaml ppx rewriter for writing stream parsers. \n Its notation is close to the notation of Camlp4 stream parsers. \n Hence, it can be used as a replacement for projects \n that still rely on the stream parser notation of Camlp4. \n \n For newer projects, it is advised to use lexer and parser generators.\n ")
 (depends
  (ocaml
   (>= 4.8.0))
  dune
  (ppxlib
   (>= 0.36.0))
  (alcotest
   (and
    :with-test
    (>= 1.2.0)))
  (ppx_deriving :with-test)
  (camlp-streams :with-test))
 (tags ("stream parser")))
