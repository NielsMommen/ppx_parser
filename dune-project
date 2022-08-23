(lang dune 3.4)

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
 (description "
 This library is an OCaml ppx rewriter for writing stream parser. 
 Its notation is close to the notation of Camlp4 stream parsers. 
 Hence, it can be used as a replacement for projects 
 that still rely on the stream parser notation of Camlp4. 
 
 For newer projects, it is advised to use lexer and parser generators.
 ")
 (depends 
  ocaml 
  dune 
  ppxlib 
  (alcotest :with-test)
  (ppx_deriving :with-test))
 (tags
  ("stream parser")))