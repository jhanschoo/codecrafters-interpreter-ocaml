(lang dune 3.16)

(name codecrafters_interpreter)

(generate_opam_files true)

(using menhir 3.0)

(package
 (name codecrafters_interpreter)
 (maintainers "CodeCrafters <hello@codecrafters.io>")
 (authors "CodeCrafters <hello@codecrafters.io>")
 (synopsis "Starter code for the Build your own Interpreter challenge")
 (homepage "https://codecrafters.io")
 (bug_reports
  "https://github.com/codecrafters-io/build-your-own-interpreter/issues")
 (license "MIT")
 (allow_empty true)
 (version 0.1)
 (depends
  (ocaml
   (>= 5.2.0))
  core
  menhir
  sedlex))
