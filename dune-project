(lang dune 3.0)

(name dyn_instr)

(generate_opam_files true)

(source
 (github letonchanh/dyn_instr))

(authors "Ton Chanh Le")

(maintainers "Ton Chanh Le")

(license LICENSE)

(documentation https://url/to/documentation)

(using menhir 2.1)

(package
 (name dyn_instr)
 (synopsis "CIL Instrumentation for Dynamic Analysis")
 (description "CIL Instrumentation for Dynamic Analysis")
 (depends ocaml dune cil)
 (allow_empty)
 (tags
  (dynamic analysis)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
