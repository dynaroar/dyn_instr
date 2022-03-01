# dyn_instr

- Install opam: https://opam.ocaml.org/doc/Install.html
- Install OCaml 4.05.0:
    ```
    opam switch create 4.05.0
    ```
- Install Dune, CIL and other dependencies:
    ```
    opam install dune cil num ocamlbuild ocamlfind
    ```
- Compile and run:
    ```
    dune exec src/instr.exe input.c
    ```
    The instrumentation result is the file `input_instr.c` in the same folder as `input.c`.
- If you want to use `ocaml-lsp-server` with an IDE such as Visual Studio Code or Emacs, you need to install OCaml >= 4.06.0 and compile and install CIL from https://github.com/letonchanh/cil.
