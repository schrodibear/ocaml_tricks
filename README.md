Example of how recursive modules with precise mutual signatures can be defined in OCaml. Let's
assume there are three modules `M1`, `M2` and `M3`.

Each of them has three values: `a`, `b` and `c`, and one function — `do_it : unit -> unit`. `M1` should only see `a` and `b` from each module, `M2` — only `a` and `c`, `M3` — `b` and `c` and the `Main` module or other modules — only the function `do_it`. The problem comes from [this][1] question about "Friend modules".

Compile with `ocamlbuild -cflags "-bin-annot -g" main.native`.

[1]: "Question about friend modules in OCaml" http://stackoverflow.com/questions/5088527/friend-modules-in-ocaml
