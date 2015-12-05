TO RUN OUR CODE:
ocamlbuild -use-ocamlfind -pkgs csv -use-menhir -libs str,unix main.byte

THEN:
./main.byte