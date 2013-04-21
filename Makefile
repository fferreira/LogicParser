default: native

native:
	ocamlbuild -use-ocamlfind -yaccflags --explain lparser.native

byte:
	ocamlbuild -use-ocamlfind -yaccflags --explain lparser.byte

clean:
	ocamlbuild -clean

# doc:
# 	ocamlbuild -use-menhir -docflag -keep-code -lib unix tt.docdir/index.html
