pretty.docdir/index.html: pretty.mli pretty.ml
	echo "Pretty" > pretty.odocl
	ocamlbuild pretty.docdir/index.html

test:
	ocamlbuild pretty_test.native
	./pretty_test.native
