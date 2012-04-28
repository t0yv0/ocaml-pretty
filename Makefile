test:
	ocamlbuild pretty_test.native
	./pretty_test.native

docs: pretty.docdir/index.html
	git checkout gh-pages
	rm *.html *.css *.stamp
	cp pretty.docdir/* .
	git add .
	git commit -am 'Auto-generated documentation'
	git checkout master

pretty.docdir/index.html: pretty.mli pretty.ml
	echo "Pretty" > pretty.odocl
	ocamlbuild pretty.docdir/index.html
