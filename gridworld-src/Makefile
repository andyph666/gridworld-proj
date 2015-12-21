compiler: grid.ml objects
	ocamlc -c grid.ml
	ocamlc -o gw ast.cmo parser.cmo scanner.cmo compile.cmo analyzer.cmo grid.cmo

objects: scanner parser generator
	ocamlc -c ast.ml sast.ml parser.mli scanner.ml parser.ml analyzer.ml compile.ml
generator: analyzer.ml compile.ml
parser: parser.mly
	ocamlyacc -v parser.mly
scanner: scanner.mll
	ocamllex scanner.mll
.PHONY: test
test: compiler
	./testall.sh

.PHONY: clean
clean:
	rm -f parser.mli scanner.ml parser.ml parser.output *.cmo *cmi test-*.py  test-*.i.* grid *~
