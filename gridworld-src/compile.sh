ocamllex scanner.mll
echo "ocamllex scanner.mll"
ocamlyacc parser.mly
echo "ocamlyacc parser.mly"
ocamlc -c ast.ml
echo "ocamlc -c ast.ml"
ocamlc -c parser.mli
echo "ocamlc -c parser.mli"
ocamlc -c scanner.ml
echo "ocamlc -c scanner.ml"
ocamlc -c parser.ml
echo "ocamlc -c parser.ml"
ocamlc -c compile.ml
echo "ocamlc -c compile.ml"
ocamlc -c grid.ml
echo "ocamlc -c grid.ml"
ocamlc -o gw parser.cmo scanner.cmo compile.cmo grid.cmo
echo "ocamlc -o gw parser.cmo scanner.cmo grid.cmo"
