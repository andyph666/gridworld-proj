OBJS = parser.cmo scanner.cmo compile.cmo grid.cmo

TESTS = \
hello \
gcd

# Choose one
YACC = ocamlyacc
# YACC = menhir --explain

TARFILES = Makefile testall.sh scanner.mll parser.mly \
	ast.ml bytecode.ml interpret.ml compile.ml execute.ml grid.ml \
	$(TESTS:%=tests/test-%.mc) \
	$(TESTS:%=tests/test-%.out)

gw : $(OBJS)
	ocamlc -o gw $(OBJS)

.PHONY : test
test : ./playgw testall.sh
	./testall.sh

scanner.ml : scanner.mll
	ocamllex scanner.mll

parser.ml parser.mli : parser.mly
	$(YACC) parser.mly

%.cmo : %.ml
	ocamlc -c $<

%.cmi : %.mli
	ocamlc -c $<

grid.tar.gz : $(TARFILES)
	cd .. && tar czf grid/grid.tar.gz $(TARFILES:%=grid/%)

.PHONY : clean
clean :
	rm -f gw parser.ml parser.mli scanner.ml testall.log \
	*.cmo *.cmi *.out *.diff


parser.cmi : ast.cmo
ast.cmo :
ast.cmx :
compile.cmo : ast.cmo
compile.cmx : ast.cmx
grid.cmo : scanner.cmo parser.cmi compile.cmo
grid.cmx : scanner.cmx parser.cmx compile.cmx
parser.cmo : ast.cmo parser.cmi
parser.cmx : ast.cmx parser.cmi
scanner.cmo : parser.cmi
scanner.cmx : parser.cmx
