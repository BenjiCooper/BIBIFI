TOP = ..
CC = ocamlc
DEPENDS = ast.cmo lexer.cmo parser.cmo graph.cmo utils.cmo minic.cmo

all: minic

clean:
	@echo “Cleaning directory”
	-@rm -rf minic
	-@rm -rf minic.cm[oi]

minic: $(DEPENDS) minic.ml
	$(CC) -o minic $(DEPENDS)

minic.cmo: minic.cmi minic.ml ast.cmi graph.cmi parser.cmi utils.cmi
	$(CC) -o minic.cmo -c minic.ml

minic.cmi: minic.mli ast.cmi graph.cmi parser.cmi utils.cmi
	$(CC) -o minic.cmi -c minic.mli

ast.cmo ast.cmi: ast.ml
	$(CC) -o ast -c ast.ml