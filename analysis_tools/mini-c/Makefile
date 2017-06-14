
OCAMLBUILD = ocamlbuild

SOURCES = lexer.ml parser.ml utils.ml ast.ml graph.ml
INTERFACE_RESULT = minic.byte

OCAMLLDFLAGS = -g
PACKS = str

all: $(INTERFACE_RESULT)

clean:
	rm -f *.byte
	rm -f *.native
	rm -f *.cmi
	rm -f *.cmo
	$(OCAMLBUILD) -clean

interface: $(INTERFACE_RESULT)

$(INTERFACE_RESULT): $(SOURCES) minic.ml
	$(OCAMLBUILD) $(INTERFACE_RESULT) -pkgs $(PACKS)