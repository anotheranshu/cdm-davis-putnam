all: build

build:
	ocamllex src_lex_parse/lexer.mll
	ocamlyacc src_lex_parse/parser.mly
	mv src_lex_parse/lexer.ml src/lexer.ml
	mv src_lex_parse/parser.ml src/parser.ml
	rm src_lex_parse/parser.mli
	corebuild src/davisputnam.native

clean:
	rm -r _build
	rm davisputnam.native