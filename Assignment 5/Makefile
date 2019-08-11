default: compile

NAME = lab5

all: compile test

compile: clean
	ocamlfind ocamlc -c -package num ${NAME}.mli ${NAME}.ml

test:
	ocamlfind ocamlc -o tests_${NAME} \
	  -package oUnit,num -linkpkg \
	  ${NAME}.cmo tests_${NAME}.ml
	./tests_${NAME}

testc:
	ocamlfind ocamlc -o tests_${NAME}c \
	  -package oUnit,num -linkpkg \
	  ${NAME}.cmo tests_${NAME}c.ml
	./tests_${NAME}c

clean:
	rm -f *.cm* *.log *.cache tests_${NAME} tests_${NAME}c
