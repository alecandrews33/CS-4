default: compile

ML = \
  pentomino_pieces.ml \
  pentomino_types.ml \
  pentominoes.ml \
  pentomino_utils.ml \
  pentomino_boards.ml \
  binmat.ml \
  solver.ml

MLI = \
  pentomino_pieces.mli \
  pentomino_types.mli \
  pentominoes.mli \
  pentomino_utils.mli \
  pentomino_boards.mli \
  binmat.mli \
  solver.mli

CMO = \
  pentomino_pieces.cmo \
  pentomino_types.cmo \
  pentominoes.cmo \
  pentomino_utils.cmo \
  pentomino_boards.cmo \
  binmat.cmo \
  solver.cmo

cmi: ${MLI}
	ocamlfind ocamlc -c ${MLI}

cmo: ${ML} cmi
	ocamlfind ocamlc -c ${ML}

compile: main.ml cmo
	ocamlfind ocamlc -c main.ml
	ocamlfind ocamlc ${CMO} main.cmo -o pentomino

run_tests_binmat:
	ocamlfind ocamlc -c binmat.mli
	ocamlfind ocamlc -c binmat.ml
	ocamlfind ocamlc -o tests_binmat \
	  -package oUnit,str -linkpkg \
	  binmat.cmo tests_binmat.ml
	./tests_binmat

run_tests_solver:
	ocamlfind ocamlc -c binmat.mli
	ocamlfind ocamlc -c binmat.ml
	ocamlfind ocamlc -c solver.mli
	ocamlfind ocamlc -c solver.ml
	ocamlfind ocamlc -o tests_solver \
	  -package oUnit,str -linkpkg \
	  binmat.cmo solver.cmo tests_solver.ml
	./tests_solver

run_tests_pentominoes:
	ocamlfind ocamlc -c pentomino_pieces.mli
	ocamlfind ocamlc -c pentomino_pieces.ml
	ocamlfind ocamlc -c pentomino_types.mli
	ocamlfind ocamlc -c pentomino_types.ml
	ocamlfind ocamlc -c pentominoes.mli
	ocamlfind ocamlc -c pentominoes.ml
	ocamlfind ocamlc -o tests_pentominoes \
	  -package oUnit,str -linkpkg \
	  pentomino_pieces.cmo pentomino_types.cmo pentominoes.cmo tests_pentominoes.ml
	./tests_pentominoes



clean:
	rm -f *.cmo *.cmi pentomino
	rm -f *.log *.cache
	rm -f tests_binmat tests_solver tests_pentominoes


