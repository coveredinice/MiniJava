all: compile

compile: 
	ocamlc ast.mli

	ocamlyacc -v mp3.mly
	ocamlc -c mp3.mli

	ocamllex mp2.mll
	ocamlc -c mp2.ml

	ocamlc -c mp3.ml
	ocamlc -c mp3.mli
	
	ocamlc -c mp4.ml
	
	ocamlc -o mp4 mp2.cmo mp3.cmo mp4.cmo

test:
	echo '#use "main.ml";;\n#use "mp4.ml";;\nrun (parse_file "test1.java");;\nrun (parse_file "test2.java");;' | ocaml mp4.cmo mp3.cmo mp2.cmo
