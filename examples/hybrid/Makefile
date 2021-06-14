walker:
	probzeluc -s main walker_cont.zls
	ocamlfind ocamlc -g -linkpkg -package probzelus walker_cont.ml main.ml -o main

clean:
	rm -rf *.cmo *.cmi *.zci
	rm -rf walker_cont.ml main.ml main