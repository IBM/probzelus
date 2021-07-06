OCAMLC = ocamlfind ocamlc -linkpkg -package probzelus

%_main.exe: %.zls
	probzeluc -deps -s main $<
	$(OCAMLC) $(<:.zls=.ml) main.ml -o $@

clean:
	rm -rf *.ml *.cmo *.cmi *.zci *.exe
