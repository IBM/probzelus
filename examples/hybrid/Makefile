OCAMLC = ocamlfind ocamlc -linkpkg -package probzelus -package zelus-io

%_main.exe: %.zls
	probzeluc -I `zeluc -where`-io -deps -noreduce -s main $<
	$(OCAMLC) $(<:.zls=.ml) main.ml -o $@

clean:
	rm -rf *.ml *.cmo *.cmi *.zci *.exe
