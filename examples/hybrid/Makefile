OCAMLC = ocamlfind ocamlc -linkpkg 

hmm: hmm.zls
	probzeluc -s main hmm.zls
	$(OCAMLC) -package probzelus hmm.ml main.ml -o main

walker: walker.zls
	probzeluc -s main walker.zls
	$(OCAMLC) -package probzelus walker.ml main.ml -o main

ball: ball.zls
	probzeluc -deps -s main ball.zls
	$(OCAMLC) -package probzelus ball.ml main.ml -o main

sho: 
	probzeluc -deps -s main sho.zls
	$(OCAMLC) -package probzelus sho.ml main.ml -o main

planet:
	probzeluc -deps -s main planet.zls
	$(OCAMLC) -package probzelus planet.ml main.ml -o main


clean:
	rm -rf *.cmo *.cmi *.zci
	rm -rf walker.ml hmm.ml ball.ml planet.ml lotka-volterra.ml main.ml main