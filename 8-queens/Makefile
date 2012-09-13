COMPILER="ocamlopt"
OBJ_EXT="cmx"


compile:
	@mkdir -p bin
	@ocamlfind $(COMPILER) -thread -linkpkg -package batteries \
		-o bin/eight_queens \
		   src/eight_queens.ml


clean:
	@rm -rf bin
	@find src \
		    -iname '*.o' \
		-or -iname '*.cmi' \
		-or -iname "*.$(OBJ_EXT)" \
		| xargs rm
