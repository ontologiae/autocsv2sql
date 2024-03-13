SRC_FILE = autocsv2sql.ml
EXE_FILE = autocsv2sql
INSTALL_PATH = /usr/local/bin

# Cible clean
clean:
	rm -f $(EXE_FILE)

# Cible build
build:
	ocamlfind ocamlopt -o $(EXE_FILE) -linkpkg -package batteries $(SRC_FILE)

# Cible install
install:
	cp $(EXE_FILE) $(INSTALL_PATH)

.PHONY: clean build install
