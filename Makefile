OCB_FLAGS = -use-ocamlfind
OCB =           ocamlbuild $(OCB_FLAGS)

test:	all
	./main.byte

all:
	 $(OCB) main.byte

clean:
	$(OCB) -clean
