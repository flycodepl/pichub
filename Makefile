all: pichub_core

pichub_core:
	(cd core; make)

clean:
	(cd core; make clean)
