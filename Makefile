all: pichub_core

pichub_core:
	(cd core; make all)

clean:
	(cd core; make clean)
