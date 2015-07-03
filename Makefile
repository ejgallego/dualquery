# Program main file
DQ=src/rundq
MW=src/runmw

# Ocamlbuild
# Little bit of a hack for the inline
OCBOPTS=-tag native -cflags -unsafe,-I,../parmap/_build -lflags -unsafe,-I,../parmap/_build -lib str -lib unix -lib bigarray -lib parmap -ocamlopt 'ocamlopt -inline 20'
# OCAMLBUILD=ocamlbuild $(OCBOPTS)
OCAMLBUILD=ocamlbuild -classic-display $(OCBOPTS)

.PHONY: rundq runmw clean clean-cplex

VERSION=native

all: rundq
# all: rundq runmw
# all: src/$(TARGET).cmo

TARGET=dummy
src/$(TARGET).cmo:
	$(OCAMLBUILD) src/$(TARGET).cmo

parmap::
	make -C parmap

rundq: parmap
	$(OCAMLBUILD) $(DQ).$(VERSION)
	cp rundq.$(VERSION) rundq

runmw: parmap
	$(OCAMLBUILD) $(MW).$(VERSION)
	cp runmw.$(VERSION) runmw

clean::
	make -C parmap clean
	rm -f cplex_dq.lp
	rm -f cplex.cfg
	rm -f *.log
	rm -f rundq runmw
	rm -f ./$(DQ) ./$(MW)
	$(OCAMLBUILD) -clean

clean-cplex::
	rm -f cplex.log
