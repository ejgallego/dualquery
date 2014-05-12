# Program main file
DQ=src/rundq

# Ocamlbuild
# Little bit of a hack for the inline
OCBOPTS=-tag native -cflags -unsafe,-I,../parmap/_build -lflags -unsafe,-I,../parmap/_build -lib str -lib unix -lib bigarray -lib parmap -ocamlopt 'ocamlopt -inline 20'
# OCAMLBUILD=ocamlbuild $(OCBOPTS)
OCAMLBUILD=ocamlbuild -classic-display $(OCBOPTS)

.PHONY: rundq clean clean-cplex rundq.byte rundq.native rundq.d.byte rundq.d.native

VERSION=native

all: rundq

parmap::
	make -C parmap

rundq: rundq.$(VERSION)
	cp rundq.$(VERSION) rundq


rundq.byte: parmap
	$(OCAMLBUILD) $(DQ).byte

rundq.native: parmap
	$(OCAMLBUILD) $(DQ).native

rundq.p.byte: parmap
	$(OCAMLBUILD) $(DQ).p.byte

rundq.d.byte: parmap
	$(OCAMLBUILD) $(DQ).d.byte

rundq.p.native: parmap
	$(OCAMLBUILD) $(DQ).p.native

clean::
	make -C parmap clean
	rm -f cplex_dq.lp
	rm -f cplex.cfg
	rm -f *.log
	rm -f rundq
	rm -f ./$(DQ)
	$(OCAMLBUILD) -clean

clean-cplex::
	rm -f cplex.log
