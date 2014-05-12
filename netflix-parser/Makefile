# Copyright (c) 2013-2014, The Trustees of the University of Pennsylvania
#  All rights reserved.
#
#  LICENSE: 3-clause BSD style.
#  See the LICENSE file for details on licensing.
OCBOPTS=-lib str -lib unix
OCAMLBUILD=ocamlbuild -classic-display $(OCBOPTS)

all: nfp

nfp: nfp.native
	cp nfp.native nfp
	cp nfp /home/egallego/media/netfix-data/

nfp.native: nfp.ml
	$(OCAMLBUILD) nfp.native

clean:
	$(OCAMLBUILD) -clean
	rm -f nfp

