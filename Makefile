#

INCLUDES=
COMPFLAGS=$(INCLUDES) -annot
OCAMLPP=

OCAMLC=ocamlc -g
OCAMLOPT=ocamlopt
OCAMLLEX=ocamllex
OCAMLYACC=ocamlyacc
CAMLP4O=camlp4o
OCAMLLIB:=`$(OCAMLC) -where`

ADDITIONAL_LIBS=str.cmxa
ADDITIONAL_LIBS_BYTE=str.cma

INSTALLDIR=$(OCAMLLIB)/stog

RM=rm -f
CP=cp -f
MKDIR=mkdir -p

SYSLIBS=unix.cmxa dynlink.cmxa
SYSLIBS_BYTE=unix.cma dynlink.cma

LIB_CMXFILES=stog_config.cmx \
	stog_misc.cmx \
	stog_tmap.cmx \
	stog_graph.cmx \
	stog_types.cmx \
	stog_find.cmx \
	stog_io.cmx \
	stog_info.cmx \
	stog_coms.cmx \
	stog_tmpl.cmx \
	stog_html.cmx

LIB_CMOFILES=$(LIB_CMXFILES:.cmx=.cmo)
LIB_CMIFILES=$(LIB_CMXFILES:.cmx=.cmi)

LIB=stog.cmxa
LIB_BYTE=$(LIB:.cmxa=.cma)

MAIN_CMXFILES=\
	stog_main.cmx

MAIN_CMOFILES=$(MAIN_CMXFILES:.cmx=.cmo)
MAIN_CMIFILES=$(MAIN_CMXFILES:.cmx=.cmi)

MAIN=stog
MAIN_BYTE=$(MAIN).byte

all: opt byte

opt: $(LIB) $(MAIN) $(CLIENT)
byte: $(LIB_BYTE) $(MAIN_BYTE) $(CLIENT_BYTE)

$(MAIN): $(LIB) $(MAIN_CMIFILES) $(MAIN_CMXFILES)
	$(OCAMLOPT) -verbose -linkall -o $@ $(COMPFLAGS) $(SYSLIBS) \
	$(ADDITIONAL_LIBS) $(LIB) $(MAIN_CMXFILES)

$(MAIN_BYTE): $(LIB_BYTE) $(MAIN_CMIFILES) $(MAIN_CMOFILES)
	$(OCAMLC) -linkall -o $@ $(COMPFLAGS) $(SYSLIBS_BYTE) \
	$(ADDITIONAL_LIBS_BYTE) $(LIB_BYTE) $(MAIN_CMOFILES)

$(LIB): $(LIB_CMIFILES) $(LIB_CMXFILES)
	$(OCAMLOPT) -a -o $@ $(LIB_CMXFILES)


$(LIB_BYTE): $(LIB_CMIFILES) $(LIB_CMOFILES)
	$(OCAMLC) -a -o $@ $(LIB_CMOFILES)

##########
install:
	$(MKDIR) $(INSTALLDIR)
	$(CP) $(LIB_CMIFILES) $(LIB) $(LIB_BYTE) $(LIB:.cmxa=.a) \
	$(INSTALLDIR)
	$(CP) $(MAIN) $(CLIENT)  `dirname \`which $(OCAMLC)\``/

#####
clean:
	$(RM) $(MAIN) $(MAIN_BYTE) *.cm* *.o *.a *.x *.annot

#############
.SUFFIXES: .mli .ml .cmi .cmo .cmx .mll .mly

stog_coms.cmo stog_coms.cmi: stog_coms.ml
	$(OCAMLC) $(COMPFLAGS) -c -pp "$(CAMLP4O)" $<
stog_coms.cmx stog_coms.cmi: stog_coms.ml
	$(OCAMLOPT) $(COMPFLAGS) -c -pp "$(CAMLP4O)" $<

%.cmi:%.mli
	$(OCAMLC) $(OCAMLPP) $(COMPFLAGS) -c $<

%.cmo:%.ml
	if test -f `dirname $<`/`basename $< .ml`.mli && test ! -f `dirname $<`/`basename $< .ml`.cmi ; then \
	$(OCAMLC) $(OCAMLPP) $(COMPFLAGS) -c `dirname $<`/`basename $< .ml`.mli; fi
	$(OCAMLC) $(OCAMLPP) $(COMPFLAGS) -c $<

%.cmi %.cmo:%.ml
	if test -f `dirname $<`/`basename $< .ml`.mli && test ! -f `dirname $<`/`basename $< .ml`.cmi ; then \
	$(OCAMLC) $(OCAMLPP) $(COMPFLAGS) -c `dirname $<`/`basename $< .ml`.mli; fi
	$(OCAMLC) $(OCAMLPP) $(COMPFLAGS) -c $<

%.cmx %.o:%.ml
	$(OCAMLOPT) $(OCAMLPP) $(COMPFLAGS) -c $<



%.o: %.c
	$(OCAMLOPT) $(COMPFLAGS) -c $< && $(MV) `basename $@` `dirname $@`

%.ml:%.mll
	$(OCAMLLEX) $<

%.mli %.ml:%.mly
	$(OCAMLYACC) -v $<

.PHONY: clean depend

.depend depend:
	ocamldep -pp $(CAMLP4O) *.ml > .depend

include .depend