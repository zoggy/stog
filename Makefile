#

INCLUDES=-I +lablgtk2 -I +lablgtk-extras -I +cameleon2 -I +xmlm $(OCAML_INCLUDES)
COMPFLAGS=$(INCLUDES) -annot -I `ocamlfind query pcre` -rectypes -g
OCAMLPP=

OCAMLC=ocamlc.opt -g
OCAMLOPT=ocamlopt.opt -g
OCAMLLEX=ocamllex
OCAMLYACC=ocamlyacc
CAMLP4O=camlp4o
OCAMLLIB:=`$(OCAMLC) -where`


INSTALLDIR=$(OCAMLLIB)/stog

RM=rm -f
CP=cp -f
MKDIR=mkdir -p

SYSLIBS=unix.cmxa dynlink.cmxa pcre.cmxa str.cmxa xmlm.cmx xtmpl.cmx xml-light.cmxa rss.cmxa
SYSLIBS_BYTE=unix.cma dynlink.cma pcre.cma str.cma xmlm.cmo xtmpl.cmo xml-light.cma rss.cma

GUI_SYSLIBS=lablgtk.cmxa \
	lablgtksourceview2.cmxa \
	config_file.cmx \
	gtksv_utils.cmx \
	gmylist.cmx \
	okey.cmx
GUI_SYSLIBS_BYTE=lablgtk.cma \
	lablgtksourceview2.cma \
	config_file.cmo \
	gtksv_utils.cmo \
	gmylist.cmo \
	okey.cmo

LIB_CMXFILES=stog_config.cmx \
	stog_misc.cmx \
	stog_tmap.cmx \
	stog_graph.cmx \
	stog_date.cmx \
	stog_types.cmx \
	stog_find.cmx \
	stog_mailparse.cmx \
	stog_io.cmx \
	stog_coms.cmx \
	stog_info.cmx \
	stog_latex.cmx \
	stog_html.cmx

LIB_CMOFILES=$(LIB_CMXFILES:.cmx=.cmo)
LIB_CMIFILES=$(LIB_CMXFILES:.cmx=.cmi)

LIB=stog.cmxa
LIB_BYTE=$(LIB:.cmxa=.cma)

MAIN=stog
MAIN_BYTE=$(MAIN).byte

GUI_MAIN_CMXFILES=\
	stog_gui_arts.cmx \
	stog_gui_main.cmx \
	stog_gui.cmx

GUI_MAIN_CMOFILES=$(Gui_MAIN_CMXFILES:.cmx=.cmo)
GUI_MAIN_CMIFILES=$(GUi_MAIN_CMXFILES:.cmx=.cmi)

GUI_MAIN=$(MAIN)-gui
GUI_MAIN_BYTE=$(GUI_MAIN).byte

OCAML_SRC_DIR=/home/guesdon/devel/ocaml-3.12/
OCAML_INCLUDES= \
	-I $(OCAML_SRC_DIR)parsing \
	-I $(OCAML_SRC_DIR)typing \
	-I $(OCAML_SRC_DIR)toplevel \
	-I $(OCAML_SRC_DIR)utils \
	-I $(OCAML_SRC_DIR)driver

OCAMLTOP_CMXFILES=$(OCAMLTOP_CMOFILES:.cmo=.cmx)

all: opt byte
gui: guiopt guibyte
ocaml: stog_ocaml.cma
ocamlopt: stog_ocaml.cmxs

opt: $(LIB) $(MAIN)
guiopt: $(GUI_MAIN)
byte: $(LIB_BYTE) $(MAIN_BYTE)
guibyte: $(GUI_MAIN_BYTE)

$(MAIN): $(LIB) stog_main.cmx
	$(OCAMLOPT) -verbose -linkall -o $@ $(COMPFLAGS) $(SYSLIBS) \
	$^

$(MAIN_BYTE): $(LIB_BYTE) stog_ocaml.cmo stog_main.cmo
	$(OCAMLC) -linkall -o $@ $(COMPFLAGS) $(SYSLIBS_BYTE) \
	toplevellib.cma $^

$(LIB): $(LIB_CMIFILES) $(LIB_CMXFILES)
	$(OCAMLOPT) -a -o $@ $(LIB_CMXFILES)

$(LIB_BYTE): $(LIB_CMIFILES) $(LIB_CMOFILES)
	$(OCAMLC) -a -o $@ $(LIB_CMOFILES)

$(GUI_MAIN): $(LIB) $(GUI_MAIN_CMIFILES) $(GUI_MAIN_CMXFILES)
	$(OCAMLOPT) -verbose -linkall -o $@ $(COMPFLAGS) $(SYSLIBS) \
	$(LIB) $(GUI_SYSLIBS) $(GUI_MAIN_CMXFILES)

$(GUI_MAIN_BYTE): $(LIB_BYTE) $(GUI_MAIN_CMIFILES) $(GUI_MAIN_CMOFILES)
	$(OCAMLC) -linkall -o $@ $(COMPFLAGS) $(SYSLIBS_BYTE) \
	$(LIB_BYTE) $(GUI_SYSLIBS_BYTE) $(GUI_MAIN_CMOFILES)

stog_ocaml.cmo: stog_ocaml.ml
	$(OCAMLC) $(COMPFLAGS) -c $(OCAML_INCLUDES) $<

##########
.PHONY: doc

doc:
	rm -fr doc-output
	(cd doc && $(MAKE) test)

webdoc:
	(cd doc && $(MAKE) $(DEST_DIR) `pwd`/../stog-pages .)

##########
install:
	$(MKDIR) $(INSTALLDIR)
	$(CP) $(LIB_CMIFILES) $(LIB) $(LIB_BYTE) $(LIB:.cmxa=.a) \
	$(INSTALLDIR)
	$(CP) $(MAIN) $(CLIENT)  `dirname \`which $(OCAMLC)\``/

#####
clean:
	$(RM) $(MAIN) $(MAIN_BYTE) $(GUI_MAIN) $(GUI_MAIN_BYTE) *.cm* *.o *.a *.x *.annot

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