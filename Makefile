#################################################################################
#                Stog                                                           #
#                                                                               #
#    Copyright (C) 2012 Maxence Guesdon. All rights reserved.                   #
#                                                                               #
#    This program is free software; you can redistribute it and/or modify       #
#    it under the terms of the GNU General Public License as                    #
#    published by the Free Software Foundation, version 3 of the License.       #
#                                                                               #
#    This program is distributed in the hope that it will be useful,            #
#    but WITHOUT ANY WARRANTY; without even the implied warranty of             #
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the               #
#    GNU Library General Public License for more details.                       #
#                                                                               #
#    You should have received a copy of the GNU General Public                  #
#    License along with this program; if not, write to the Free Software        #
#    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA                   #
#    02111-1307  USA                                                            #
#                                                                               #
#    As a special exception, you have permission to link this program           #
#    with the OCaml compiler and distribute executables, as long as you         #
#    follow the requirements of the GNU GPL in regard to all of the             #
#    software in the executable aside from the OCaml compiler.                  #
#                                                                               #
#    Contact: Maxence.Guesdon@inria.fr                                          #
#                                                                               #
#################################################################################

#
VERSION=0.2

OCAMLC=ocamlc.opt
OCAMLOPT=ocamlopt.opt
OCAMLLEX=ocamllex
OCAMLYACC=ocamlyacc
CAMLP4O=camlp4o
OCAMLLIB:=`$(OCAMLC) -where`
OCAMLFIND=ocamlfind

INCLUDES=-I +lablgtk2 -I +lablgtk-extras \
	`$(OCAMLFIND) query -i-format xmlm` \
	`$(OCAMLFIND) query -i-format rss` \
	`$(OCAMLFIND) query -i-format xtmpl` \
	`$(OCAMLFIND) query -i-format pcre` \
	`$(OCAMLFIND) query -i-format config-file` \
	`$(OCAMLFIND) query -i-format compiler-libs.toplevel`
COMPFLAGS=$(INCLUDES) -annot -rectypes -g
OCAMLPP=

PLUGINS_BYTE= \
	plugins/stog_disqus.cmo \
	plugins/stog_markdown.cmo \
	plugins/plugin_example.cmo
PLUGINS_OPT=$(PLUGINS_BYTE:.cmo=.cmxs)

RM=rm -f
CP=cp -f
MKDIR=mkdir -p

SYSLIBS=unix.cmxa dynlink.cmxa pcre.cmxa str.cmxa xmlm.cmxa xtmpl.cmx rss.cmxa config_file.cmx
SYSLIBS_BYTE=unix.cma pcre.cma str.cma xmlm.cma xtmpl.cmo rss.cma config_file.cmo

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

LIB_CMXFILES= \
	stog_msg.cmx \
	stog_misc.cmx \
	stog_config.cmx \
	stog_trie.cmx \
	stog_tmap.cmx \
	stog_graph.cmx \
	stog_date.cmx \
	stog_types.cmx \
	stog_intl.cmx \
	stog_find.cmx \
	stog_mailparse.cmx \
	stog_cst.cmx \
	stog_io.cmx \
	stog_info.cmx \
	stog_latex.cmx \
	stog_html.cmx \
	stog_plug.cmx \
	stog_dyn.cmx \

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

all: opt byte
gui: guiopt guibyte

opt: $(LIB) $(MAIN) plugins/plugin_example.cmxs $(PLUGINS_OPT)
guiopt: $(GUI_MAIN)
byte: $(LIB_BYTE) $(MAIN_BYTE) plugins/plugin_example.cmo $(PLUGINS_BYTE)
guibyte: $(GUI_MAIN_BYTE)

$(MAIN): $(LIB) stog_dyn_opt.cmx stog_main.cmx
	$(OCAMLOPT) -verbose -linkall -o $@ $(COMPFLAGS) $(SYSLIBS) \
	$^

$(MAIN_BYTE): $(LIB_BYTE) stog_ocaml.cmo stog_dyn_byte.cmo stog_main.cmo
	$(OCAMLC) -linkall -o $@ $(COMPFLAGS) $(SYSLIBS_BYTE) \
	`$(OCAMLFIND) query -predicates byte -r -a-format compiler-libs.toplevel` $^

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

stog_ocaml.cmo: stog_ocaml.ml errors.cmi
	$(OCAMLC) $(COMPFLAGS) -c $<

##########
.PHONY: doc webdoc

doc:
	rm -fr doc-output
	(cd doc && $(MAKE) test)

webdoc:
	(cd doc && $(MAKE) DEST_DIR=`pwd`/../../stog-pages)

##########
install:
	@$(OCAMLFIND) install stog META \
		$(PLUGINS_BYTE) $(PLUGINS_OPT) $(PLUGINS_OPT:.cmxs=.cmx) $(PLUGINS_OPT:.cmxs=.o) \
		$(LIB_CMIFILES) $(LIB_CMXFILES) $(LIB_CMXFILES:.cmx=.o) \
		$(LIB_BYTE) $(LIB) $(LIB:.cmxa=.a)
	$(CP) $(MAIN) $(MAIN_BYTE)  `dirname \`which $(OCAMLC)\``/

uninstall:
	@$(OCAMLFIND) remove stog
	for i in $(MAIN) $(MAIN_BYTE); do $(RM) `dirname \`which $(OCAMLC)\``/$$i; done
#####
clean:
	$(RM) $(MAIN) $(MAIN_BYTE) $(GUI_MAIN) $(GUI_MAIN_BYTE) *.cm* *.o *.a *.x *.annot
	(cd plugins && $(RM) *.cm* *.o *.a *.x *.annot)

# archive :
###########
archive:
	git archive --prefix=stog-$(VERSION)/ HEAD | gzip > ../stog-pages/stog-$(VERSION).tar.gz

# headers :
###########
HEADFILES= Makefile *.ml *.mli doc/Makefile
headers:
	echo $(HEADFILES)
	headache -h header -c .headache_config `ls $(HEADFILES) | grep -v plugin_example`

noheaders:
	headache -r -c .headache_config `ls $(HEADFILES)`

#############
.SUFFIXES: .mli .ml .cmi .cmo .cmx .mll .mly

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

%.cmxs: %.ml
	$(OCAMLOPT) $(OCAMLPP) $(COMPFLAGS) -shared -o $@ $<

%.o: %.c
	$(OCAMLOPT) $(COMPFLAGS) -c $< && $(MV) `basename $@` `dirname $@`

%.ml:%.mll
	$(OCAMLLEX) $<

%.mli %.ml:%.mly
	$(OCAMLYACC) -v $<

stog_coms.cmo stog_coms.cmi: stog_coms.ml
	$(OCAMLC) $(COMPFLAGS) -c -pp "$(CAMLP4O)" $<
stog_coms.cmx: stog_coms.ml
	$(OCAMLOPT) $(COMPFLAGS) -c -pp "$(CAMLP4O)" $<

.PHONY: clean depend

.depend depend:
	ocamldep -pp $(CAMLP4O) *.ml > .depend

include .depend
