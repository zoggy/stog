#################################################################################
#                Stog                                                           #
#                                                                               #
#    Copyright (C) 2012-2013 Maxence Guesdon. All rights reserved.              #
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
VERSION=0.7.0

OCAMLC=ocamlc.opt
OCAMLOPT=ocamlopt.opt
OCAMLDOC=ocamldoc.opt
OCAMLLEX=ocamllex
OCAMLYACC=ocamlyacc
CAMLP4O=camlp4o
OCAMLLIB:=`$(OCAMLC) -where`
OCAMLFIND=ocamlfind

PACKAGES=xmlm,rss,xtmpl,config-file,dynlink,unix,str
OCAML_SESSION_PACKAGES=xtmpl,unix,str,compiler-libs.toplevel

COMPFLAGS= -annot -rectypes -g #-w +K
OCAMLPP=

PLUGINS_BYTE= \
	plugins/stog_disqus.cmo \
	plugins/stog_markdown.cmo \
	plugins/plugin_example.cmo
PLUGINS_OPT=$(PLUGINS_BYTE:.cmo=.cmxs)

RM=rm -f
CP=cp -f
MKDIR=mkdir -p

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
	stog_tags.cmx \
	stog_io.cmx \
	stog_info.cmx \
	stog_ocaml_types.cmx \
	stog_ocaml.cmx \
	stog_latex.cmx \
	stog_deps.cmx \
	stog_tmpl.cmx \
	stog_cache.cmx \
	stog_html.cmx \
	stog_plug.cmx \
	stog_dyn.cmx \

LIB_CMOFILES=$(LIB_CMXFILES:.cmx=.cmo)
LIB_CMIFILES=$(LIB_CMXFILES:.cmx=.cmi)

LIB=stog.cmxa
LIB_BYTE=$(LIB:.cmxa=.cma)

MAIN=stog
MAIN_BYTE=$(MAIN).byte

OCAML_SESSION=$(MAIN)-ocaml-session

MK_STOG=mk-$(MAIN)
MK_STOG_BYTE=mk-$(MAIN_BYTE)
MK_STOG_OCAML=mk-$(OCAML_SESSION)

OCAML_SESSION_CMOFILES= \
	stog_ocaml_types.cmo \
	stog_misc.cmo \
	stog_ocaml_session.cmo
OCAML_SESSION_CMIFILES=$(OCAML_SESSION_CMOFILES:.cmo=.cmi)

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

opt: $(LIB) $(MAIN) plugins/plugin_example.cmxs $(PLUGINS_OPT) $(MK_STOG)
guiopt: $(GUI_MAIN)
byte: $(LIB_BYTE) $(MAIN_BYTE) $(OCAML_SESSION) plugins/plugin_example.cmo $(PLUGINS_BYTE) \
	$(MK_STOG_BYTE) $(MK_STOG_OCAML)
guibyte: $(GUI_MAIN_BYTE)

$(MAIN): $(LIB) stog_main.cmx
	$(OCAMLFIND) ocamlopt -package $(PACKAGES) -verbose -linkall -linkpkg -o $@ $(COMPFLAGS) $^

$(MAIN_BYTE): $(LIB_BYTE) stog_main.cmo
	$(OCAMLFIND) ocamlc -package $(PACKAGES) -linkall -linkpkg -o $@ $(COMPFLAGS) $^
#	`$(OCAMLFIND) query -predicates byte -r -a-format compiler-libs.toplevel` $^

$(LIB): $(LIB_CMIFILES) $(LIB_CMXFILES)
	$(OCAMLFIND) ocamlopt -a -o $@ $(LIB_CMXFILES)

$(LIB_BYTE): $(LIB_CMIFILES) $(LIB_CMOFILES)
	$(OCAMLFIND) ocamlc -a -o $@ $(LIB_CMOFILES)

$(OCAML_SESSION): $(OCAML_SESSION_CMIFILES) $(OCAML_SESSION_CMOFILES)
	$(OCAMLFIND) ocamlc -package $(OCAML_SESSION_PACKAGES) -linkpkg -linkall -o $@ $(COMPFLAGS) $(OCAML_SESSION_CMOFILES)

stog_ocaml_session.cmo: stog_ocaml_session.ml
	$(OCAMLFIND) ocamlc -package $(OCAML_SESSION_PACKAGES) $(COMPFLAGS) -c $<

# mk scripts
$(MK_STOG): $(LIB)
	@echo -n "Creating $@... "
	@$(RM) $@
	@echo "# Multi-shell script.  Works under Bourne Shell, MPW Shell, zsh." > $@
	@echo "if : == x" >> $@
	@echo "then # Bourne Shell or zsh" >> $@
	@echo "  exec $(OCAMLFIND) ocamlopt -package stog -linkpkg -linkall \"\$$@\" stog_main.cmx" >> $@
	@echo "else #MPW Shell" >> $@
	@echo "  exec $(OCAMLFIND) ocamlopt -package stog -linkpkg -linkall {\"parameters\"} stog_main.cmx" >> $@
	@echo "End # uppercase E because \"end\" is a keyword in zsh" >> $@
	@echo "fi" >> $@
	@chmod ugo+rx $@
	@chmod a-w $@
	@echo done

$(MK_STOG_BYTE): $(LIB)
	@echo -n "Creating $@... "
	@$(RM) $@
	@echo "# Multi-shell script.  Works under Bourne Shell, MPW Shell, zsh." > $@
	@echo "if : == x" >> $@
	@echo "then # Bourne Shell or zsh" >> $@
	@echo "  exec $(OCAMLFIND) ocamlc -package stog -linkpkg -linkall \"\$$@\" stog_main.cmo" >> $@
	@echo "else #MPW Shell" >> $@
	@echo "  exec $(OCAMLFIND) ocamlc -package stog -linkpkg -linkall {\"parameters\"} stog_main.cmo" >> $@
	@echo "End # uppercase E because \"end\" is a keyword in zsh" >> $@
	@echo "fi" >> $@
	@chmod ugo+rx $@
	@chmod a-w $@
	@echo done

$(MK_STOG_OCAML): $(LIB) $(OCAML_SESSION_CMOFILES)
	@echo -n "Creating $@... "
	@$(RM) $@
	@echo "# Multi-shell script.  Works under Bourne Shell, MPW Shell, zsh." > $@
	@echo "if : == x" >> $@
	@echo "then # Bourne Shell or zsh" >> $@
	@echo "  exec $(OCAMLFIND) ocamlc -package $(OCAML_SESSION_PACKAGES) -linkpkg `ocamlfind query -i-format stog` -linkall \"\$$@\" $(OCAML_SESSION_CMOFILES)" >> $@
	@echo "else #MPW Shell" >> $@
	@echo "  exec $(OCAMLFIND) ocamlc -package $(OCAML_SESSION_PACKAGES) -linkpkg `ocamlfind query -i-format stog` -linkall {\"parameters\"} $(OCAML_SESSION_CMOFILES)" >> $@
	@echo "End # uppercase E because \"end\" is a keyword in zsh" >> $@
	@echo "fi" >> $@
	@chmod ugo+rx $@
	@chmod a-w $@
	@echo done

##########
.PHONY: doc webdoc ocamldoc

ocamldoc:
	$(MKDIR) ocamldoc
	$(OCAMLFIND) ocamldoc -package $(PACKAGES) -rectypes -d ocamldoc -html -t "Stog" \
	$(LIB_CMXFILES:.cmx=.ml) $(LIB_CMXFILES:.cmx=.mli)

PKGS := $(shell echo $(PACKAGES) | sed -e "s/,/ /g")
depocamldoc:
	$(MKDIR) ocamldoc
	$(OCAMLDOC) `$(OCAMLFIND) query -i-format $(PKGS)` -rectypes -d ocamldoc -g odoc_depgraph.cmxs -t "Stog" \
	$(LIB_CMXFILES:.cmx=.ml) $(LIB_CMXFILES:.cmx=.mli) -width 700 -height 700

doc:
	rm -fr doc-output
	(cd doc && $(MAKE) test)

webdoc:
	(cd doc && $(MAKE) DEST_DIR=`pwd`/../../stog-pages)

##########
install: install-lib install-bin

install-lib:
	@$(OCAMLFIND) install stog META \
		$(PLUGINS_BYTE) $(PLUGINS_OPT) $(PLUGINS_OPT:.cmxs=.cmx) $(PLUGINS_OPT:.cmxs=.o) \
		$(LIB_CMIFILES) $(LIB_CMXFILES) $(LIB_CMXFILES:.cmx=.o) \
		$(LIB_BYTE) $(LIB) $(LIB:.cmxa=.a) stog_main.cm* stog_main.o \
		$(OCAML_SESSION_CMOFILES)

install-bin:
	$(CP) $(MAIN) $(MAIN_BYTE) $(OCAML_SESSION) \
	  $(MK_STOG) $(MK_STOG_BYTE) $(MK_STOG_OCAML) \
		`dirname \`which $(OCAMLC)\``/

uninstall: uninstall-lib uninstall-bin

uninstall-lib:
	@$(OCAMLFIND) remove stog

uninstall-bin:
	for i in $(MAIN) $(MAIN_BYTE) $(OCAML_SESSION) \
		$(MK_STOG) $(MK_STOG_BYTE) $(MK_STOG_OCAML); \
		do $(RM) `dirname \`which $(OCAMLC)\``/$$i; done

#####
clean:
	$(RM) $(MAIN) $(MAIN_BYTE) $(GUI_MAIN) $(GUI_MAIN_BYTE) *.cm* *.o *.a *.x *.annot
	$(RM) $(MK_STOG) $(ML_STOG_BYTE) $(MK_STOG_OCAML)
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
	$(OCAMLFIND) ocamlc -package $(PACKAGES) $(OCAMLPP) $(COMPFLAGS) -c $<

%.cmo:%.ml
	if test -f `dirname $<`/`basename $< .ml`.mli && test ! -f `dirname $<`/`basename $< .ml`.cmi ; then \
	$(OCAMLFIND) ocamlc -package $(PACKAGES) $(OCAMLPP) $(COMPFLAGS) -c `dirname $<`/`basename $< .ml`.mli; fi
	$(OCAMLFIND) ocamlc -package $(PACKAGES) $(OCAMLPP) $(COMPFLAGS) -c $<

%.cmi %.cmo:%.ml
	if test -f `dirname $<`/`basename $< .ml`.mli && test ! -f `dirname $<`/`basename $< .ml`.cmi ; then \
	$(OCAMLFIND) ocamlc -package $(PACKAGES) $(OCAMLPP) $(COMPFLAGS) -c `dirname $<`/`basename $< .ml`.mli; fi
	$(OCAMLFIND) ocamlc -package $(PACKAGES) $(OCAMLPP) $(COMPFLAGS) -c $<

%.cmx %.o:%.ml
	$(OCAMLFIND) ocamlopt -package $(PACKAGES) $(OCAMLPP) $(COMPFLAGS) -c $<

%.cmxs: %.ml
	$(OCAMLFIND) ocamlopt -package $(PACKAGES) $(OCAMLPP) $(COMPFLAGS) -shared -o $@ $<

%.ml:%.mll
	$(OCAMLLEX) $<

%.mli %.ml:%.mly
	$(OCAMLYACC) -v $<

.PHONY: clean depend

.depend depend:
	$(OCAMLFIND) ocamldep -pp $(CAMLP4O) *.ml *.mli > .depend

include .depend
