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

STOGDIR=..
STOG=$(STOGDIR)/stog --plugin ../plugins/plugin_example.cmxs --plugin ../plugins/stog_disqus.cmxs
#STOG=$(STOGDIR)/stog.byte --plugin ../plugins/plugin_example.cmo --plugin ../plugins/stog_disqus.cmo
DEST_DIR=`pwd`/../doc-output
BASE_URL_OPTION=
STOG_OPTIONS= -d $(DEST_DIR) -v -v $(BASE_URL_OPTION) $(MORE_OPTS)
LESSC=lessc

.PHONY: build site test style

build:
	for i in html css rss svg png; do rm -fr $(DEST_DIR)/*.$$i ; done
	rm -f $(DEST_DIR)/*/*html
	$(MAKE) site

site: style
	$(STOG) $(STOG_OPTIONS) .
	$(MAKE) ocamldoc

style:
	mkdir -p $(DEST_DIR)
	$(LESSC) less/style.less > $(DEST_DIR)/style.css

ocamldoc:
	rm -fr $(DEST_DIR)/ref-doc
	(cd .. && $(MAKE) depocamldoc) && cp -fr ../ocamldoc $(DEST_DIR)/ref-doc

#	cp -f *png tmpl/*png $(DEST_DIR)/
#	cp -f *.svg $(DEST_DIR)/

test:
	$(MAKE) BASE_URL_OPTION="--site-url file://$(DEST_DIR)" site

