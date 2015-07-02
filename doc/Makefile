#################################################################################
#                Stog                                                           #
#                                                                               #
#    Copyright (C) 2012-2015 INRIA All rights reserved.                         #
#    Author: Maxence Guesdon, INRIA Saclay                                      #
#                                                                               #
#    This program is free software; you can redistribute it and/or modify       #
#    it under the terms of the GNU General Public License as                    #
#    published by the Free Software Foundation, version 3 of the License.       #
#                                                                               #
#    This program is distributed in the hope that it will be useful,            #
#    but WITHOUT ANY WARRANTY; without even the implied warranty of             #
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the               #
#    GNU General Public License for more details.                               #
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

include ../master.Makefile

STOGDIR=../src
STOG=$(STOGDIR)/stog #--plugin ../plugins/plugin_example.cmxs --plugin ../plugins/stog_disqus.cmxs
STOG_SERVER=$(STOGDIR)/stog-server
#STOG=$(STOGDIR)/stog.byte --plugin ../plugins/plugin_example.cmo --plugin ../plugins/stog_disqus.cmo
DEST_DIR=`pwd`/../doc-output
BASE_URL_OPTION=
STOG_OPTIONS=\
	--plugin ../src/plugins/stog_asy.cmxs \
  --plugin ../src/plugins/stog_dot.cmxs \
	--plugin ../src/plugins/stog_markdown.cmxs \
	--plugin ../src/plugins/stog_multi_doc.cmxs \
	--plugin ../src/plugins/stog_disqus.cmxs \
	--plugin ../src/plugins/stog_sitemap.cmxs \
	-d $(DEST_DIR) --depcut $(BASE_URL_OPTION) $(MORE_OPTS) #-v -v -v -v -v
LESSC=lessc

.PHONY: build site test style

all: build

build:
	for i in html css rss svg png; do rm -fr $(DEST_DIR)/*.$$i ; done
	rm -f $(DEST_DIR)/*/*html
	$(MAKE) site

site: stog-tmpl-usage.txt stog-tmpl-tree.txt style plugins/rdf_fig.svg
	$(MKDIR) $(DEST_DIR) $(DEST_DIR)/plugins
	$(STOG) -d $(DEST_DIR) \
		--tmpl $(ROOT)/share/templates\
		--mods $(ROOT)/share/modules \
		--site-url https://zoggy.github.io/stog \
		examples/article-example.html
	$(STOG) $(STOG_OPTIONS) .

plugins/rdf_fig.svg: plugins/rdf_fig.dot
	dot -Tsvg -o $@ $<

stog-tmpl-usage.txt:
	../src/stog-tmpl --help > $@
stog-tmpl-tree.txt:
	$(MKDIR)  stog-tmpl-tree
	../src/stog-tmpl -d stog-tmpl-tree -t software --site-title Foobar --sw-name foobar
	tree stog-tmpl-tree | head -n -2 > $@
	$(RM) -fr stog-tmpl-tree

style:
	mkdir -p $(DEST_DIR)
	$(LESSC) less/style.less > style.css
	$(CP) style.css $(DEST_DIR)/style.css
	$(LESSC) less/slides.less > $(DEST_DIR)/slides.css
	$(LESSC) less/video-style.less > $(DEST_DIR)/video-style.css

#	cp -f *png tmpl/*png $(DEST_DIR)/
#	cp -f *.svg $(DEST_DIR)/

test:
	$(MAKE) BASE_URL_OPTION="--local" site

tree:
	tree -I less | grep -v release | grep -v comment | grep -v ocaml\\. \
	| grep -v gradient.png | grep -v search.png | grep -v left.png | grep -v right.png

server:
	$(STOG_SERVER) $(STOG_OPTIONS) .

clean:
	rm -fr .stog/cache

.PHONY: videos

videos:
	cd videos && $(MAKE)

	