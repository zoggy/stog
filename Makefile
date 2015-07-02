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

include master.Makefile

# Compilation
#############

all: src

src: dummy
	cd src && $(MAKE) all

re : depend clean all

# Documentation :
#################
doc: dummy
	cd src && $(MAKE) doc

# myself

master.Makefile: master.Makefile.in config.status \
	src/META.in \
	src/stog_config.ml.in src/stog_install.ml.in
	./config.status

config.status: configure
	./config.status --recheck

configure: configure.ac
	autoconf

# backup, clean and depend :
############################

distclean: clean
	$(RM) master.Makefile src/stog_config.ml src/stog_install.ml src/META
	$(RM) -fr config.status autom4te.cache config.log ocaml_config.sh


clean:: dummy
	$(RM) *~ \#*\#
	cd src && $(MAKE) clean

depend: dummy
	cd src && $(MAKE) depend

dummy:

# Web site:
###########
website:
	(cd doc && $(MAKE) DEST_DIR=`pwd`/../../stog-pages)

# archive :
###########
archive:
	git archive --prefix=stog-$(VERSION)/ HEAD | gzip > ../stog-pages/stog-$(VERSION).tar.gz

###########
# Headers
###########
HEADFILES=configure.ac configure \
	master.Makefile.in Makefile src/Makefile doc/Makefile \
	checkocaml.ml src/*.ml src/*.mli src/plugins/*.ml

headers: dummy
	headache -h header -c .headache_config $(HEADFILES) \
	`ls $(HEADFILES) | grep -v plugin_example`

noheaders: dummy
	headache -r -c .headache_config $(HEADFILES) \
	`ls $(HEADFILES) | grep -v plugin_example`


############

#webdoc:
#	(cd src && $(MAKE) docstog)
#	cd web && $(MAKE)


#################
# installation
#################

install: install-share
	cd src && $(MAKE) install
install-lib: dummy
	cd src && $(MAKE) install-lib
install-bin: dummy
	cd src && $(MAKE) install-bin
install-share: dummy
	$(MKDIR) $(SHARE_DIR)
	$(CP) -r share/templates $(SHARE_DIR)/
	$(CP) -r share/modules $(SHARE_DIR)/

uninstall: dummy
	cd src && $(MAKE) uninstall
uninstall-lib: dummy
	cd src && $(MAKE) uninstall-lib
uninstall-bin: dummy
	cd src && $(MAKE) uninstall-bin
uninstall-share: dummy
	cd src && $(MAKE) uninstall-share

###########################
# additional dependencies
###########################

# DO NOT DELETE
