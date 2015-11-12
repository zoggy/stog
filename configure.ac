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

# check for one particular file of the sources
AC_INIT(master.Makefile.in)

VERSION=0.15.0
PACKAGE=stog

# The root directory where we will compile
ROOT=`pwd`
USER=`basename $HOME`
GROUP=`groups | cut -d' ' -f 1`
OCAMLFIND_PATH=`ocamlfind printconf path`

ocaml unix.cma str.cma checkocaml.ml ocaml_config.sh || exit 1
. ./ocaml_config.sh

if test "$prefix" = "NONE" ; then
  prefix=`dirname $OCAMLBIN`
fi

if test "$datadir" = "\${datarootdir}" ; then
        datadir=${prefix}/share/stog
fi
if test "$datadir" = "\${prefix}/share" ; then
        datadir=$prefix/share
fi
SHARE_DIR=$datadir


PACKAGES=ppx_blob,xtmpl,netstring,ocf,ocf.ppx,dynlink,unix,str,higlo.lexers,lwt.unix,xtmpl.ppx
OCAML_SESSION_PACKAGES=xtmpl,unix,str,compiler-libs.toplevel
SERVER_PACKAGES=lwt.preemptive,lwt.ppx,websocket,xmldiff,yojson,ppx_deriving_yojson,ojs.server,sha
SERVER_JS_PACKAGES=js_of_ocaml,js_of_ocaml.syntax,xmldiff.js,yojson,ppx_deriving_yojson,xtmpl.js,ojs.js
MSERVER_PACKAGES=ojs.filetree.server,ojs.editor.server
MSERVER_JS_PACKAGES=ojs.filetree.js,ojs.editor.js
if test "$SERVER" = "" ; then SERVER_PACKAGES=""; MSERVER_PACKAGES="" ; fi

#############################
# substitutions to perform
AC_SUBST(OCAMLC)
AC_SUBST(OCAMLOPT)
AC_SUBST(OCAMLDEP)
AC_SUBST(OCAMLLEX)
AC_SUBST(OCAMLYACC)
AC_SUBST(OCAMLBEST)
AC_SUBST(OCAMLVERSION)
AC_SUBST(OCAMLLIB)
AC_SUBST(OCAMLBIN)
AC_SUBST(OCAMLDOC_OPT)
AC_SUBST(OCAMLFIND)
AC_SUBST(EXEC_PREFIX)
AC_SUBST(ROOT)
AC_SUBST(USER)
AC_SUBST(GROUP)
AC_SUBST(OCAMLFIND_PATH)
AC_SUBST(VERSION)
AC_SUBST(PACKAGE)
AC_SUBST(SHARE_DIR)
AC_SUBST(JS_OF_OCAML)

AC_SUBST(PACKAGES)
AC_SUBST(OCAML_SESSION_PACKAGES)
AC_SUBST(SERVER_PACKAGES)
AC_SUBST(SERVER_JS_PACKAGES)
AC_SUBST(MSERVER_PACKAGES)
AC_SUBST(MSERVER_JS_PACKAGES)
AC_SUBST(SERVER)
AC_SUBST(SERVER_BYTE)
AC_SUBST(MSERVER)
AC_SUBST(MSERVER_BYTE)

# Finally create the master.Makefile.in
ACFILES="master.Makefile src/META src/stog_config.ml src/stog_install.ml"
AC_OUTPUT($ACFILES)
chmod a-w master.Makefile
chmod a-w src/META
chmod a-w src/stog_config.ml
chmod a-w src/stog_install.ml

echo "
### Results of configuration ###

Version is $VERSION
Binaries will be installed in "`dirname \`which ${OCAMLC}\``"
Shared files will be installed in ${SHARE_DIR}"

