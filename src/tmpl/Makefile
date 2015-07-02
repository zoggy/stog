MKSTOG=mk-stog
MKSTOG_OCAML_SESSION=mk-stog-ocaml-session
STOG=./mystog
STOG_OCAML_SESSION=./mystog-ocaml-session
STOG_SERVER=./mystog-server
DEST_DIR=/tmp
BASE_URL_OPTION=
PACKAGES=stog.markdown
STOG_OPTIONS=--stog-ocaml-session "$(STOG_OCAML_SESSION)  -I ../src" -d $(DEST_DIR) $(BASE_URL_OPTION)

build:
	$(MAKE) site

site: stog
	$(STOG) $(STOG_OPTIONS) .

ocamldoc:
	rm -f refdoc/*html ref-doc/style.css
	(cd ../src && make docstog)

test:
	$(MAKE) BASE_URL_OPTION="--local" site

stog:$(STOG) $(STOG_OCAML_SESSION)
server: $(STOG_SERVER) $(STOG_OCAML_SESSION)
	$(STOG_SERVER) $(STOG_OPTIONS) .

.PHONY:$(STOG) $(STOG_OCAML_SESSION)

$(STOG):
	$(MKSTOG) -o $@ -package $(PACKAGES) -linkall

$(STOG_SERVER):
	$(MKSTOG) -o $@ -package stog.server,$(PACKAGES) -thread -linkall

$(STOG_OCAML_SESSION):
	$(MKSTOG_OCAML_SESSION) -o $@

clean:
	rm -fr .stog/cache

cleanall: clean
	rm -f $(STOG) $(STOG_OCAML_SESSION) $(STOG_SERVER)


