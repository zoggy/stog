#!/bin/sh

mv .stog/templates/elt-in-list.tmpl .stog/templates/doc-in-list.tmpl
sed -i -e "s/<elements/<documents/g" *html */*html
sed -i -e "s/element/document/g" .stog/config
sed -i -e "s/elt-path/elt-navpath/g" *html */*html .stog/templates/*
sed -i -e "s/hid/path/g" *html */*html .stog/templates/*
sed -i -e "s/elt/doc/g" *html */*html .stog/templates/*
