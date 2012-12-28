#!/usr/bin/env bash 

set -ex

LINGUA=en
MILESTONE=$1

if [ "${MILESTONE}" = "" ]; then
  echo Usage: $0 milestone-name
  exit 1
fi

# build the various binaries we need
cd scripts
./build.sh
./buildgh.sh
cd ..

# generate the syntax highlighted CSS
pygmentize -S trac -O linenos=1 -a .highlight -f html > commenting/build_template/media/css/code.css
# generate the commenting HTML (no syntax highlighting)
python commenting/bin/generate_commenting_site.py --github-milestone ${MILESTONE}

# syntax highlight the commenting HTML
mkdir -p commenting-build/${LINGUA}/${MILESTONE}
for i in commenting-build/${LINGUA}/html/*.html; do
  cat $i | ./scripts/_build/html_code_highlight.native > commenting-build/${LINGUA}/${MILESTONE}/`basename $i`
done

# now parse the HTML and generate a sexp dump of the paragraph fragments
mkdir -p data/fragments
./scripts/_build/dump_paragraph_fragments.native data/fragments/${MILESTONE} ./commenting-build/en/${MILESTONE}/*.html

mkdir -p data/live_site/${MILESTONE}/html
cp ./commenting-build/en/${MILESTONE}/* data/live_site/${MILESTONE}/html/
cp -r ./commenting-build/media data/live_site/${MILESTONE}/

# at this point, we have:
# - a syntax highlighted HTML set in data/live_site/<milestone>
# - the sexp dumps of the id->paragraph fragment in data/fragments/<milestone>
# the webserver looks up the sexp data to map issue comments correctly.


