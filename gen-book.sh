#!/usr/bin/env bash
set -e

function usage {
  echo "Usage: $0 [-m <milestone>] [-l <lingua>] [-c <chapters>]"
  echo "  -l : Lingua (only en supported for now, so stick with that)"
  echo "  -c : Chapter list (default: chapters.scm)"
  echo "  -p : Output only public chapters, as marked in the chapters file"
  echo "  -m : Milestone name. Optional, will generate website if specified"
  exit 1
}

# detect where the XSLT scripts are
OS=$(uname -s)
case "${OS}" in
Darwin)
  DOCBOOK_XSL_PATH=$(brew --prefix)/Cellar/docbook/5.0/docbook/xsl/1.76.1
  ;;
Linux)
  DOCBOOK_XSL_PATH=/usr/share/xml/docbook/stylesheet/docbook-xsl
  ;;
*)
  echo Unknown OS. Edit this script and add an XSL!
  ;;
esac

# parse options
LINGUA=en
CHAPTERS=chapters.scm
MILESTONE=
PUBLIC=
while getopts ":l:c:m:ph" opt; do
  case $opt in
    l)
      LINGUA=$OPTARG
      ;;
    c)
      CHAPTERS=$OPTARG
      ;;
    m)
      MILESTONE=$OPTARG
      ;;
    p) 
      PUBLIC=--public
      ;;
    h)
      usage
      ;;
    \?)
      echo "Invalid option: -$OPTARG"
      usage
      ;;
    :)
      echo "Option -$OPTARG requires an argument."
      usage
      ;;
  esac
done
if [ ! -e "$CHAPTERS" ]; then
  echo Chapters file $CHAPTERS does not exist.
  usage
fi
cd scripts && ./build.sh && cd ..
SRCS="en/00-toc.md $(./scripts/_build/get_chapter_files.native ${PUBLIC} ${CHAPTERS} ${LINGUA}/)"
TRANSFORM_DOCBOOK=./scripts/_build/transform_pandocbook.native
echo Lingua: ${LINGUA}
echo Chapters file: ${CHAPTERS}
echo Source files: ${SRCS}
rm -rf build/${LINGUA}
mkdir -p build/${LINGUA}/source build/${LINGUA}/html
ln -nfs ${DOCBOOK_XSL_PATH} stylesheets/system-xsl
pandoc -f markdown -t docbook --chapters --template rwo.docbook -o build/${LINGUA}/source/rwo-pre.xml ${SRCS}
pandoc -f markdown -t docbook --chapters --template rwo-oreilly.docbook -o build/${LINGUA}/source/rwo-pre-oreilly.xml ${SRCS}
${TRANSFORM_DOCBOOK} ${PUBLIC} ${CHAPTERS} build/${LINGUA}/source/rwo-pre.xml > build/${LINGUA}/source/rwo.xml
${TRANSFORM_DOCBOOK} ${PUBLIC} ${CHAPTERS} build/${LINGUA}/source/rwo-pre.xml > build/${LINGUA}/source/rwo-oreilly.xml
xsltproc --output build/${LINGUA}/html/ stylesheets/${LINGUA}/web.xsl build/${LINGUA}/source/rwo.xml

echo The raw HTML is in build/${LINGUA}/html.
echo "The Docbook is in build/${LINGUA}/source/rwo[-oreilly].xml"

# If no milestone is specified, finish now
if [ "$MILESTONE" = "" ]; then
  exit 0
fi
# Otherwise continue to generate the commenting website

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

mkdir -p data/live_site/${MILESTONE}/${LINGUA}/html
cp ./commenting-build/en/${MILESTONE}/* data/live_site/${MILESTONE}/${LINGUA}/html/
cp -r ./commenting-build/media data/live_site/${MILESTONE}/

# at this point, we have:
# - a syntax highlighted HTML set in data/live_site/<milestone>
# - the sexp dumps of the id->paragraph fragment in data/fragments/<milestone>
