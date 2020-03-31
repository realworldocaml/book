# -------------------------------------------------------------------------

# Compilation and installation rules.

.PHONY: all install uninstall clean

all:
	@ dune build @install
# note: @install is smaller than @all,
#       as it does not include the tests nor the stage3 executable.

uninstall:
	@ dune uninstall

clean::
	@ dune clean

install: all
	@ dune install

# -------------------------------------------------------------------------

# The rest of this Makefile helps perform tests and prepare releases.
# These commands are intended to be used by Menhir's developers.

# Require bash.
SHELL := bash
# Prevent the built-in bash cd from displaying information.
export CDPATH=

# -------------------------------------------------------------------------

# Testing.

# [make test] runs the tests found in the test/ and demos/ directories.

.PHONY: test
test:
	@ dune build --display short @test

# [make data] extracts statistics and performance data out of the files
# produced by [make test]. Be careful: the timing data is definitely not high
# quality, because every test is run only once and because the tests are run
# in parallel on a heavily-loaded machine. Furthermore, this data is
# machine-dependent. The data is written to analysis/data.csv.

# A test that caused a TIMEOUT is excluded from the data.

# On MacOS, we require gsed instead of sed.
SED=$(shell if [[ "$$OSTYPE" == "darwin"* ]] ; then echo gsed ; else echo sed ; fi)

.PHONY: data
data: test
	@ echo "Collecting data (using $(SED))..." && \
	  echo "name,mode,terminals,nonterminals,lr0states,lr1states,lr1time" > analysis/data.csv && \
	  directory=_build/default/test/static/src && \
	  successful=0 && timedout=0 && \
	  for timings in $$directory/*.out.timings ; do \
	    name=$${timings%.out.timings} ; \
	    out=$$name.out ; \
	    name=`basename $$name` ; \
	    if grep --quiet "TIMEOUT after" $$out ; then \
	      ((timedout++)) ; \
	    else \
	      ((successful++)) ; \
	      mode=`$(SED) -n -e "s/^The construction mode is \([a-z\-]\+\)./\1/p" $$out` ; \
	      terminals=`$(SED) -n -e "s/^Grammar has \([0-9]\+\) terminal symbols./\1/p" $$out` ; \
	      nonterminals=`$(SED) -n -e "s/^Grammar has \([0-9]\+\) nonterminal symbols, among which [0-9]\+ start symbols./\1/p" $$out` ; \
	      lr0states=`$(SED) -n -e "s/^Built an LR(0) automaton with \([0-9]\+\) states./\1/p" $$out` ; \
	      lr1states=`$(SED) -n -e "s/^Built an LR(1) automaton with \([0-9]\+\) states./\1/p" $$out` ; \
	      lr1time=`$(SED) -n -e "s/^Construction of the LR(1) automaton: \(.*\)s/\1/p" $$timings` ; \
	      echo "$$name,$$mode,$$terminals,$$nonterminals,$$lr0states,$$lr1states,$$lr1time" >> analysis/data.csv ; \
	    fi \
	  done && \
	echo "$$successful successful tests; $$timedout timed out tests."

clean::
	@ rm -f analysis/data.csv

# [make plot] uses Rscript to plot the data extracted by [make data].

.PHONY: plot
plot:
	@ echo "Running R..."
	@ cd analysis && ./analysis.r

clean::
	@ rm -f analysis/*.pdf

# [make speed] runs the speed test in test/dynamic/speed.

.PHONY: speed
speed:
	@ dune build --force --no-buffer @speed

# [make versions] compiles and tests Menhir under many versions of
# OCaml, whose list is specified below.

# Note: [make test] can fail on an unusually slow or unusually fast
# machine due to the choice of an arbitrary timeout value to stop
# certain very-long-running tests. E.g., a bytecode-only switch is
# usually too slow to complete certain tests in time.

# This requires appropriate opam switches to exist. A missing switch
# can be created like this:
#   opam switch create 4.03.0

VERSIONS := \
  4.02.3 \
  4.03.0 \
  4.04.2 \
  4.05.0 \
  4.06.1 \
  4.07.1 \
  4.08.1 \
  4.09.0 \
  4.09.0+bytecode-only \
  4.10.0 \

.PHONY: versions
versions:
	@(echo "(lang dune 2.0)" && \
	  for v in $(VERSIONS) ; do \
	    echo "(context (opam (switch $$v)))" ; \
	  done) > dune-workspace.versions
	@ dune build --workspace dune-workspace.versions @all @test

.PHONY: dune
dune:
	@ current=`opam switch show` ; \
	  for v in $(VERSIONS) ; do \
	    opam switch $$v && \
	    eval $$(opam env) && \
	    opam install --yes dune.2.2.0 ; \
	  done ; \
	  opam switch $$current

# [make expected] updates the contents of *all* reference files (those
# that contain the expected output of every test). This command should
# be run only when you trust that every test produces correct output.

# [make expected] can *update* existing reference files, but will not
# *create* them when they do not exist. Also, it updates *all* files;
# it cannot be used to refresh a few specific files. Both of these
# limitations can be worked around by using the script [promote.sh].

.PHONY: expected
expected:
	@ dune build @test --auto-promote

# [make depend] regenerates the files dune.auto. This command should
# be run every time some tests are added or removed or renamed in the
# subdirectories test/static/{good,bad} and test/dynamic/semantics/data.

.PHONY: depend
depend:
	@ dune build @depend --auto-promote || true

# -------------------------------------------------------------------------

# Cleaning up.

clean::
	@ find . -name "*~" -exec rm '{}' \;
	@ for i in demos doc ; do \
	  $(MAKE) -C $$i $@ ; \
	done

# -------------------------------------------------------------------------

# Distribution.

# The version number is automatically set to the current date,
# unless DATE is defined on the command line.
DATE     := $(shell /bin/date +%Y%m%d)
DATEDASH := $(shell /bin/date +%Y-%m-%d)

PACKAGE  := menhir-$(DATE)
CURRENT  := $(shell pwd)
TARBALL  := $(CURRENT)/$(PACKAGE).tar.gz

# -------------------------------------------------------------------------

# The names of the modules in MenhirLib are obtained by reading the
# non-comment lines in menhirLib.mlpack.

MENHIRLIB_MODULES := \
  $(shell grep -ve "^[ \t\n\r]*\#" lib/pack/menhirLib.mlpack)

# The names of the source files in MenhirLib are obtained by adding
# an .ml or .mli extension to the module name. (We assume that the
# first letter of the file name is a capital letter.)

MENHIRLIB_FILES   := \
  $(shell for m in $(MENHIRLIB_MODULES) ; do \
    ls lib/$$m.{ml,mli} 2>/dev/null ; \
  done)

# -------------------------------------------------------------------------

# Propagating an appropriate header into every file.

# This requires a version of headache that supports UTF-8; please use
# https://github.com/Frama-C/headache

# This used to be done at release time and not in the repository, but
# it is preferable to do in it the repository too, for two reasons: 1-
# the repository is (or will be) publicly accessible; and 2- this makes
# it easier to understand the line numbers that we sometimes receive as
# part of bug reports.

# Menhir's standard library (standard.mly) as well as the source files
# in MenhirLib carry the "library" license, while every other file
# carries the "regular" license.

HEADACHE        := headache
SRCHEAD         := $(CURRENT)/headers/regular-header
LIBHEAD         := $(CURRENT)/headers/library-header
COQLIBHEAD      := $(CURRENT)/headers/coq-library-header
HEADACHECOQCONF := $(CURRENT)/headers/headache-coq.conf
FIND            := $(shell if command -v gfind >/dev/null ; then echo gfind ; else echo find ; fi)

.PHONY: headache
headache:
	@ cd src && $(FIND) . -regex ".*\.ml\(i\|y\|l\)?" \
	    -exec $(HEADACHE) -h $(SRCHEAD) "{}" ";"
	@ cd sdk && $(FIND) . -regex ".*\.ml\(i\|y\|l\)?" \
	    -exec $(HEADACHE) -h $(SRCHEAD) "{}" ";"
	@ for file in src/standard.mly $(MENHIRLIB_FILES) ; do \
	    $(HEADACHE) -h $(LIBHEAD) $$file ; \
	  done
	@ for file in coq-menhirlib/src/*.v ; do \
	    $(HEADACHE) -h $(COQLIBHEAD) -c $(HEADACHECOQCONF) $$file ; \
	  done

# -------------------------------------------------------------------------

# Creating a release.

# A release commit is created off the main branch, on the side, and tagged.
# Indeed, some files need to be changed or removed for a release.

BRANCH := release-branch-$(DATE)

# The documentation files $(DOC) are copied to the directory $(RELEASE) on the
# master branch. They are also copied to the directory $(WWW).

DOC     := doc/manual.pdf doc/manual.html doc/manual*.png
RELEASE := releases/$(DATE)
WWW     := www

# Prior to making a release, please make sure that `CHANGES.md` has been
# properly updated. Run [make test] and [make versions] to make sure that
# Menhir can be compiled and passes all tests under all supported versions of
# OCaml. Run [make speed] and have a look at the performance figures to make
# sure that they are in the right ballpark. Finally, test the opam package by
# running [make pin]. (You may wish to run [make pin] in a dedicated switch,
# so as to avoid clobbering your regular installation of Menhir.)

.PHONY: release
release:
# Check if this is the master branch.
	@ if [ "$$(git symbolic-ref --short HEAD)" != "master" ] ; then \
	  echo "Error: this is not the master branch." ; \
	  git branch ; \
	  exit 1 ; \
	fi
# Check if everything has been committed.
	@ if [ -n "$$(git status --porcelain)" ] ; then \
	    echo "Error: there remain uncommitted changes." ; \
	    git status ; \
	    exit 1 ; \
	  fi
# Check the current package description.
	@ opam lint
# Create a fresh git branch and switch to it.
	@ echo "Preparing a release commit on a fresh release branch..."
	@ git checkout -b $(BRANCH)
# Remove subdirectories that do not need to (or must not) be distributed.
	@ make --quiet -C coq-menhirlib clean
	@ git rm -rf analysis attic demos headers releases test www --quiet
# Remove files that do not need to (or must not) be distributed.
# Keep check-tarball.sh because it is used below.
	@ git rm --quiet \
	    Makefile \
	    promote.sh \
	    *.md TODO* \
	    *.opam coq-menhirlib/descr
# Hardcode Menhir's version number in the files that need it.
	@ sed -i.bak 's/unreleased/$(DATE)/' dune-project
	@ rm -f dune-project.bak
	@ git add dune-project
	@ echo '\gdef\menhirversion{$(DATE)}' > doc/version.tex
	@ git add doc/version.tex
	@ echo 'Definition require_$(DATE) := tt.' > coq-menhirlib/src/Version.v
	@ git add coq-menhirlib/src/Version.v
# Compile the documentation.
	@ echo "Building the documentation..."
	@ make --quiet -C doc clean >/dev/null
	@ make --quiet -C doc all   >/dev/null
	@ git add -f $(DOC)
	@ echo '(include dune.manual)' >> doc/dune
	@ git add doc/dune
# Commit.
	@ echo "Committing..."
	@ git commit -m "Release $(DATE)." --quiet
# Check that the build and installation seem to work.
# We build our own archive, which is not necessarily identical to the one
# that gitlab creates for us once we publish our release. This should be
# good enough.
	@ echo "Creating an archive..."
	@ git archive --prefix=$(PACKAGE)/ --format=tar.gz --output=$(TARBALL) HEAD
	@ echo "Checking that this archive can be compiled and installed..."
	@ ./check-tarball.sh $(PACKAGE)
	@ echo "Removing this archive..."
	@ rm $(TARBALL)
# Create a git tag.
	@ git tag -a $(DATE) -m "Release $(DATE)."
# Save a copy of the manual.
	@ mkdir -p $(RELEASE)/doc
	@ cp $(DOC) $(RELEASE)/doc
# Switch back to the master branch.
	@ echo "Switching back to the master branch..."
	@ git checkout master
# Commit a copy of the manual *in the master branch* in releases/.
	@ echo "Committing a copy of the documentation..."
	@ cd $(RELEASE) && git add -f $(DOC)
	@ echo "Publishing the documentation online..."
	@ cd $(WWW) && git rm -rf doc
	@ cd $(WWW) && cp -r ../$(RELEASE)/doc .
	@ cd $(WWW) && git add $(DOC)
	@ git commit -m "Saved and published documentation for release $(DATE)."
# Done.
	@ echo "Done."
	@ echo "If happy, please type:"
	@ echo "  \"make publish\"   to push this release to gitlab.inria.fr"
	@ echo "  \"make export\"    to upload the manual to yquem.inria.fr"
	@ echo "  \"make opam\"      to create a new opam package"
	@ echo "Otherwise, please type:"
	@ echo "  \"make undo\"      to undo this release"

.PHONY: publish
publish:
# Push the new branch and tag to gitlab.inria.fr.
	@ git push origin $(BRANCH)
	@ git push --tags

.PHONY: undo
undo:
# Delete the new branch and tag.
	@ git branch -D $(BRANCH)
	@ git tag -d $(DATE)
# Delete the new commit on the master branch.
	@ git reset --hard HEAD~1

# -------------------------------------------------------------------------

# Copying the documentation to François' page on yquem.

# I would have like to serve these files on gitlab.inria.fr,
# but I don't know how to make them look like native .html
# and .pdf files.
# Also, I don't know how to obtain a stable URL that always
# points to the latest released version of the documentation.

RSYNC   := scp -p -C
TARGET  := yquem.inria.fr:public_html/menhir/

# This assumes that [make release] has been run.

.PHONY: export
export:
# Copy the documentation to yquem.
	$(RSYNC) $(RELEASE)/doc/* $(TARGET)

# -------------------------------------------------------------------------

# Publishing a new version of the opam packages.

# This entry assumes that [make release] has been run on the same day.

# There are two opam packages: one for menhir (part of the OCaml opam
# repository) and one for coq-menhirlib (part of the Coq opam repository).

# You need a version of opam-publish that supports --packages-directory:
#   git clone git@github.com:fpottier/opam-publish.git
#   cd opam-publish
#   git checkout 2.0
#   opam pin add opam-publish.dev .

# The following command should have been run once:
#   opam publish repo add opam-coq-archive coq/opam-coq-archive

# An abbreviation.
COQLIB   := coq-menhirlib

# Menhir's repository URL (https).
REPO     := https://gitlab.inria.fr/fpottier/menhir

# The archive URL (https).
ARCHIVE  := $(REPO)/repository/$(DATE)/archive.tar.gz

# Additional options for coq-menhirlib.
COQ_MENHIRLIB_PUBLISH_OPTIONS := \
  --repo coq/opam-coq-archive \
  --packages-directory released/packages \

.PHONY: opam
opam:
# Publish opam descriptions for menhirLib, menhirSdk, menhir.
	@ opam publish -v $(DATE) menhirLib.opam menhirSdk.opam menhir.opam $(ARCHIVE)
# Patch coq-menhirlib.opam.
# We replace the string DATEDASH with $(DATEDASH).
# We replace the string DATE with $(DATE).
	@ cat $(COQLIB).opam \
	  | sed -e 's/DATEDASH/$(DATEDASH)/g' \
	  | sed -e 's/DATE/$(DATE)/g' \
	  > $(COQLIB).patched.opam
# Publish an opam description for coq-menhirlib.
	@ opam publish -v $(DATE) $(COQ_MENHIRLIB_PUBLISH_OPTIONS) \
	    $(COQLIB).patched.opam $(ARCHIVE)
	@ rm $(COQLIB).patched.opam

# -------------------------------------------------------------------------

# Re-installing locally. This can overwrite an existing local installation.

.PHONY: pin
pin:
	opam pin --yes add menhirLib.dev . && \
	opam pin --yes add menhirSdk.dev . && \
	opam pin --yes add menhir.dev .

.PHONY: unpin
unpin:
	opam pin --yes remove menhirLib menhirSdk menhir

# -------------------------------------------------------------------------

# Running the Markdown linter on our Markdown files.

# For an explanation of mdl's error messages, see:
# https://github.com/mivok/markdownlint/blob/master/docs/RULES.md

# We use the command [expand] to expand tabs to spaces.

MDFILES = $(shell find . -name "*.md" | grep -v _build)

.PHONY: mdl
mdl:
	@ for f in $(MDFILES) ; do \
	  cp $$f $$f.bak && expand $$f.bak > $$f && rm $$f.bak ; \
	done
	@ mdl $(MDFILES)
