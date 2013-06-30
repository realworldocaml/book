dirs=        json

# All the source files

ml_src=       $(wildcard $(dirs:%=%/*.ml))
mli_src=      $(wildcard $(dirs:%=%/*.mli))
json_src=     $(wildcard $(dirs:%=%/*.json))
atd_src=      $(wildcard $(dirs:%=%/*.atd))
scripts=      $(wildcard $(dirs:%=%/*.sh))
topscripts=   $(wildcard $(dirs:%=%/*.topscript))

all_src=      $(ml_src) $(mli_src) $(json_src) $(atd_src)
all_scripts=  $(scripts:%.sh=%.out) $(topscripts)

# The source files all map to %.<part>.html and %.<part>.md

all_src_html= $(all_src:%=%.0.html) $(all_scripts:%=%.0.html)
all_src_md=   $(all_src:%=%.0.md) $(all_scripts:%=%.0.md)

# Binaries we run
runtop=      $(realpath ../scripts/_build/run_core_toplevel.byte)
highlight=   $(realpath ../scripts/_build/syntax_highlight_code.native)

.PHONY: all
all: $(all_src_html) $(all_src_md)
	@ :

%.mli.0.html %.mli.0.md: %.mli
	$(highlight) -cow $< || (cat $@ && rm -f $@)

%.ml.0.html %.ml.0.md: %.ml
	$(highlight) -cow $<  || (cat $@ && rm -f $@)
	
%.json.0.html %.json.0.md: %.json
	$(highlight) -pygments json $< || (cat $@ && rm -f $@)

%.atd.0.html: %.atd
	$(highlight) -cow $< || (cat $@ && rm -f $@)

%.topscript.0.md %.topscript.0.html: %.topscript
	cd $(*D) && $(runtop) $(*F).topscript

%.out: %.sh
	./exec_script.sh $< > $@ || (cat $@ && rm -f $@)

%.out.0.html %.out.0.md: %.out
	$(highlight) -console $<

clean:
	find . -name \*.out | xargs rm
	find . -name \*.html | xargs rm
	find . -name \*.md | xargs rm
