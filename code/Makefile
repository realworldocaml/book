dirs=        json

# html/md files directory
OBJ=         _build

# All the source files
ml_src=       $(wildcard $(dirs:%=%/*.ml))
mli_src=      $(wildcard $(dirs:%=%/*.mli))
json_src=     $(wildcard $(dirs:%=%/*.json))
atd_src=      $(wildcard $(dirs:%=%/*.atd))
scripts=      $(wildcard $(dirs:%=%/*.sh))
rawscripts=   $(wildcard $(dirs:%=%/*.rawsh))
topscripts=   $(wildcard $(dirs:%=%/*.topscript))

all_src=      $(ml_src) $(mli_src) $(json_src) $(atd_src)
all_scripts=  $(scripts:%.sh=%.out) $(topscripts) $(rawscripts:%.rawsh=%.out)

# The source files all map to %.<part>.html and %.<part>.md
all_src_html= $(all_src:%=$(OBJ)/%.0.html) $(all_scripts:%=$(OBJ)/%.0.html)
all_src_md=   $(all_src:%=$(OBJ)/%.0.md) $(all_scripts:%=$(OBJ)/%.0.md)

# Binaries we run
runtop=      $(realpath ../scripts/_build/run_core_toplevel.byte)
highlight=   $(realpath ../scripts/_build/syntax_highlight_code.native)

.PHONY: all
all: $(all_src_html) $(all_src_md)
	@ :

print-%:
	@echo $($*)

json/run_github_org.out:: json/build_github_org.out


$(OBJ)/%.mli.0.html $(OBJ)/%.mli.0.md: %.mli
	@mkdir -p $(@D)
	$(highlight) -builddir $(OBJ) -cow $< || (cat $@ && rm -f $@)

$(OBJ)/%.ml.0.html $(OBJ)/%.ml.0.md: %.ml
	@mkdir -p $(@D)
	$(highlight) -builddir $(OBJ) -cow $<  || (cat $@ && rm -f $@)
	
$(OBJ)/%.json.0.html $(OBJ)/%.json.0.md: %.json
	@mkdir -p $(@D)
	$(highlight) -builddir $(OBJ) -pygments json $< || (cat $@ && rm -f $@)

$(OBJ)/%.atd.0.html: %.atd
	@mkdir -p $(@D)
	$(highlight) -builddir $(OBJ) -cow $< || (cat $@ && rm -f $@)

$(OBJ)/%.topscript.0.md $(OBJ)/%.topscript.0.html: %.topscript
	@mkdir -p $(@D)
	cd $(*D) && $(runtop) $(*F).topscript -builddir "../$(@D)"

$(OBJ)/%.out.0.html $(OBJ)/%.out.0.md: %.out
	@mkdir -p $(@D)
	$(highlight) -builddir $(OBJ) -console $<

# The outputs of the shell scripts should be checked in.
%.out: %.sh
	./exec_script.sh $< > $@ || (cat $@ && rm -f $@)

%.out: %.rawsh
	cp $< $@

clean:
	find . -name \*.out | xargs rm
	find . -name \*.html | xargs rm
	find . -name \*.md | xargs rm
	rm -rf $(OBJ)

.PRECIOUS: $(all_src) $(all_scripts)
