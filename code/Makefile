#OCAMLPARAM=_,we=+A
#export OCAMLPARAM

dirs= json \
      command-line-parsing guided-tour \
      variables-and-functions \
      lists-and-patterns \
      sexpr \
      parsing \
      parsing-test \
      maps-and-hash-tables \
      files-modules-and-programs \
      files-modules-and-programs-freq \
      files-modules-and-programs-freq-with-counter \
      files-modules-and-programs-freq-with-sig \
      files-modules-and-programs-freq-with-sig-abstract \
      files-modules-and-programs-freq-with-sig-abstract-fixed \
      files-modules-and-programs-freq-fast \
      files-modules-and-programs-freq-median \
      files-modules-and-programs-freq-with-sig-mismatch \
      files-modules-and-programs-freq-with-missing-def \
      files-modules-and-programs-freq-with-type-mismatch \
      files-modules-and-programs-freq-cyclic1 \
      files-modules-and-programs-freq-cyclic2 \
      records \
      variants \
      variants-termcol \
      variants-termcol-annotated \
      variants-termcol-fixed \
      objects \
      classes \
      classes-async \
      imperative-programming \
      error-handling \
      memory-repr \
      gc \
      front-end \
      back-end \
      back-end-bench \
      back-end-embed \
      functors \
      fcm \
      async

# html/md files directory
OBJ=         _build

# All the source files
ml_src=       $(wildcard $(dirs:%=%/*.ml))
mli_src=      $(wildcard $(dirs:%=%/*.mli))
mll_src=      $(wildcard $(dirs:%=%/*.mll))
mly_src=      $(wildcard $(dirs:%=%/*.mly))
json_src=     $(wildcard $(dirs:%=%/*.json))
java_src=     $(wildcard $(dirs:%=%/*.java))
c_src=        $(wildcard $(dirs:%=%/*.c))
asm_src=      $(wildcard $(dirs:%=%/*.S))
cmd_src=      $(wildcard $(dirs:%=%/*.cmd))
cpp_src=      $(wildcard $(dirs:%=%/*.cpp))
atd_src=      $(wildcard $(dirs:%=%/*.atd))
scm_src=      $(wildcard $(dirs:%=%/*.scm))
syn_src=      $(wildcard $(dirs:%=%/*.syntax))
ascii_src=    $(wildcard $(dirs:%=%/*.ascii))
scripts=      $(wildcard $(dirs:%=%/*.sh))
rawscripts=   $(wildcard $(dirs:%=%/*.rawsh))
errscripts=   $(wildcard $(dirs:%=%/*.errsh))
topscripts=   $(wildcard $(dirs:%=%/*.topscript))
rawtscripts=   $(wildcard $(dirs:%=%/*.rawscript))

all_src=      $(ml_src) $(mli_src) $(json_src) $(java_src) $(c_src) \
	      $(cmd_src) $(cpp_src) $(atd_src) $(scm_src) $(syn_src) \
	      $(ascii_src) $(asm_src) $(mll_src) $(mly_src)
all_scripts=  $(scripts:%.sh=%.out) $(errscripts:%.errsh=%.out) \
	      $(topscripts) $(rawtscripts) $(rawscripts:%.rawsh=%.out)

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
command-line-parsing/run_basic_md5.out:: command-line-parsing/build_basic_md5.out
command-line-parsing/run_basic_and_default_md5.out:: command-line-parsing/build_basic_md5_with_optional_file.out command-line-parsing/build_basic_md5_with_default_file.out
command-line-parsing/run_cal_add_sub_days.out:: command-line-parsing/build_cal_add_sub_days.out
command-line-parsing/md5_completion.out:: command-line-parsing/build_basic_md5_with_flags.out
command-line-parsing/cal_completion.out:: command-line-parsing/build_cal_add_sub_days.out
parsing-test/run_broken_test.out:: parsing-test/build_test.out

$(OBJ)/%.mli.0.html $(OBJ)/%.mli.0.md: %.mli
	@mkdir -p $(@D)
	$(highlight) -builddir $(OBJ) -cow $< || (cat $@ && rm -f $@)

$(OBJ)/%.ml.0.html $(OBJ)/%.ml.0.md: %.ml
	@mkdir -p $(@D)
	$(highlight) -builddir $(OBJ) -cow $<  || (cat $@ && rm -f $@)

$(OBJ)/%.mll.0.html $(OBJ)/%.mll.0.md: %.mll
	@mkdir -p $(@D)
	$(highlight) -builddir $(OBJ) -cow $<  || (cat $@ && rm -f $@)

$(OBJ)/%.mly.0.html $(OBJ)/%.mly.0.md: %.mly
	@mkdir -p $(@D)
	$(highlight) -builddir $(OBJ) -cow $<  || (cat $@ && rm -f $@)

$(OBJ)/%.json.0.html $(OBJ)/%.json.0.md: %.json
	@mkdir -p $(@D)
	$(highlight) -builddir $(OBJ) -pygments json $< || (cat $@ && rm -f $@)

$(OBJ)/%.syntax.0.html: %.syntax
	@mkdir -p $(@D)
	$(highlight) -builddir $(OBJ) -pygments html $< || (cat $@ && rm -f $@)

$(OBJ)/%.cmd.0.html: %.cmd
	@mkdir -p $(@D)
	$(highlight) -builddir $(OBJ) -pygments bash $< || (cat $@ && rm -f $@)

$(OBJ)/%.atd.0.html: %.atd
	@mkdir -p $(@D)
	$(highlight) -builddir $(OBJ) -cow $< || (cat $@ && rm -f $@)

$(OBJ)/%.ascii.0.html: %.ascii
	@mkdir -p $(@D)
	$(highlight) -builddir $(OBJ) -raw "Ascii Art" $< || (cat $@ && rm -f $@)

$(OBJ)/%.java.0.html: %.java
	@mkdir -p $(@D)
	$(highlight) -builddir $(OBJ) -pygments java $< || (cat $@ && rm -f $@)

$(OBJ)/%.S.0.html: %.S
	@mkdir -p $(@D)
	$(highlight) -builddir $(OBJ) -pygments gas $< || (cat $@ && rm -f $@)

$(OBJ)/%.c.0.html: %.c
	@mkdir -p $(@D)
	$(highlight) -builddir $(OBJ) -pygments c $< || (cat $@ && rm -f $@)

$(OBJ)/%.cpp.0.html: %.cpp
	@mkdir -p $(@D)
	$(highlight) -builddir $(OBJ) -pygments cpp $< || (cat $@ && rm -f $@)

$(OBJ)/%.scm.0.html: %.scm
	@mkdir -p $(@D)
	$(highlight) -builddir $(OBJ) -pygments scheme $< || (cat $@ && rm -f $@)

$(OBJ)/%.topscript.0.md $(OBJ)/%.topscript.0.html: %.topscript
	@mkdir -p $(@D)
	cd $(*D) && $(runtop) $(*F).topscript -builddir "../$(@D)"

$(OBJ)/%.rawscript.0.md $(OBJ)/%.rawscript.0.html: %.rawscript
	@mkdir -p $(@D)
	$(highlight) -builddir $(OBJ) -rawscript $<

$(OBJ)/%.out.0.html $(OBJ)/%.out.0.md: %.out
	@mkdir -p $(@D)
	$(highlight) -builddir $(OBJ) -console $<

# The outputs of the shell scripts should be checked in.
%.out.full: %.sh
	./exec_script.sh $< > $@ 2>&1 || (cat $@ && rm -f $@)

%.out: %.out.full
	egrep -v "(^ocamlfind |^\+)" < $< > $@

%.out.full: %.errsh
	! ./exec_script.sh $< > $@ 2>&1 || (cat $@ && rm -f $@)

%.out: %.rawsh
	cp $< $@

clean:
	find . -name \*.out | xargs rm
	rm -rf $(OBJ)

clean-%:
	find $* -name \*.out | xargs rm
	rm -rf $(OBJ)/$*

.PRECIOUS: $(all_src) $(all_scripts)
