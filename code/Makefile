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
      files-modules-and-programs-freq-obuild \
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
      packing \
      principal \
      back-end \
      back-end-bench \
      back-end-embed \
      functors \
      fcm \
      async \
      ffi \
      ctypes \
      ocp-index \
      installation

# html/xml files directory
OBJ=         _build

# All the source files
ml_src=       $(wildcard $(dirs:%=%/*.ml))
mli_src=      $(wildcard $(dirs:%=%/*.mli))
mll_src=      $(wildcard $(dirs:%=%/*.mll))
mly_src=      $(wildcard $(dirs:%=%/*.mly))
json_src=     $(wildcard $(dirs:%=%/*.json))
java_src=     $(wildcard $(dirs:%=%/*.java))
c_src=        $(wildcard $(dirs:%=%/*.c))
h_src=        $(wildcard $(dirs:%=%/*.h))
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
	      $(ascii_src) $(asm_src) $(mll_src) $(mly_src) $(h_src)
all_scripts=  $(scripts:%.sh=%.out) $(errscripts:%.errsh=%.out) \
	      $(topscripts) $(rawtscripts) $(rawscripts:%.rawsh=%.out)

# The source files all map to %.<part>.html and %.<part>.xml
all_src_html= $(all_src:%=$(OBJ)/%.0.html) $(all_scripts:%=$(OBJ)/%.0.html)
all_src_md=   $(all_src:%=$(OBJ)/%.0.xml) $(all_scripts:%=$(OBJ)/%.0.xml)

# Binaries we run
runtop=      rwo-run-toplevel
highlight=   rwo-syntax-highlight

.PHONY: all
all: $(all_src_html) $(all_src_md)
	@ :

print-%:
	@echo $($*)

json/run_github_org.out:: json/build_github_org.out
command-line-parsing/run_basic_md5.out:: command-line-parsing/build_basic_md5.out
command-line-parsing/get_basic_md5_version.out:: command-line-parsing/build_basic_md5.out
command-line-parsing/run_basic_and_default_md5.out:: command-line-parsing/build_basic_md5_with_optional_file.out command-line-parsing/build_basic_md5_with_default_file.out
command-line-parsing/run_cal_add_sub_days.out:: command-line-parsing/build_cal_add_sub_days.out
command-line-parsing/md5_completion.out:: command-line-parsing/build_basic_md5_with_flags.out
command-line-parsing/cal_completion.out:: command-line-parsing/build_cal_add_sub_days.out
parsing-test/run_broken_test.out:: parsing-test/build_test.out

$(OBJ)/%.mli.0.html $(OBJ)/%.mli.0.xml: %.mli
	@mkdir -p $(@D)
	$(highlight) -builddir $(OBJ) -cow $< || (cat $@ && rm -f $@)

$(OBJ)/%.ml.0.html $(OBJ)/%.ml.0.xml: %.ml
	@mkdir -p $(@D)
	$(highlight) -builddir $(OBJ) -cow $<  || (cat $@ && rm -f $@)

$(OBJ)/%.mll.0.html $(OBJ)/%.mll.0.xml: %.mll
	@mkdir -p $(@D)
	$(highlight) -builddir $(OBJ) -cow $<  || (cat $@ && rm -f $@)

$(OBJ)/%.mly.0.html $(OBJ)/%.mly.0.xml: %.mly
	@mkdir -p $(@D)
	$(highlight) -builddir $(OBJ) -cow $<  || (cat $@ && rm -f $@)

$(OBJ)/%.json.0.html $(OBJ)/%.json.0.xml: %.json
	@mkdir -p $(@D)
	$(highlight) -builddir $(OBJ) -pygments json $< || (cat $@ && rm -f $@)

$(OBJ)/%.syntax.0.html $(OBJ)/%.syntax.0.xml: %.syntax
	@mkdir -p $(@D)
	$(highlight) -builddir $(OBJ) -pygments html $< || (cat $@ && rm -f $@)

$(OBJ)/%.cmd.0.html $(OBJ)/%.cmd.0.xml: %.cmd
	@mkdir -p $(@D)
	$(highlight) -builddir $(OBJ) -pygments bash $< || (cat $@ && rm -f $@)

$(OBJ)/%.atd.0.html $(OBJ)/%.atd.0.xml: %.atd
	@mkdir -p $(@D)
	$(highlight) -builddir $(OBJ) -cow $< || (cat $@ && rm -f $@)

$(OBJ)/%.ascii.0.html $(OBJ)/%.ascii.0.xml: %.ascii
	@mkdir -p $(@D)
	$(highlight) -builddir $(OBJ) -raw "Ascii Art" $< || (cat $@ && rm -f $@)

$(OBJ)/%.java.0.html $(OBJ)/%.java.0.xml: %.java
	@mkdir -p $(@D)
	$(highlight) -builddir $(OBJ) -pygments java $< || (cat $@ && rm -f $@)

$(OBJ)/%.S.0.html $(OBJ)/%.S.0.xml: %.S
	@mkdir -p $(@D)
	$(highlight) -builddir $(OBJ) -pygments gas $< || (cat $@ && rm -f $@)

$(OBJ)/%.c.0.html $(OBJ)/%.c.0.xml: %.c
	@mkdir -p $(@D)
	$(highlight) -builddir $(OBJ) -pygments c $< || (cat $@ && rm -f $@)

$(OBJ)/%.h.0.html $(OBJ)/%.h.0.xml: %.h
	@mkdir -p $(@D)
	$(highlight) -builddir $(OBJ) -pygments c $< || (cat $@ && rm -f $@)

$(OBJ)/%.cpp.0.html $(OBJ)/%.cpp.0.xml: %.cpp
	@mkdir -p $(@D)
	$(highlight) -builddir $(OBJ) -pygments cpp $< || (cat $@ && rm -f $@)

$(OBJ)/%.scm.0.html $(OBJ)/%.scm.0.xml: %.scm
	@mkdir -p $(@D)
	$(highlight) -builddir $(OBJ) -pygments scheme $< || (cat $@ && rm -f $@)

$(OBJ)/%.topscript.0.xml $(OBJ)/%.topscript.0.html: %.topscript
	@mkdir -p $(@D)
	cd $(*D) && $(runtop) $(*F).topscript -fullfile "$<" -builddir "../$(@D)"

$(OBJ)/%.rawscript.0.xml $(OBJ)/%.rawscript.0.html: %.rawscript
	@mkdir -p $(@D)
	$(highlight) -builddir $(OBJ) -rawscript $<

$(OBJ)/%.out.0.html $(OBJ)/%.out.0.xml: %.out
	@mkdir -p $(@D)
	$(highlight) -builddir $(OBJ) -console $<

# The outputs of the shell scripts should be checked in.
%.out.full: %.sh
	./exec_script.sh $< > $@ 2>&1 || (cat $@ && rm -f $@)

%.out: %.out.full
	egrep -v '(^ocamlfind |^\+|^menhir --|added to search path$$)' < $< > $@

%.out.full: %.errsh
	! ./exec_script.sh $< > $@ 2>&1 || (cat $@ && rm -f $@)

%.out: %.rawsh
	cp $< $@

clean:
	find . -name \*.out | xargs rm -f
	rm -rf $(OBJ)

clean-%:
	find $* -name \*.out | xargs rm -f
	rm -rf $*/_build
	rm -rf _build/$*

distclean:
	for i in $(dirs); do $(MAKE) clean-$$i; done

.PRECIOUS: $(all_src) $(all_scripts)
