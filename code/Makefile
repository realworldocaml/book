dirs=json
scripts=$(wildcard $(dirs:%=%/*.sh))
topscripts=$(wildcard $(dirs:%=%/*.topscript))
out=$(scripts:%.sh=%.out) $(topscripts:%.topscript=%.0.out)

runtop=$(realpath ../scripts/_build/run_core_toplevel.byte)

.PHONY: all
all: $(out)
	@ :

%.0.out: %.topscript
	cd $(*D) && $(runtop) $(*F).topscript

%.out: %.sh
	./exec_script.sh $< > $@ || rm -f $@

clean:
	find . -name \*.out | xargs rm
