dirs=json
scripts=$(wildcard $(dirs:%=%/*.sh))
out=$(scripts:%.sh=%.out)

.PHONY: all
all: $(out)
	@ :

%.out: %.sh
	./exec_script.sh $< > $@ || rm -f $@

clean:
	rm -f $(out)
