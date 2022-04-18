BACKENDS := $(patsubst %.flags,%,$(wildcard *.flags))

# Copy TEMPLATE, with a suitable replacement of FLAGS,
# under the name [dune] in every .backend directory.

.PHONY: all
all:
	@ for b in $(BACKENDS) ; do \
	    mkdir -p $$b.backend && \
	    sed -e "s/FLAGS/$$(cat $$b.flags)/" TEMPLATE > $$b.backend/dune ; \
	  done

# Remove every .backend directory.

.PHONY: clean
clean:
	@ rm -rf *.backend
