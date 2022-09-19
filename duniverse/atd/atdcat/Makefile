DUNE ?= dune

.PHONY: build
build:
	$(DUNE) build

.PHONY: test
test:
	$(DUNE) runtest -f

# Show the expected JSON Schema in YAML format (for visual inspection)
.PHONY: yaml
yaml:
	yq e -P test/schema.expected.json
