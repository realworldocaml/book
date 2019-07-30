# This makefile is used for dev convenience. It is removed
# by the distribution process.

.PHONY: build doc clean

build:
	jbuilder build -j16

doc:
	@echo "waiting for jbuilder odoc support"
	@#topkg doc

clean:
	-rm -R _build
	-rm octavius.install
