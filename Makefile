trunk: 
	./gen-book.sh -m trunk

depend:
	cd scripts && ./build.sh
	cd code && $(MAKE)

milestone-%: all
	./gen-book.sh -p -m $*

html:
	./gen-book.sh -m trunk

pdf: trunk
	./gen-oreilly-pdf.sh

server:
	cd scripts && ./buildgh.sh && cd ../live_site && ../scripts/rwoserver.sh

clean:
	rm -rf build
	rm -rf scripts/_build
