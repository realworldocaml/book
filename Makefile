all: 
	./gen-book.sh

milestone-%: all
	./gen-book.sh -p -m $*

trunk: all
	./gen-book.sh -m trunk

server:
	cd scripts && ./buildgh.sh && cd ../live_site && ../scripts/rwoserver.sh

clean:
	rm -rf build
