all: 
	./gen-book.sh

milestone-%: all
	./gen-book.sh -p -m $*

server:
	cd scripts && ./buildgh.sh && cd ../live_site && ../scripts/rwoserver.sh

clean:
	rm -rf build
