all: http-server-unix http-fetch-unix vchan-client-xen vchan-server-xen
	@

http-server-unix:
	cd http-server && mirage configure -t unix && make

http-fetch-unix:
	cd http-fetch && mirage configure -t unix && make

vchan-client-xen:
	cd vchan && mirage configure -f config_client.ml -t xen && make

vchan-server-xen:
	cd vchan && mirage configure -f config_server.ml -t xen && make
