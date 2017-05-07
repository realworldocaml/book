corebuild -pkg cohttp.async,yojson,textwrap search_with_timeout_no_leak_simple.native
./search_with_timeout_no_leak_simple.native "concurrent programming" ocaml -timeout 0.2s
