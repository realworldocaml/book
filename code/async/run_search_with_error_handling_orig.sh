corebuild -pkg cohttp.async,yojson,textwrap search_with_error_handling.native
./search_with_error_handling.native -servers localhost,api.duckduckgo.com "Concurrent Programming" OCaml
