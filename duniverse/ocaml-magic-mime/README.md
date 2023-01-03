## magic-mime -- map filenames to common MIME types

This library contains a database of MIME types that maps filename extensions
into MIME types suitable for use in many Internet protocols such as HTTP or
e-mail.  It is generated from the `mime.types` file found in Unix systems, but
has no dependency on a filesystem since it includes the contents of the
database as an ML datastructure.

For example, here's how to lookup MIME types in the `utop` REPL:

    #require "magic-mime";;
    Magic_mime.lookup "/foo/bar.txt";;
    - : bytes = "text/plain"
    Magic_mime.lookup "bar.css";;
    - : bytes = "text/css"

### Internals

The following files need to be edited to add MIME types:

- mime.types: this is obtained by syncing from the Apache Foundation's
  [mime.types](https://svn.apache.org/repos/asf/httpd/httpd/trunk/docs/conf/mime.types)
  in the Apache Subversion repository.
- x-mime.types: these are the extension types, so non-standard `x-` prefixes are used here.
- file.types: full filenames of common occurrences that are useful to map onto a MIME type.
  OCaml-specific things like `opam` files show up here.

## More information

* WWW: <https://github.com/mirage/ocaml-magic-mime>
* E-mail: <mirageos-devel@lists.xenproject.org>
* Issues <https://github.com/mirage/ocaml-magic-mime/issues>
* Discourse: <https://discuss.ocaml.org> with the `mirageos` tag
