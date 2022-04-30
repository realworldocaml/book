5.1.2 (unreleased)
------------------

- Fix PKG_CONFIG_PATH for latest macOS xquartz (#36, @smorimoto)

5.1.1 (02/02/2021)
------------------

- Fix configurator detection on native Windows (#19, @fdopen)
- Use caml_alloc_custom_mem when available (#23, @hhugo)
- Fix windows dependencies (#20, @jeremiedimino)
- Safe-string updates for native Windows (#28, fixes #27, @dra27)

5.1.0 (05/12/2019)
------------------

- Use pkg-config to query x11 compilation and linking flags + hardcode
  a few pkg-config paths for OSX (#17, fixes #16, @jeremiedimino)

5.0.0 (16/09/2019)
------------------

Initial release for OCaml >= 4.09.0.

2.0.0 (12/03/2019)
------------------

Initial release. It never made it to the opam repository as the
version number was too low.
