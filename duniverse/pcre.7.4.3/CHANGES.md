### 7.4.3 (2019-10-27)

  * Switched from `caml_alloc_custom` to `caml_alloc_custom_mem`.

    This should improve memory usage and GC performance.

  * Switched to OPAM file generation via `dune-project`


### 7.4.2 (2019-10-11)

  * Fixed warnings in C-stubs


### 7.4.1 (2019-02-21)

  * Fixed pattern execution bug due to DFA implementation


### 7.4.0 (2019-02-05)

  * Added DFA support

    New functions:

      * pcre_dfa_exec
      * unsafe_pcre_dfa_exec

    Thanks to Chas Emerick <chas@cemerick.com> for this contribution!


### 7.3.5 (2018-10-25)

  * Switched to dune, dune-release, and OPAM 2.0


### 7.3.4 (2017-11-22)

  * Improved finalization of regular expressions and tables for better
    performance


### 7.3.3 (2017-10-17)

  * Fixed external declaration bug in internal regexp compile function


### 7.3.2 (2017-10-10)

  * Improved compatibility with MSVC


### 7.3.1 (2017-10-08)

  * Used untagged integers when declaring external functions


### 7.3.0 (2017-07-27)

  * Switched to jbuilder and topkg
