## 2.0.2

*2022-08-09*

### Added

- Expanded documentation of exceptions (@sim642, #148)

### Removed

- Removed undocumented and unused functions `write_float_fast` and
  `write_std_float_fast` from `Yojson`, `Yojson.Basic` and `Yojson.Safe`
  (@sim642, #149)

### Fixed

- Fix out-of-bounds error occurring when parsing object field names
  with atdgen parsers using `map_ident` or `map_lexeme` (@mjambon, #150)

## 2.0.1

*2022-06-28*

### Fixed

- Fix pretty-printing behavior for lists/objects inside lists (@c-cube
  #141, @mjambon #142)

## 2.0.0

*2022-06-02*

### Removed

- Removed dependency on easy-format and removed `pretty_format` from
  `Yojson`, `Yojson.Basic`, `Yojson.Safe` and `Yojson.Raw`. (@c-cube, #90)
- Removed dependency on `biniou`, simplifying the chain of dependencies. This
  changes some APIs:
  * `Bi_outbuf.t` in signatures is replaced with `Buffer.t`
  * `to_outbuf` becomes `to_buffer` and `stream_to_outbuf` becomes
    `stream_to_buffer`
  (@Leonidas-from-XIV, #74, and @gasche, #132)
- Removed `yojson-biniou` library
- Removed deprecated `json` type aliasing type `t` which has been available
  since 1.6.0 (@Leonidas-from-XIV, #100).
- Removed `json_max` type (@Leonidas-from-XIV, #103)
- Removed constraint that the "root" value being rendered (via either
  `pretty_print` or `to_string`) must be an object or array. (@cemerick, #121)
- Removed `validate_json` as it only made sense if the type was called `json`.
  (@Leonidas-from-XIV, #137)

### Add

- Add an opam package `yojson-bench` to deal with benchmarks dependency
  (@tmcgilchrist, #117)
- Add a benchmark to judge the respective performance of providing a buffer vs
  letting Yojson create an internal (#134, @Leonidas-from-XIV)
- Add an optional `suf` keyword argument was added to functions that write
  serialized JSON, thus allowing NDJSON output. Most functions default to not
  adding any suffix except for `to_file` (#124, @panglesd) and functions
  writing sequences of values where the default is `\n` (#135,
  @Leonidas-from-XIV)

### Change

- The `stream_from_*` and `stream_to_*` functions now use a `Seq.t` instead of a
  `Stream.t`, and they are renamed into `seq_from_*` and `seq_to_*` (@gasche, #131).

### Fix

- Avoid copying unnecessarily large amounts of strings when parsing (#85, #108,
  @Leonidas-from-XIV)
- Fix `stream_to_file` (#133, @tcoopman and @gasche)

## 1.7.0

*2019-02-14*

### Add

- Add documented `write_t` and `read_t` to modules defining a JSON ast type for compatibility
  with atdgen

## 1.6.0

*2019-01-30*

### Deprecate

- `json` types are deprecated in favor of their new `t` aliases, ahead of their removal in the next
  major release (#73, @Leonidas-from-XIV)

### Add

- Add a type `t` and monomorphic `equal`, `pp` and `show` (#73, @Leonidas-from-XIV)

## 1.5.0

### Change

- Use dune as a build system (#67, @Leonidas-from-XIV)
- reraise exceptions in `finish_string` instead of silencing them by raising a `Failure _`
- raise finalizer exceptions in `from_channel` and `from_lexbuf` readers

### Fix

- Fix a race condition in builds (#57, @avsm)

## 1.2.0

*2014-12-26*

- new function `Yojson.Safe.buffer_json` for saving a raw JSON string while
  parsing in order to parse later

## 1.1.8

*2014-01-19*

- cmxs is now generated for supported platforms

## 1.1.7

*2013-05-24*

- tolerate double quoted boolean "true" and "false" when a boolean is expected

## 1.1.6

*2013-05-16*

- fix a bug in float printing. now print number of significant figures rather
  than decimal places for `write_float_prec` and `write_std_float_prec`

## 1.1.5

*2013-03-19*

- new function `Yojson.sort` to sort fields in objects, and corresponding
  cmdline option.

## 1.1.4

*2012-12-31*

- proper support for escaped code points above U+FFFF

## 1.1.3

*2012-03-19*

- new function `Yojson.to_output` for writing to an OO channel; requires
  `biniou` >= 1.0.2

## 1.1.2

*2012-02-27*

- various enhancements

## 1.1.1

*2012-02-07*

- ydump now implies -s i.e. multiple whitespace-separated records are accepted.

## 1.1.0

*2012-01-26*

- `Yojson.Biniou` becomes `Yojson_biniou`, package `yojson.biniou`

## 1.0.2

*2011-04-27*

- improved error messages showing several lookahead bytes
- factored out `lexer_state` and `init_lexer` definitions
- added `read_null_if_possible` function (used by `atdgen`)

## 1.0.1

*2011-01-22*

- fixed serialization of negative ints using the `write_int` function (affects
  `atdgen`)

## 1.0.0

*2010-12-04*

- now requires `biniou` version 1.0.0 or higher

## 0.8.1

*2010-09-13*

- added `INSTALL` file

## 0.8.0

*2010-08-04*

- first release
