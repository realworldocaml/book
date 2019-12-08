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
