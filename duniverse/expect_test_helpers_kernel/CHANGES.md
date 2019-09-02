## v0.11

- Removed `print_bin_ios*` functions; use `print_and_check_stable_type`.

## v0.10

- Removed `show_allocation`; instead use `require_no_allocation` or
  `require_allocation_does_not_exceed`.

- Fixed the CR produced by the require function when a test fails to use the
  correct name for require.
