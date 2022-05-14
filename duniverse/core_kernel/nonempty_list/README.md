Nonempty_list
=============

A `'a Nonempty_list.t` is a list which is guaranteed to not be empty.
Its interface is the same as `List`'s, except that some types are
changed, and the interface may not be as complete.

For example, several `List` functions which may raise exceptions, such
as `hd_exn` and `reduce_exn`, have non-exception-raising versions.
Where possible, `List` functions which return another `List` have been
changed to return a `Nonempty_list` instead (e.g.,
`Nonempty_list.reverse` returns another `Nonempty_list`).  One case
where that's not possible is `Nonempty_list.tl`, which returns a
`List.t` rather than a `Nonempty_list.t`.

The constructors of a `Nonempty_list.t` are like those of a `List.t`
except there is no `[]` constructor. You can therefore match on and
make a `Nonempty_list.t` using the regular ocaml list syntax of
`hd::tl` or `[first; second; third]`.
