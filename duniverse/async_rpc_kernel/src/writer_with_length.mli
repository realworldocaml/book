(** A hack to make the bin-prot representation of things look as though they are
    packed into bigstrings, for backwards compatibility.

    The original version of the protocol used bigstrings to hide away user-defined types.
    The bin-prot representation of a record with such a bigstring in it looks like

    {v
      {field1|field2|...|{length|content}}
    v}

    where the content is the bin-prot representation of the value being written, and the
    length is that content's length.  This writer writes data in the same format without
    using an intermediate bigstring by first calculating the size of the content, then
    writing that as a Nat0 before writing the actual content.

    When reading, we use the fact that the length can be interpreted as a bin-prot encoded
    Nat0.  The grouping is changed from:

    {v
      {field1|field2|...|{length|content}}
    v}

    (only one bin-prot read) to:

    {v
      {field1|field2|...|length}|{content}
    v}

    (two bin-prot reads).

    If the last field is a variant type, there will be a variant tag before the length,
    but the exact same change of grouping will work.

    Note that this only works when the bigstrings are in the last field of a record (or a
    variant in the last field of a record).  It's easy to verify that this is true.
*)

val of_writer     : 'a Bin_prot.Type_class.writer -> 'a Bin_prot.Type_class.writer
val of_type_class : 'a Bin_prot.Type_class.t      -> 'a Bin_prot.Type_class.writer
