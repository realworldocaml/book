open! Import
include Bin_prot

module Writer = struct
  type 'a t = 'a Bin_prot.Type_class.writer =
    { size : 'a Size.sizer
    ; write : 'a Write.writer
    }

  let to_string t v =
    let len = t.size v in
    let buf = Bigstring.create len in
    let pos = t.write buf ~pos:0 v in
    assert (pos = Bigstring.length buf);
    let str = Bigstring.to_string buf in
    Bigstring.unsafe_destroy buf;
    str
  ;;

  let to_bytes t v =
    let len = t.size v in
    let buf = Bigstring.create len in
    let pos = t.write buf ~pos:0 v in
    assert (pos = Bigstring.length buf);
    let str = Bigstring.to_bytes buf in
    Bigstring.unsafe_destroy buf;
    str
  ;;
end
