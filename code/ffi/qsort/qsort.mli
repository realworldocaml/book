val compare_t :
  (unit Ctypes_static.ptr -> unit Ctypes_static.ptr -> int) Ctypes_static.fn
val qsort :
  unit Ctypes_static.ptr ->
  PosixTypes.size_t ->
  PosixTypes.size_t ->
  (unit Ctypes_static.ptr -> unit Ctypes_static.ptr -> int) -> unit
val qsort' :
  ('a -> 'a -> int) -> 'a Ctypes_static.carray -> 'a Ctypes_static.carray
val sort_stdin : unit -> unit
