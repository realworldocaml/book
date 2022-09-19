open Core

val bin_read_from_bigstring
  :  'a Bin_prot.Type_class.reader
  -> ?add_len:('a -> int)
  -> Bin_prot.Common.buf
  -> pos_ref:int ref
  -> len:Nat0.t
  -> location:string
  -> ('a, Protocol.Rpc_error.t) result
