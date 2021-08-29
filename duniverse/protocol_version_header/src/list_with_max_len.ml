open! Core_kernel
include List_with_max_len_intf

module Make (Config : Config) = struct
  include Config

  type 'a t = 'a list [@@deriving bin_shape, bin_write]

  let __bin_read_t__ = List.__bin_read_t__

  let bin_read_t bin_read_el buf ~pos_ref =
    try Bin_prot.Read.bin_read_list_with_max_len ~max_len bin_read_el buf ~pos_ref with
    | exn -> Exn.reraise exn (Info.to_string_hum context)
  ;;

  let bin_reader_t (bin_reader_a : 'a Bin_prot.Type_class.reader) =
    { Bin_prot.Type_class.read = bin_read_t bin_reader_a.read
    ; vtag_read = __bin_read_t__ bin_reader_a.read
    }
  ;;

  let bin_t (bin_a : 'a Bin_prot.Type_class.t) =
    { Bin_prot.Type_class.shape = bin_shape_t bin_a.shape
    ; writer = bin_writer_t bin_a.writer
    ; reader = bin_reader_t bin_a.reader
    }
  ;;

  let of_list_exn l =
    let len = List.length l in
    if len > max_len
    then
      raise_s
        [%message "List is too large" (context : Info.t) (len : int) (max_len : int)];
    l
  ;;

  let sexp_of_t = List.sexp_of_t

  let t_of_sexp t_of_a sexp =
    let t = List.t_of_sexp t_of_a sexp in
    of_list_exn t
  ;;
end
