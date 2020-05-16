open Core
open Async

let get_unique_id =
  Rpc.Rpc.create
    ~name:"get-unique-id"
    ~version:0
    ~bin_query:Unit.bin_t
    ~bin_response:Int.bin_t
;;

let set_id_counter =
  Rpc.Rpc.create
    ~name:
      "set-id-counter"
    (* Note that the version number is 1, because there is an older v0 query defined below
       around. *)
    ~version:1
    ~bin_query:Int.bin_t
    ~bin_response:Unit.bin_t
;;

(* This type is here only for the purpose of getting the ability to bin_prot an int
   pair. *)
module Int_pair = struct
  type t = int * int [@@deriving bin_io]
end

let set_id_counter_v0 =
  Rpc.Rpc.create
    ~name:"set-id-counter"
    ~version:0
    ~bin_query:Int_pair.bin_t
    ~bin_response:Unit.bin_t
;;

let counter_values =
  Rpc.Pipe_rpc.create
    ~name:"counter-values"
    ~version:0
    ~bin_query:Unit.bin_t
    ~bin_response:Int.bin_t
    ~bin_error:Unit.bin_t
    ()
;;
