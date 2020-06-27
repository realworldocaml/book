let def  x = function None -> x | Some y -> y
let def' x = fun y -> if y = x then None else Some y

let decode codec cs =
  let open Rresult.R.Infix in
  Asn.decode codec cs >>= fun (a, cs) ->
  if Cstruct.len cs = 0 then Ok a else Error (`Parse "Leftover")

let projections_of encoding asn =
  let c = Asn.codec encoding asn in (decode c, Asn.encode c)

module Hashtbl(T : Hashtbl.HashedType) = struct
  include Hashtbl.Make (T)
  let of_assoc xs =
    let ht = create 16 in List.iter (fun (a, b) -> add ht a b) xs; ht
end

module OID_H = Hashtbl (struct
  type t = Asn.oid let (equal, hash) = Asn.OID.(equal, hash)
end)

let case_of_oid ~default xs =
  let ht = OID_H.of_assoc xs in fun a ->
    try OID_H.find ht a with Not_found -> default a

let case_of_oid_f ~default xs =
  let ht = OID_H.of_assoc xs in fun (a, b) ->
    (try OID_H.find ht a with Not_found -> default a) b

(*
 * A way to parse by propagating (and contributing to) exceptions, so those can
 * be handles up in a single place. Meant for parsing embedded structures.
 *
 * XXX Would be nicer if combinators could handle embedded structures.
 *)
let project_exn asn =
  let c = Asn.(codec der) asn in
  let dec cs = match decode c cs with
    | Ok a      -> a
    | Error err -> Asn.S.error err in
  (dec, Asn.encode c)

let err_to_msg f = Rresult.R.reword_error (function `Parse msg -> `Msg msg) f

(* specified in RFC 5280 4.1.2.5.2 - "MUST NOT include fractional seconds" *)
let generalized_time_no_frac_s =
  Asn.S.(map
           (fun x ->
              if Ptime.Span.(equal zero (Ptime.frac_s x)) then
                x
              else
                parse_error "generalized time has fractional seconds")
           (fun y -> Ptime.truncate ~frac_s:0 y)
           generalized_time)
