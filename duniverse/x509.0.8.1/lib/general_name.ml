type _ k =
  | Other : Asn.oid -> string list k
  | Rfc_822 : string list k
  | DNS : string list k
  | X400_address : unit k
  | Directory : Distinguished_name.t list k
  | EDI_party : (string option * string) list k
  | URI : string list k
  | IP : Cstruct.t list k
  | Registered_id : Asn.oid list k

module K = struct
  type 'a t = 'a k

  let compare : type a b. a t -> b t -> (a, b) Gmap.Order.t = fun t t' ->
    let open Gmap.Order in
    match t, t' with
    | Rfc_822, Rfc_822 -> Eq | Rfc_822, _ -> Lt | _, Rfc_822 -> Gt
    | DNS, DNS -> Eq | DNS, _ -> Lt | _, DNS -> Gt
    | X400_address, X400_address -> Eq | X400_address, _ -> Lt | _, X400_address -> Gt
    | Directory, Directory -> Eq | Directory, _ -> Lt | _, Directory -> Gt
    | EDI_party, EDI_party -> Eq | EDI_party, _ -> Lt | _, EDI_party -> Gt
    | URI, URI -> Eq | URI, _ -> Lt | _, URI -> Gt
    | IP, IP -> Eq | IP, _ -> Lt | _, IP -> Gt
    | Registered_id, Registered_id -> Eq | Registered_id, _ -> Lt | _, Registered_id -> Gt
    | Other a, Other b -> match Asn.OID.compare a b with
      | 0 -> Eq
      | x when x < 0 -> Lt
      | _ -> Gt
end

include Gmap.Make(K)

let pp_k : type a. a k -> Format.formatter -> a -> unit = fun k ppf v ->
  let pp_strs = Fmt.(list ~sep:(unit "; ") string) in
  match k, v with
  | Rfc_822, x -> Fmt.pf ppf "rfc822 %a" pp_strs x
  | DNS, x ->
    Fmt.pf ppf "dns %a" Fmt.(list ~sep:(unit "; ") string) x
  | X400_address, () -> Fmt.string ppf "x400 address"
  | Directory, x ->
    Fmt.pf ppf "directory %a"
      Fmt.(list ~sep:(unit "; ") Distinguished_name.pp) x
  | EDI_party, xs ->
    Fmt.pf ppf "edi party %a"
      Fmt.(list ~sep:(unit "; ")
               (pair ~sep:(unit ", ")
                  (option ~none:(unit "") string) string)) xs
  | URI, x -> Fmt.pf ppf "uri %a" pp_strs x
  | IP, x -> Fmt.pf ppf "ip %a" Fmt.(list ~sep:(unit ";") Cstruct.hexdump_pp) x
  | Registered_id, x ->
    Fmt.pf ppf "registered id %a"
      Fmt.(list ~sep:(unit ";") Asn.OID.pp) x
  | Other oid, x -> Fmt.pf ppf "other %a: %a" Asn.OID.pp oid pp_strs x

let pp ppf m = iter (fun (B (k, v)) -> pp_k k ppf v ; Fmt.sp ppf ()) m

let merge_values : type a. a k -> a -> a -> a = fun k v v' ->
  match k, v, v' with
  | Other _, a, b -> a @ b
  | Registered_id, a, b -> a @ b
  | IP, a, b -> a @ b
  | URI, a, b -> a @ b
  | EDI_party, a, b -> a @ b
  | Directory, a, b -> a @ b
  | X400_address, (), () -> ()
  | DNS, a, b -> a @ b
  | Rfc_822, a, b -> a @ b

module Asn = struct
  open Asn.S
  (* GeneralName is also pretty pervasive. *)

  (* OID x ANY. Hunt down the alternatives.... *)
  (* XXX
   * Cross-check. NSS seems to accept *all* oids here and just assumes UTF8.
   * *)
  let another_name =
    let open Registry in
    let f = function
      | (oid, `C1 n) -> (oid, n)
      | (oid, `C2 n) -> (oid, n)
      | (oid, `C3 _) -> (oid, "")
    and g = function
      | (oid, "") -> (oid, `C3 ())
      | (oid, n ) when Name_extn.is_utf8_id oid -> (oid, `C1 n)
      | (oid, n ) -> (oid, `C2 n) in
    map f g @@
    sequence2
      (required ~label:"type-id" oid)
      (required ~label:"value" @@
       explicit 0
         (choice3 utf8_string ia5_string null))

  and or_address = null (* Horrible crap, need to fill it. *)

  let edi_party_name =
    sequence2
      (optional ~label:"nameAssigner" @@ implicit 0 Distinguished_name.Asn.directory_name)
      (required ~label:"partyName"    @@ implicit 1 Distinguished_name.Asn.directory_name)

  let general_name =
    let f = function
      | `C1 (`C1 (oid, x)) -> B (Other oid, [ x ])
      | `C1 (`C2 x) -> B (Rfc_822, [ x ])
      | `C1 (`C3 x) -> B (DNS, [ x ])
      | `C1 (`C4 _x) -> B (X400_address, ())
      | `C1 (`C5 x) -> B (Directory, [ x ])
      | `C1 (`C6 x) -> B (EDI_party, [ x ])
      | `C2 (`C1 x) -> B (URI, [ x ])
      | `C2 (`C2 x) -> B (IP, [ x ])
      | `C2 (`C3 x) -> B (Registered_id, [ x ])
    and g (B (k, v)) = match k, v with
      | Other oid, [ x ] -> `C1 (`C1 (oid, x))
      | Rfc_822, [ x ] -> `C1 (`C2 x)
      | DNS, [ x ] -> `C1 (`C3 x)
      | X400_address, () -> `C1 (`C4 ())
      | Directory, [ x ] -> `C1 (`C5 x)
      | EDI_party, [ x ] -> `C1 (`C6 x)
      | URI, [ x ] -> `C2 (`C1 x)
      | IP, [ x ] -> `C2 (`C2 x)
      | Registered_id, [ x ] -> `C2 (`C3 x)
      | _ -> Asn.S.error (`Parse "bad general name")
    in
    map f g @@
    choice2
      (choice6
         (implicit 0 another_name)
         (implicit 1 ia5_string)
         (implicit 2 ia5_string)
         (implicit 3 or_address)
         (* Everybody uses this as explicit, contrary to x509 (?) *)
         (explicit 4 Distinguished_name.Asn.name)
         (implicit 5 edi_party_name))
      (choice3
         (implicit 6 ia5_string)
         (implicit 7 octet_string)
         (implicit 8 oid))

  let gen_names =
    let f exts =
      List.fold_left (fun map (B (k, v)) ->
          match find k map with
          | None -> add k v map
          | Some b -> add k (merge_values k b v) map)
        empty exts
    and g map =
      List.flatten (List.map (fun (B (k, v)) ->
          match k, v with
          | Other oid, xs -> List.map (fun d -> B (Other oid, [ d ])) xs
          | Registered_id, xs -> List.map (fun d -> B (Registered_id, [ d ])) xs
          | IP, xs -> List.map (fun d -> B (IP, [ d ])) xs
          | URI, xs -> List.map (fun d -> B (URI, [ d ])) xs
          | EDI_party, xs -> List.map (fun d -> B (EDI_party, [ d ])) xs
          | Directory, xs -> List.map (fun d -> B (Directory, [ d ])) xs
          | X400_address, () -> [ B (X400_address, ()) ]
          | DNS, xs -> List.map (fun d -> B (DNS, [ d ])) xs
          | Rfc_822, xs -> List.map (fun d -> B (Rfc_822, [ d ])) xs)
          (bindings map))
    in
    map f g @@ sequence_of general_name
end

