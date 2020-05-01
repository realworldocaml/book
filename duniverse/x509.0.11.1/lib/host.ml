type t = [ `Strict | `Wildcard ] * [ `host ] Domain_name.t

let pp_typ ppf = function
  | `Strict -> Fmt.nop ppf ()
  | `Wildcard -> Fmt.string ppf "*."

let pp ppf (typ, nam) =
  Fmt.pf ppf "%a%a" pp_typ typ Domain_name.pp nam

module Set = struct
  include Set.Make(struct
      type nonrec t = t
      let compare a b = match a, b with
        | (`Strict, a), (`Strict, b)
        | (`Wildcard, a), (`Wildcard, b) -> Domain_name.compare a b
        | (`Strict, _), (`Wildcard, _) -> -1
        | (`Wildcard, _), (`Strict, _) -> 1
    end)

  let pp ppf s =
    Fmt.(list ~sep:(unit ", ") pp) ppf (elements s)
end

let is_wildcard name =
  match Domain_name.get_label name 0 with
  | Ok "*" -> Some (Domain_name.drop_label_exn name)
  | _ -> None

let host name =
  match Domain_name.of_string name with
  | Error _ -> None
  | Ok dn ->
    let wild, name = match is_wildcard dn with
      | None -> `Strict, dn
      | Some dn' -> `Wildcard, dn'
    in
    match Domain_name.host name with
    | Error _ -> None
    | Ok hostname -> Some (wild, hostname)

