open Core
open Filename
open Poly

(** Path *)

let explode path =
  let rec aux = function
    | "" | "." -> []
    | "/" -> ["/"]
    | path ->
      let dirname, basename = split path in
      basename :: aux dirname
  in
  List.rev (aux path)

let implode = function
  | [] -> "."
  | "/"::rest -> "/" ^ (String.concat ~sep:"/" rest)
  | l -> String.concat ~sep:"/" l

(* Takes out all "../" and "./" in a path, except that if it's a relative path it may
   start with some "../../" stuff at the front. *)
let normalize_path p =
  List.fold p
    ~init:[]
    ~f:(fun acc path_element ->
      match path_element, acc with
      (* parent of root is root, and root can only appear as first part of path *)
      | "..", ["/"] -> ["/"]
      (* just pop the stack, e.g. /foo/bar/../ becomes just /foo/ *)
      | "..", h::rest when h <> ".." -> rest
      | ".", v -> v
      | _ -> path_element :: acc
      (* accumulate regular dirs or chains of ... at the beginning of a
         relative path*))
  |> List.rev

let make_relative ?to_ f =
  if to_ = None && is_relative f then
    f
  else
    let to_ = match to_ with
      | Some dir ->
        if is_relative f <> is_relative dir then
          failwithf "make_relative ~to_:%s %s: cannot work on an absolute path and a \
                     relative one"
            dir
            f
            ();
        dir
      | None -> Sys.getcwd ()
    in
    let rec aux = function
      | (h :: t), (h' :: t') when String.equal h h' -> aux (t,t')
      | ".."::_, _ ->
        failwithf "make_relative ~to_:%s %s: negative lookahead (ie goes \"above\" the current directory)"
          to_
          f
          ()
      | p, p' -> (List.map ~f:(fun _ -> parent_dir_name) p) @ p'
    in
    let to_ = normalize_path (explode to_)
    and f   = normalize_path (explode f) in
    implode (aux (to_,f))

let%test_module "make_relative" = (module struct
  let make_relative ~to_ f =
    try
      Some (make_relative ~to_ f)
    with Failure _ -> None

  let%test _ = make_relative ~to_:".." "a" = None
  let%test _ = make_relative ~to_:".." "../a"= Some "a"
  let%test _ = make_relative ~to_:"c" "a/b" = Some "../a/b"
  let%test _ = make_relative ~to_:"/" "a/b" = None
end)

let normalize p = implode (normalize_path (explode p))

let%test_module "normalize" = (module struct
  let%test "id" = normalize "/mnt/local" ="/mnt/local"
  let%test "dot_dotdot" = normalize "/mnt/./../local" = "/local"
  let%test _ = normalize "/mnt/local/../global/foo" = "/mnt/global/foo"
  let%test "beyond_root" = normalize "/mnt/local/../../.." = "/"
  let%test "negative_lookahead" = normalize "../a/../../b" = "../../b"
end)

let (//) src p =
  if is_absolute p then
    p
  else
    concat src p

let make_absolute p = Sys.getcwd () // p

let user_home username =
  match Unix.Passwd.getbyname username with
  | Some user ->
    let pw_dir = user.Unix.Passwd.dir in
    if String.length pw_dir = 0 then
      failwithf "user's \"%s\"'s home is an empty string" username ()
    else pw_dir
  | None -> failwithf "user \"%s\" not found" username ()

let expand_user s =
  let expand_home = function
    | "~" -> user_home (Shell_internal.whoami ())
    | s -> user_home (String.chop_prefix_exn s ~prefix:"~")
  in
  if (String.is_prefix ~prefix:"~" s) then
    match String.lsplit2 ~on:'/' s with
    | Some (base,rest) ->
      expand_home base ^ "/" ^ rest
    | None -> expand_home s
  else
    s

let expand ?(from=".") p = normalize (Sys.getcwd () // from // expand_user p)

let rec is_parent_path p1 p2 =
  match p1, p2 with
  | ["/"], _ -> true
  | ((h1 :: p1) as l), (h2 :: p2) ->
    (h1 = h2 && is_parent_path p1 p2)
    || (h2 <> ".." && h2 <> "/" && List.for_all l ~f:((=) parent_dir_name))
  | l, [] -> List.for_all l ~f:((=) parent_dir_name)
  | [], (h :: _) -> h <> ".." && h <> "/"

let is_parent f1 f2 =
  is_parent_path (normalize_path (explode f1)) (normalize_path (explode f2))

(** Filename comparison *)

(*
   Extension comparison:
   We have a list of lists of extension that should appear consecutive to one
   another. Our comparison function works by mapping extensions to
   (extension*int) couples, for instance "c" is mapped to "h,1" meaning it
   should come right after h.
*)
let create_extension_map l =
  List.fold l
    ~f:(fun init l ->
      match l with
      | [] -> init
      | idx::_ ->
        List.foldi l
          ~f:(fun pos map v ->
            if Core.Map.mem map v then
              failwithf "Extension %s is defined twice" v ();
            Core.Map.set map
              ~key:v
              ~data:(idx,pos)
          )
          ~init
    )
    ~init:Map.empty

let extension_cmp map h1 h2 =
  let lookup e =
    Option.value (Map.find map e) ~default:(e,0)
  in
  Tuple2.compare (lookup h1) (lookup h2)
    ~cmp1:(String_extended.collate)
    ~cmp2:(Int.compare)


let basename_compare map f1 f2 =
  let ext_split s =
    Option.value (String.lsplit2 ~on:'.' s) ~default:(s,"")
  in
  Tuple2.compare (ext_split f1) (ext_split f2)
    ~cmp1:(String_extended.collate)
    ~cmp2:(extension_cmp map)

let filename_compare map v1 v2 =
  let v1 = explode v1
  and v2 = explode v2 in
  List.compare (basename_compare map) v1 v2

let parent p = normalize (concat p parent_dir_name)

let%test_module "parent" = (module struct
  let%test _ = parent "/mnt/local" = "/mnt"
  let%test _ = parent "/mnt/local/../global/foo" = "/mnt/global"
  let%test _ = parent "/mnt/local/../../global" = "/"
end)

let extension_map = create_extension_map [["h";"c"];["mli";"ml"]]

let compare = filename_compare extension_map

let with_open_temp_file ?in_dir ?(write=ignore) ~f prefix suffix =
  protectx (open_temp_file ?in_dir prefix suffix)
    ~f:(fun (fname,oc) ->
      protectx oc
        ~f:write
        ~finally:Out_channel.close;
      f fname)
    ~finally:(fun (fname,_) -> Unix.unlink fname)

let with_temp_dir ?in_dir prefix suffix ~f =
  protectx (temp_dir ?in_dir prefix suffix)
    ~f
    ~finally:(fun dirname -> ignore (Sys.command (sprintf "rm -rf '%s'" dirname)))
