(** Extensions to [Core.Core_filename]. *)

(** [normalize path] Removes as much "." and ".." from the path as possible. If the path
    is absolute they will all be removed. *)
val normalize : string -> string

(** [parent path] The parent of the root directory is the root directory @return the path
    to the parent of [path]. *)
val parent : string -> string

(** [make_relative ~to_:src f] returns [f] relative to [src].

    @raise Failure if [is_relative f <> is_relative src] *)
val make_relative : ?to_:string -> string -> string

(** [make_absolute src] Turn [src] into an absolute path expanded from the current working
    directory. *)
val make_absolute : string -> string

(** [expand] Makes a path absolute and expands [~] [~username] to home directories.  In
    case of error (e.g.: path home of a none existing user) raises [Failure] with a
    (hopefully) helpful message. *)
val expand : ?from:string -> string -> string

(** Splits a given path into a list of strings. *)
val explode : string -> string list

(** dual to explode *)
val implode : string list -> string

(**/**)
(* this is exported because it is used by core_extended.filename. *)
val normalize_path : string list -> string list
(**/**)

(** Filename.compare is a comparison that normalizes filenames ("./a" = "a"), uses a more
    human ready algorithm based on [String_extended.collate] ("rfc02.txt > rfc1.txt") and
    extenstions ("a.c" > "a.h").

    It is a total comparison on normalized filenames. *)
val compare: string -> string -> int

(** [with_open_temp_file ~write ~f prefix suffix] create a temporary file; runs [write] on
    its [out_channel] and then [f] on the resulting file. The file is removed once [f] is
    done running. *)
val with_open_temp_file:
  ?in_dir: string
  -> ?write:(out_channel -> unit)
  -> f: (string -> 'a)
  -> string -> string
  -> 'a

(** Runs [f] with a temporary dir as option and removes the directory afterwards. *)
val with_temp_dir: ?in_dir:string -> string -> string -> f:(string -> 'a) -> 'a

(** [is_parent dir1 dir2] returns [true] if [dir1] is a parent of [dir2]

    Note: This function is context independent, use [expand] if you want to consider
    relatives paths from a given point.

    In particular:
    - A directory is always the parent of itself.
    - The root is the parent of any directory
    - An absolute path is never the parent of relative one and vice versa.
    - ["../../a"] is never the parent of ["."] even if this could be true given
      form the current working directory.
*)
val is_parent : string -> string -> bool
