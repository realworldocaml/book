(** Book processing. The book's files are in HTMLBook format with some
    custom variations, e.g. we support <link rel="import"> nodes to
    enable inclusion of code blocks in external files. This module
    supports the processing of these files.
*)
open! Core
open Async

(** Source from which to build an HTML page.

    - [`Chapter file] - The file for a chapter. Path must be relative
    to current working directory.

    - [`Frontpage] - No other information needed. We generate the
    whole page programmatically. Output file is named "index.html".
*)
type src = [
| `Chapter of string
| `Frontpage
| `Toc_page
| `FAQs
| `Install
]

(** Make an HTML page from the given [src] and save it to [out_dir]. *)
val make
  :  ?repo_root:string
  -> out_dir:string
  -> src
  -> unit Deferred.t
