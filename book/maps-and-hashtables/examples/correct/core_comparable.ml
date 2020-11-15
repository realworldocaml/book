open Core

module Book = struct
  module T = struct
    type t = { title: string; isbn: string }
    [@@deriving compare, sexp]
  end
  include T
  include Comparable.Make(T)
end

let title_set = Set.empty (module String)

let add_title title_set (book : Book.t) =
  let open Book in
  Set.add title_set book.title

(* 
Error: This expression has type string but an expression was expected of type t
 *)