module Tree = Tree
module Comment = Comment
module Targets = Targets

module Generator =
struct
  module ML = ML
  module Reason = Reason
end

(* Exposed as an unstable public API for third-party packages to "hack" on, see

    https://github.com/ocaml/odoc/pull/252
    https://github.com/ocaml/odoc/issues/236. *)
module Url = Url
