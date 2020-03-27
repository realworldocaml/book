module type S =
sig
  type t
end

module type S1 = functor (_ : S) -> S

module F1 : functor (Arg : S) -> S

module F2 : functor (Arg : S) -> (S with type t = Arg.t)

module F3 : functor (Arg : S) ->
sig
  type t = Arg.t
end

module F4 (Arg : S) : S
