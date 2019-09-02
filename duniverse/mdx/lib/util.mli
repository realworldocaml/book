module Result : sig
  module List : sig
    val fold :
      f: ('acc -> 'a -> ('acc, 'err) Result.result) ->
      init: 'acc ->
      'a list ->
      ('acc, 'err) Result.result

    val map :
      f: ('a -> ('b, 'err) Result.result) ->
      'a list ->
      ('b list, 'err) Result.result
  end
end

module File : sig
  val read_lines : string -> string list
end
