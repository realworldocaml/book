include Parsexp_intf.Parsexp

(*_ For tests *)
(*_ See the Jane Street Style Guide for an explanation of [Private] submodules:

  https://opensource.janestreet.com/standards/#private-submodules *)
module Private : sig
  module Parser_automaton = Parser_automaton
end
