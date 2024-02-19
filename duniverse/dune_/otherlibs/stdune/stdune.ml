[@@@alert
unstable "The API of this library is not stable and may change without notice."]

[@@@alert "-unstable"]

module Appendable_list = Appendable_list
module Nonempty_list = Nonempty_list
module Ansi_color = Ansi_color
module Array = Array
module Bytes = Bytes
module Char = Char
module Comparator = Comparator
module Either = Either
module Exn = Exn
module Exn_with_backtrace = Exn_with_backtrace
module Filename = Filename
module Hashtbl = Hashtbl
module Table = Table
module Int = Int
module Id = Id
module Io = Io
module List = List
module Map = Map
module Option = Option
module Or_exn = Or_exn
module Ordering = Ordering
module Pp = Pp
module Result = Result
module Set = Set
module Signal = Signal
module Comparable = Comparable
module Comparable_intf = Comparable_intf
module Staged = Staged
module String = String
module Bool = Bool
module Sexp = Sexp
module Path = Path
module Fpath = Fpath
module Univ_map = Univ_map
module Loc = Loc
module Env = Env
module Proc = Proc
module Type_eq = Type_eq
module Nothing = Nothing
module Bin = Bin
module Fdecl = Fdecl
module Unit = Unit
module Monad = Monad
module State = State
module Monoid = Monoid
module Float = Float
module Tuple = Tuple
module Poly = Poly
module Code_error = Code_error
module User_error = User_error
module User_message = User_message
module User_warning = User_warning
module Lexbuf = Lexbuf
module Scanf = Scanf
module Sys = Sys
module Pid = Pid
module Applicative = Applicative
module Top_closure_intf = Top_closure_intf
module Top_closure = Top_closure
module Seq = Seq
module Temp = Temp
module Queue = Queue
module Caller_id = Caller_id
module Dune_filesystem_stubs = Dune_filesystem_stubs
module Predicate = Predicate
module Bytes_unit = Bytes_unit

module Unix_error = struct
  include Dune_filesystem_stubs.Unix_error

  module Detailed = struct
    include Dune_filesystem_stubs.Unix_error.Detailed

    let to_dyn (error, syscall, arg) =
      Dyn.Record
        [ ("error", String (Unix.error_message error))
        ; ("syscall", String syscall)
        ; ("arg", String arg)
        ]

    let pp ?(prefix = "") unix_error =
      Pp.verbatim (prefix ^ to_string_hum unix_error)
  end
end

module File_kind = struct
  include Dune_filesystem_stubs.File_kind

  let to_dyn t = Dyn.String (to_string t)
end

module type Applicative = Applicative_intf.S

module type Monad = Monad_intf.S

module type Monoid = Monoid_intf.S

external reraise : exn -> _ = "%reraise"

let compare _ _ = `Use_Poly_compare

(* The following types are re-exported here so that they are always available in
   scope *)

type ('a, 'error) result = ('a, 'error) Result.t =
  | Ok of 'a
  | Error of 'error

type ('a, 'b) either = ('a, 'b) Either.t =
  | Left of 'a
  | Right of 'b

type ordering = Ordering.t =
  | Lt
  | Eq
  | Gt

let sprintf = Printf.sprintf

let ksprintf = Printf.ksprintf

let printfn a = ksprintf print_endline a
