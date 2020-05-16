(** [Core_kernel] greatly expands the functionality available in Base while still
    remaining platform-agnostic.  Core_kernel changes more frequently (i.e., is less
    stable) than Base.

    Some modules are mere extensions of their counterparts in Base, usually adding generic
    functionality by including functors that make them binable, comparable, sexpable,
    blitable, etc.  The bulk of Core_kernel, though, is modules providing entirely new
    functionality.

    It is broken in two pieces, [Std_kernel] and [Std], where the first includes modules
    that aren't overridden by [Core], and the second defines modules that are. *)

open! Import

(** {1 Std_kernel}

    [Std_kernel] defines modules exposed by [Core_kernel] that are not overridden by
    [Core]. It is used in [core.ml] to re-export these modules. *)

(** {2 Modules imported from Base without modification} *)

module Applicative = Applicative
module Avltree = Avltree
module Backtrace = Backtrace
module Binary_search = Binary_search
module Buffer = Base.Buffer
module Comparisons = Comparisons
module Continue_or_stop = Continue_or_stop
module Equal = Equal
module Exn = Base.Exn
module Expect_test_config = Expect_test_config
module Field = Field
module Floatable = Floatable
module Formatter = Formatter
module Hash = Hash
module Heap_block = Heap_block
module In_channel = In_channel
module Int_conversions = Int_conversions
module Int_math = Int_math
module Intable = Intable
module Invariant = Invariant
module Monad = Monad
module Ordered_collection_common = Ordered_collection_common
module Out_channel = Out_channel
module Poly = Poly
module Polymorphic_compare = Poly [@@deprecated "[since 2018-11] use [Poly] instead"]
module Pretty_printer = Pretty_printer
module Random = Base.Random
module Sexp_maybe = Sexp.Sexp_maybe
module Staged = Base.Staged
module Stringable = Stringable
module Uchar = Uchar
module Validate = Validate
module Variant = Variant
module With_return = With_return
module Word_size = Word_size

(** {2 Modules that extend Base} *)

module Array = Array
module Binary_searchable = Binary_searchable
module Blit = Blit
module Bool = Bool
module Bytes = Bytes
module Char = Char
module Comparable = Comparable
module Comparator = Comparator
module Container = Container
module Either = Either
module Error = Error
module Float = Float
module Fn = Fn
module Hash_set = Hash_set
module Hashtbl = Hashtbl
module Hashtbl_intf = Hashtbl_intf
module Info = Info
module Int = Int
module Int_intf = Int_intf
module Int32 = Int32
module Int63 = Int63
module Int64 = Int64
module Lazy = Lazy
module Linked_queue = Linked_queue
module List = List
module Maybe_bound = Maybe_bound
module Nativeint = Nativeint
module Option = Option
module Ordering = Ordering
module Or_error = Or_error
module Printf = Printf
module Ref = Ref
module Result = Result
module Sequence = Sequence
module Set = Set
module Sexp = Sexp
module Sexpable = Sexpable
module Sign = Sign
module Sign_or_nan = Sign_or_nan
module Source_code_position = Source_code_position
module String = String
module Type_equal = Type_equal
module Unit = Unit

(** {2 Modules added by Core_kernel} *)

module Arg = Arg
module Bag = Bag
module Bigbuffer = Bigbuffer
module Bigsubstring = Bigsubstring
module Binable = Binable
module Bin_prot = Core_bin_prot
module Blang = Blang
module Bounded_index = Bounded_index
module Bus = Bus
module Byte_units = Byte_units
module Day_of_week = Day_of_week
module Debug = Debug
module Deque = Deque
module Deriving_hash = Deriving_hash
module Doubly_linked = Doubly_linked
module Ephemeron = Ephemeron
module Fdeque = Fdeque
module Float_with_finite_only_serialization = Float_with_finite_only_serialization
module Fqueue = Fqueue
module Gc = Gc
module Hash_queue = Hash_queue
module Hashable = Hashable
module Hexdump = Hexdump
module Hexdump_intf = Hexdump_intf
module Host_and_port = Host_and_port
module Identifiable = Identifiable
module Immediate_option = Immediate_option
module Immediate_option_intf = Immediate_option_intf
module Interfaces = Interfaces
module Map = Map
module Md5 = Md5
module Memo = Memo
module Month = Month
module No_polymorphic_compare = No_polymorphic_compare
module Nothing = Nothing
module Only_in_test = Only_in_test
module Option_array = Option_array
module Optional_syntax = Optional_syntax
module Percent = Percent
module Perms = Perms
module Pid = Pid

module Popcount = Base.Popcount
[@@warning "-3"]
[@@deprecated "[since 2018-10] use [popcount] functions in individual int modules"]

module Printexc = Printexc
module Queue = Queue
module Quickcheck = Quickcheck
module Quickcheck_intf = Quickcheck_intf
module Quickcheckable = Quickcheckable
module Robustly_comparable = Robustly_comparable
module Set_once = Set_once
module Splittable_random = Splittable_random
module Stable_comparable = Stable_comparable
module Stable_unit_test = Stable_unit_test
module Stack = Stack
module String_id = String_id
module Substring = Substring
module Substring_intf = Substring_intf
module Tuple = Tuple
module Tuple2 = Tuple.T2
module Tuple3 = Tuple.T3
module Type_immediacy = Type_immediacy
module Uniform_array = Uniform_array
module Union_find = Union_find
module Unique_id = Unique_id
module Unit_of_time = Unit_of_time
module Univ_map = Univ_map
module Validated = Validated
module Weak = Weak

module type Unique_id = Unique_id.Id

include T (** @open *)

(** {2 Top-level values} *)

type 'a _maybe_bound = 'a Maybe_bound.t =
  | Incl of 'a
  | Excl of 'a
  | Unbounded

let does_raise = Exn.does_raise

(** We perform these side effects here because we want them to run for any code that uses
    [Core_kernel].  If this were in another module in [Core_kernel] that was not used in
    some program, then the side effects might not be run in that program.  This will run
    as long as the program refers to at least one value directly in [Std_kernel];
    referring to values in [Std_kernel.Bool], for example, is not sufficient. *)
let () = Exn.initialize_module ()

let am_running_inline_test = am_running_inline_test
let am_running_test = am_running_test
let sec = Time_float.Span.of_sec

include Std_internal
include Not_found
