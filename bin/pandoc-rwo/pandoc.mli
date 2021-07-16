(** Library to write pandoc filters. *)

(** {2 Types for pandoc} *)

(** Attributes: identifier, classes, key/values. *)
type attr = string * string list * (string * string) list

(** Target of a link: url and title. *)
type target = string * string

type list_number_style = DefaultStyle | Example | Decimal | LowerRoman | UpperRoman | LowerAlpha | UpperAlpha

type list_number_delim = DefaultDelim | Period | OneParen | TwoParensPeriod

type list_attributes = int * list_number_style * list_number_delim

type math_type = DisplayMath | InlineMath

type quote_type = DoubleQuote | SingleQuote

(** Format for raw blocks. *)
type format = string

(** Inline elements. *)
type inline =
  | Code of attr * string
  | Emph of inline list
  | Image of attr * inline list * target
  | Link of attr * inline list * target
  | Quoted of quote_type * inline list
  | RawInline of string * string
  | Space
  | SmallCaps of inline list
  | Str of string
  | UnhandledInline of Yojson.Basic.t

(** Block elements. *)
and block =
  | BulletList of block list list
  | CodeBlock of attr * string
  | Header of int * attr * inline list
  | OrderedList of list_attributes * block list list
  | Para of inline list
  | Plain of inline list
  | RawBlock of format * string
  | Div of attr * block list
  | UnhandledBlock of Yojson.Basic.t

(** JSON representation of a pandoc file. *)
type t

(** {2 Reading and writing} *)

(** Internal representation from JSON representation. *)
val of_json : Yojson.Basic.t -> t

(** JSON representation from internal representation. *)
val to_json : t -> Yojson.Basic.t

(** Construct representation of a markdown file. *)
val of_md_file : string -> t

(** API version. *)
val api_version : t -> int list

(** Blocks. *)
val blocks : t -> block list

(** {2 Metadata} *)

(** Value of a boolean metadata. *)
val meta_bool : t -> string -> bool

(** Value of a string metadata. *)
val meta_string : t -> string -> string

(** {2 Mapping functions} *)

(** General mapping function which maps a function on blocks and a function on
    inlines. If the functions return [None] the mapping is further recursed
    into. *)
val map : ?block:(block -> block list option) -> ?inline:(inline -> inline list option) -> t -> t

(** Map a function to every list of inlines. *)
val map_inlines : (inline list -> inline list) -> t -> t

(** Map a function to every block. If the function returns [None] then the block
    is further recursed into. *)
val map_blocks : (block -> block list option) -> t -> t

(** Map a function to every block at toplevel. *)
val map_top_blocks : (block -> block list) -> t -> t
