(*---------------------------------------------------------------------------
   Copyright (c) 2014 Daniel C. Bünzli. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

(** Basic OS interaction.

    Open the module to use it, this defines only modules in your scope.

    {b WARNING.} This API is still subject to change in the future but
    feedback and suggestions are welcome on the project's issue tracker.

    {e %%VERSION%% - {{:%%PKG_HOMEPAGE%% }homepage}} *)

(** {1 Basic types} *)

open Rresult
open Astring

(** Named string patterns.

    Named string patterns are strings with variables of the form
    ["$(VAR)"] where [VAR] is any (possibly empty) sequence of bytes
    except [')'] or [',']. In a named string pattern a ["$"] literal
    must be escaped by ["$$"].

    Named string patterns can be used to {{!format}format} strings or
    to {{!match}match} data. *)
module Pat : sig

  (** {1:pats Patterns} *)

  type t
  (** The type for patterns. *)

  val v : string -> t
  (** [v s] is a pattern from the string [s].

      @raise Invalid_argument if [s] is not a valid pattern. Use
      {!of_string} to deal with errors. *)

  val empty : t
  (** [empty] is an empty pattern. *)

  val dom : t -> String.Set.t
  (** [dom p] is the set of variables in [p]. *)

  val equal : t -> t -> bool
  (** [equal p p'] is [p = p']. *)

  val compare : t -> t -> int
  (** [compare p p'] is {!Pervasives.compare}[ p p']. *)

  val of_string : string -> (t, [> R.msg]) Result.result
  (** [of_string s] parses [s] according to the pattern syntax
      (i.e. a literal ['$'] must be represented by ["$$"] in [s]). *)

  val to_string : t -> string
  (** [to_string p] converts [p] to a string according to the pattern
      syntax (i.e. a literal ['$'] will be represented by ["$$"]).  *)

  val pp : Format.formatter -> t -> unit
  (** [pp ppf p] prints [p] on [ppf] according to the pattern syntax. *)

  val dump : Format.formatter -> t -> unit
  (** [dump ppf p] prints [p] as a syntactically valid OCaml string on
      [ppf]. *)

  (** {1:subst Substitution}

      {b Note.} Substitution replaces variables with data,
      i.e. strings. It cannot substitute variables with variables. *)

  type defs = string String.Map.t
  (** Type type for variable definitions. Maps pattern variable names
      to strings. *)

  val subst : ?undef:(string -> string option) -> defs -> t -> t
  (** [subst ~undef defs p] tries to substitute variables in [p] by
      their value. First a value is looked up in [defs] and if
      not found in [undef]. [undef] defaults to [(fun _ -> None)]. *)

  val format : ?undef:(string -> string) -> defs -> t -> string
  (** [format ~undef defs p] substitutes all variables in [p] with
      data. First a value is looked up in [defs] and if not found in
      [undef] (defaults to [fun _ -> ""]). The resulting string is
      not in pattern syntax (i.e.  a literal ['$'] is represented by
      ['$'] in the result). *)

  (** {1:match Matching}

      Pattern variables greedily match from zero to more bytes from
      left to right. This is [.*] in regexp speak. *)

  val matches : t -> string -> bool
  (** [matches p s] is [true] iff the string [s] matches [p]. Here are a few
      examples:
      {ul
      {- [matches (v "$(mod).mli") "string.mli"] is [true].}
      {- [matches (v "$(mod).mli") "string.mli "] is [false].}
      {- [matches (v "$(mod).mli") ".mli"] is [true].}
      {- [matches (v "$(mod).$(suff)") "string.mli"] is [true].}
      {- [matches (v "$(mod).$(suff)") "string.mli "] is [true].}} *)

  val query : ?init:defs -> t -> string -> defs option
  (** [query ~init p s] is like {!matches} except that a matching
      string returns a map from each pattern variable to its matched
      part in the string (mappings are added to [init], defaults to
      {!String.Map.empty}) or [None] if [s] doesn't match [p].  If a
      variable appears more than once in [pat] the first match is
      returned in the map. *)
end

(** Command lines.

    Both command lines and command line fragments using the same are
    represented with the same {{!t}type}.

    When a command line is {{!section:OS.Cmd.run}run}, the first
    element of the line defines the program name and each other
    element is an argument that will be passed {e as is} in the
    program's [argv] array: no shell interpretation or any form of
    argument quoting and/or concatenation occurs.

    See {{!ex}examples}. *)
module Cmd : sig

  (** {1:frags Command line fragments} *)

  type t
  (** The type for command line fragments. *)

  val v : string -> t
  (** [v cmd] is a new command line (or command line fragment)
      whose first argument is [cmd]. *)

  val empty : t
  (** [empty] is an empty command line. *)

  val is_empty : t -> bool
  (** [is_empty l] is [true] iff [l] is empty. *)

  val ( % ) : t -> string -> t
    (** [l % arg] adds [arg] to the command line [l]. *)

  val ( %% ) : t -> t -> t
  (** [l %% frag] appends the line fragment [frag] to [l]. *)

  val add_arg : t -> string -> t
  (** [add_arg l arg] is [l % arg]. *)

  val add_args : t -> t -> t
  (** [add_args l frag] is [l %% frag]. *)

  val on : bool -> t -> t
  (** [on bool line] is [line] if [bool] is [true] and {!empty}
      otherwise. *)

  val p : Fpath.t -> string
  (** [p] is {!Fpath.to_string}. This combinator makes path argument
      specification brief. *)

  (** {1:lines Command lines} *)

  val line_tool : t -> string option
  (** [line_tool l] is [l]'s first element, usually the executable tool
      name or file path. *)

  val get_line_tool : t -> string
  (** [get_line_tool l] is like {!line_tool} but @raise Invalid_argument
      if there's no first element. *)

  val line_args : t -> string list
  (** [line_args] is [l]'s command line arguments, the elements of [l] without
      the command name. *)

  val line_exec : t -> string option
  (** @deprecated Use {!line_tool} instead. *)

  val get_line_exec : t -> string
  (** @deprecated Use {!get_line_tool} instead. *)

  (** {1:predicates Predicates and comparison} *)

  val equal : t -> t -> bool
  (** [equal l l'] is [true] iff [l] and [l'] are litterally equal. *)

  val compare : t -> t -> int
  (** [compare l l'] is a total order on command lines. *)

  (** {1:convert Conversions and pretty printing} *)

  val of_string : string -> (t, R.msg) result
  (** [of_string s] tokenizes [s] into a command line. The tokens
      are recognized according to the [token] production of the following
      grammar which should be mostly be compatible with POSIX shell
      tokenization.
{v
white   ::= ' ' | '\t' | '\n' | '\x0B' | '\x0C' | '\r'
squot   ::= '\''
dquot   ::= '\"'
bslash  ::= '\\'
tokens  ::= white+ tokens | token tokens | ϵ
token   ::= ([^squot dquot white] | squoted | dquoted) token | ϵ
squoted ::= squot [^squot]* squot
dquoted ::= dquot (qchar | [^dquot])* dquot
qchar   ::= bslash (bslash | dquot | '$' | '`' | '\n')
v}

      [qchar] are substitued by the byte they escape except for ['\n']
      which removes the backslash and newline from the byte stream.
      [squoted] and [dquoted] represent the bytes they enclose. *)

  val to_string : t -> string
  (** [to_string l] converts [l] to a string that can be passed
      to the
      {{:http://pubs.opengroup.org/onlinepubs/9699919799/functions/system.html}
      [command(3)]} POSIX system call. *)

  val to_list : t -> string list
  (** [to_list l] is [l] as a list of strings. *)

  val of_list : ?slip:string -> string list -> t
  (** [of_list ?slip l] is a command line from the list of arguments
      [l].  If [slip] is specified it is added on the command line
      before each element of [l]. *)

  val of_values : ?slip:string -> ('a -> string) -> 'a list -> t
  (** [of_values ?slip conv l] is like {!of_list} but acts on a list
      of values, each converted to an argument with [conv]. *)

  val pp : Format.formatter -> t -> unit
  (** [pp ppf l] formats an unspecified representation of [l] on
      [ppf]. *)

  val dump : Format.formatter -> t -> unit
  (** [dump ppf l] dumps and unspecified representation of [l]
      on [ppf]. *)

  (** {1:ex Examples}
{[
let ls path = Cmd.(v "ls" % "-a" % p path)

let tar archive path = Cmd.(v "tar" % "-cvf" % p archive % p path)

let opam cmd = Cmd.(v "opam" % cmd)

let opam_install pkgs = Cmd.(opam "install" %% of_list pkgs)

let ocamlc ?(debug = false) file =
  Cmd.(v "ocamlc" % "-c" %% on debug (v "-g") % p file)

let ocamlopt ?(profile = false) ?(debug = false) incs file =
  let profile = Cmd.(on profile @@ v "-p") in
  let debug = Cmd.(on debug @@ v "-g") in
  let incs = Cmd.of_list ~slip:"-I" incs in
  Cmd.(v "ocamlopt" % "-c" %% debug %% profile %% incs % p file)
]} *)
end

(** {1 OS interaction} *)

(** OS interaction *)
module OS : sig

  (** {1 Results}

      The functions of this module never raise {!Sys_error} or
      {!Unix.Unix_error} instead they turn these errors into
      {{!Rresult.R.msgs}error messages}. If you need fine grained
      control over unix errors use the lower level functions in
      {!Bos.OS.U}. *)

  type ('a, 'e) result = ('a, [> R.msg] as 'e) Result.result
  (** The type for OS results. *)

  (** {1:env Environment variables and program arguments} *)

  (** Environment variables. *)
  module Env : sig

    (** {1:env Process environment} *)

    type t = string String.map
    (** The type for process environments. *)

    val current : unit -> (t, 'e) result
    (** [current ()] is the current process environment. *)

    (** {1:vars Variables} *)

    val var : string -> string option
    (** [var name] is the value of the environment variable [name], if
        defined. *)

    val set_var : string -> string option -> (unit, 'e) result
    (** [set_var name v] sets the environment variable [name] to [v].

        {b BUG.} The {!Unix} module doesn't bind to [unsetenv(3)],
        hence for now using [None] will not unset the variable, it
        will set it to [""]. This behaviour may change in future
        versions of the library. *)

    val opt_var : string -> absent:string -> string
    (** [opt_var name absent] is the value of the optionally defined
        environment variable [name] if defined and [absent] if
        undefined. *)

    val req_var : string -> (string, 'e) result
    (** [req_var name]  is the value of the environment variable [name] or
        an error if [name] is undefined in the environment. *)

    (** {1 Typed lookup}

        See the {{!examples}examples}. *)

    type 'a parser = string -> ('a, R.msg) Result.result
    (** The type for environment variable value parsers. *)

    val parser : string -> (string -> 'a option) -> 'a parser
    (** [parser kind k_of_string] is an environment variable value
        from the [k_of_string] function. [kind] is used for error
        reports (e.g. could be ["int"] for an [int] parser). *)

    val bool : bool parser
    (** [bool s] is a boolean parser. The string is lowercased and
        the result is:
        {ul
        {- [Ok false] if it is one of [""], ["false"], ["no"], ["n"] or ["0"].}
        {- [Ok true] if it is one of ["true"], ["yes"], ["y"] or ["1"].}
        {- An [Error] otherwise.}} *)

    val string : string parser
    (** [string s] is a string parser, it always succeeds. *)

    val path : Fpath.t parser
    (** [path s] is a path parser using {!Fpath.of_string}. *)

    val cmd : Cmd.t parser
    (** [cmd s] is a {b non-empty} command parser using
        {!Cmd.of_string}. *)

    val some : 'a parser -> 'a option parser
    (** [some p] is wraps [p]'s parse result in [Some]. *)

    val parse :
      string -> 'a parser -> absent:'a -> ('a, 'e) result
    (** [parse name p ~absent] is:
        {ul
        {- [Ok absent] if [Env.var name = None]}
        {- [Ok v] if [Env.var name = Some s] and [p s = Ok v]}
        {- [Error (`Msg m)] otherwise with [m] an error message
           that mentions [name] and the parse error of [p].}} *)

    val value : ?log:Logs.level -> string -> 'a parser -> absent:'a -> 'a
    (** [value ~log name p ~absent] is like {!parse} but in case
        of error the message is logged with level [log] (defaults to
        {!Logs.Error}) and [~absent] is returned. *)

     (** {1:examples Examples}
{[
let debug : bool = OS.Env.(value "DEBUG" bool ~absent:false)
let msg : string = OS.Env.(value "MSG" string ~absent:"no message")

let timeout : int option =
  let int = OS.Env.(some @@ parser "int" String.to_int) in
  OS.Env.value "TIMEOUT" int ~absent:None
]}
*)
  end

  (** Quick and dirty program arguments parsing.

      This is for quick hacks and scripts. If your program evolves to
      a tool for end users you should rather use {!Cmdliner} to parse
      your command lines: it generates man pages and its parsing is
      more flexible and user friendly. The syntax of command lines
      parsed by this module is a subset of what {!Cmdliner} is able to
      parse so migrating there should not be a problem for existing
      invocations of your program.

      This module supports short and long options with option
      arguments either glued to the option or specified as the next
      program argument.  It also supports the [--] program argument to
      notify that subsequent arguments have to be treated as
      positional arguments. Parsing functions always respond to the
      [-h], [-help] or [--help] flags by showing the program's usage
      and command line options documentation.

      See the {{!argbasics}basics}.

      {b Warning.} This module is not thread-safe. *)
  module Arg : sig

    (** {1 Executable name} *)

    val exec : string
    (** [exec] is the name of the executable. This is [Sys.argv.(0)]
        if [Sys.argv] is non-empty or {!Sys.executable_name} otherwise. *)

    (** {1:conv Argument converters}

        Argument converters transform string arguments of the command
        line to OCaml values. Consult the predefined
        {{!predefconvs}converters}. *)

    type 'a conv
    (** The type for argument converters. *)

    val conv :
      ?docv:string ->
      (string -> ('a, R.msg) Result.result) ->
      (Format.formatter -> 'a -> unit) -> 'a conv
    (** [conv ~docv parse print] is an argument converter parsing
        values with [parse] and printing them with [print]. [docv]
        is a documentation meta-variable used in the documentation
        to stand for the argument value, defaults to ["VALUE"]. *)

    val conv_parser : 'a conv -> (string -> ('a, R.msg) Result.result)
    (** [conv_parser c] is [c]'s parser. *)

    val conv_printer : 'a conv -> (Format.formatter -> 'a -> unit)
    (** [conv_printer c] is [c]'s printer. *)

    val conv_docv : 'a conv -> string
    (** [conv_printer c] is [c]'s documentation meta-variable. *)

    val parser_of_kind_of_string :
      kind:string -> (string -> 'a option) ->
      (string -> ('a, R.msg) Result.result)
    (** [parser_of_kind_of_string ~kind kind_of_string] is an argument
        parser using the [kind_of_string] function for parsing and
        [kind] for errors (e.g. could be ["an integer"] for an [int]
        parser). *)

    val some : ?none:string -> 'a conv -> 'a option conv
    (** [some none c] is like the converter [c] but wraps its result
        in [Some]. This is used for command line arguments that
        default to [None] when absent. [none] is what should be printed
        by the printer for [None] (defaults to [""]). *)

    (** {1:queries Flag and option queries}

        {b Flag and option names.} They are specified without dashes.
        A one character name defines a short option; ["d"] is [-d].
        Longer names define long options ["debug"] is [--debug].

        {b Option argument specification.} On the command line, option
        arguments are either specified as the next program argument or
        glued to the option. For short options gluing is done
        directly: [-farchive.tar]. For long options an ["="] characters
        stands between the option and the value: [--file=archive.tar].

        {b Warning.} These functions are effectful invoking them twice
        on the same option names will result in parse errors.  All the
        following functions raise [Invalid_argument] if they are
        invoked after {{!section:parse}parsing}. *)

    val flag : ?doc:string -> ?env:string -> string list -> bool
    (** [flag ~doc ~env names] is [true] if one of the flags in
        [names] is present on the command line {e at most once} and
        [false] otherwise.

        If there is no flag on the command line and [env] is specified
        and defined in the environment, its value is parsed with
        {!Env.bool} and the resulting value is used. [doc] is a
        documentation string. *)

    val flag_all : ?doc:string -> ?env:string -> string list -> int
    (** [flag_all] is like {!flag} but counts the number of occurences
        of the flag on the command line. If there is no flag on the command
        line and [env] is specified and defined in the environment, its
        value is parsed with {!Env.bool} and converted to an integer. *)

    val opt : ?docv:string -> ?doc:string -> ?env:string -> string list ->
      'a conv -> absent:'a -> 'a
    (** [opt ~docv ~doc ~env names c ~absent] is a value defined by
        the value of an optional argument that may appear {e at most
        once} on the command line under one of the names specified by
        [names].

        The argument of the option is converted with [c] and [absent]
        is used if the option is absent from the command line. If
        there is no option on the command line and [env] is specified
        and defined in the environment, its value is parsed with
        [parse] and that value is used instead of [absent].

        [doc] is is a documentation string. [docv] a documentation
        meta-variable used in the documentation to stand for the option
        argument, if unspecified [c]'s {!conv_docv} is used. In [doc]
        occurences of the substring ["$(docv)"] in are replaced by the value
        of [docv]. *)

    val opt_all : ?docv:string -> ?doc:string -> ?env:string -> string list ->
      'a conv -> absent:'a list -> 'a list
    (** [opt_all] is like {!opt} but the optional argument can be repeated. *)

    (** {1:parse Parsing}

        {b Note.} Before parsing make sure you have invoked all the
        {{!queries}queries}.

        {b Warning.} All the following functions raise
        [Invalid_argument] if they are reinvoked after
        {{!section:parse}parsing}. *)

    val parse_opts : ?doc:string -> ?usage:string -> unit -> unit
    (** [parse_opts ()] can:
        {ul
        {- Return [()] if no command line error occured and [-help] or [--help]
           was not specified.}
        {- Never return and exit the program with [0] after having
           printed the help on {!stdout}.}
        {- Never return and exit the program with [1] after having
           printed an error on {!stderr} if a parsing
           error occured.}}

        A parsing error occurs either if an option parser failed, if a
        non repeatable option was specified more than once, if there
        is an unknown option on the line, if there is a positional
        argument on the command line (use {!parse} to parse them).
        [usage] is the command argument synopsis (default is
        automatically inferred).  [doc] is a documentation string for
        the program. *)

    val parse : ?doc:string -> ?usage:string -> pos:'a conv -> unit ->
      'a list
    (** [parse ~pos] is like {!parse_opts} but returns and converts
        the positional arguments with [pos] rather than error on them.
        Note that any thing that comes after a [--] argument on the
        command line is deemed to be a positional argument. *)

    (** {1:predefconvs Predefined argument converters} *)

    val string : string conv
    (** [string] converts a string argument. This never errors. *)

    val path : Fpath.t conv
    (** [path] converts a path argument using {!Fpath.of_string}. *)

    val bin : Cmd.t conv
    (** [bin] is {!string} mapped by {!Cmd.v}. *)

    val cmd : Cmd.t conv
    (** [cmd] converts a {b non-empty} command line with {!Cmd.of_string} *)

    val char : char conv
    (** [char] converts a single character. *)

    val bool : bool conv
    (** [bool] converts a boolean with {!String.to_bool}. *)

    val int : int conv
    (** [int] converts an integer with {!String.to_int}. *)

    val nativeint : nativeint conv
    (** [int] converts a [nativeint] with {!String.to_nativeint}. *)

    val int32 : int32 conv
    (** [int32] converts an [int32] with {!String.to_int32}. *)

    val int64 : int64 conv
    (** [int64] converts an [int64] with {!String.to_int64}. *)

    val float : float conv
    (** [float] converts an float with {!String.to_float}. *)

    val enum : (string * 'a) list -> 'a conv
    (** [enum l p] converts values such that string names in [l]
        map to the corresponding value of type ['a].

        {b Warning.} The type ['a] must be comparable with
        {!Pervasives.compare}.

        @raise Invalid_argument if [l] is empty. *)

    val list : ?sep:string -> 'a conv -> 'a list conv
    (** [list ~sep c] converts a list of [c]. For parsing the
        argument is first {!String.cuts}[ ~sep] and the resulting
        string list is converted using [c]. *)

    val array : ?sep:string -> 'a conv -> 'a array conv
    (** [array ~sep c] is like {!list} but returns an array instead. *)

    val pair : ?sep:string -> 'a conv -> 'b conv -> ('a * 'b) conv
    (** [pair ~sep fst snd] converts a pair of [fst] and [snd]. For parsing
        the argument is {!String.cut}[ ~sep] and the resulting strings
        are converted using [fst] and [snd]. *)

    (** {1:argbasics Basics}

        To parse a command line, {b first} perform all the option
        {{!queries}queries} and then invoke one of the
        {{!parse}parsing} functions. Do not invoke any query after
        parsing has been done, this will raise [Invalid_argument].
        This leads to the following program structure:
{[
(* It is possible to define things at the toplevel as follows. But do not
   abuse this. The following flag, if unspecified on the command line, can
   also be specified with the DEBUG environment variable. *)
let debug = OS.Arg.(flag ["g"; "debug"] ~env:"DEBUG" ~doc:"debug mode.")
...

let main () =
  let depth =
    OS.Arg.(opt ["d"; "depth"] int ~absent:2 ~doc:"recurses $(docv) times.")
  in
  let pos_args = OS.Arg.(parse ~pos:string ()) in
  (* No command line error or help request occured, run the program. *)
  ...

let main () = main ()
]}
 *)
  end

  (** {1 File system operations}

      {b Note.} When paths are relative they are expressed relative to
      the {{!Dir.current}current working directory}. *)

  (** Path operations.

      These functions operate on files and directories equally. Similar
      and specific functions operating only on one kind of path
      can be found in the {!File} and {!Dir} modules. *)
  module Path : sig

    (** {1:ops Existence, move, deletion, information and mode } *)

    val exists : Fpath.t -> (bool, 'e) result
    (** [exists p] is [true] if [p] exists for the file system
        and [false] otherwise. *)

    val must_exist : Fpath.t -> (Fpath.t, 'e) result
    (** [must_exist p] is [Ok p] if [p] exists for the file system
        and an error otherwise. *)

    val move :
      ?force:bool -> Fpath.t -> Fpath.t -> (unit, 'e) result
    (** [move ~force src dst] moves path [src] to [dst]. If [force] is
        [true] (defaults to [false]) the operation doesn't error if
        [dst] exists and can be replaced by [src]. *)

    val delete :
      ?must_exist:bool -> ?recurse:bool -> Fpath.t -> (unit, 'e) result
    (** [delete ~must_exist ~recurse p] deletes the path [p]. If
        [must_exist] is [true] (defaults to [false]) an error is returned
        if [p] doesn't exist. If [recurse] is [true] (defaults to [false])
        and [p] is a directory, no error occurs if the directory is
        non-empty: its contents is recursively deleted first. *)

    val stat : Fpath.t -> (Unix.stats, 'e) result
    (** [stat p] is [p]'s file information. *)

    (** Path permission modes. *)
    module Mode : sig

      (** {1:modes Modes} *)

      type t = int
      (** The type for file path permission modes. *)

      val get : Fpath.t -> (t, 'e) result
      (** [get p] is [p] is permission mode. *)

      val set : Fpath.t -> t -> (unit, 'e) result
      (** [set p m] sets [p]'s permission mode to [m]. *)
    end

    (** {1:link Path links} *)

    val link :
      ?force:bool -> target:Fpath.t -> Fpath.t ->
      (unit, 'e) result
    (** [link ~force target p] hard links [target] to [p].  If
        [force] is [true] (defaults to [false]) and [p] exists, it is
        is [rmdir]ed or [unlink]ed before making the link. *)

    val symlink :
      ?force:bool -> target:Fpath.t -> Fpath.t ->
      (unit, 'e) result
    (** [symlink ~force target p] symbolically links [target] to
        [p]. If [force] is [true] (defaults to [false]) and [p]
        exists, it is [rmdir]ed or [unlink]ed before making the
        link.*)

    val symlink_target : Fpath.t -> (Fpath.t, 'e) result
    (** [slink_target p] is [p]'s target iff [p] is a symbolic link. *)

    val symlink_stat : Fpath.t -> (Unix.stats, 'e) result
    (** [symlink_stat p] is the same as {!stat} but if [p] is a link
        returns information about the link itself. *)

    (** {1:pathmatch Matching path patterns against the file system}

        A path pattern [pat] is a path whose segments are made of
        {{!Pat}named string patterns}. Each variable of the pattern
        greedily matches a segment or sub-segment. For example the path
        pattern:
{[
        Fpath.(v "data" / "$(dir)" / "$(file).txt")
]}
        matches any existing path of the file system that matches the
        regexp  [data/.*/.*\.txt].

        {b Warning.} When segments with pattern variables are matched
        against the file system they never match ["."]  and
        [".."]. For example the pattern ["$(file).$(ext)"] does not
        match ["."]. *)

    val matches :
      ?dotfiles:bool -> Fpath.t -> (Fpath.t list, 'e) result
    (** [matches ~dotfiles pat] is the list of paths in the file
        system that match the path pattern [pat]. If [dotfiles] is
        [false] (default) segments that start with a pattern variable
        do not match dotfiles. *)

    val query :
      ?dotfiles:bool -> ?init:Pat.defs -> Fpath.t ->
      ((Fpath.t * Pat.defs) list, 'e) result
    (** [query ~init pat] is like {!matches} except each matching path
        is returned with an environment mapping pattern variables to
        their matched part in the path. For each path the mappings are
        added to [init] (defaults to {!String.Map.empty}). *)

    (** {1:fold Folding over file system hierarchies} *)

    type traverse = [ `Any | `None | `Sat of Fpath.t -> (bool, R.msg) result ]
    (** The type for controlling directory traversals. The predicate
        of [`Sat] should only be called with directory paths, however this
        may not be the case due to OS races. *)

    type elements = [ `Any | `Files | `Dirs
                    | `Sat of Fpath.t -> (bool, R.msg) result ]
    (** The type for specifying elements being folded over. *)

    type 'a fold_error = Fpath.t -> ('a, R.msg) result -> (unit, R.msg) result
    (** The type for managing fold errors.

        During the fold, errors may be generated at different points
        of the process. For example, determining traversal with
        {!traverse}, determining folded {!elements} or trying to
        [readdir(3)] a directory without having permissions.

        These errors are given to a function of this type. If the
        function returns [Error _] the fold stops and returns that
        error. If the function returns [`Ok ()] the path is ignored
        for the operation and the fold continues. *)

    val log_fold_error : level:Logs.level -> 'a fold_error
    (** [log_fold_error level] is a {!fold_error} function that logs
        error with level [level] and always returns [`Ok ()]. *)

    val fold :
      ?err:'b fold_error -> ?dotfiles:bool -> ?elements:elements ->
      ?traverse:traverse -> (Fpath.t -> 'a -> 'a) -> 'a -> Fpath.t list ->
      ('a, 'e) result
    (** [fold err dotfiles elements traverse f acc paths] folds over the list of
        paths [paths] traversing directories according to [traverse]
        (defaults to [`Any]) and selecting elements to fold over
        according to [elements] (defaults to [`Any]).

        If [dotfiles] if [false] (default) both elements and
        directories to traverse that start with a [.] except [.] and
        [..] are skipped without being considered by [elements] or
        [traverse]'s values.

        [err] manages fold errors (see {!fold_error}), defaults to
        {!log_fold_error}[ ~level:Log.Error]. *)
  end

  (** File operations. *)
  module File : sig

    (** {1:paths Famous file paths} *)

    val null : Fpath.t
    (** [null] is [Fpath.v "/dev/null"] on POSIX and [Fpath.v "NUL"] on
        Windows. It represents a file on the OS that discards all
        writes and returns end of file on reads. *)

    val dash : Fpath.t
    (** [dash] is [Fpath.v "-"]. This value is used by {{!input}input}
        and {{!output}output} functions to respectively denote [stdin]
        and [stdout].

        {b Note.} Representing [stdin] and [stdout] by this path is a
        widespread command line tool convention. However it is
        perfectly possible to have files that bear this name in the
        file system. If you need to operate on such path from the
        current directory you can simply specify them as
        [Fpath.(cur_dir / "-")] and so can your users on the command
        line by using ["./-"]. *)

    (** {1:ops Existence, deletion and properties} *)

    val exists : Fpath.t -> (bool, 'e) result
    (** [exists file] is [true] if [file] is a regular file in the
        file system and [false] otherwise. Symbolic links are
        followed. *)

    val must_exist : Fpath.t -> (Fpath.t, 'e) result
    (** [must_exist file] is [Ok file] if [file] is a regular file in the
        file system and an error otherwise. Symbolic links are
        followed. *)

    val delete : ?must_exist:bool -> Fpath.t -> (unit, 'e) result
    (** [delete ~must_exist file] deletes file [file]. If [must_exist]
        is [true] (defaults to [false]) an error is returned if [file]
        doesn't exist. *)

    val truncate : Fpath.t -> int -> (unit, 'e) result
    (** [truncate p size] truncates [p] to [s]. *)

    val is_executable : Fpath.t -> bool
    (** [is_executable p] is [true] iff file [p] exists and is
        executable. *)

    (** {1:input Input}

        {b Stdin.} In the following functions if the path is {!dash},
        bytes are read from [stdin]. *)

    type input = unit -> (Bytes.t * int * int) option
    (** The type for file inputs. The function is called by the client
        to input more bytes. It returns [Some (b, pos, len)] if the
        bytes [b] can be read in the range \[[pos];[pos+len]\]; this
        byte range is immutable until the next function call.  [None]
        is returned at the end of input. *)

    val with_input :
      ?bytes:Bytes.t ->
      Fpath.t -> (input -> 'a -> 'b) -> 'a -> ('b, 'e) result
    (** [with_input ~bytes file f v] provides contents of [file] with an
        input [i] using [bytes] to read the data and returns [f i v]. After
        the function returns (normally or via an exception) a call to [i] by
        the client raises [Invalid_argument].

        @raise Invalid_argument if the length of [bytes] is [0]. *)

    val with_ic :
      Fpath.t -> (in_channel -> 'a -> 'b) -> 'a -> ('b, 'e) result
    (** [with_ic file f v] opens [file] as a channel [ic] and returns
        [Ok (f ic v)]. After the function returns (normally or via an
        exception), [ic] is ensured to be closed.  If [file] is
        {!dash}, [ic] is {!Pervasives.stdin} and not closed when the
        function returns. [End_of_file] exceptions raised by [f] are
        turned it into an error message. *)

    val read : Fpath.t -> (string, 'e) result
    (** [read file] is [file]'s content as a string. *)

    val read_lines : Fpath.t -> (string list, 'e) result
    (** [read_lines file] is [file]'s content, split at each
        ['\n'] character. *)

    val fold_lines :
      ('a -> string -> 'a) -> 'a -> Fpath.t -> ('a, 'e) result
    (** [fold_lines f acc file] is like
        [List.fold_left f acc (read_lines p)]. *)

    (** {1:output Output}

        The following applies to every function in this section.

        {b Stdout.} If the path is {!dash}, bytes are written to
        [stdout].

        {b Default permission mode.} The optional [mode] argument
        specifies the permissions of the created file. It defaults to
        [0o644] (readable by everyone writeable by the user).

        {b Atomic writes.} Files are written atomically by the
        functions. They create a temporary file [t] in the directory
        of the file [f] to write, write the contents to [t] and
        renames it to [f] on success. In case of error [t] is
        deleted and [f] left intact. *)

    type output = (Bytes.t * int * int) option -> unit
    (** The type for file outputs. The function is called by the
        client with [Some (b, pos, len)] to output the bytes of [b] in
        the range \[[pos];[pos+len]\]. [None] is called to denote
        end of output. *)

    val with_output :
      ?mode:int -> Fpath.t ->
      (output -> 'a -> (('c, 'd) Result.result as 'b)) -> 'a ->
      ('b, 'e) result
    (** [with_output file f v] writes the contents of [file] using an
        output [o] given to [f] and returns [Ok (f o v)]. [file] is
        not written if [f] returns an error. After the function
        returns (normally or via an exception) a call to [o] by the
        client raises [Invalid_argument]. *)

    val with_oc :
      ?mode:int -> Fpath.t ->
      (out_channel -> 'a -> (('c, 'd) Result.result as 'b)) ->
      'a -> ('b, 'e) result
    (** [with_oc file f v] opens [file] as a channel [oc] and returns
        [Ok (f oc v)]. After the function returns (normally or via an
        exception) [oc] is closed. [file] is not written if [f]
        returns an error. If [file] is {!dash}, [oc] is
        {!Pervasives.stdout} and not closed when the function
        returns. *)

    val write :
      ?mode:int -> Fpath.t -> string -> (unit, 'e) result
    (** [write file content] outputs [content] to [file]. If [file]
        is {!dash}, writes to {!Pervasives.stdout}. If an error is
        returned [file] is left untouched except if {!Pervasives.stdout}
        is written. *)

    val writef :
      ?mode:int -> Fpath.t ->
      ('a, Format.formatter, unit, (unit, 'e) result ) format4 ->
      'a
    (** [write file fmt ...] is like [write file (Format.asprintf fmt ...)]. *)

    val write_lines :
      ?mode:int -> Fpath.t -> string list -> (unit, 'e) result
    (** [write_lines file lines] is like [write file (String.concat
        ~sep:"\n" lines)]. *)

    (** {1:tmpfiles Temporary files} *)

    type tmp_name_pat = (string -> string, Format.formatter, unit, string)
        format4
    (** The type for temporary file name patterns. The string format is
        replaced by random characters. *)

    val tmp :
      ?mode:int -> ?dir:Fpath.t -> tmp_name_pat ->
      (Fpath.t, 'e) result
    (** [tmp mode dir pat] is a new empty temporary file in [dir]
        (defaults to {!Dir.default_tmp}) named according to [pat] and
        created with permissions [mode] (defaults to [0o600] only
        readable and writable by the user). The file is deleted at the
        end of program execution using a {!Pervasives.at_exit}
        handler.

        {b Warning.} If you want to write to the file, using
        {!with_tmp_output} or {!with_tmp_oc} is more secure as it
        ensures that noone replaces the file, e.g. by a symbolic link,
        between the time you create the file and open it. *)

    val with_tmp_output :
      ?mode:int -> ?dir:Fpath.t -> tmp_name_pat ->
      (Fpath.t -> output -> 'a -> 'b) -> 'a -> ('b, 'e) result
    (** [with_tmp_output dir pat f v] is a new temporary file in [dir]
        (defaults to {!Dir.default_tmp}) named according to [pat] and
        atomically created and opened with permissions [mode]
        (defaults to [0o600] only readable and writable by the
        user). Returns [Ok (f file o v)] with [file] the file
        path and [o] an output to write the file. After the function
        returns (normally or via an exception), calls to [o] raise
        [Invalid_argument] and [file] is deleted. *)

    val with_tmp_oc :
      ?mode:int -> ?dir:Fpath.t -> tmp_name_pat ->
      (Fpath.t -> out_channel -> 'a -> 'b) -> 'a ->
      ('b, 'e) result
    (** [with_tmp_oc mode dir pat f v] is a new temporary file in
        [dir] (defaults to {!Dir.default_tmp}) named according to
        [pat] and atomically created and opened with permission [mode]
        (defaults to [0o600] only readable and writable by the
        user). Returns [Ok (f file oc v)] with [file] the file path
        and [oc] an output channel to write the file. After the
        function returns (normally or via an exception), [oc] is
        closed and [file] is deleted. *)
end

  (** Directory operations. *)
  module Dir : sig

    (** {1:dirops Existence, creation, deletion and contents} *)

    val exists : Fpath.t -> (bool, 'e) result
    (** [exists dir] is [true] if [dir] is a directory in the file system
        and [false] otherwise. Symbolic links are followed. *)

    val must_exist : Fpath.t -> (Fpath.t, 'e) result
    (** [must_exist dir] is [Ok dir] if [dir] is a directory in the file system
        and an error otherwise. Symbolic links are followed. *)

    val create :
      ?path:bool -> ?mode:int -> Fpath.t -> (bool, 'e) result
    (** [create ~path ~mode dir] creates, if needed, the directory [dir] with
        file permission [mode] (defaults [0o755] readable and traversable
        by everyone, writeable by the user). If [path] is [true]
        (default) intermediate directories are created with the same
        [mode], otherwise missing intermediate directories lead to an
        error. The result is [false] if [dir] already exists.

        {b Note.} The mode of existing directories, including
        [dir] if this is the case is kept unchanged. *)

    val delete :
      ?must_exist:bool -> ?recurse:bool -> Fpath.t ->
      (unit, 'e) result
    (** [delete ~must_exist ~recurse dir] deletes the directory [dir]. If
        [must_exist] is [true] (defaults to [false]) an error is returned
        if [dir] doesn't exist. If [recurse] is [true] (default to [false])
        no error occurs if the directory is non-empty: its contents is
        recursively deleted first. *)

    val contents :
      ?dotfiles:bool -> ?rel:bool -> Fpath.t -> (Fpath.t list, 'e) result
    (** [contents ~dotfiles ~rel dir] is the list of directories and files
        in [dir]. If [rel] is [true] (defaults to [false]) the resulting paths
        are relative to [dir], otherwise they have [dir] prepended. See also
        {!fold_contents}. If [dotfiles] is [false] (default) elements that
        start with a [.] are omitted. *)

    val fold_contents :
      ?err:'b Path.fold_error -> ?dotfiles:bool -> ?elements:Path.elements ->
      ?traverse:Path.traverse -> (Fpath.t -> 'a -> 'a) -> 'a -> Fpath.t ->
      ('a, 'e) result
    (** [contents_fold err dotfiles elements traverse f acc d] is:
{[
contents d >>= Path.fold err dotfiles elements traverse f acc
]}
        For more details see {!Path.fold}. *)

    (** {1:user_current User and current working directory} *)

    val user : unit -> (Fpath.t, 'e) result
    (** [user ()] is the home directory of the user executing
        the process. Determined by consulting the [passwd] database
        with the user id of the process. If this fails or on Windows
        falls back to parse a path from the [HOME] environment variable. *)

    val current : unit -> (Fpath.t, 'e) result
    (** [current ()] is the current working directory. The resulting
        path is guaranteed to be absolute. *)

    val set_current : Fpath.t -> (unit, 'e) result
    (** [set_current dir] sets the current working directory to [dir]. *)

    val with_current : Fpath.t -> ('a -> 'b) -> 'a -> ('b, 'e) result
    (** [with_current dir f v] is [f v] with the current working directory
        bound to [dir]. After the function returns the current working
        directory is back to its initial value. *)

    (** {1:tmpdirs Temporary directories} *)

    type tmp_name_pat = (string -> string, Format.formatter, unit, string)
        format4
    (** The type for temporary directory name patterns. The string format is
        replaced by random characters. *)

    val tmp :
      ?mode:int -> ?dir:Fpath.t -> tmp_name_pat ->
      (Fpath.t, 'e) result
    (** [tmp mode dir pat] is a new empty directory in [dir] (defaults
        to {!Dir.default_tmp}) named according to [pat] and created
        with permissions [mode] (defaults to [0o700] only readable and
        writable by the user). The directory path and its content is
        deleted at the end of program execution using a
        {!Pervasives.at_exit} handler. *)

    val with_tmp :
      ?mode:int -> ?dir:Fpath.t -> tmp_name_pat -> (Fpath.t -> 'a -> 'b) ->
      'a -> ('b, 'e) result
    (** [with_tmp mode dir pat f v] is a new empty directory in [dir]
        (defaults to {!Dir.default_tmp}) named according to [pat] and
        created with permissions [mode] (defaults to [0o700] only
        readable and writable by the user). Returns the value of [f
        tmpdir v] with [tmpdir] the directory path. After the function
        returns the directory path [tmpdir] and its content is
        deleted. *)

    (** {1:defaulttmpdir Default temporary directory} *)

    val default_tmp : unit -> Fpath.t
    (** [default_tmp ()] is the directory used as a default value for
        creating {{!File.tmpfiles}temporary files} and
        {{!tmpdirs}directories}. If {!set_default_tmp} hasn't been
        called this is:
        {ul
        {- On POSIX, the value of the [TMPDIR] environment variable or
           [Fpath.v "/tmp"] if the variable is not set or empty.}
        {- On Windows, the value of the [TEMP] environment variable or
           {!Fpath.cur_dir} if it is not set or empty}} *)

    val set_default_tmp : Fpath.t -> unit
    (** [set_default_tmp p] sets the value returned by {!default_tmp} to
        [p]. *)
  end

  (** {1 Commands} *)

  (** Executing commands. *)
  module Cmd : sig

    (** {1:exist Tool existence and search}

      {b Tool search procedure.} Given a list of directories, the
      {{!Cmd.line_tool}tool} of a command line is searched, in list
      order, for the first matching {e executable} file. If the tool
      name is already a file path (i.e. contains a
      {!Fpath.dir_sep}) it is neither searched nor tested for
      {{!File.is_executable}existence and executability}. In the
      functions below if the list of directories [search] is
      unspecified the result of parsing the [PATH] environment
      variable with {!search_path_dirs} is used.

      {b Portability.} In order to maximize portability no [.exe]
      suffix should be added to executable names on Windows, the tool
      search procedure will add the suffix during the tool search
      procedure if absent. *)

    val find_tool : ?search:Fpath.t list -> Cmd.t -> (Fpath.t option, 'e) result
    (** [find_tool ~search cmd] is the path to the {{!Cmd.line_tool}tool}
        of [cmd] as found by the tool search procedure in [search]. *)

    val get_tool : ?search:Fpath.t list -> Cmd.t -> (Fpath.t, 'e) result
    (** [get_tool cmd] is like {!find_tool} except it errors if the
        tool path cannot be found. *)

    val exists : ?search:Fpath.t list -> Cmd.t -> (bool, 'e) result
    (** [exists ~search cmd] is [Ok true] if {!find_tool} finds a path
        and [Ok false] if it does not. *)

    val must_exist : ?search:Fpath.t list -> Cmd.t -> (Cmd.t, 'e) result
    (** [must_exist ~search cmd] is [Ok cmd] if {!get_tool} succeeds. *)

    val resolve : ?search:Fpath.t list -> Cmd.t -> (Cmd.t, 'e) result
    (** [resolve ~search cmd] is like {!must_exist} except the tool of the
        resulting command value has the path to the tool of [cmd] as
        determined by {!get_tool}. *)

    val search_path_dirs : ?sep:string -> string -> (Fpath.t list, 'e) result
    (** [search_path_dirs ~sep s] parses [sep] seperated file paths
        from [s]. [sep] is not allowed to appear in the file paths, it
        defaults to [";"] if {!Sys.win32} is [true] and [":"]
        otherwise. *)

    (** {1:run Command runs}

        The following set of combinators are designed to be used with
        {!Pervasives.(|>)} operator. See a few {{!ex}examples}.

        {b WARNING Windows.} The [~append:true] options for appending
        to files are unsupported on Windows.
        {{:http://caml.inria.fr/mantis/view.php?id=4431}This} old
        feature request should be fixed upstream.

    {2:run_exit Run statuses & information} *)

    type status = [ `Exited of int | `Signaled of int ]
    (** The type for process exit statuses. *)

    val pp_status : status Fmt.t
    (** [pp_status] is a formatter for statuses. *)

    type run_info
    (** The type for run information. *)

    val run_info_cmd : run_info -> Cmd.t
    (** [run_info_cmd ri] is the command that was run. *)

    type run_status = run_info * status
    (** The type for run statuses the run information and the process
        exit status. *)

    val success : ('a * run_status, 'e) result -> ('a, 'e) result
    (** [success r] is:
        {ul
        {- [Ok v] if [r = Ok (v, (_, `Exited 0))]}
        {- [Error _] otherwise. Non [`Exited 0] statuses are turned
           into an error message.}} *)

    (** {2:stderrs Run standard errors} *)

    type run_err
    (** The type for representing the standard error of a command run. *)

    val err_file : ?append:bool -> Fpath.t -> run_err
    (** [err_file f] is a standard error that writes to file [f]. If [append]
        is [true] (defaults to [false]) the data is appended to [f]. *)

    val err_null : run_err
    (** [err_null] is [err_file File.null]. *)

    val err_run_out : run_err
    (** [err_run_out] is a standard error that is redirected to the run's
        standard output. *)

    val err_stderr : run_err
    (** [err_stderr] is a standard error that is redirected to the current
        process standard error. *)

    (** {2:stdins Run standard inputs} *)

    type run_in
    (** The type for representing the standard input of a command run. *)

    val in_string : string -> run_in
    (** [in_string s] is a standard input that reads [s]. *)

    val in_file : Fpath.t -> run_in
    (** [in_file f] is a standard input that reads from file [f]. *)

    val in_null : run_in
    (** [in_null] is [in_file File.null]. *)

    val in_stdin : run_in
    (** [in_stdin] is a standard input that reads from the current
        process standard input. *)

    (** {2:stdouts Run standard outputs}

        The following functions trigger actual command runs, consume
        their standard output and return the command and its status. In
        {{!out_run_in}pipelined} runs, the reported status is the one
        of the first failing run in the pipeline.

        {b Warning.} When a value of type {!run_out} has been "consumed"
        with one of the following functions it cannot be reused. *)

    type run_out
    (** The type for representing the standard output and status of a
        command run. *)

    val out_string : ?trim:bool -> run_out -> (string * run_status, 'e) result
    (** [out_string ~trim o] captures the standard output [o] as
        a string. If [trim] is [true] (default) the result is passed through
        {!String.trim}. *)

    val out_lines :
      ?trim:bool -> run_out -> (string list * run_status, 'e) result
    (** [out_lines] is like {!out_string} but the result is splitted on
        newlines (['\n']). If the standard output is empty then the empty
        list is returned. *)

    val out_file :
      ?append:bool -> Fpath.t -> run_out -> (unit * run_status, 'e) result
    (** [out_file f o] writes the standard output [o] to file [f]. If
        [append] is [true] (defaults to [false]) the data is appended
        to [f]. *)

    val out_run_in : run_out -> (run_in, 'e) result
    (** [out_run_in o] is a run input that can be used to feed the
        standard output of [o] to the standard input of another, {b
        single}, command run. Note that when the function returns the
        command run of [o] may not be terminated yet. The run using
        the resulting input will report an unsucessful status or
        error of [o] rather than its own error. *)

    val out_null : run_out -> (unit * run_status, 'e) result
    (** [out_null o] is [out_file File.null o]. *)

    val out_stdout : run_out -> (unit * run_status, 'e) result
    (** [to_stdout o] redirects the standard output [o] to the current
        process standard output. *)

    (** {3:success Extracting success}

        The following functions can be used if you only care about
        success. *)

    val to_string : ?trim:bool -> run_out -> (string, 'e) result
    (** [to_string ~trim o] is [(out_string ~trim o |> success)]. *)

    val to_lines : ?trim:bool -> run_out -> (string list, 'e) result
    (** [to_lines ~trim o] is [(out_lines ~trim o |> success)]. *)

    val to_file : ?append:bool -> Fpath.t -> run_out -> (unit, 'e) result
    (** [to_file ?append f o] is [(out_file ?append f o |> success)]. *)

    val to_null : run_out -> (unit, 'e) result
    (** [to_null o] is [to_file File.null o]. *)

    val to_stdout : run_out -> (unit, 'e) result
    (** [to_stdout o] is [(out_stdout o |> success)]. *)

    (** {2:runs Run specifications} *)

    val run_io : ?env:Env.t -> ?err:run_err -> Cmd.t -> run_in -> run_out
    (** [run_io ~env ~err cmd i] represents the standard output of the
        command run [cmd] performed in process environment [env] with
        its standard error output handled according to [err] (defaults
        to {!err_stderr}) and standard input connected to [i]. Note that
        the command run is not started before the output is consumed, see
        {{!stdouts}run standard outputs}. *)

    val run_out : ?env:Env.t -> ?err:run_err -> Cmd.t -> run_out
    (** [run_out ?env ?err cmd] is [(in_stdin |> run_io ?env ?err cmd)]. *)

    val run_in : ?env:Env.t -> ?err:run_err -> Cmd.t -> run_in ->
      (unit, 'e) result
    (** [run_in ?env ?err cmd i] is [(run_io ?env ?err cmd |> to_stdout)]. *)

    val run : ?env:Env.t -> ?err:run_err -> Cmd.t -> (unit, 'e) result
    (** [run ?env ?err cmd] is
        [(in_stdin |> run_io ?env ?err cmd |> to_stdout)]. *)

    val run_status : ?env:Env.t -> ?err:run_err -> ?quiet:bool -> Cmd.t ->
      (status, 'e) result
    (** [run_status ?env ?err ?quiet cmd] is
        [(in_stdin |> run_io ?env ?err ?cmd |> out_stdout)] and extracts
        the run status.

        If [quiet] is [true] (defaults to [false]), [in_stdin] and
        [out_stdout] are respectively replaced by [in_null] and
        [out_null] and [err] defaults to [err_null] rather than
        [err_stderr]. *)

    (** {1:ex Examples}

    Identity miaouw.
{[
let id s = OS.Cmd.(in_string s |> run_io Cmd.(v "cat") |> out_string)
]}
    Get the current list of git tracked files in OCaml:
{[
let git = Cmd.v "git"
let git_tracked () =
  let git_ls_files = Cmd.(git % "ls-files") in
  OS.Cmd.(run_out git_ls_files |> to_lines)
]}
   Tarbzip the current list of git tracked files, without reading the
   tracked files in OCaml:
{[
let tbz_git_tracked dst =
  let git_ls_files = Cmd.(git % "ls-files") in
  let tbz = Cmd.(v "tar" % "-cvzf" % p dst % "-T" % "-") in
  OS.Cmd.(run_out git_ls_files |> out_run_in) >>= fun tracked ->
  OS.Cmd.(tracked |> run_in tbz)
]}
    Send the email [mail].
{[
let send_email mail =
  let sendmail = Cmd.v "sendmail" in
  OS.Cmd.(in_string mail |> run_in sendmail)
]} *)

  end

  (** {1 Low level {!Unix} access} *)

  (** Low level {!Unix} access.

      These functions simply {{!call}call} functions from the {!Unix}
      module and replace strings with {!Fpath.t} where appropriate.  They
      also provide more fine grained error handling, for example
      {!OS.Path.stat} converts the error to a message while {!stat}
      gives you the {{!Unix.error}Unix error}. *)
  module U : sig

    (** {1 Error handling} *)

    type 'a result = ('a, [`Unix of Unix.error]) Result.result
    (** The type for Unix results. *)

    val pp_error : Format.formatter -> [`Unix of Unix.error] -> unit
    (** [pp_error ppf e] prints [e] on [ppf]. *)

    val open_error :
      'a result -> ('a, [> `Unix of Unix.error]) Result.result
    (** [open_error r] allows to combine a closed unix error
        variant with other variants. *)

    val error_to_msg : 'a result -> ('a, [> R.msg]) Result.result
    (** [error_to_msg r] converts unix errors in [r] to an error message. *)

    (** {1 Wrapping {!Unix} calls} *)

    val call : ('a -> 'b) -> 'a -> 'b result
    (** [call f v] is [Ok (f v)] but {!Unix.EINTR} errors are catched
        and handled by retrying the call. Other errors [e] are catched
        aswell and returned as [Error (`Unix e)]. *)

    (** {1 File system operations} *)

    val mkdir : Fpath.t -> Unix.file_perm -> unit result
    (** [mkdir] is {!Unix.mkdir}, see
        {{:http://pubs.opengroup.org/onlinepubs/9699919799/functions/mkdir.html}
        POSIX [mkdir]}. *)

    val link : Fpath.t -> Fpath.t -> unit result
    (** [link] is {!Unix.link}, see
        {{:http://pubs.opengroup.org/onlinepubs/9699919799/functions/link.html}
        POSIX [link]}. *)

    val unlink : Fpath.t -> unit result
    (** [stat] is {!Unix.unlink},
        {{:http://pubs.opengroup.org/onlinepubs/9699919799/functions/unlink.html}
        POSIX [unlink]}. *)

    val rename : Fpath.t -> Fpath.t -> unit result
    (** [rename] is {!Unix.rename}, see
        {{:http://pubs.opengroup.org/onlinepubs/9699919799/functions/rename.html}
        POSIX [rename]}. *)

    val stat : Fpath.t -> Unix.stats result
    (** [stat] is {!Unix.stat}, see
        {{:http://pubs.opengroup.org/onlinepubs/9699919799/functions/stat.html}
        POSIX [stat]}. *)

    val lstat : Fpath.t -> Unix.stats result
    (** [lstat] is {!Unix.lstat}, see
        {{:http://pubs.opengroup.org/onlinepubs/9699919799/functions/lstat.html}
        POSIX [lstat]}. *)

    val truncate : Fpath.t -> int -> unit result
    (** [truncate] is {!Unix.truncate}, see
        {{:http://pubs.opengroup.org/onlinepubs/9699919799/functions/truncate.html}
        POSIX [truncate]}. *)
  end
end

(*---------------------------------------------------------------------------
   Copyright (c) 2014 Daniel C. Bünzli

   Permission to use, copy, modify, and/or distribute this software for any
   purpose with or without fee is hereby granted, provided that the above
   copyright notice and this permission notice appear in all copies.

   THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
   WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
   MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
   ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
   WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
   ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
   OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
  ---------------------------------------------------------------------------*)
