(******************************************************************************)
(*                                                                            *)
(*                                   Menhir                                   *)
(*                                                                            *)
(*                       François Pottier, Inria Paris                        *)
(*              Yann Régis-Gianas, PPS, Université Paris Diderot              *)
(*                                                                            *)
(*  Copyright Inria. All rights reserved. This file is distributed under the  *)
(*  terms of the GNU Library General Public License version 2, with a         *)
(*  special exception on linking, as described in the file LICENSE.           *)
(*                                                                            *)
(******************************************************************************)

(* -------------------------------------------------------------------------- *)

(* The type functor. *)

module Symbols (T : sig

  type 'a terminal
  type 'a nonterminal

end) = struct

  open T

  (* This should be the only place in the whole library (and generator!)
     where these types are defined. *)

  type 'a symbol =
    | T : 'a terminal -> 'a symbol
    | N : 'a nonterminal -> 'a symbol

  type xsymbol =
    | X : 'a symbol -> xsymbol

end

(* -------------------------------------------------------------------------- *)

(* The code functor. *)

module Make
  (TT : TableFormat.TABLES)
  (IT : InspectionTableFormat.TABLES
        with type 'a lr1state = int)
  (ET : EngineTypes.TABLE
        with type terminal = int
         and type nonterminal = int
         and type semantic_value = Obj.t)
  (E : sig
     type 'a env = (ET.state, ET.semantic_value, ET.token) EngineTypes.env
   end)
= struct

  (* Including [IT] is an easy way of inheriting the definitions of the types
     [symbol] and [xsymbol]. *)

  include IT

  (* This auxiliary function decodes a packed linearized array, as created by
     [TableBackend.linearize_and_marshal1]. Here, we read a row all at once. *)

  let read_packed_linearized
    (data, entry : PackedIntArray.t * PackedIntArray.t) (i : int) : int list
  =
    LinearizedArray.read_row_via
      (PackedIntArray.get data)
      (PackedIntArray.get entry)
      i

  (* This auxiliary function decodes a symbol. The encoding was done by
     [encode_symbol] or [encode_symbol_option] in the table back-end. *)

  let decode_symbol (symbol : int) : IT.xsymbol =
    (* If [symbol] is 0, then we have no symbol. This could mean e.g.
       that the function [incoming_symbol] has been applied to an
       initial state. In principle, this cannot happen. *)
    assert (symbol > 0);
    (* The low-order bit distinguishes terminal and nonterminal symbols. *)
    let kind = symbol land 1 in
    let symbol = symbol lsr 1 in
    if kind = 0 then
      IT.terminal (symbol - 1)
    else
      IT.nonterminal symbol

  (* These auxiliary functions convert a symbol to its integer code. For speed
     and for convenience, we use an unsafe type cast. This relies on the fact
     that the data constructors of the [terminal] and [nonterminal] GADTs are
     declared in an order that reflects their internal code. In the case of
     nonterminal symbols, we add [start] to account for the presence of the
     start symbols. *)

  let n2i (nt : 'a IT.nonterminal) : int =
    let answer = TT.start + Obj.magic nt in
    (* For safety, check that the above cast produced a correct result. *)
    assert (IT.nonterminal answer = X (N nt));
    answer

  let t2i (t : 'a IT.terminal) : int =
    let answer = Obj.magic t in
    (* For safety, check that the above cast produced a correct result. *)
    assert (IT.terminal answer = X (T t));
    answer

  (* Ordering functions. *)

  let compare_terminals t1 t2 =
    (* Subtraction is safe because overflow is impossible. *)
    t2i t1 - t2i t2

  let compare_nonterminals nt1 nt2 =
    (* Subtraction is safe because overflow is impossible. *)
    n2i nt1 - n2i nt2

  let compare_symbols symbol1 symbol2 =
    match symbol1, symbol2 with
    | X (T _), X (N _) ->
        -1
    | X (N _), X (T _) ->
        1
    | X (T t1), X (T t2) ->
        compare_terminals t1 t2
    | X (N nt1), X (N nt2) ->
        compare_nonterminals nt1 nt2

  let compare_productions prod1 prod2 =
    (* Subtraction is safe because overflow is impossible. *)
    prod1 - prod2

  let compare_items (prod1, index1) (prod2, index2) =
    let c = compare_productions prod1 prod2 in
    (* Subtraction is safe because overflow is impossible. *)
    if c <> 0 then c else index1 - index2

  (* The function [incoming_symbol] goes through the tables [IT.lr0_core] and
     [IT.lr0_incoming]. This yields a representation of type [xsymbol], out of
     which we strip the [X] quantifier, so as to get a naked symbol. This last
     step is ill-typed and potentially dangerous. It is safe only because this
     function is used at type ['a lr1state -> 'a symbol], which forces an
     appropriate choice of ['a]. *)

  let incoming_symbol (s : 'a IT.lr1state) : 'a IT.symbol =
    let core = PackedIntArray.get IT.lr0_core s in
    let symbol = decode_symbol (PackedIntArray.get IT.lr0_incoming core) in
    match symbol with
    | IT.X symbol ->
        Obj.magic symbol

  (* The function [lhs] reads the table [TT.lhs] and uses [IT.nonterminal]
     to decode the symbol. *)

  let lhs prod =
    IT.nonterminal (PackedIntArray.get TT.lhs prod)

  (* The function [rhs] reads the table [IT.rhs] and uses [decode_symbol]
     to decode the symbol. *)

  let rhs prod =
    List.map decode_symbol (read_packed_linearized IT.rhs prod)

  (* The function [items] maps the LR(1) state [s] to its LR(0) core,
     then uses [core] as an index into the table [IT.lr0_items]. The
     items are then decoded by the function [export] below, which is
     essentially a copy of [Item.export]. *)

  type item =
      int * int

  let low_bits =
    10

  let low_limit =
    1 lsl low_bits

  let export t : item =
    (t lsr low_bits, t mod low_limit)

  let items s =
    (* Map [s] to its LR(0) core. *)
    let core = PackedIntArray.get IT.lr0_core s in
    (* Now use [core] to look up the table [IT.lr0_items]. *)
    List.map export (read_packed_linearized IT.lr0_items core)

  (* The function [nullable] maps the nonterminal symbol [nt] to its
     integer code, which it uses to look up the array [IT.nullable].
     This yields 0 or 1, which we map back to a Boolean result. *)

  let decode_bool i =
    assert (i = 0 || i = 1);
    i = 1

  let nullable nt =
    decode_bool (PackedIntArray.get1 IT.nullable (n2i nt))

  (* The function [first] maps the symbols [nt] and [t] to their integer
     codes, which it uses to look up the matrix [IT.first]. *)

  let first nt t =
    decode_bool (PackedIntArray.unflatten1 IT.first (n2i nt) (t2i t))

  let xfirst symbol t =
    match symbol with
    | X (T t') ->
        compare_terminals t t' = 0
    | X (N nt) ->
        first nt t

  (* The function [foreach_terminal] exploits the fact that the
     first component of [TT.error] is [Terminal.n - 1], i.e., the
     number of terminal symbols, including [error] but not [#]. *)

  let rec foldij i j f accu =
    if i = j then
      accu
    else
      foldij (i + 1) j f (f i accu)

  let foreach_terminal f accu =
    let n, _ = TT.error in
    foldij 0 n (fun i accu ->
      f (IT.terminal i) accu
    ) accu

  let foreach_terminal_but_error f accu =
    let n, _ = TT.error in
    foldij 0 n (fun i accu ->
      if i = TT.error_terminal then
        accu
      else
        f (IT.terminal i) accu
    ) accu

  (* ------------------------------------------------------------------------ *)

  (* The following is the implementation of the function [feed]. This function
     is logically part of the LR engine, so it would be nice if it were placed
     in the module [Engine], but it must be placed here because, to ensure
     type safety, its arguments must be a symbol of type ['a symbol] and a
     semantic value of type ['a]. The type ['a symbol] is not available in
     [Engine]. It is available here. *)

  open EngineTypes
  open ET
  open E

  (* [feed] fails if the current state does not have an outgoing transition
     labeled with the desired symbol. This check is carried out at runtime. *)

  let feed_failure () =
    invalid_arg "feed: outgoing transition does not exist"

  (* Feeding a nonterminal symbol [nt]. Here, [nt] has type [nonterminal],
     which is a synonym for [int], and [semv] has type [semantic_value],
     which is a synonym for [Obj.t]. This type is unsafe, because pushing
     a semantic value of arbitrary type into the stack can later cause a
     semantic action to crash and burn. The function [feed] is given a safe
     type below. *)

  let feed_nonterminal
        (nt : nonterminal) startp (semv : semantic_value) endp (env : 'b env)
      : 'b env
  =
    (* Check if the source state has an outgoing transition labeled [nt].
       This is done by consulting the [goto] table. *)
    let source = env.current in
    match ET.maybe_goto_nt source nt with
    | None ->
        feed_failure()
    | Some target ->
        (* Push a new cell onto the stack, containing the identity of the state
           that we are leaving. The semantic value [semv] and positions [startp]
           and [endp] contained in the new cell are provided by the caller. *)
        let stack = { state = source; semv; startp; endp; next = env.stack } in
        (* Move to the target state. *)
        { env with stack; current = target }

  let reduce   _env _prod = feed_failure()
  let initiate _env       = feed_failure()

  let feed_terminal
        (terminal : terminal) startp (semv : semantic_value) endp (env : 'b env)
      : 'b env
  =
    (* Check if the source state has an outgoing transition labeled [terminal].
       This is done by consulting the [action] table. *)
    let source = env.current in
    ET.action source terminal semv
      (fun env _please_discard _terminal semv target ->
        (* There is indeed a transition toward the state [target].
           Push a new cell onto the stack and move to the target state. *)
        let stack = { state = source; semv; startp; endp; next = env.stack } in
        { env with stack; current = target }
      ) reduce initiate env

  (* The type assigned to [feed] ensures that the type of the semantic value
     [semv] is appropriate: it must be the semantic-value type of the symbol
     [symbol]. *)

  let feed (symbol : 'a symbol) startp (semv : 'a) endp env =
    let semv : semantic_value = Obj.repr semv in
    match symbol with
    | N nt ->
        feed_nonterminal (n2i nt) startp semv endp env
    | T terminal ->
        feed_terminal (t2i terminal) startp semv endp env

end
