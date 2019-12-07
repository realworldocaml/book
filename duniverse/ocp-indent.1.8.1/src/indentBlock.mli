(**************************************************************************)
(*                                                                        *)
(*  Copyright 2011 Jun Furuse                                             *)
(*  Copyright 2012,2013 OCamlPro                                          *)
(*                                                                        *)
(*  All rights reserved.This file is distributed under the terms of the   *)
(*  GNU Lesser General Public License version 2.1 with linking            *)
(*  exception.                                                            *)
(*                                                                        *)
(*  TypeRex is distributed in the hope that it will be useful,            *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *)
(*  Lesser GNU General Public License for more details.                   *)
(*                                                                        *)
(**************************************************************************)

(** Indenter block *)
type t

(** Shift a block by a given offset *)
val shift: t -> int -> t

(** Set the start column of the given block to [column] *)
val set_column: t -> int -> t

(** [reverse block] updates the stack to account for the original indentation,
    assumed as correct. Useful for partial indentation *)
val reverse: t -> t

(** Return the current line offset *)
val offset: t -> int

(** Return the padding of the block, ie expected relative indentation of
    sub-blocks *)
val padding: t -> int

(** Return the block indentation *)
val indent: t -> int

(** Return the block original starting column *)
val original_column: t -> int

(** The empty block *)
val empty: t

(** [update t str tok] computes the new block state after processing
    the token [tok] in block [t]. The next tokens can be observed in
    the stream [str]. *)
val update : IndentConfig.t -> t -> Nstream.t -> Nstream.token -> t

(** Display token and stack of the block *)
val dump: t -> unit

(** [guess_indent line block]
    For indenting empty lines: attempt to guess what the most probable
    indent at this point would be *)
val guess_indent: int -> t -> int

(** A block is considered clean when it is not linked to any parser state (ie
    it's not within a comment, string, or ocamldoc stuff). This is not enough
    for a safe checkpoint: lots of rules depend on the previous/next token to
    decide indentation. *)
val is_clean: t -> bool

(** True only when the block is at the root of the file (the stack is empty, the
   block isn't included in any syntactical construct), and for top-level
   constructs. Implies is_clean *)
val is_at_top: t -> bool

(** True for any block that is alone on its stack *)
val no_parents: t -> bool

(** Returns true if the given block is at a top-level declaration level, ie not
    within any expression or type definition, but possibly inside a module,
    signature or class definition. Implies is_clean. Should be safe for
    checkpoints *)
val is_declaration: t -> bool

(** Either we are at a comment, or within an ocamldoc block *)
val is_in_comment: t -> bool
