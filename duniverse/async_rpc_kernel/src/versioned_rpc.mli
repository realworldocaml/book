(** Infrastructure code for managing RPCs that evolve over time to use different types at
    different versions.

    Three scenarios are supported:

    {ul

    {li

    The {e caller} is responsible for managing versions and dispatches to callees that
    are written in a version-oblivious way.

    The prototypical example of this scenario is a commander that needs to call out to
    many assistants for that same system.  In this scenario, the assistants each
    implement a single version of the rpc and the commander has to take this into
    account. }

    {li

    The {e callee} is responsible for managing versions and callers need not bother
    themselves with any versions.

    The proto-typical example of this scenario is an assistant from one system calling out
    the commander of another system.  In this scenario, the assistants each know a single
    version of the rpc to call and the commander has to implement them all. }

    {li

    Both {e caller} and {e callee} cooperate to decide which version to use, each one
    being able to use some subset of all possible versions.

    The prototypical example of this scenario is when two systems developed independently
    with their rpc types defined in some shared library that has yet another independent
    rollout schedule.  In this case one may roll out a new rpc version (V) in the shared
    library (L) and then the caller and callee systems can each upgrade to the new version
    of L supporting version V at their own pace, with version V only being exercised once
    both caller and callee have upgraded.  } }

    In each scenario, it is desirable that the party responsible for managing versions be
    coded largely in terms of a single "master" version of the types involved, with all
    necessary type conversions relegated to a single module.  [Versioned_rpc] is intended
    for implementing such a module.

    Type coercions into and out of the model go in the directions indicated by the
    following diagram:

    {v

        Caller converts                 Callee converts
        ===============                 ===============

            caller                        callee
            |       callee                |      callee
            |       |       caller        |      |       callee
            |       |       |             |      |       |
         ,-->-- Q1 --> R1 -->-.      Q1 -->-.    |    ,-->-- R1
        /                      \             \   |   /
       Q --->-- Q2 --> R2 -->-- R    Q2 -->-- Q --> R --->-- R2
        \                      /             /       \
         `-->-- Q3 --> R3 -->-´      Q3 -->-´         `-->-- R3
    v}
*)

open! Core_kernel
open! Async_kernel

open Rpc

(** Over-the-network discovery of rpc names and versions supported by a callee.

    This is used by the [dispatch_multi] functions in [Caller_converts] and [Both_convert]
    to dynamically determine the most appropriate version to use. *)
module Menu : sig

  type t (** A directory of supported rpc names and versions. *)

  (** [add impls] extends a list of rpc implementations with an additional rpc
      implementation for providing a [Menu.t] when one is requested via [Menu.request]. *)
  val add : 's Implementation.t list -> 's Implementation.t list

  (** Specifies directly how to handle the version menu rpc. *)
  val implement_multi
    :  ?log_not_previously_seen_version:(name:string -> int -> unit)
    -> ('s -> version:int -> unit -> Description.t list Deferred.t)
    -> 's Implementation.t list

  (** Requests an rpc version menu from an rpc connection. *)
  val request : Connection.t -> t Or_error.t Deferred.t

  (** Finds what rpcs are supported. *)
  val supported_rpcs : t -> Description.t list

  (** Finds what versions of a particular rpc are supported. *)
  val supported_versions : t -> rpc_name:string -> Int.Set.t

  (** Creates a menu directly -- generally you should use [request] instead. *)
  val create : Description.t list -> t

  (** The internal name of this RPC -- for example to be used in [Rpc.Expert] to
      distinguish it from other queries. *)
  val rpc_name : string
end

module Connection_with_menu : sig
  type t (** An rpc connection paired with the menu of rpcs one may call on it. *)

  val create : Connection.t -> t Deferred.Or_error.t
  val create_directly : Connection.t -> Menu.t -> t
  val connection : t -> Connection.t
  val menu : t -> Menu.t
end

(** A [Versioned_direct_stream_writer.t] is an extension of an [Rpc.Direct_stream_writer].
    @see [Rpc.Pipe_rpc.Direct_stream_writer] for documentation. *)
module Versioned_direct_stream_writer : sig
  type 'a t

  val write : 'a t -> 'a -> [`Flushed of unit Deferred.t | `Closed]
  val write_without_pushback : 'a t -> 'a -> [`Ok | `Closed]
  val close : _ t -> unit
  val closed : _ t -> unit Deferred.t
  val is_closed : _ t -> bool
end

module Caller_converts : sig

  module Rpc : sig

    (* signature for rpc dispatchers *)
    module type S = sig
      type query
      type response

      (** Multi-version dispatch. *)
      val dispatch_multi
        : Connection_with_menu.t -> query -> response Or_error.t Deferred.t

      (** All rpcs supported by [dispatch_multi]. *)
      val rpcs : unit -> Any.t list

      (** All versions supported by [dispatch_multi] (useful for computing which old
          versions may be pruned). *)
      val versions : unit -> Int.Set.t

      val name : string
    end

    (** Given a model of the types involved in a family of RPCs, this functor provides a
        single RPC versioned dispatch function [dispatch_multi] in terms of that model and
        a mechanism for registering the individual versions that [dispatch_multi] knows
        about.  Registration requires knowing how to get into and out of the model.

        {v
           ,-->-- Q1 --> R1 -->-.
          /                      \
         Q --->-- Q2 --> R2 -->-- R
          \                      /
           `-->-- Q3 --> R3 -->-´
       v}
    *)
    module Make (Model : sig
        val name : string  (* the name of the Rpc's being unified in the model *)
        type query
        type response
      end) : sig

      (** Adds a new version to the set of versions available via [dispatch_multi]. *)
      module Register (Version_i : sig
          val version : int
          type query [@@deriving bin_io]
          type response [@@deriving bin_io]
          val query_of_model : Model.query -> query
          val model_of_response : response -> Model.response
        end) : sig
        val rpc : (Version_i.query, Version_i.response) Rpc.t
      end

      (** A variant of [Register] in which the query is made available when transforming
          the response *)
      module Register' (Version_i : sig
          val version : int
          type query [@@deriving bin_io]
          type response [@@deriving bin_io]
          val query_of_model : Model.query -> query
          val model_of_response : Model.query -> response -> Model.response
        end) : sig
        val rpc : (Version_i.query, Version_i.response) Rpc.t
      end

      include S
        with type query := Model.query
        with type response := Model.response
    end

  end

  module Pipe_rpc : sig

    (* signature for dispatchers *)
    module type S = sig
      type query
      type response
      type error

      (** Multi-version dispatch.

       The return type varies slightly from [Rpc.Pipe_rpc.dispatch] to make it clear
          that conversion of each individual element in the returned pipe may fail. *)

      val dispatch_multi
        : Connection_with_menu.t
        -> query
        -> ( response Or_error.t Pipe.Reader.t * Pipe_rpc.Metadata.t
           , error
           ) Result.t Or_error.t Deferred.t

      val dispatch_iter_multi
        :  Connection_with_menu.t
        -> query
        -> f:(response Pipe_rpc.Pipe_message.t -> Pipe_rpc.Pipe_response.t)
        -> (Pipe_rpc.Id.t, error) Result.t Or_error.t Deferred.t

      val abort_multi
        :  Connection_with_menu.t
        -> Pipe_rpc.Id.t
        -> unit Or_error.t

      (** All rpcs supported by [dispatch_multi] *)
      val rpcs : unit -> Any.t list

      (** All versions supported by [dispatch_multi].
          (useful for computing which old versions may be pruned) *)
      val versions : unit -> Int.Set.t

      val name : string
    end

    (** Given a model of the types involved in a family of Pipe_RPCs, this functor
        provides a single Pipe_RPC versioned dispatch function [dispatch_multi] in terms
        of that model and a mechanism for registering the individual versions that
        [dispatch_multi] knows about.  Registration requires knowing how to get into and
        out of the model.

        {v
            ,-->-- Q1 --> R1 -->-.    E1 -->-.
           /                      \           \
          Q --->-- Q2 --> R2 -->-- R  E2 -->-- E
           \                      /           /
            `-->-- Q3 --> R3 -->-´    E3 -->-´
        v}
    *)
    module Make (Model : sig
        val name : string  (* the name of the Rpc's being unified in the model *)
        type query
        type response
        type error
      end) : sig

      module type Version_shared = sig
        type query [@@deriving bin_io]
        type response [@@deriving bin_io]
        type error [@@deriving bin_io]
        val version : int
        val query_of_model : Model.query -> query
        val model_of_error : error -> Model.error
        val client_pushes_back : bool
      end

      (** add a new version to the set of versions available via [dispatch_multi]
          and [dispatch_iter_multi]. *)
      module Register (Version_i : sig
        include Version_shared
        val model_of_response : response -> Model.response
      end) : sig
        val rpc : (Version_i.query, Version_i.response, Version_i.error) Pipe_rpc.t
      end

      (** [Register_raw] is like [Register] except you get to deal with the whole pipe.
          This is useful if, e.g., your [model_of_response] function can fail, so that
          you'd like to filter items out from the result pipe. *)
      module Register_raw (Version_i : sig
        include Version_shared

        (** [model_of_response] should never raise exceptions.  If it does,
            [dispatch_multi] is going to raise, which is not supposed to happen.

            One may not call [dispatch_iter_multi] when using [Register_raw] as
            [Pipe_rpc.dispatch_iter] never has access to a pipe.
        *)
        val model_of_response
          :  response Pipe.Reader.t
          -> Model.response Or_error.t Pipe.Reader.t
      end) : sig
        val rpc : (Version_i.query, Version_i.response, Version_i.error) Pipe_rpc.t
      end

      include S
        with type query    := Model.query
        with type response := Model.response
        with type error    := Model.error
    end
  end

  module State_rpc : sig
    (* signature for dispatchers *)
    module type S = sig
      type query
      type state
      type update
      type error

      (** Multi-version dispatch

          The return type varies slightly from [Rpc.State_rpc.dispatch] to make it clear
          that conversion of each individual element in the returned pipe may fail. *)
      val dispatch_multi
        : Connection_with_menu.t
        -> query
        -> ( state * update Or_error.t Pipe.Reader.t * State_rpc.Metadata.t
           , error
           ) Result.t Or_error.t Deferred.t

      (** All rpcs supported by [dispatch_multi] *)
      val rpcs : unit -> Any.t list

      (** All versions supported by [dispatch_multi] (useful for computing which old
          versions may be pruned). *)
      val versions : unit -> Int.Set.t

      val name : string
    end

    (** Given a model of the types involved in a family of State_RPCs, this functor
        provides a single State_RPC versioned dispatch function [dispatch_multi] in terms
        of that model and a mechanism for registering the individual versions that
        [dispatch_multi] knows about.  Registration requires knowing how to get into and
        out of the model.

        {v
            ,-->-- Q1 --> (S1, U1s) -->----.       E1 -->-.
           /                                \              \
          Q --->-- Q2 --> (S2, U2s) -->-- (S, Us)  E2 -->-- E
           \                                /              /
            `-->-- Q3 --> (S3, U3s) -->----´       E3 -->-´
        v}
    *)
    module Make (Model : sig
        val name : string  (* the name of the Rpc's being unified in the model *)
        type query
        type state
        type update
        type error
      end) : sig

      module type Version_shared = sig
        type query  [@@deriving bin_io]
        type state  [@@deriving bin_io]
        type update [@@deriving bin_io]
        type error  [@@deriving bin_io]
        val version : int
        val query_of_model : Model.query -> query
        val model_of_state : state -> Model.state
        val model_of_error : error -> Model.error
        val client_pushes_back : bool
      end

      (** Adds a new version to the set of versions available via [dispatch_multi]. *)
      module Register (Version_i : sig
        include Version_shared
        val model_of_update : update -> Model.update
      end) : sig
        val rpc : (Version_i.query, Version_i.state, Version_i.update, Version_i.error)
                    State_rpc.t
      end

      (** [Register_raw] is like [Register] except you get to deal with the whole pipe.
          This is useful if, e.g., your [model_of_update] function can fail, so that
          you'd like to filter items out from the result pipe. *)
      module Register_raw (Version_i : sig
        include Version_shared

        (** [model_of_update] should never raise exceptions.  If it does,
            [dispatch_multi] is going to raise, which is not supposed to happen. *)
        val model_of_update
          :  update Pipe.Reader.t
          -> Model.update Or_error.t Pipe.Reader.t
      end) : sig
        val rpc : (Version_i.query, Version_i.state, Version_i.update, Version_i.error)
                    State_rpc.t
      end

      include S
        with type query  := Model.query
        with type state  := Model.state
        with type update := Model.update
        with type error  := Model.error
    end
  end

  module One_way : sig

    (** signature for rpc dispatchers *)
    module type S = sig
      type msg

      (** multi-version dispatch *)
      val dispatch_multi : Connection_with_menu.t -> msg -> unit Or_error.t

      (** All rpcs supported by [dispatch_multi] *)
      val rpcs : unit -> Any.t list

      (** All versions supported by [dispatch_multi].
          (useful for computing which old versions may be pruned) *)
      val versions : unit -> Int.Set.t

      val name : string
    end

    (** Given a model of the types involved in a family of RPCs, this functor provides a
        single RPC versioned dispatch function [dispatch_multi] in terms of that model and
        a mechanism for registering the individual versions that [dispatch_multi] knows
        about.  Registration requires knowing how to get out of the model.

        {v
           ,-->-- M1
          /
         M --->-- M2
          \
           `-->-- M3
       v}
    *)
    module Make (Model : sig
        val name : string  (* the name of the Rpc's being unified in the model *)
        type msg
      end) : sig

      (** add a new version to the set of versions available via [dispatch_multi]. *)
      module Register (Version_i : sig
          val version : int
          type msg [@@deriving bin_io]
          val msg_of_model : Model.msg -> msg
        end) : sig
        val rpc : Version_i.msg One_way.t
      end

      include S with type msg := Model.msg
    end

  end

end

module Callee_converts : sig

  module Rpc : sig

    module Simple : sig
      type ('query, 'response) t

      val create : name:string -> ('query, 'response) t

      val name : (_, _) t -> string

      val add_version
        :  ('query, 'response) t
        -> version      : int
        -> bin_query    : 'old_query Bin_prot.Type_class.t
        -> bin_response : 'old_response Bin_prot.Type_class.t
        -> ('old_query -> 'query)
        -> ('response -> 'old_response)
        -> ('query, 'response) t Or_error.t

      val add_version_with_failure
        :  ('query, 'response Or_error.t) t
        -> version      : int
        -> bin_query    : 'old_query Bin_prot.Type_class.t
        -> bin_response : ('old_response, string) Result.t Bin_prot.Type_class.t
        -> ('old_query -> 'query Or_error.t)
        -> ('response -> 'old_response Or_error.t)
        -> ('query, 'response Or_error.t) t Or_error.t

      val add_rpc_version
        :  ('query, 'response) t
        -> ('old_query, 'old_response) Rpc.t
        -> ('old_query -> 'query)
        -> ('response -> 'old_response)
        (** [Error _] if the version has already been implemented, or the name
            disagrees. *)
        -> ('query, 'response) t Or_error.t

      val add_rpc_version_with_failure
        :  ('query, 'response Or_error.t) t
        -> ('old_query, ('old_response, string) Result.t) Rpc.t
        -> ('old_query -> 'query Or_error.t)
        -> ('response -> 'old_response Or_error.t)
        -> ('query, 'response Or_error.t) t Or_error.t

      val implement
        :  ('query, 'response) t
        -> ('state -> 'query -> 'response Deferred.t)
        -> 'state Implementation.t list
    end

    module type S = sig
      type query
      type response

      (** implement multiple versions at once *)
      val implement_multi
        :  ?log_not_previously_seen_version : (name:string -> int -> unit)
        -> ('state -> version:int -> query -> response Deferred.t)
        -> 'state Implementation.t list

      (** All rpcs implemented by [implement_multi] *)
      val rpcs : unit -> Any.t list

      (** All versions implemented by [implement_multi].
          (useful for computing which old versions may be pruned) *)
      val versions : unit -> Int.Set.t

      val name : string
    end

    (** Given a model of the types involved in a family of RPCs, this functor provides a
        single multi-version implementation function [implement_multi] in terms of that
        model and a mechanism for registering the individual versions that
        [implement_multi] knows about.  Registration requires knowing how to get into and
        out of the model.

        {v
          Q1 -->-.         ,-->-- R1
                  \       /
          Q2 -->-- Q --> R --->-- R2
                  /       \
          Q3 -->-´         `-->-- R3
        v}
    *)
    module Make (Model : sig
      val name : string  (* the name of the Rpc's being unified in the model *)
      type query
      type response
    end) : sig

      (** Add a new version to the set of versions implemented by [implement_multi]. *)
      module Register (Version_i : sig
          val version : int
          type query [@@deriving bin_io]
          type response [@@deriving bin_io]
          val model_of_query : query -> Model.query
          val response_of_model : Model.response -> response
        end) : sig
        val rpc : (Version_i.query, Version_i.response) Rpc.t
      end

      include S
        with type query := Model.query
        with type response := Model.response
    end

  end

  module Pipe_rpc : sig

    module type S = sig
      type query
      type response
      type error

      (** implement multiple versions at once *)
      val implement_multi
        :  ?log_not_previously_seen_version : (name:string -> int -> unit)
        -> ('state
            -> version : int
            -> query
            -> (response Pipe.Reader.t, error) Result.t Deferred.t)
        -> 'state Implementation.t list

      (** implement multiple versions at once, using a [Versioned_direct_stream_writer] *)
      val implement_direct_multi
        :  ?log_not_previously_seen_version:(name:string -> int -> unit)
        -> ('state
            -> version:int
            -> query
            -> response Versioned_direct_stream_writer.t
            -> (unit, error) Result.t Deferred.t)
        -> 'state Implementation.t list

      (** All rpcs implemented by [implement_multi] *)
      val rpcs : unit -> Any.t list

      (** All versions supported by [implement_multi].
          (useful for computing which old versions may be pruned) *)
      val versions : unit -> Int.Set.t

      val name : string
    end

    (** Given a model of the types involved in a family of Pipe_RPCs, this functor
        provides a single multi-version implementation function [implement_multi] in terms
        of that model and a mechanism for registering the individual versions that
        [implement_multi] knows about.  Registration requires knowing how to get into and
        out of the model.

        {v
          Q1 -->-.         ,-->-- R1
                  \       /
          Q2 -->-- Q --> R --->-- R2
                  /       \
          Q3 -->-´         `-->-- R3
        v}
    *)
    module Make (Model : sig
        val name : string  (* the name of the Rpc's being unified in the model *)
        type query
        type response
        type error
      end) : sig

      module type Version_shared = sig
        type query [@@deriving bin_io]
        type response [@@deriving bin_io]
        type error [@@deriving bin_io]
        val version : int
        val model_of_query : query -> Model.query
        val error_of_model : Model.error -> error
        val client_pushes_back : bool
      end

      (** add a new version to the set of versions available via [implement_multi]
          or [implement_direct]. *)
      module Register (Version_i : sig
          include Version_shared
          val response_of_model : Model.response -> response
        end) : sig
        val rpc : (Version_i.query, Version_i.response, Version_i.error) Pipe_rpc.t
      end

      (** [Register_raw] is like [Register] except you get the whole pipe to deal with.

          This is useful if, e.g., your [response_of_model] function can fail, so that
          you'd like to filter items out from the result pipe.

          You may not call [implement_direct_multi] when using [Register_raw], as
          [Pipe_rpc.implement_direct] never has access to a pipe.
      *)
      module Register_raw (Version_i : sig
        include Version_shared
        val response_of_model : Model.response Pipe.Reader.t -> response Pipe.Reader.t
      end) : sig
        val rpc : (Version_i.query, Version_i.response, Version_i.error) Pipe_rpc.t
      end

      include S
        with type query    := Model.query
        with type response := Model.response
        with type error    := Model.error
    end
  end

  module State_rpc : sig

    module type S = sig
      type query
      type state
      type update
      type error

      (** implement multiple versions at once *)
      val implement_multi
        :  ?log_not_previously_seen_version : (name:string -> int -> unit)
        -> ('connection_state
            -> version : int
            -> query
            -> (state * update Pipe.Reader.t, error) Result.t Deferred.t)
        -> 'connection_state Implementation.t list

      (** All rpcs implemented by [implement_multi] *)
      val rpcs : unit -> Any.t list

      (** All versions supported by [implement_multi].
          (useful for computing which old versions may be pruned) *)
      val versions : unit -> Int.Set.t

      val name : string
    end

    (** Given a model of the types involved in a family of State_RPCs, this functor
        provides a single multi-version implementation function [implement_multi] in terms
        of that model and a mechanism for registering the individual versions that
        [implement_multi] knows about.  Registration requires knowing how to get into and
        out of the model.

        {v
          Q1 -->-.            ,---->-- (S1, U1s)
                  \          /
          Q2 -->-- Q --> (S, Us) -->-- (S2, U2s)
                  /          \
          Q3 -->-´            `---->-- (S3, U3s)
        v}
    *)
    module Make (Model : sig
        val name : string  (* the name of the Rpc's being unified in the model *)
        type query
        type state
        type update
        type error
      end) : sig

      module type Version_shared = sig
        type query  [@@deriving bin_io]
        type state  [@@deriving bin_io]
        type update [@@deriving bin_io]
        type error  [@@deriving bin_io]
        val version : int
        val model_of_query : query -> Model.query
        val state_of_model : Model.state -> state
        val error_of_model : Model.error -> error
        val client_pushes_back : bool
      end

      (** add a new version to the set of versions available via [implement_multi]. *)
      module Register (Version_i : sig
          include Version_shared
          val update_of_model : Model.update -> update
        end) : sig
        val rpc : (Version_i.query, Version_i.state, Version_i.update, Version_i.error)
                    State_rpc.t
      end

      (** [Register_raw] is like [Register] except you get the whole update pipe to deal
          with. This is useful if, e.g., your [update_of_model] function can fail, so
          that you'd like to filter items out from the result pipe. *)
      module Register_raw (Version_i : sig
          include Version_shared
          val update_of_model
            :  Model.state
            -> Model.update Pipe.Reader.t
            -> update Pipe.Reader.t
        end) : sig
        val rpc : (Version_i.query, Version_i.state, Version_i.update, Version_i.error)
                    State_rpc.t
      end

      include S
        with type query  := Model.query
        with type state  := Model.state
        with type update := Model.update
        with type error  := Model.error
    end

  end

  module One_way : sig

    module type S = sig
      type msg

      (** implement multiple versions at once *)
      val implement_multi
        :  ?log_not_previously_seen_version : (name:string -> int -> unit)
        -> ('state -> version:int -> msg -> unit)
        -> 'state Implementation.t list

      (** All rpcs implemented by [implement_multi] *)
      val rpcs : unit -> Any.t list

      (** All versions implemented by [implement_multi].
          (useful for computing which old versions may be pruned) *)
      val versions : unit -> Int.Set.t

      val name : string
    end

    (** Given a model of the types involved in a family of RPCs, this functor provides a
        single multi-version implementation function [implement_multi] in terms of that
        model and a mechanism for registering the individual versions that
        [implement_multi] knows about.  Registration requires knowing how to get into the
        model.

        {v
          M1 -->-.
                  \
          M2 -->-- M
                  /
          M3 -->-´
        v}
    *)
    module Make (Model : sig
      val name : string  (* the name of the Rpc's being unified in the model *)
      type msg
    end) : sig

      (** Add a new version to the set of versions implemented by [implement_multi]. *)
      module Register (Version_i : sig
          val version : int
          type msg [@@deriving bin_io]
          val model_of_msg : msg -> Model.msg
        end) : sig
        val rpc : Version_i.msg One_way.t
      end

      include S with type msg := Model.msg
    end

  end

end

module Both_convert : sig

  (** [Both_convert] rpcs combine features of both caller-converts and callee-converts
      versioning schemes in such a way that one can smoothly add a new version of the rpc
      to a shared library, and it doesn't matter whether the callee or caller upgrades to
      the latest version of the shared library first, the new version will not be
      exercised until both sides support it. *)

  module Plain : sig
    (**
       {v
                     (conv)   (conv)                          (conv)   (conv)
                     caller   callee                          callee   caller
                     |        |                               |        |
                     |        |                               |        |
        Q.caller ---->-- Q1 -->-.             (impl)        .->-- R1 -->---- R.caller
                \                \            callee       /                /
                 \--->-- Q2 -->---\           |           /--->-- R2 -->---/
                  \                \          |          /                /
                   `->-- Q3 -->---- Q.callee --> R.callee ---->-- R3 -->-´
      v}
    *)
    module type S = sig
      type caller_query
      type caller_response
      type callee_query
      type callee_response

      (** multi-version dispatch *)
      val dispatch_multi
        : Connection_with_menu.t -> caller_query -> caller_response Or_error.t Deferred.t

      (** implement multiple versions at once *)
      val implement_multi
        :  ?log_not_previously_seen_version : (name:string -> int -> unit)
        -> ('state -> version:int -> callee_query -> callee_response Deferred.t)
        -> 'state Implementation.t list

      (** All supported rpcs. *)
      val rpcs : unit -> Any.t list

      (** All supported versions.  Useful for detecting old versions that may be pruned. *)
      val versions : unit -> Int.Set.t

      val name : string
    end


    module Make (Model : sig
      val name : string
      module Caller : sig type query type response end
      module Callee : sig type query type response end
    end) : sig

      open Model

      module Register
          (Version : sig
             val version : int
             type query    [@@deriving bin_io]
             type response [@@deriving bin_io]
             val query_of_caller_model : Caller.query -> query
             val callee_model_of_query : query -> Callee.query
             val response_of_callee_model : Callee.response -> response
             val caller_model_of_response : response -> Caller.response
           end) : sig
        val rpc : (Version.query, Version.response) Rpc.t
      end

      include S
        with type caller_query    := Caller.query
        with type caller_response := Caller.response
        with type callee_query    := Callee.query
        with type callee_response := Callee.response
    end
  end

  module Pipe_rpc : sig
    (**
       {v
                     (conv)   (conv)                          (conv)   (conv)
                     caller   callee                          callee   caller
                     |        |                               |        |
                     |        |                               |        |
        Q.caller ---->-- Q1 -->-.             (impl)        .->-- R1 -->---- R.caller
                \                \            callee       /                /
                 \--->-- Q2 -->---\           |           /--->-- R2 -->---/
                  \                \          |          /                /
                   `->-- Q3 -->---- Q.callee --> R.callee ---->-- R3 -->-´
      v}
    *)
    module type S = sig
      type caller_query
      type caller_response
      type caller_error
      type callee_query
      type callee_response
      type callee_error

      (** multi-version dispatch *)
      val dispatch_multi
        :  Connection_with_menu.t
        -> caller_query
        -> ( caller_response Or_error.t Pipe.Reader.t * Pipe_rpc.Metadata.t
           , caller_error
           ) Result.t Or_error.t Deferred.t

      val dispatch_iter_multi
        :  Connection_with_menu.t
        -> caller_query
        -> f:(caller_response Pipe_rpc.Pipe_message.t -> Pipe_rpc.Pipe_response.t)
        -> (Pipe_rpc.Id.t, caller_error) Result.t Or_error.t Deferred.t

      val abort_multi
        :  Connection_with_menu.t
        -> Pipe_rpc.Id.t
        -> unit Or_error.t

      (** implement multiple versions at once *)
      val implement_multi
        :  ?log_not_previously_seen_version : (name:string -> int -> unit)
        -> ('state
            -> version : int
            -> callee_query
            -> (callee_response Pipe.Reader.t, callee_error) Result.t Deferred.t)
        -> 'state Implementation.t list

      val implement_direct_multi
        :  ?log_not_previously_seen_version:(name:string -> int -> unit)
        -> ('state
            -> version:int
            -> callee_query
            -> callee_response Versioned_direct_stream_writer.t
            -> (unit, callee_error) Result.t Deferred.t)
        -> 'state Implementation.t list

      (** All supported rpcs. *)
      val rpcs : unit -> Any.t list

      (** All supported versions. Useful for detecting old versions that may be
          pruned. *)
      val versions : unit -> Int.Set.t

      val name : string
    end

    module Make (Model : sig
        val name : string
        module Caller : sig type query type response type error end
        module Callee : sig type query type response type error end
      end) : sig

      open Model

      module type Version_shared = sig
        val version : int
        type query    [@@deriving bin_io]
        type response [@@deriving bin_io]
        type error    [@@deriving bin_io]
        val query_of_caller_model : Caller.query -> query
        val callee_model_of_query : query -> Callee.query
        val error_of_callee_model : Callee.error -> error
        val caller_model_of_error : error -> Caller.error
        val client_pushes_back : bool
      end

      module Register (Version_i : sig
          include Version_shared
          val response_of_callee_model : Callee.response -> response
          val caller_model_of_response : response -> Caller.response
        end) : sig
        val rpc : (Version_i.query, Version_i.response, Version_i.error) Pipe_rpc.t
      end

      (** [Register_raw] is like [Register] except you get the whole pipe to deal with.

          This is useful if, e.g., your [caller_model_of_response] function can fail, so
          that you'd like to filter items out from the result pipe.

          You can use neither [dispatch_iter_multi] nor [implement_direct_multi] if you
          use this, as their non-versioned counterparts do not get access to pipes.
      *)
      module Register_raw (Version_i : sig
          include Version_shared
          val response_of_callee_model
            : Callee.response Pipe.Reader.t -> response Pipe.Reader.t
          val caller_model_of_response
            : response Pipe.Reader.t -> Caller.response Or_error.t Pipe.Reader.t
        end) : sig
        val rpc : (Version_i.query, Version_i.response, Version_i.error) Pipe_rpc.t
      end

      include S
        with type caller_query    := Caller.query
        with type caller_response := Caller.response
        with type caller_error    := Caller.error
        with type callee_query    := Callee.query
        with type callee_response := Callee.response
        with type callee_error    := Callee.error
    end
  end

  module State_rpc : sig
    (**
       {v
             (conv)  (conv)                      (conv)        (conv)
             caller  callee                      callee        caller
              |      |                           |             |
        caller|      |                           |             |     caller
        Q ---->- Q1 ->-.          (impl)       .->- (S1, U1s) ->---- (S, Us)
         \              \         callee      /                     /
          \--->- Q2 ->---\        |          /--->- (S2, U2s) ->---/
           \              \callee |  callee /                     /
            `->- Q3 ->---- Q ------> (S, Us) ---->- (S3, U3s) ->-´
      v}
    *)
    module type S = sig
      type caller_query
      type callee_query
      type caller_state
      type callee_state
      type caller_update
      type callee_update
      type caller_error
      type callee_error

      (** multi-version dispatch *)
      val dispatch_multi
        :  Connection_with_menu.t
        -> caller_query
        -> ( caller_state * caller_update Or_error.t Pipe.Reader.t * State_rpc.Metadata.t
           , caller_error
           ) Result.t Or_error.t Deferred.t

      (** implement multiple versions at once *)
      val implement_multi
        :  ?log_not_previously_seen_version:(name:string -> int -> unit)
        -> ('state
            -> version : int
            -> callee_query
            -> (callee_state * callee_update Pipe.Reader.t, callee_error) Result.t Deferred.t)
        -> 'state Implementation.t list

      (** All supported rpcs. *)
      val rpcs : unit -> Any.t list

      (** All supported versions. Useful for detecting old versions that may be
          pruned. *)
      val versions : unit -> Int.Set.t

      val name : string
    end

    module Make (Model : sig
        val name : string
        module Caller : sig type query type state type update type error end
        module Callee : sig type query type state type update type error end
      end) : sig

      open Model

      module type Version_shared = sig
        val version : int
        type query  [@@deriving bin_io]
        type state  [@@deriving bin_io]
        type update [@@deriving bin_io]
        type error  [@@deriving bin_io]
        val query_of_caller_model : Model.Caller.query -> query
        val callee_model_of_query : query -> Model.Callee.query
        val caller_model_of_state : state -> Model.Caller.state
        val state_of_callee_model : Model.Callee.state -> state
        val caller_model_of_error : error -> Model.Caller.error
        val error_of_callee_model : Model.Callee.error -> error
        val client_pushes_back : bool
      end

      module Register (Version_i : sig
          include Version_shared
          val update_of_callee_model : Model.Callee.update -> update
          val caller_model_of_update : update -> Model.Caller.update
        end) : sig
        val rpc : (Version_i.query, Version_i.state, Version_i.update, Version_i.error)
                    State_rpc.t
      end

      (** [Register_raw] is like [Register] except you get the whole pipe to deal with.

          This is useful if, e.g., your [caller_model_of_update] function can fail, so
          that you'd like to filter items out from the result pipe. *)
      module Register_raw (Version_i : sig
          include Version_shared
          (** [caller_model_of_update] should never raise exceptions.  If it does,
              [dispatch_multi] is going to raise, which is not supposed to happen. *)
          val caller_model_of_update : update Pipe.Reader.t
            -> Model.Caller.update Or_error.t Pipe.Reader.t
          val update_of_callee_model : Model.Callee.state
            -> Model.Callee.update Pipe.Reader.t -> update Pipe.Reader.t
        end) : sig
        val rpc : (Version_i.query, Version_i.state, Version_i.update, Version_i.error)
                    State_rpc.t
      end

      include S
        with type caller_query  := Caller.query
        with type caller_state  := Caller.state
        with type caller_update := Caller.update
        with type caller_error  := Caller.error
        with type callee_query  := Callee.query
        with type callee_state  := Callee.state
        with type callee_update := Callee.update
        with type callee_error  := Callee.error
    end
  end

  module One_way : sig
    (**
       {v
                     (conv)   (conv)
                     caller   callee
                     |        |
                     |        |
        M.caller ---->-- M1 -->-.             (impl)
                \                \            callee
                 \--->-- M2 -->---\           |
                  \                \          |
                   `->-- M3 -->---- M.callee --> unit
      v}
    *)
    module type S = sig
      type caller_msg
      type callee_msg

      (** multi-version dispatch *)
      val dispatch_multi : Connection_with_menu.t -> caller_msg -> unit Or_error.t

      (** implement multiple versions at once *)
      val implement_multi
        :  ?log_not_previously_seen_version : (name:string -> int -> unit)
        -> ('state -> version:int -> callee_msg -> unit)
        -> 'state Implementation.t list

      (** All supported rpcs. *)
      val rpcs : unit -> Any.t list

      (** All supported versions.  Useful for detecting old versions that may be
          pruned. *)
      val versions : unit -> Int.Set.t

      val name : string
    end

    module Make (Model : sig
      val name : string
      module Caller : sig type msg end
      module Callee : sig type msg end
    end) : sig

      open Model

      module Register
          (Version : sig
             val version : int
             type msg [@@deriving bin_io]
             val msg_of_caller_model : Caller.msg -> msg
             val callee_model_of_msg : msg -> Callee.msg
           end) : sig
        val rpc : Version.msg One_way.t
      end

      include S
        with type caller_msg := Caller.msg
        with type callee_msg := Callee.msg
    end
  end
end
