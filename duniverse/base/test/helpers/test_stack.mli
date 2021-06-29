open! Base
module Debug (S : Stack.S) : Stack.S with type 'a t = 'a S.t
module Test (S : Stack.S) : sig end
