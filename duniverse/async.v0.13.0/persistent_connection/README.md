# Persistent_connection

A persistent connection is an actively maintained connection to some
service that eagerly and repeatedly attempts to reconnect whenever the
underlying connection is lost, until a new one can be established.

There are convenience functions for the special case where the service
is an Async RPC service.
