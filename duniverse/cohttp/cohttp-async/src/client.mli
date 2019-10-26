(** Send an HTTP request with an arbitrary body
    The request is sent as-is. *)
val request :
  ?interrupt:unit Async_kernel.Deferred.t ->
  ?ssl_config:Conduit_async.V2.Ssl.Config.t ->
  ?uri:Uri.t ->
  ?body:Body.t ->
  Cohttp.Request.t ->
  (Cohttp.Response.t * Body.t) Async_kernel.Deferred.t

(** Send an HTTP request with arbitrary method and a body
    Infers the transfer encoding *)
val call :
  ?interrupt:unit Async_kernel.Deferred.t ->
  ?ssl_config:Conduit_async.V2.Ssl.Config.t ->
  ?headers:Cohttp.Header.t ->
  ?chunked:bool ->
  ?body:Body.t ->
  Cohttp.Code.meth ->
  Uri.t ->
  (Cohttp.Response.t * Body.t) Async_kernel.Deferred.t

val callv :
  ?interrupt:unit Async_kernel.Deferred.t ->
  ?ssl_config:Conduit_async.V2.Ssl.Config.t ->
  Uri.t ->
  (Cohttp.Request.t * Body.t) Async_kernel.Pipe.Reader.t ->
  (Cohttp.Response.t * Body.t) Async_kernel.Pipe.Reader.t Async_kernel.Deferred.t

(** Send an HTTP GET request *)
val get :
  ?interrupt:unit Async_kernel.Deferred.t ->
  ?ssl_config:Conduit_async.V2.Ssl.Config.t ->
  ?headers:Cohttp.Header.t ->
  Uri.t ->
  (Cohttp.Response.t * Body.t) Async_kernel.Deferred.t

(** Send an HTTP HEAD request *)
val head :
  ?interrupt:unit Async_kernel.Deferred.t ->
  ?ssl_config:Conduit_async.V2.Ssl.Config.t ->
  ?headers:Cohttp.Header.t ->
  Uri.t ->
  Cohttp.Response.t Async_kernel.Deferred.t

(** Send an HTTP DELETE request *)
val delete :
  ?interrupt:unit Async_kernel.Deferred.t ->
  ?ssl_config:Conduit_async.V2.Ssl.Config.t ->
  ?headers:Cohttp.Header.t ->
  ?chunked:bool ->
  ?body:Body.t ->
  Uri.t ->
  (Cohttp.Response.t * Body.t) Async_kernel.Deferred.t

(** Send an HTTP POST request.
    [chunked] encoding is off by default as not many servers support it
*)
val post :
  ?interrupt:unit Async_kernel.Deferred.t ->
  ?ssl_config:Conduit_async.V2.Ssl.Config.t ->
  ?headers:Cohttp.Header.t ->
  ?chunked:bool ->
  ?body:Body.t ->
  Uri.t ->
  (Cohttp.Response.t * Body.t) Async_kernel.Deferred.t

(** Send an HTTP PUT request.
    [chunked] encoding is off by default as not many servers support it
*)
val put :
  ?interrupt:unit Async_kernel.Deferred.t ->
  ?ssl_config:Conduit_async.V2.Ssl.Config.t ->
  ?headers:Cohttp.Header.t ->
  ?chunked:bool ->
  ?body:Body.t ->
  Uri.t ->
  (Response.t * Body.t) Async_kernel.Deferred.t

(** Send an HTTP PATCH request.
    [chunked] encoding is off by default as not many servers support it
*)
val patch :
  ?interrupt:unit Async_kernel.Deferred.t ->
  ?ssl_config:Conduit_async.V2.Ssl.Config.t ->
  ?headers:Cohttp.Header.t ->
  ?chunked:bool ->
  ?body:Body.t ->
  Uri.t ->
  (Response.t * Body.t) Async_kernel.Deferred.t

(** Send an HTTP POST request in form format *)
val post_form:
  ?interrupt:unit Async_kernel.Deferred.t ->
  ?ssl_config:Conduit_async.V2.Ssl.Config.t ->
  ?headers:Cohttp.Header.t ->
  params:(string * string list) list ->
  Uri.t ->
  (Response.t * Body.t) Async_kernel.Deferred.t
