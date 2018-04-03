### build
  $ jbuilder build github_org_info.exe
### run
%% --non-deterministic
  $ jbuilder exec -- ./github_org_info.exe mirage
  MirageOS (131943) with 125 public repos
%% --non-deterministic
  $ jbuilder exec -- ./github_org_info.exe janestreet
  ??? (3384712) with 145 public repos
### generate json
  $ jbuilder build github_org_j.mli
  $ cat _build/default/github_org_j.mli
  (* Auto-generated from "github_org.atd" *)
  
  
  type org = Github_org_t.org = {
    login: string;
    id: int;
    url: string;
    name: string option;
    blog: string option;
    email: string option;
    public_repos: int
  }
  
  val write_org :
    Bi_outbuf.t -> org -> unit
    (** Output a JSON value of type {!org}. *)
  
  val string_of_org :
    ?len:int -> org -> string
    (** Serialize a value of type {!org}
        into a JSON string.
        @param len specifies the initial length
                   of the buffer used internally.
                   Default: 1024. *)
  
  val read_org :
    Yojson.Safe.lexer_state -> Lexing.lexbuf -> org
    (** Input JSON data of type {!org}. *)
  
  val org_of_string :
    string -> org
    (** Deserialize JSON data of type {!org}. *)
  
### generate types
  $ jbuilder build github_org_t.mli
  $ cat _build/default/github_org_t.mli
  (* Auto-generated from "github_org.atd" *)
  
  
  type org = {
    login: string;
    id: int;
    url: string;
    name: string option;
    blog: string option;
    email: string option;
    public_repos: int
  }
