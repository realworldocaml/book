  $ atdgen -t github.atd
  $ atdgen -j github.atd
  $ ocamlfind ocamlc -package atd -i github_t.mli
  type scope =
      [ `Delete_repo | `Gist | `Public_repo | `Repo | `Repo_status | `User ]
  type app = { app_name : string; app_url : string; }
  type authorization_response = {
    scopes : scope list;
    token : string;
    app : app;
    url : string;
    id : int;
    note : string option;
    note_url : string option;
  }
  type authorization_request = {
    auth_req_scopes : scope list;
    auth_req_note : string;
  }
