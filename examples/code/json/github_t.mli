(* Auto-generated from "github.atd" *)


type scope = [
    `User | `Public_repo | `Repo | `Repo_status | `Delete_repo | `Gist
]

type app = { app_name (*atd name *): string; app_url (*atd url *): string }

type authorization_response = {
  scopes: scope list;
  token: string;
  app: app;
  url: string;
  id: int;
  note: string option;
  note_url: string option
}

type authorization_request = {
  auth_req_scopes (*atd scopes *): scope list;
  auth_req_note (*atd note *): string
}
