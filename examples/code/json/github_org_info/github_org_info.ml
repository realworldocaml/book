open Core

let print_org file () =
  let url = sprintf "https://api.github.com/orgs/%s" file in
  Core_extended.Shell.run_full "curl" [url]
  |> Github_org_j.org_of_string
  |> fun org ->
  let open Github_org_t in
  let name = Option.value ~default:"???" org.name in
  printf "%s (%d) with %d public repos\n"
    name org.id org.public_repos

let () =
  let open Command.Let_syntax in
  Command.basic ~summary:"Print Github organization information"
    [%map_open
      let org = anon ("organization" %: string) in
      print_org org
    ]
  |> Command.run
