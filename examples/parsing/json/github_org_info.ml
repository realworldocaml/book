open Core.Std

let () =
  let open Github_org_t in
  In_channel.read_all "js_org.json"
  |> Github_org_j.org_of_string
  |> fun org ->
      let name = Option.value ~default:"???" org.name in
      printf "%s (%d) with %d public repos\n"
        name org.id org.public_repos
