### build
  $ jbuilder build github_org_info.exe
### run
  $ jbuilder exec -- ./github_org_info.exe mirage
  $ jbuilder exec -- ./github_org_info.exe janestreet
### generate json
  $ jbuilder build github_org_j.mli
  $ cat _build/default/github_org_j.mli
### generate types
  $ jbuilder build github_org_t.mli
  $ cat _build/default/github_org_t.mli
