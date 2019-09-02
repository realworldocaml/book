open Util

let header current categories pages =
  let aux short =
    let long = match Category.find short with
      | None -> failwith ("cannot find category " ^ short)
      | Some c -> c in
    let url =
      try
        let first = List.find
            (fun p -> p.Page.category = Some short && p.Page.id = 1)
            pages in
        first.Page.permalink
      with Not_found ->
        (* we are processing a blog entry
           or an index page *)
        Config.url short / "index.html" in
    if short = current then
      <:xhtml<<li class="current"><a href=$str:url$>$str:long$</a></li>&>>
    else
      <:xhtml<<li><a href=$str:url$>$str:long$</a></li>&>> in
  <:xhtml<
    <div class="nav">
      <div class="container">
        <div class="nav_left">
          <a href=$str:Config.url "index.html"$><div class="banner">.</div></a>
        </div>
        <div class="nav_right">
          <ul>$list:List.map aux categories$</ul>
        </div>
      </div>
    </div>
  >>

let footer current categories pages =
  let categories =
    List.map (fun short ->
        let long = match Category.find short with
          | None -> failwith ("cannot find category " ^ short)
          | Some c -> c in
        short,
        long,
        List.sort Page.compare (List.filter (fun p -> p.Page.category = Some short) pages)
      ) categories in
  let aux (short, long, pages) =
    let pages = List.map (fun p ->
        if p.Page.footer then
          <:xhtml<
            <li><a href=$str:p.Page.permalink$>$str:p.Page.title$</a></li>
          >> else
          Xhtml.empty
      ) pages in
    <:xhtml<
      <ul>
      <li><h2>$str:long$</h2></li>
      $list:pages$
      </ul>
    >> in
  <:xhtml<
    <div class="container">
      <div class="nav">
        $list:List.map aux categories$
      </div>
    </div>
  >>
