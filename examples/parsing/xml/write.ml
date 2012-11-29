type response_field =
  |Heading of string
  |Image of string
  |Abstract of string
  |AbstractText of string
and result =
  |Result of result_field list
and result_field =
  |Text of string
  |FirstURL of string
and response = response_field list * result list with xml

let _ =
  let mk_result r = Result r in
  let results = List.map mk_result
  [
    [ Text "Official site"; FirstURL "https://duckduckgo.com/" ];
    [ Text "Mirror site"; FirstURL "https://ddg.gg/" ];
  ] in
  let fields = [
    Heading "DuckDuckGo";
    Image "https://i.duckduckgo.com/i/d9dea591.png";
    Abstract "DuckDuckGo is an Internet search engine";
  ] in
  let response = ( fields, results ) in
  let xml = xml_of_response response in
  prerr_endline (Cow.Xml.to_string xml)
