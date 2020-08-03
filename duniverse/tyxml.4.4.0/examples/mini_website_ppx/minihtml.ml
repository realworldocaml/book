open Tyxml

let%html mycontent = {|
  <div class="content">
    <h1>A fabulous title</h1>
    This is a fabulous content.
  </div>
|}


let mytitle = Html.txt "A Fabulous Web Page"

let%html mypage =
  {|<html>
     <head>
       <title>|}mytitle{|</title>
     </head>
     <body>|}[mycontent]{|</body>
   </html>
  |}

let () =
  let file = open_out "index.html" in
  let fmt = Format.formatter_of_out_channel file in
  Format.fprintf fmt "%a@." (Html.pp ~indent:true ()) mypage;
  close_out file
