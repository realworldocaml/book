open Tyxml;

let createElement = (~title: string, ~children: list('a), ()): Html.doc => {
  <html>
    <head>
      <title> {Html.txt(title)} </title>
      <link rel="stylesheet" href="home.css" />
    </head>
    <body> ...children </body>
  </html>
};
