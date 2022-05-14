let bar _x = ()

let foo x =
  bar x;
  [%probe "testing" (bar x)];
  if [%probe_is_enabled "testing"] then
    bar x
