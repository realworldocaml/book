
let pretty_print ?std out x =
  Pretty.pp ?std out x

let pretty_to_string ?std x =
  Pretty.to_string ?std x

let pretty_to_channel ?std oc x =
  Pretty.to_channel ?std oc x
