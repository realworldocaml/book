let (_:some_type) = <expr>
let () = ignore (<expr> : some_type)
)(* if the expression returns a unit Deferred.t *)
let () = don't_wait_for (<expr>
