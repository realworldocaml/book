type 'constructor t = {
  name : string;
  (* the position of the constructor in the type definition, starting from 0 *)
  rank : int;

  constructor : 'constructor
}
