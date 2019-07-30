val skip : decoder -> [ `A | `O | `Element | `Member ] -> unit
(** [skip d construct] skips lexemes depending on [construct]:
    {ul
    {- [`A] skips {e past} the [`A_end] matching the last decoded [`A_start].}
    {- [`O] skips {e past} the [`O_end] matching the last decoded [`O_start].} 
    {- [`Element] skips {e to} the next element of the last decoded 
       [`A_start] or {e to} [`A_end] if there is none.}
    {- [`Member] skips {e to} the next [`Name] of the last decoded 
       [`O_start] or {e to} [`O_end] if there is none.}}

    {b Raises} [Invalid_argument] if [d] is not in the right 
    state to skip [construct]. *)
