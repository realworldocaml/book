
%token <X.t> T
%{ let y = Y.y %}
%parameter <X : sig type t val x: t end>
%parameter <Y : sig type u = X.t val y: u end>
%token <Y.u> U
%{ let x = X.x %}

%start <X.t * Y.u> main

%%

main:
  T U { ($1, $2) }

