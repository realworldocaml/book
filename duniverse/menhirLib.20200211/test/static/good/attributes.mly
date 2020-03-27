%token A [@a 0]
%token B [@b 0]
%token C [@c 0] [@d 1]

%attribute A B [@foo "foo"]
%attribute C [@bar "bar"] [@baz "baz"]
%attribute main [@main ()]

%[@claim "this is a grammar attribute"]

%start<unit> main

%%

main [@cost 0]:
  A B [@producer true] C {}
