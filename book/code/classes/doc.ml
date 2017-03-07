type doc =
  | Heading of string
  | Paragraph of text_item list
  | Definition of string list_item list

and text_item =
  | Raw of string
  | Bold of text_item list
  | Enumerate of int list_item list
  | Quote of doc

and 'a list_item = 
  { tag: 'a;
    text: text_item list }


[@@@part "1"];;
open Core

class ['a] folder = object(self)
  method doc acc = function
  | Heading _ -> acc
  | Paragraph text -> List.fold ~f:self#text_item ~init:acc text
  | Definition list -> List.fold ~f:self#list_item ~init:acc list

  method list_item: 'b. 'a -> 'b list_item -> 'a = 
    fun acc {tag; text} ->
      List.fold ~f:self#text_item ~init:acc text

  method text_item acc = function
  | Raw _ -> acc
  | Bold text -> List.fold ~f:self#text_item ~init:acc text
  | Enumerate list -> List.fold ~f:self#list_item ~init:acc list
  | Quote doc -> self#doc acc doc
end


[@@@part "2"];;
class counter = object
  inherit [int] folder as super

  method list_item acc li = acc

  method text_item acc ti =
    let acc = super#text_item acc ti in
    match ti with
    | Bold _ -> acc + 1
    | _ -> acc
end

let count_doc = (new counter)#doc


[@@@part "3"];;
class ['a] folder2 = object(self)
  method doc acc = function
  | Heading str -> self#heading acc str
  | Paragraph text -> self#paragraph acc text
  | Definition list -> self#definition acc list

  method list_item: 'b. 'a -> 'b list_item -> 'a =
    fun acc {tag; text} ->
      List.fold ~f:self#text_item ~init:acc text

  method text_item acc = function
  | Raw str -> self#raw acc str
  | Bold text -> self#bold acc text
  | Enumerate list -> self#enumerate acc list
  | Quote doc -> self#quote acc doc

  method private heading acc str = acc
  method private paragraph acc text =
    List.fold ~f:self#text_item ~init:acc text
  method private definition acc list =
    List.fold ~f:self#list_item ~init:acc list

  method private raw acc str = acc
  method private bold acc text = 
    List.fold ~f:self#text_item ~init:acc text
  method private enumerate acc list = 
    List.fold ~f:self#list_item ~init:acc list
  method private quote acc doc = self#doc acc doc
end

let f :
  < doc : int -> doc -> int;
    list_item : 'a . int -> 'a list_item -> int;
    text_item : int -> text_item -> int >  = new folder2


[@@@part "4"];;
class counter_with_private_method = object
  inherit [int] folder2 as super

  method list_item acc li = acc

  method private bold acc txt = 
    let acc = super#bold acc txt in
    acc + 1
end


[@@@part "5"];;
class counter_with_sig : object
  method doc : int -> doc -> int
  method list_item : int -> 'b list_item -> int
  method text_item : int -> text_item -> int
end = object
  inherit [int] folder2 as super

  method list_item acc li = acc

  method private bold acc txt = 
    let acc = super#bold acc txt in
    acc + 1
end
