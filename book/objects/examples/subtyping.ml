

[@@@part "1"];;
type shape = < area : float >

type square = < area : float; width : int >

let square w = object
  method area = Float.of_int (w * w)
  method width = w
end

type circle = < area : float; radius : int >

let circle r = object
  method area = 3.14 *. (Float.of_int r) ** 2.0
  method radius = r
end


[@@@part "2"];;
type 'a stack = < pop: 'a option; push: 'a -> unit >

let square_stack: square stack = stack [square 30; square 10]

let circle_stack: circle stack = stack [circle 20; circle 40]
