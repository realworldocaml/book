class square w = object(self) 
  method width = w
  method area = Float.of_int (self#width * self#width)
  method larger other = self#area > other#area
end
