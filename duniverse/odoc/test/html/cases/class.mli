class type empty =
object
end

class type mutually =
object
end

and recursive =
object
end

class mutually' : mutually
and recursive' : recursive

class type virtual empty_virtual =
object
end

class virtual empty_virtual' : empty

class type ['a] polymorphic =
object
end

class ['a] polymorphic' : ['a] polymorphic
