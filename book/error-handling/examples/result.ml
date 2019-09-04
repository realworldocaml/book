module Result = struct
   type ('a,'b) t = | Ok of 'a
                    | Error of 'b
end
