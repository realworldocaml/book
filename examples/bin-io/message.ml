open Core.Std

module Source = struct
  type t = { hostname: string;
             port: int;
             desc: string;
           }
  with bin_io
end

type t = { topic: string list;
           content: string;
           source: Source.t;
         }
with bin_io

