open Core.Std

module Check_name : Identifiable = String 
module User : Identifiable = String
module Host : Identifiable = String

module Check_result = struct
  type t =
    { check: Check_name.t;
      host: Host.t;
      time: Time.t;
      exit_code: int;
      output: string;
    }
end

type event =
[ `Check_result of Check_result.t
| `Downtimed_until of Time.t * User.t * Check_name.t * Host.t
| `Downtime_cancelled of Check_name.t * User.t
| `Acknowledged of Check_name.t * User.t
| `Host_added of Host.t
| `Host_removed of Host.t    
]

