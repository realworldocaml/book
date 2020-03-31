(* Original file: links.0.8/links-0.8/core/jsonparse.mly *)
%{
open Utility
open ProcessTypes
open WebsocketMessages

(* let unparse_label = function *)
(*   | `Char c -> String.make 1 c *)
(*   | `List (`Char _::_) as s -> Value.unbox_string s *)
(*   | r -> (failwith "(json) error decoding label " ^ Show.show Value.show_t r) *)

(* BUG: need to unescape strings
   (where they are escaped in json.ml)
*)
let websocket_req assoc_list =
  (* Pervasives is opened in order to get "pipe" behaviour of |> *)
  let open Pervasives in
  let opcode = Value.unbox_string @@ List.assoc "opcode" assoc_list in
  (* If we add more opcodes in, we might have to push these inwards *)
  let get_field field = List.assoc field assoc_list in
  let get_and_unbox_str = Value.unbox_string -<- get_field in

  let parse_srv_ap_msg () =
    let blocked_pid =
          ProcessID.of_string (get_and_unbox_str "blockedClientPid") in
    let server_apid =
      AccessPointID.of_string (get_and_unbox_str "serverAPID") in
    (blocked_pid, server_apid) in

    match opcode with
    | "CLIENT_TO_CLIENT" ->
        let pid = ProcessID.of_string (get_and_unbox_str "destPid") in
        let msg = get_field "msg" in
        let client_id =
          ClientID.of_string @@ get_and_unbox_str "destClientId" in
        ClientToClient (client_id, pid, msg)
    | "CLIENT_TO_SERVER" ->
        let pid = ProcessID.of_string (get_and_unbox_str "destPid") in
        let msg = (List.assoc "msg" assoc_list) in
        ClientToServer (pid, msg)
    | "SERVER_AP_REQUEST" ->
        let (pid, apid) = parse_srv_ap_msg () in
        APRequest (pid, apid)
    | "SERVER_AP_ACCEPT" ->
        let (pid, apid) = parse_srv_ap_msg () in
        APAccept (pid, apid)
    | "REMOTE_SESSION_SEND" ->
        let parse_deleg_entry entry =
          let chan =
            List.assoc "chan" entry |> Value.unbox_channel in
          let buf =
            List.assoc "buffer" entry
            |> Value.unbox_list
            |> List.rev in (* Client representation of a buffer is reversed... *)
          (chan, buf) in

        let remote_ep =
          List.assoc "remoteEP" assoc_list
            |> Value.unbox_string
            |> ChannelID.of_string in
        let deleg_set =
          List.assoc "delegatedSessions" assoc_list
            |> Value.unbox_list
            |> List.map (parse_deleg_entry -<- Value.unbox_record) in
        let msg = List.assoc "msg" assoc_list in
        ChanSend (remote_ep, deleg_set, msg)
    | "LOST_MESSAGES" ->
        let carrier_chan = get_and_unbox_str "epID" |> ChannelID.of_string in
        let unboxed_msgs = get_field "msgs" |> Value.unbox_record in

        let parse_lost_message_entry (chan_str, msgs) =
          let chan_id = ChannelID.of_string chan_str in
          let msgs = Value.unbox_list msgs |> List.rev in
          (chan_id, msgs) in

        let msg_table = List.map parse_lost_message_entry unboxed_msgs in
        LostMessageResponse (carrier_chan, msg_table)
    | "CHANNEL_CANCELLATION" ->
        let notify_ep = get_and_unbox_str "notify_ep" |> ChannelID.of_string in
        let cancelled_ep = get_and_unbox_str "cancelled_ep" |> ChannelID.of_string in
        ChannelCancellation ( { notify_ep = notify_ep; cancelled_ep = cancelled_ep } )
    | _ -> failwith "Invalid opcode in websocket message"
%}

%token LBRACE RBRACE LBRACKET RBRACKET LPAREN RPAREN
%token COLON COMMA UNDERSCORE TRUE FALSE NULL
%token <string> STRING
%token <int> INT
%token <float> FLOAT

%start parse_json
%start parse_websocket_request
%type <Value.t> parse_json
%type <WebsocketMessages.incoming_websocket_message> parse_websocket_request

%%

parse_json:
| value { $1 }

parse_websocket_request:
| LBRACE members RBRACE { websocket_req $2 }

object_:
| LBRACE RBRACE         { `Record [] }
| LBRACE members RBRACE { match $2 with
                            | ["_c", c] -> Value.box_char ((Value.unbox_string c).[0])
                            | ["_tail", xs; "_head", x] -> `List (x :: (Value.unbox_list xs))
                            | ["_label", l; "_value", v]
                            | ["_value", v; "_label", l] ->
                                Value.box_variant (Value.unbox_string l) v
                            | ["_clientAPID", apid_str; "_clientId", cid_str]
                            | ["_clientId", cid_str; "_clientAPID", apid_str] ->
                                let apid = AccessPointID.of_string @@ Value.unbox_string apid_str in
                                let cid = ClientID.of_string @@ Value.unbox_string cid_str in
                                `AccessPointID (`ClientAccessPoint (cid, apid))
                            | ["_serverAPID", apid_str] ->
                                let apid = AccessPointID.of_string @@ Value.unbox_string apid_str in
                                `AccessPointID (`ServerAccessPoint (apid))
                            | ["_serverPid", v] ->
                                `Pid (`ServerPid (ProcessID.of_string (Value.unbox_string v)))
                            | ["_clientPid", pid_str; "_clientId", client_id_str]
                            | ["_clientId", client_id_str; "_clientPid", pid_str] ->
                                let client_id = ClientID.of_string (Value.unbox_string client_id_str) in
                                let pid = ProcessID.of_string (Value.unbox_string pid_str) in
                                `Pid (`ClientPid (client_id, pid))
                            | ["_clientSpawnLoc", client_id_str] ->
                                let client_id = ClientID.of_string (Value.unbox_string client_id_str) in
                                `SpawnLocation (`ClientSpawnLoc (client_id))
                            | ["_serverSpawnLoc", _] ->
                                `SpawnLocation (`ServerSpawnLoc)
                            | ["_sessEP1", ep1_str; "_sessEP2", ep2_str]
                            | ["_sessEP2", ep2_str; "_sessEP1", ep1_str] ->
                                let ep1 = ChannelID.of_string @@ Value.unbox_string ep1_str in
                                let ep2 = ChannelID.of_string @@ Value.unbox_string ep2_str in
                                `SessionChannel (ep1, ep2)
                            | ["_db", db] ->
                                begin
                                  match db with
                                    | `Record bs ->
                                        let driver = Value.unbox_string (List.assoc "driver" bs)
                                        and params =
                                          Value.reconstruct_db_string
                                            (Value.unbox_string (List.assoc "name" bs),
                                             Value.unbox_string (List.assoc "args" bs)) in
                                          `Database (Value.db_connect driver params)
                                    | _ -> failwith ("jsonparse: database value must be a record")
                                end
                            | ["_table", t] ->
                                begin
                                  match t with
                                    | `Record bs ->
                                        let db =
                                          begin
                                            match List.assoc "db" bs with
                                              | `Database db -> db
                                              | _ -> failwith ("jsonparse: first argument to a table must be a database")
                                          end
					and name = Value.unbox_string (List.assoc "name" bs)
                                        and row =
                                          begin
                                            match DesugarDatatypes.read ~aliases:Env.String.empty (Value.unbox_string (List.assoc "row" bs)) with
                                                | `Record row -> row
                                                | _ -> failwith ("jsonparse: tables must have record type")
                                          end
                                        and keys =
					  begin
					    match List.assoc "keys" bs with
					      | `List keys ->
						  List.map
						    (function
						       | `List part_keys ->
							   List.map Value.unbox_string part_keys
						       | _ -> failwith "jsonparse: keys must be lists of strings")
						    keys
					      | _ -> failwith ("jsonparse: table keys must have list type")
					  end
                                        in
                                          `Table (db, name, keys, row)
                                    | _ -> failwith ("jsonparse: table value must be a record")
                                end
                            | ["_xml", t] ->
                              begin
                                match t with
                                  | `Record kvps ->
                                    let get_assoc = (fun key -> match (List.find (function
                                      | (k, _) when (k = key) -> true
                                      | _ -> false
                                    ) kvps) with
                                      | (_, value) -> value) in
                                    let elemType = Value.unbox_string (get_assoc "type") in
                                    (
                                      match elemType with
                                        | "TEXT" -> `XML (Value.Text (Value.unbox_string (get_assoc "text")))
                                        | "ELEMENT" ->
                                          let tag = Value.unbox_string (get_assoc "tagname") in
                                          let attrs = get_assoc "attributes" in
                                          let attrs = match attrs with
                                            | `Record attrs -> attrs
                                            | _ -> failwith ("jsonparse: xml attributes should be an attribute record") in
                                          let attrs = List.fold_left (fun attrs (label, value) ->
                                            Value.Attr (label, Value.unbox_string value) :: attrs
                                          ) [] attrs in
                                          let body = get_assoc "body" in
                                          let body = match body with
                                            | `List body -> List.map (function
                                                | `XML body -> body
                                                | _ -> failwith ("jsonparse: xml body should be a list of xmlitems")
                                              ) body
                                            | _ -> failwith ("jsonparse: xml body should be a list of xmlitems")
                                          in `XML (Value.Node (tag, attrs @ body))
                                        | _ -> failwith ("Jsonparse: xml of unknown type in jsonparse. Got type: " ^ elemType)
                                      )
                                  | _ ->  failwith (
                                      "jsonparse: xml should be either a text node or an element node. Got: " ^
                                      Value.string_of_value t
                                    )
                                end
                            | ["_closureTable", id] ->
                              `ClientFunction("_closureTable["^Value.string_of_value id^"]")
                            | ["_serverFunc", id]
                            | ["_serverFunc", id; "_env", `Record []]
                            | ["_env", `Record []; "_serverFunc", id] ->
                              `FunctionPtr(Value.unbox_int id, None)
                            | ["_serverFunc", id; "_env", fvs]
                            | ["_env", fvs; "_serverFunc", id] ->
                              `FunctionPtr(Value.unbox_int id, Some fvs)
                            | ["_domRefKey", id] ->
                              `ClientDomRef(Value.unbox_int id)
                            | _ -> `Record (List.rev $2)
                        }

members:
| id COLON value                     { [$1, $3] }
| members COMMA id  COLON value      { ($3, $5) :: $1 }

array:
| LBRACKET RBRACKET                  { `List ([]) (* For now, we denote Nil as [] *) }

value:
| string                             { $1 }
| number                             { $1 }
| object_                            { $1 }
| array                              { $1 }
| TRUE                               { `Bool true }
| FALSE                              { `Bool false }
| NULL                               { `List [] }

string:
| STRING                             { Value.box_string $1 }
id:
| STRING                             { $1 }

number:
| FLOAT                             { `Float $1 }
| INT                               { `Int $1 }
