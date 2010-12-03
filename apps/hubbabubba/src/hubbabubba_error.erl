-module(hubbabubba_error).

-export([response/4]).

-define(REASONS,
	[{missing_mode, "Missing REQUIRED parameter hub.mode!"},
	 {missing_topic, "Missing REQUIRED parameter hub.topic!"},
	 {missing_callback, "Missing REQUIRED parameter hub.callback!"},
	 {missing_verify, "Missing REQUIRED parameter hub.verify!"},
	 {invalid_topic, "Invalid value for hub.topic!"},
	 {invalid_mode, "Invalid value for hub.mode!"},
	 {not_implemented, "Not implemented!"}
	]).

response(Code, Reason, ReqData, Context) when is_atom(Reason) ->
    ReqData1 = wrq:set_resp_headers([{"Content-Type", "text/plain"}], ReqData),
    Message = get_msg(Reason),
    ReqData2 = wrq:set_resp_body(Message, ReqData1),
    {{halt, Code}, ReqData2, Context}.

get_msg(Reason) -> 
    case proplists:get_value(Reason, ?REASONS) of
	undefined ->
	    "Error! " ++ atom_to_list(Reason);
	ReasonStr ->
	    ReasonStr
    end.
