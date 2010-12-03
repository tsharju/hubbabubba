-module(hubbabubba_error).

-export([get_msg/1]).

-define(REASONS, [{missing_mode, "Missing REQUIRED parameter hub.mode!"},
		  {missing_topic, "Missing REQUIRED parameter hub.topic!"},
		  {missing_callback,
		   "Missing REQUIRED parameter hub.callback!"},
		  {missing_verify, "Missing REQUIRED parameter hub.verify!"},
		  {invalid_topic, "Invalid value for hub.topic!"},
		  {invalid_mode, "Invalid value for hub.mode!"},
		  {not_implemented, "Not implemented!"}]).

get_msg(Reason) -> 
    case proplists:get_value(Reason, ?REASONS) of
	undefined ->
	    "Error! " ++ atom_to_list(Reason);
	ReasonStr ->
	    ReasonStr
    end.
