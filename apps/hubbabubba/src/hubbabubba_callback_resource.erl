-module(hubbabubba_callback_resource).

-export([init/1,
	 allowed_methods/2,
	 content_types_provided/2,
	 to_text/2]).

-include_lib("webmachine/include/webmachine.hrl").

init([]) ->
    {ok, []}.

allowed_methods(ReqData, Context) ->
    {['HEAD', 'GET'], ReqData, Context}.

content_types_provided(ReqData, Context) ->
    {[{"text/plain", to_text}], ReqData, Context}.

to_text(ReqData, Context) ->
    case wrq:get_qs_value("hub.challenge", ReqData) of
	undefined ->
	    hubbabubba_utils:error_response(400, "Missing challenge",
					    ReqData, Context);
	Challenge ->
	    case wrq:get_qs_value("hub.mode", ReqData) of
		undefined ->
		    hubbabubba_utils:error_response(400, "Missing mode",
						    ReqData, Context);
		_ ->
		    case wrq:get_qs_value("hub.topic", ReqData) of
			undefined ->
			    hubbabubba_utils:error_response(400,
							    "Missing Topic",
							    ReqData, Context);
			_ ->
			    {Challenge, ReqData, Context}
		    end
	    end
    end.
