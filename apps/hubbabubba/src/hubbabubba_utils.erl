-module(hubbabubba_utils).

%% API
-export([get_version/0,
	 error_response/4]).

%% ===================================================================
%% API
%% ===================================================================

get_version() ->
    {ok, Version} = application:get_key(hubbabubba, vsn),
    Version.

error_response(Code, Reason, ReqData, Context) when is_atom(Reason) ->
    ReqData1 = wrq:set_resp_headers([{"Content-Type", "text/plain"}], ReqData),
    Message = hubbabubba_error:get_msg(Reason),
    ReqData2 = wrq:set_resp_body(Message, ReqData1),
    {{halt, Code}, ReqData2, Context}.
