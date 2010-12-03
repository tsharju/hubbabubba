-module(hubbabubba_resource).

%% API
-export([init/1,
	 allowed_methods/2,
	 content_types_accepted/2,
	 to_html/2,
	 post_is_create/2,
	 create_path/2,
	 process_form/2,
	 finish_request/2]).

-include_lib("webmachine/include/webmachine.hrl").

-include("hubbabubba.hrl").

-record(context, {version}).

%% ===================================================================
%% API
%% ===================================================================

init([]) ->
    Version = hubbabubba_utils:get_version(),
    Context = #context{version=Version},
    {ok, Context}.

allowed_methods(ReqData, Context) ->
    {['HEAD', 'GET', 'POST'], ReqData, Context}.

content_types_accepted(ReqData, Context) ->
    {[{"application/x-www-form-urlencoded", process_form}], ReqData, Context}.

to_html(ReqData, Context) ->
    {hubbabubba_web:index(), ReqData, Context}.

post_is_create(ReqData, Context) ->
    {true, ReqData, Context}.

create_path(ReqData, Context) ->
    {"/", ReqData, Context}.

finish_request(ReqData, Context) ->
    ReqData1 = wrq:set_resp_header("X-Hubbabubba-Version",
				   Context#context.version,
				   ReqData),
    {true, ReqData1, Context}.

process_form(ReqData, Context) ->
    Args = mochiweb_util:parse_qs(wrq:req_body(ReqData)),
    case hubbabubba_req_parser:parse_arguments(Args) of
	{error, Reason} ->
	    hubbabubba_error:response(400, Reason, ReqData, Context);
	{ok, R} ->
	    process_request(R, ReqData, Context)
    end.

set_verify_token(ReqData, R) when R#request.verify_token =:= undefined ->
    ReqData;
set_verify_token(ReqData, R) ->
    wrq:set_resp_body(R#request.verify_token, ReqData).

process_request(R, ReqData, Context) when R#request.mode =:= subscribe ->
    case R#request.verify of
	sync ->
	    {ok, Status} =
		hub_server:subscribe(sync,
				     R#request.callback,
				     R#request.topic,
				     R#request.lease_seconds,
				     R#request.secret,
				     R#request.verify_token),
	    ReqData1 = set_verify_token(ReqData, R),
	    {true, ReqData1, Context};
	async ->
	    ok =
		hub_server:subscribe(async,
				     R#request.callback,
				     R#request.topic,
				     R#request.lease_seconds,
				     R#request.secret,
				     R#request.verify_token),
	    ReqData1 = set_verify_token(ReqData, R),
	    {true, ReqData1, Context}
    end;
process_request(R, ReqData, Context) when R#request.mode =:= unsubscribe ->
    hubbabubba_error:response(501, not_implemented, ReqData, Context);
process_request(R, ReqData, Context) when R#request.mode =:= publish ->
    hubbabubba_error:response(501, not_implemented, ReqData, Context).

