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

-record(context, {version}).

%% ===================================================================
%% API
%% ===================================================================

init([]) ->
    Version = hubbabubba_utils:get_version(),
    Context = #context{version=Version},
    {{trace, "/"}, Context}.

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
    NewReqData = wrq:set_resp_header("X-Hubbabubba-Version",
				     Context#context.version,
				     ReqData),
    {true, NewReqData, Context}.

process_form(ReqData, Context) ->
    Args = mochiweb_util:parse_qs(wrq:req_body(ReqData)),
    
    {true, ReqData, Context}.
