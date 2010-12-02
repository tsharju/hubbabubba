-module(hubbabubba_req_parser).

-include("hubbabubba.hrl").

-include_lib("eunit/include/eunit.hrl").

%% API
-export([parse_arguments/1]).

%% ===================================================================
%% API
%% ===================================================================

parse_arguments(Args) ->
    case proplists:get_value("hub.mode", Args) of
	"publish" ->
	    parse_request(Args, publish);
	"subscribe" ->
	    parse_request(Args, subscribe);
	"unsubscribe" ->
	    parse_request(Args, unsubscribe);
	undefined ->
	    {error, missing_mode};
	_ ->
	    {error, invalid_mode}
    end.

%% ===================================================================
%% Internal functions
%% ===================================================================

parse_request(Data, Mode) when is_atom(Mode) ->
    parse_request(Data, #request{mode=Mode});
parse_request([], #request{topic=undefined, mode=subscribe}) ->
    {error, missing_topic};
parse_request([], #request{callback=undefined, mode=subscribe}) ->
    {error, missing_callback};
parse_request([], #request{verify=undefined, mode=subscribe}) ->
    {error, missing_verify};
parse_request([], #request{url=[], mode=publish}) ->
    {error, missing_url};
parse_request([], Request) ->
    {ok, Request};
parse_request([{"hub.callback", Callback}|Rest], R) ->
    case http_uri:parse(Callback) of
	{error, _} ->
	    {error, invalid_callback};
	CallbackParsed ->
	    parse_request(Rest, R#request{callback=Callback,
					  callback_parsed=CallbackParsed})
    end;
parse_request([{"hub.topic", Topic}|Rest], R) ->
    case http_uri:parse(Topic) of
	{error, _} ->
	    {error, invalid_topic};
	TopicParsed ->
	    parse_request(Rest, R#request{topic=Topic,
					  topic_parsed=TopicParsed})
    end;
parse_request([{"hub.verify", Verify}|Rest], R) ->
    case Verify of
	"sync" ->
	    parse_request(Rest, R#request{verify=sync});
	"async" ->
	    parse_request(Rest, R#request{verify=async});
	_ ->
	    {error, invalid_verify}
    end;
parse_request([{"hub.lease_seconds", LeaseSeconds}|Rest], R) ->
    case LeaseSeconds of
	"" ->
	    parse_request(Rest, R#request{lease_seconds=0});
	_ ->
	    try list_to_integer(LeaseSeconds) of
		Integer ->
		    parse_request(Rest, R#request{lease_seconds=Integer})
	    catch
		_:_ ->
		    {error, invalid_lease_seconds}
	    end
    end;
parse_request([{"hub.secret", Secret}|Rest], R) ->
    parse_request(Rest, R#request{secret=Secret});
parse_request([{"hub.verify_token", VerifyToken}|Rest], R) ->
    parse_request(Rest, R#request{verify_token=VerifyToken});
parse_request([{"hub.url", Url}|Rest], R) ->
    case http_uri:parse(Url) of
	{error, _} ->
	    {error, invalid_url};
	UrlParsed ->
	    Urls = lists:append(R#request.url, [UrlParsed]),
	    parse_request(Rest, R#request{url=Urls})
    end;
parse_request([{_,_}|Rest], R) ->
    parse_request(Rest, R).

parse_arguments_parse_mode_test() ->
    ?assert(parse_arguments([]) =:= {error, missing_mode}),
    ?assert(parse_arguments([{"hub.mode", "foo"}]) =:= {error, invalid_mode}),
    ?assert(parse_arguments([{"hub.mode", "publish"}]) =:= {error,
							    missing_url}),
    ?assert(parse_arguments([{"hub.mode", "subscribe"}]) =:= {error,
							      missing_topic}).
