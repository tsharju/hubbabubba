-module(hub).
-author("teemu.harju@gmail.com").

-compile(export_all).
%-export([start/1, stop/0, loop/1]).

-include("hub.hrl").

%-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
%-endif.

-define(LOOP, {?MODULE, loop}).
-define(INDEX_PAGE,
	"<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Transitional//EN\" " ++
	"\"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd\">" ++
	"<html xmlns=\"http://www.w3.org/1999/xhtml\">" ++
	"<head>" ++
	"<title>Hubbabubba - PubSubHubbub Hub</title>" ++
	"</head>" ++
	"<body>" ++
	"<p>Hubbabubba server</p>"
	"</body>" ++
	"</html>").
-define(HTTP_TIMEOUT, 3).
-define(CHALLENGE_CHARS,
	"abcdefghijklmnopqrstuvxyzABCDEFHIJKLMNOPQRSTUVXYZ1234567890").
-define(CHALLENGE_LENGTH, 32).

start(Options) ->
    inets:start(), % using the inets http client api
    hub_storage:start(),
    mochiweb_http:start([{name, ?MODULE}, {loop, ?LOOP} | Options]).

stop() ->
    mochiweb_http:stop(?MODULE),
    hub_storage:stop().

loop(Req) ->
    "/" ++ Path = Req:get(path),
    case Req:get(method) of
        Method when Method =:= 'GET'->
            case Path of
                _ ->
                    Req:respond({200,
				 [{"Content-Type",
				   "application/xhtml+xml"}], ?INDEX_PAGE})
            end;
        'POST' ->
            case Path of
		"" ->
		    case parse_request(Req:parse_post()) of
			{error, Description} ->
			    Req:respond({400, [], Description});
			R ->
			    case R#hub_request.mode of
				subscribe ->
				    case subscribe(R) of
					ok ->
					    Req:respond({204, [], []});
					accepted ->
					    Req:respond({202, [], []});
					{bad_request, Msg} ->
					    Req:respond({400, [], Msg});
					{error, Msg} ->
					    Req:respond({500, [], Msg})
				    end;
				unsubscribe ->
				    Req:respond({501, [], []})
			    end
		    end;
		_ ->
		    Req:not_found()
            end;
        _ ->
            Req:respond({501, [], []})
    end.

parse_request(Data) ->
    parse_request(Data, #hub_request{}).

parse_request([], #hub_request{topic=undefined}) ->
    {error, "Error: hub.topic is REQUIRED"};
parse_request([], #hub_request{mode=undefined}) ->
    {error, "Error: hub.topic is REQUIRED"};
parse_request([], #hub_request{callback=undefined}) ->
    {error, "Error: hub.topic is REQUIRED"};
parse_request([], #hub_request{verify=undefined}) ->
    {error, "Error: hub.topic is REQUIRED"};
parse_request([], Request) ->
    Request;
parse_request([{"hub.callback", Callback}|Rest], R) ->
    CallbackParsed = http_uri:parse(Callback),
    case CallbackParsed of
	{error, _} ->
	    {error, "Error: hub.callback '" ++ Callback ++
	     "' is not valid URL"};
	_ ->
	    parse_request(Rest, R#hub_request{callback=CallbackParsed})
    end;
parse_request([{"hub.mode", Mode}|Rest], R) ->
    case Mode of
	"subscribe" ->
	    parse_request(Rest, R#hub_request{mode=subscribe});
	"unsubscribe" ->
	    parse_request(Rest, R#hub_request{mode=unsubscribe});
	_ ->
	    {error, "Error: hub.mode '" ++ Mode ++ "' not supported"}
    end;
parse_request([{"hub.topic", Topic}|Rest], R) ->
    TopicParsed = http_uri:parse(Topic),
    case TopicParsed of
	{error, _} ->
	    {error, "Error: hub.topic '" ++ Topic ++ "' is not valid URL"};
	_ ->
	    parse_request(Rest, R#hub_request{topic=TopicParsed})
    end;
parse_request([{"hub.verify", Verify}|Rest], R) ->
    case Verify of
	"sync" ->
	    parse_request(Rest, R#hub_request{verify=sync});
	"async" ->
	    parse_request(Rest, R#hub_request{verify=async});
	_ ->
	    {error, "Error: hub.verify '" ++ Verify ++ "' not supported"}
    end;
parse_request([{"hub.lease_seconds", LeaseSeconds}|Rest], R) ->
    case LeaseSeconds of
	"" ->
	    parse_request(Rest, R#hub_request{lease_seconds=0});
	_ ->
	    try list_to_integer(LeaseSeconds) of
		_ ->
		    parse_request(
		      Rest,
		      R#hub_request{
			lease_seconds=list_to_integer(LeaseSeconds)})
	    catch
		_:_ ->
		    {error, "Error: hub.lease_seconds '" ++ LeaseSeconds ++
		     "' invalid value"}
	    end
    end;
parse_request([{"hub.secret", Secret}|Rest], R) ->
    parse_request(Rest, R#hub_request{secret=Secret});
parse_request([{"hub.verify_token", VerifyToken}|Rest], R) ->
    parse_request(Rest, R#hub_request{verify_token=VerifyToken}).

verify_subscription(Mode, S = #subscription{callback=Callback,
					    topic=Topic,
					    lease_seconds=LeaseSeconds,
					    verify_token=VerifyToken}) ->
    Challenge = get_challenge(),
    Qs = mochiweb_util:urlencode([{"hub.mode", Mode},
				  {"hub.topic", url_to_string(Topic)},
				  {"hub.challenge", Challenge},
				  {"hub.lease_seconds", LeaseSeconds},
				  {"hub.verify_token", VerifyToken}]),
    Url = append_qs(Callback, Qs),
    case http:request(get,
		      {url_to_string(Url), []}, [], []) of
	{ok, {{_Version, 200, _ReasonPhrase}, _Headers, Body}} ->
	    case Body =:= Challenge of
		true ->
		    {ok, S#subscription{verified=true}};
		false ->
		    {failed, "Echoed challenge is not equal to the original."}
	    end;
	_ ->
	    {error, "Could not verify the subscription."}
    end.

%%-----------------------------------------------------------------------------
%% Function: get_challenge/0
%% Purpose: Get a rangom string that can be used as a value for the challenge
%%          when verifying subscriptions.
%% Args: None.
%% Returns: A string.
%%-----------------------------------------------------------------------------
get_challenge() ->
    lists:foldl(fun(_, Acc) ->
			[lists:nth(random:uniform(length(?CHALLENGE_CHARS)),
				   ?CHALLENGE_CHARS)]
			    ++ Acc
		end, [], lists:seq(1, ?CHALLENGE_LENGTH)).

%%-----------------------------------------------------------------------------
%% Function: append_qs/2
%% Purpose: Appends a query string to url that is a result of http:uri_parse.
%% Args: The URL (tuple) and the query string (string).
%% Returns: A string.
%%-----------------------------------------------------------------------------
append_qs({Scheme, [], Host, Port, Path, Query}, Qs) ->
    case Query of
	[] ->
	    {Scheme, [], Host, Port, Path, "?" ++ Qs};
	_ ->
	    {Scheme, [], Host, Port, Path, Query ++ "&" ++ Qs}
    end.

url_to_string({Scheme, [], Host, Port, Path, Query}) ->
    Url = atom_to_list(Scheme) ++ "://" ++ Host,
    case Port of
	80 when Scheme == http ->
	    Url2 = Url;
	443 when Scheme == https ->
	    Url2 = Url;
	_ ->
	    Url2 = Url ++ ":" ++ integer_to_list(Port)
    end,
    Url2 ++ Path ++ Query.

url_to_string_test() ->
    Url = "http://www.foobar.com/foobar.html",
    UrlParsed = http_uri:parse(Url),
    ?assert(url_to_string(UrlParsed) =:= Url),
    Url2 = "http://www.foobar.com:8080/foobar.html",
    UrlParsed2 = http_uri:parse(Url2),
    ?assert(url_to_string(UrlParsed2) =:= Url2),
    Url3 = "http://www.foobar.com/foobar.html?foo=bar",
    UrlParsed3 = http_uri:parse(Url3),
    ?assert(url_to_string(UrlParsed3) =:= Url3),
    Url4 = "https://www.foobar.com/foobar.html?foo=bar",
    UrlParsed4 = http_uri:parse(Url4),
    ?assert(url_to_string(UrlParsed4) =:= Url4).


subscribe(R) ->
    S = #subscription{callback=R#hub_request.callback,
		      topic=R#hub_request.topic,
		      lease_seconds=R#hub_request.lease_seconds,
		      secret=R#hub_request.secret,
		      verify_token=R#hub_request.verify_token},
    subscribe(R#hub_request.verify, S).

subscribe(sync, S) ->
    case verify_subscription(subscribe, S) of
	{ok, S1} ->
	    hub_storage:add_subscription(S1);
	{failed, Msg} ->
	    {bad_request, Msg};
	{error, Msg} ->
	    {error, Msg}
    end;
subscribe(async, S) ->
    hub_storage:add_subscription(S).
