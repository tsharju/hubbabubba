-module(hub).
-author("teemu.harju@gmail.com").

%-compile(export_all).
-export([start/1, stop/0, loop/1]).

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
				subscribe
				when R#hub_request.verify =:= sync ->
				    case verify_subscription(R) of
					ok ->
					    Req:respond({200, [], []});
					_ ->
					    Req:respond({500, [], []})
				    end;
				subscribe
				when R#hub_request.verify =:= async ->
				    Req:respond({200, [], []});
				unsubscribe ->
				    Req:respond({200, [], []})
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
	    parse_request(Rest, R#hub_request{callback=TopicParsed})
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

verify_subscription(R = #hub_request{verify_token=VerifyToken}) ->
    case http:request(get, {R#hub_request.callback, []},
		      [], []) of
	{ok, {{_Version, 200, _ReasonPhrase}, _Headers, Body}} ->
	    io:format("~p~n", [Body]);
	_ ->
	    error
    end.

url_to_string({Scheme, [], Host, Port, Path, Query}) ->
    Url = atom_to_list(Scheme) ++ "://" ++ Host,
    case Port of
	80 when Scheme == http ->
	    Url2 = Url;
	443 when Scheme == https ->
	    Url2 = Url;
	_ ->
	    Url2 = Url ++ integer_to_list(Port)
    end,
    Url2 ++ Path ++ Query.
