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

start(Options) ->
    inets:start(), % using the inets http client api
    hub_storage:start(),
    hub_server:start_link(),
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
		    RequestArgs = Req:parse_post(),
		    case parse_mode(RequestArgs) of
			{error, Description} ->
			    Req:respond({400, [], Description});
			subscribe ->
			    case parse_request(RequestArgs, subscribe) of
				{error, Description} ->
				    Req:respond({400, [], Description});
				R ->
				    case R#hub_request.verify of
					sync ->
					    Rsp = hub_server:subscribe_sync(
						   R#hub_request.callback,
						    R#hub_request.topic),
					    case Rsp of
						ok ->
						    Req:respond({204, [], []});
						{error, Code, Reason} ->
						    Req:respond({Code, [], Reason})
					    end;
					async ->
					    hub_server:subscribe_async(),
					    Req:respond({202, [], []})
				    end
			    end;
			unsubscribe ->
			    Rsp = hub_server:unsubscribe("callback", "topic"),
			    case Rsp of
				ok ->
				    Req:respond({204, [], []});
				{error, {Code, Error}} ->
				    Req:respond({Code, [], Error})
			    end;
			publish ->
			    case parse_request(RequestArgs, publish) of
				{error, Description} ->
				    Req:respond({400, [], Description});
				R ->
				    case publish(R#hub_request.url) of
					ok ->
					    Req:respond({204, [], []});
					_ ->
					    Req:respond({500, [], []})
				    end
			    end;
			UnsupportedMode ->
			    Req:respond({400, [], "Error: hub.mode '" ++
					 atom_to_list(UnsupportedMode) ++
					 "' is not supported"})
		    end;
		_ ->
		    Req:not_found()
            end;
        _ ->
            Req:respond({501, [], []})
    end.

parse_mode([{"hub.mode", Mode}|_Rest]) ->
    list_to_atom(Mode);
parse_mode([_|Rest]) ->
    parse_mode(Rest);
parse_mode([]) ->
    {error, "Error: hub.mode is REQUIRED"}.

parse_request(Data, Mode) when is_atom(Mode) ->
    parse_request(Data, #hub_request{mode=Mode});
parse_request([], #hub_request{topic=undefined, mode=subscribe}) ->
    {error, "Error: hub.topic is REQUIRED"};
parse_request([], #hub_request{callback=undefined, mode=subscribe}) ->
    {error, "Error: hub.callback is REQUIRED"};
parse_request([], #hub_request{verify=undefined, mode=subscribe}) ->
    {error, "Error: hub.verify is REQUIRED"};
parse_request([], #hub_request{url=[], mode=publish}) ->
    {error, "Error: hub.url is REQUIRED"};
parse_request([], Request) ->
    Request;
parse_request([{"hub.callback", Callback}|Rest], R) ->
    case http_uri:parse(Callback) of
	{error, _} ->
	    {error, "Error: hub.callback '" ++ Callback ++
	     "' is not valid URL"};
	CallbackParsed ->
	    parse_request(Rest, R#hub_request{callback=CallbackParsed})
    end;
parse_request([{"hub.topic", Topic}|Rest], R) ->
    case http_uri:parse(Topic) of
	{error, _} ->
	    {error, "Error: hub.topic '" ++ Topic ++ "' is not valid URL"};
	TopicParsed ->
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
		LeaseSeconds1 ->
		    parse_request(Rest, R#hub_request{
					  lease_seconds=LeaseSeconds1})
	    catch
		_:_ ->
		    {error, "Error: hub.lease_seconds '" ++ LeaseSeconds ++
		     "' invalid value"}
	    end
    end;
parse_request([{"hub.secret", Secret}|Rest], R) ->
    parse_request(Rest, R#hub_request{secret=Secret});
parse_request([{"hub.verify_token", VerifyToken}|Rest], R) ->
    parse_request(Rest, R#hub_request{verify_token=VerifyToken});
parse_request([{"hub.url", Url}|Rest], R) ->
    case http_uri:parse(Url) of
	{error, _} ->
	    {error, "Error: hub.url '" ++ Url ++
	     "' is not valid URL"};
	UrlParsed ->
	    Urls = lists:append(R#hub_request.url, [UrlParsed]),
	    parse_request(Rest, R#hub_request{url=Urls})
    end;
parse_request([{_,_}|Rest], R) ->
    parse_request(Rest, R).

publish([Url|Rest]) ->
    % TODO: Implement the publish
    publish(Rest);
publish([]) ->
    error.

subscribe(R) ->
    S = #subscription{callback=R#hub_request.callback,
		      topic=R#hub_request.topic,
		      lease_seconds=R#hub_request.lease_seconds,
		      secret=R#hub_request.secret,
		      verify_token=R#hub_request.verify_token},
    subscribe(R#hub_request.verify, S).

subscribe(sync, S) ->
    case hub_verifier:verify_subscription(subscribe, S) of
	{ok, S1} ->
	    hub_storage:store_subscription(S1);
	{failed, Msg} ->
	    {bad_request, Msg};
	{error, Msg} ->
	    {error, Msg}
    end;
subscribe(async, S) ->
    % we only store the subscription and it's verified by a background process
    hub_storage:store_subscription(S).

parse_mode_test() ->
    ?assert(parse_mode([{"hub.mode", "publish"}]) =:= publish),
    ?assert(parse_mode([]) =:= {error, "Error: hub.mode is REQUIRED"}).

parse_publish_request_test() ->
    Args = [{"hub.url", "http://www.feed.org/feed.xml"}],
    Result = #hub_request{mode=publish,
			  url=[{http, [], "www.feed.org", 80,
				 "/feed.xml", []}]},
    ?assert(parse_request(Args, publish) =:= Result).

parse_subscribe_request_test() ->
    Args = [{"hub.topic", "http://www.feed.org/feed.xml"},
	    {"hub.callback", "http://www.service.org/callback"},
	    {"hub.verify", "async"}],
    Result = #hub_request{mode=subscribe,
			  topic={http, [], "www.feed.org", 80,
				 "/feed.xml", []},
			  callback={http, [], "www.service.org", 80,
				    "/callback", []},
			  verify=async,
			  lease_seconds=0},
    ?assert(parse_request(Args, subscribe) =:= Result).
