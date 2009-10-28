-module(hub).
-author("teemu.harju@gmail.com").

-export([start/1, loop/1]).

-define(LOOP, {?MODULE, loop}).
-define(INDEX_PAGE,
	"<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Transitional//EN\" " ++
	"\"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd\">" ++
	"<html xmlns=\"http://www.w3.org/1999/xhtml\">" ++
	"<head>" ++
	"<title>Hubbabubba - PubSubHubbub Hub</title>" ++
	"</head>" ++
	"<body>" ++
	"<p>Hubbabubba</p>"
	"</body>" ++
	"</html>").
-define(CONTENT_TYPE, "application/x-www-form-urlencoded").

start(Options = [{port, _Port}]) ->
    mochiweb_http:start([{name, ?MODULE}, {loop, ?LOOP} | Options]).

loop(Req) ->
    "/" ++ Path = Req:get(path),
    ContentType = Req:get_header_value("Content-Type"),
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
		"" when ContentType =:= ?CONTENT_TYPE ->
		    Data = dict:from_list(Req:parse_post()),
		    case dict:find("hub.mode", Data) of
			{ok, Mode} ->
			    case Mode of
				"subscribe" ->
				    Req:respond({200, [], []});
				"unsubscribe" ->
				    Req:respond({200, [], []});
				_ ->
				    Req:respond({400, [], []})
			    end;
			error ->
			    Req:respond({400, [], []})
		    end;
		"" when ContentType =/= ?CONTENT_TYPE ->
		    Req:respond({415, [], []});
                _ ->
                    Req:not_found()
            end;
        _ ->
            Req:respond({501, [], []})
    end.
