-module(hubbabubba).
-author("teemu.harju@gmail.com").

-export([start/0, stop/0]).

ensure_started(App) ->    
    case application:start(App) of
        ok ->
            ok;
	{error, {already_started, App}} ->
            ok
    end.

start() ->
    ensure_started(crypto),
    application:start(hubbabubba).

stop() ->
    Res = application:stop(hubbabubba),
    application:stop(crypto),
    Res.
