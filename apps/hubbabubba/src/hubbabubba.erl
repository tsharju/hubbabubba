-module(hubbabubba).

%% API
-export([start/0, stop/0]).

%% ===================================================================
%% API
%% ===================================================================

start() ->
    ok = application:load(hubbabubba),
    ok = start_deps(),
    application:start(hubbabubba).

stop() ->
    application:stop(hubbabubba).

%% ===================================================================
%% Internal API
%% ===================================================================

start_deps() ->
    {ok, Deps} = application:get_key(hubbabubba, applications),
    [ensure_started(App) || App <- Deps],
    ok.

ensure_started(App) ->
    case application:start(App) of
	ok ->
	    ok;
	{error, {already_started, App}} ->
	    ok
    end.
