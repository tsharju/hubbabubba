-module(hub_app).
-author("teemu.harju@gmail.com").

-behaviour(application).
-export([start/2,stop/1]).

start(_Type, _StartArgs) ->    
hub_sup:start_link().

stop(_State) ->
    ok.
