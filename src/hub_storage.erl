-module(hub_storage).
-author("teemu.harju@gmail.com").

-include("hub.hrl").
-include_lib("stdlib/include/qlc.hrl").

-export([start/0, stop/0]).

start() ->
    mnesia:create_schema([node()]),
    mnesia:start(),
    
    mnesia:create_table(subscription,
			[{attributes, record_info(fields, subscription)}]).

stop() ->
    mnesia:stop().
