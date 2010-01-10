-module(hub_storage).
-author("teemu.harju@gmail.com").

-include("hub.hrl").
-include_lib("stdlib/include/qlc.hrl").

-export([start/0, stop/0, store_subscription/1,
	 get_unverified_subscriptions/0, get_subscriptions/0]).

start() ->
    mnesia:create_schema([node()]),
    mnesia:start(),
    
    mnesia:create_table(subscription,
			[{disc_copies, [node()]},
			 {attributes, record_info(fields, subscription)}]).

stop() ->
    mnesia:stop().

add(Row) ->
    F = fun() ->
		mnesia:write(Row)
	end,
    mnesia:transaction(F).

store_subscription(S) ->
    case add(S) of
	{atomic, ok} ->
	    case S#subscription.verified of
		true ->
		    ok;
		false ->
		    accepted
	    end;
	{atomic, _} ->
	    {error, "Could not store subscription."};
	{aborted, _Reason} ->
	    {error, "Could not store subscription."}
    end.

get_unverified_subscriptions() ->
    do(qlc:q([X || X <- mnesia:table(subscription),
		   X#subscription.verified =:= false,
		   X#subscription.verifying =:= false])).

get_subscriptions() ->
    do(qlc:q([X || X <- mnesia:table(subscription)])).

do(Q) ->
    F = fun() ->
		qlc:e(Q)
	end,
    case mnesia:transaction(F) of
	{atomic, Val} ->
	    Val;
	{aborted, {no_exists, _}} ->
	    []
    end.
