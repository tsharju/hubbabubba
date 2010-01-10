-module(hub_verifier).
-author("teemu.harju@gmail.com").

-export([start/0, stop/0]).

-include("hub.hrl").

-define(VERIFY_INTERVAL, 5000).
-define(CHALLENGE_CHARS,
	"abcdefghijklmnopqrstuvxyzABCDEFHIJKLMNOPQRSTUVXYZ1234567890").
-define(CHALLENGE_LENGTH, 32).

start() ->
    register(verifier, spawn(fun verifier/0)).

stop() ->
    verifier ! stop.

verifier() ->
    receive
	stop ->
	    ok
    after
	?VERIFY_INTERVAL ->
	    io:format("Verifying unverified subscriptions.~n", []),
	    Subs = hub_storage:get_unverified_subscriptions(),
	    lists:map(fun verify/1, Subs),
	    verifier()
    end.

verify(Sub) ->
    io:format("Verifying ~p...~n", [Sub]),
    Sub1 = Sub#subscription{verifying=true},
    hub_storage:store_subscription(Sub1),
    verify_subscription(subscribe, Sub1).
    
verify_subscription(Mode, S = #subscription{callback=Callback,
					    topic=Topic,
					    lease_seconds=LeaseSeconds,
					    verify_token=VerifyToken}) ->
    Challenge = hub_utils:get_challenge(?CHALLENGE_CHARS, ?CHALLENGE_LENGTH),
    Qs = mochiweb_util:urlencode([{"hub.mode", Mode},
				  {"hub.topic",
				   hub_utils:url_to_string(Topic)},
				  {"hub.challenge", Challenge},
				  {"hub.lease_seconds", LeaseSeconds},
				  {"hub.verify_token", VerifyToken}]),
    Url = hub_utils:append_qs(Callback, Qs),
    case http:request(get,
		      {hub_utils:url_to_string(Url), []}, [], []) of
	{ok, {{_Version, 200, _ReasonPhrase}, _Headers, Body}} ->
	    case Body =:= Challenge of
		true ->
		    {ok, S#subscription{verified=true, verifying=false}};
		false ->
		    {failed, "Echoed challenge is not equal to the original."}
	    end;
	_ ->
	    {error, "Could not verify the subscription."}
    end.
