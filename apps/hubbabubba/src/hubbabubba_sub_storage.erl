-module(hubbabubba_sub_storage).

-behaviour(gen_server).

-define(SERVER, ?MODULE).

-include("hubbabubba.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% API
-export([start_link/0,
	 new_subscription/1,
	 get_unverified_subscriptions/0,
	 get_subscription/1]).

%% gen_server
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SUBSCRIPTIONS_TABLE, hubbabubba_subscriptions).

-record(state, {tid}).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

new_subscription(S) when is_record(S, subscription) ->
    gen_server:call(?SERVER, {new_subscription, S}).

get_subscription(Topic) ->
    gen_server:call(?SERVER, {get_subscription, Topic}).
    
get_unverified_subscriptions() ->
    gen_server:call(?SERVER, get_unverified_subscriptions).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(Args) ->
    Tid = ets:new(?SUBSCRIPTIONS_TABLE,
		  [bag, protected, {keypos, #subscription.topic}]),
    {ok, #state{tid=Tid}}.

handle_call({new_subscription, S}, _From, State) ->
    S1 = S#subscription{created=stamp(), active=true},
    true = ets:insert(State#state.tid, S1),
    {reply, {ok, S1}, State};

handle_call(get_unverified_subscriptions, _From, State) ->
    Match = ets:match_object(State#state.tid,
			     #subscription{active=true,
					   verified=false,
					   _='_'}),
    {reply, {ok, Match}, State};

handle_call({get_subscription, Topic}, _From, State) ->
    S = ets:lookup(State#state.tid, Topic),
    {reply, {ok, S}, State}.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, State) ->
    true = ets:delete(State#state.tid),
    ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

stamp() ->
    erlang:universaltime().

-ifdef(TEST).

%% ------------------------------------------------------------------
%% Tests
%% ------------------------------------------------------------------

setup() ->
    {ok, _Pid} = start_link(),
    Topic1 = "http://test.com/feed",
    Callback1 = "http://subscriber.com/callback",
    S1 = #subscription{topic=Topic1,
		      topic_parsed=http_uri:parse(Topic1),
		      callback=Callback1,
		      callback_parsed=http_uri:parse(Callback1)},
    Topic2 = "http://foo.com/feed",
    Callback2 = "http://polling.com/callback",
    S2 = #subscription{topic=Topic2,
		       topic_parsed=http_uri:parse(Topic2),
		       callback=Callback2,
		       callback_parsed=http_uri:parse(Callback2),
		       verified=true},
    {S1, S2}.

sub_storage_test() ->
    {S1, S2} = setup(),

    {ok, S3} = new_subscription(S1),
    {ok, _S4} = new_subscription(S2),
    ?assert(is_record(S3, subscription)),
    ?assert(S3#subscription.topic =:= S1#subscription.topic),
    ?assert(S3#subscription.callback =:= S1#subscription.callback),
    ?assert(S3#subscription.created =/= undefined),
    ?assert(S3#subscription.active =:= true),
    ?assert(S3#subscription.verified =:= false),

    {ok, Unverified} = get_unverified_subscriptions(),
    ?assert(Unverified =/= []),
    [S5] = Unverified,
    ?assert(is_record(S5, subscription)),
    ?assert(S5#subscription.topic =:= S1#subscription.topic),
    ?assert(S5#subscription.active =:= true),
    ?assert(S5#subscription.verified =:= false).

-endif.
