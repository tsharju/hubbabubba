-module(hubbabubba_server).

-behaviour(gen_server).

-define(SERVER, ?MODULE).

-define(CHALLENGE_CHARS,
	"abcdefghijklmnopqrstuvxyzABCDEFHIJKLMNOPQRSTUVXYZ1234567890").
-define(CHALLENGE_LENGTH, 32).

%% API
-export([start_link/0, subscribe/3, generate_challenge/2]).

%% gen_server exports
-export([init/1, handle_call/3, handle_cast/2,
	 handle_info/2, terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API
%% ------------------------------------------------------------------

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

subscribe(async, CallbackURL, TopicURL) ->
    gen_server:cast(?SERVER, {subscribe, CallbackURL, TopicURL});
subscribe(sync, CallbackURL, TopicURL) ->
    gen_server:call(?SERVER, {subscribe, CallbackURL, TopicURL}).

%% ------------------------------------------------------------------
%% gen_server methods
%% ------------------------------------------------------------------

init(Args) ->
    {ok, Args}.

handle_call({subscribe, CallbackURL, TopicURL}, _From, State) ->
    Reply = verify_subscription(CallbackURL, TopicURL),
    {reply, Reply, State}.

handle_cast({subscribe, CallbackURL, _TopicURL}, State) ->
    {ok, _Status, _Headers, _Body} = ibrowse:send_req(CallbackURL, [], get),
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

verify_subscription(CallbackURL, TopicURL) ->
    Args = [{"hub.mode", "subscribe"},
	    {"hub.topic", TopicURL},
	    {"hub.challenge", generate_challenge(?CHALLENGE_LENGTH,
						 ?CHALLENGE_CHARS)}],
    VerifyURL = CallbackURL ++ "?" ++ mochiweb_util:urlencode(Args),
    {ok, Status, _Headers, _Body} = ibrowse:send_req(VerifyURL, [], get),
    {ok, Status}.

generate_challenge(0, _Chars) ->
    [];
generate_challenge(Len, Chars) ->
    [random_char(Chars)|generate_challenge(Len-1, Chars)].

random_char(Chars) ->
    lists:nth(random:uniform(length(Chars)), Chars).
