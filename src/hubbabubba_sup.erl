-module(hubbabubba_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    WebConfig = hubbabubba_web:config(),
    Web = {webmachine_mochiweb,
	   {webmachine_mochiweb, start, [WebConfig]},
	   permanent, 5000, worker, dynamic},
    Server = {hubbabubba_server,
	      {hubbabubba_server, start_link, []},
	      permanent, 5000, worker, dynamic},
    Processes = [Web, Server],
    {ok, { {one_for_one, 5, 10}, Processes} }.
