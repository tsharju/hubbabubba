-module(hubbabubba_utils).

%% API
-export([get_version/0]).

%% ===================================================================
%% API
%% ===================================================================

get_version() ->
    {ok, Version} = application:get_key(hubbabubba, vsn),
    Version.

