-module(hubbabubba_web).

%% API
-export([config/0, index/0]).

%% ===================================================================
%% API
%% ===================================================================

config() ->
    [{ip, "127.0.0.1"},
     {port, 8080},
     {dispatch, get_url_mappings()}].

index() ->
    Version = hubbabubba_utils:get_version(),
    "
<html>
    <head>
        <title>Hubbabubba v." ++ Version ++ "</title>
    </head>
    <body>
        <h1>Hubbabubba v" ++ Version ++ "</h1>
        <p>Read more about PubSubHubbub
        <a href=\"http://pubsubhubbub.googlecode.com\">here</a>.</p>
        <p>
            <form method=\"post\" action=\"/\">
                <input type=\"text\" name=\"hub.topic\" />
                <input type=\"text\" name=\"hub.callback\" />
                <input type=\"submit\" />
            </form>
        </p>
    </body>
</html>".

%% ===================================================================
%% Internal API
%% ===================================================================

get_url_mappings() ->
    [{[], hubbabubba_resource, []},
     {["test", "callback"], hubbabubba_callback_resource, []}].
