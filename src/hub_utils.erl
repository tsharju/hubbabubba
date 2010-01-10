-module(hub_utils).
-author("teemu.harju@gmail.com").

-export([get_challenge/2, append_qs/2, url_to_string/1]).

%%-----------------------------------------------------------------------------
%% Function: get_challenge/2
%% Purpose: Get a rangom string that can be used as a value for the challenge
%%          when verifying subscriptions.
%% Args: Chars (string) of charachters used in the challenge and Length (int)
%%       of the challenge generated.
%% Returns: A string.
%%-----------------------------------------------------------------------------
get_challenge(Chars, Length) ->
    lists:foldl(fun(_, Acc) ->
			[lists:nth(random:uniform(length(Chars)),
				   Chars)]
			    ++ Acc
		end, [], lists:seq(1, Length)).

%%-----------------------------------------------------------------------------
%% Function: append_qs/2
%% Purpose: Appends a query string to url that is a result of http:uri_parse.
%% Args: The URL (tuple) and the query string (string).
%% Returns: A string.
%%-----------------------------------------------------------------------------
append_qs({Scheme, [], Host, Port, Path, Query}, Qs) ->
    case Query of
	[] ->
	    {Scheme, [], Host, Port, Path, "?" ++ Qs};
	_ ->
	    {Scheme, [], Host, Port, Path, Query ++ "&" ++ Qs}
    end.

%%-----------------------------------------------------------------------------
%% Function: url_to_string/1
%% Purpose: Coverts the URL tuple to a string.
%% Args: The URL (tuple).
%% Returns: A string.
%%-----------------------------------------------------------------------------
url_to_string({Scheme, [], Host, Port, Path, Query}) ->
    Url = atom_to_list(Scheme) ++ "://" ++ Host,
    case Port of
	80 when Scheme == http ->
	    Url2 = Url;
	443 when Scheme == https ->
	    Url2 = Url;
	_ ->
	    Url2 = Url ++ ":" ++ integer_to_list(Port)
    end,
    Url2 ++ Path ++ Query.

