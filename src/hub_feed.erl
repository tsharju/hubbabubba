-module(hub_feed).
-author("teemu.harju@gmail.com").

-include_lib("xmerl/include/xmerl.hrl").
-include("hub_feed.hrl").

-define(TEST_FEED_PATH, "../test/atom_wordpress.xml").

-define(NS_ATOM, "http://www.w3.org/2005/Atom").

-export([parse/1]).

parse(Data) ->
    case Data of
	test ->
	    {RootEl, _} = xmerl_scan:file(?TEST_FEED_PATH);
	_ ->
	    {RootEl, _} = xmerl_scan:string(Data)
    end,
    extract(RootEl).

extract(Elem) when is_record(Elem, xmlElement)->
    case Elem#xmlElement.name of
	feed ->
	    extract_atom(Elem#xmlElement.content);
	rss ->
	    rss
    end.

extract_atom(Elements) ->
    extract_atom(Elements, undefined, []).
extract_atom([Elem|Rest], Id, Items) when is_record(Elem, xmlElement) ->
    case Elem#xmlElement.name of
	id ->
	    [IdText] = Elem#xmlElement.content,
	    Id1 = IdText#xmlText.value,
	    extract_atom(Rest, Id1, Items);
	entry ->
	    case extract_atom_entry(Elem#xmlElement.content) of
		Entry when is_record(Entry, feed_item) ->
		    Items1 = lists:append(Items, [Entry]),
		    extract_atom(Rest, Id, Items1);
		_ ->
		    extract_atom(Rest, Id, Items)
	    end;
	_ ->
	    extract_atom(Rest, Id, Items)
    end;
extract_atom([_Elem|Rest], Id, Items) ->
    extract_atom(Rest, Id, Items);
extract_atom([], Id, Items) ->
    {Id, Items}.

extract_atom_entry(Entry) ->
    XmlData = lists:flatten(xmerl:export_simple_content(Entry, xmerl_xml)),
    try erlang:md5(XmlData) of
	Hash ->
	    extract_atom_entry(Entry, #feed_item{content_hash=Hash})
    catch
	error:_Error ->
	    error_logger:error_msg("Failed hashing entry: ~p~n", [XmlData])
    end.
extract_atom_entry([Elem|Rest], Entry) when is_record(Elem, xmlElement) ->
    case Elem#xmlElement.name of
	id ->
	    [IdText] = Elem#xmlElement.content,
	    Id = IdText#xmlText.value,
	    extract_atom_entry(Rest, Entry#feed_item{id=Id});
	_ ->
	    extract_atom_entry(Rest, Entry)
    end;
extract_atom_entry([_Elem|Rest], Entry) ->
    extract_atom_entry(Rest, Entry);
extract_atom_entry([], Entry) ->
    Entry.
