%%%===================================================================
%%% @doc THIS IS A DRAFT
%%%
%%% This module implement the support of nip05 on the server side. By
%%% default, it should return all active users using the current
%%% session.
%%%
%%% https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Access-Control-Allow-Origin
%%%
%%% @todo add filter per users (some of them will probably don't allow
%%%       to be in this feature)
%%% @todo add filter per domains
%%% @todo add support for relays per users.
%%% @todo ensure CORS is working
%%% @todo data are hardcoded, needs to find a way to correctly update
%%%       those information
%%% @end
%%%===================================================================
-module(nostr_relay_nip05_handler).
-export([init/2]).
-define(HEADERS, #{ <<"content-type">> => <<"application/json">>
                  , <<"Access-Control-Allow-Origin">> => <<"*">>
                  }).

%%--------------------------------------------------------------------
%% @hidden
%% @doc internal.
%% @end
%%--------------------------------------------------------------------
-spec init(Req, State) -> Return when
      Req :: cowboy_req:req(),
      State :: any(),
      Return :: {ok, Req, State}.

init(Req0=#{ method := <<"HEAD">> }, State) ->
    Req1 = cowboy_req:reply(200, ?HEADERS, <<>>, Req0),
    {ok, Req1, State};
init(Req0=#{method := <<"GET">>}, State) ->
    Query = cowboy_req:parse_qs(Req0),
    case proplists:get_value(<<"name">>, Query, undefined) of
        undefined -> all(Req0, State);
        <<>> -> all(Req0, State);
        Name -> lookup(Name, Req0, State)
    end.

%%--------------------------------------------------------------------
%% @hidden
%% @doc internal.
%% returns the list of all active users on this node.
%% @end
%%--------------------------------------------------------------------
all(Req0, State) ->
    {ok, All} = list_all(),
    Response = thoas:encode(#{ <<"names">> => All }),
    Req1 = cowboy_req:reply(200, ?HEADERS, Response, Req0),
    {ok, Req1, State}.

%%--------------------------------------------------------------------
%% @hidden
%% @doc internal.
%% @todo fix this function, it has been created for test.
%% @end
%%--------------------------------------------------------------------
list_all() ->
    All = #{ <<"toriko">> => <<"151e6b8357313e5cee561225b5dd08a76f58924a24ac10781aebab4ed36ac655">>,
             <<"zero">> => <<"0000000000000000000000000000000000000000000000000000000000000000">>,
             <<"one">> => <<"0000000000000000000000000000000000000000000000000000000000000001">> 
           },
    {ok, All}.

%%--------------------------------------------------------------------
%% @hidden
%% @doc internal.
%% @end
%%--------------------------------------------------------------------
lookup(Name, Req0, State) ->
    case lookup_public_key(Name) of
        {ok, PublicKey} -> found(Name, PublicKey, Req0, State);
        _ -> not_found(Req0, State)
    end.

%%--------------------------------------------------------------------
%% @hidden
%% @doc internal.
%% @end
%%--------------------------------------------------------------------
found(Name, PublicKey, Req0, State) ->
    Response = thoas:encode(#{ <<"names">> => #{ Name => PublicKey }}),
    Req1 = cowboy_req:reply(200, ?HEADERS, Response, Req0),
    {ok, Req1, State}.

%%--------------------------------------------------------------------
%% @hidden
%% @doc internal.
%% @end
%%--------------------------------------------------------------------
not_found(Req0, State) ->    
    Response = thoas:encode(#{ <<"names">> => #{} }),
    Req1 = cowboy_req:reply(200, ?HEADERS, Response, Req0),
    {ok, Req1, State}.

%%--------------------------------------------------------------------
%% @hidden
%% @doc internal.
%% Search if an user is present and returns its public key.
%% @end
%%--------------------------------------------------------------------
% @todo to fix this pattern, this is just a test
lookup_public_key(<<"toriko">>) ->
    {ok, <<"151e6b8357313e5cee561225b5dd08a76f58924a24ac10781aebab4ed36ac655">>};
lookup_public_key(_Name) ->
    {error, notfound}.
    
    
