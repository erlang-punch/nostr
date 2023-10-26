%%%===================================================================
%%% @doc
%%% @end
%%%===================================================================
-module(nostrlib_url).
-export([check/1]).
-export([check_hostname/1]).
-include_lib("eunit/include/eunit.hrl").

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
-spec test() -> any().

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
-spec check(Url) -> Return when
      Url :: iodata(),
      Return :: {ok, Url}
              | {error, proplists:proplists()}.

check(Url) -> 
    CheckHost = fun(Hostname, VUrl) ->
                        case check_hostname(Hostname) of
                            {ok, Hostname} ->
                                {ok, VUrl};
                            Elsewise -> 
                                Elsewise
                        end
                end,
    case uri_string:parse(Url) of
        #{host := Host, path := <<>>, scheme := <<"ws">>} ->
            CheckHost(Host, Url);
        #{host := Host, path := <<>>, scheme := <<"wss">>} ->
            CheckHost(Host, Url);
        #{scheme := Scheme, path := <<>>} -> 
            {error, [{scheme, Scheme}]};
        #{path := Path} -> 
            {error, [{path, Path}]}
    end.


-spec check_test() -> any().
check_test() ->
    [?assertEqual({ok, <<"wss://rsslay.fiatjaf.com">>}
                 , check(<<"wss://rsslay.fiatjaf.com">>))
    ,?assertEqual({ok, <<"wss://somerelay.com">>}
                 , check(<<"wss://somerelay.com">>))
    ,?assertEqual({ok, <<"ws://somerelay.com">>}
                 , check(<<"ws://somerelay.com">>))
    ,?assertEqual({ok, <<"wss://d.a.c.are.somerelay.com">>}
                 , check(<<"wss://d.a.c.are.somerelay.com">>))
    ,?assertEqual({error, [{path, <<"/test">>}]}
                 , check(<<"wss://somerelay.com/test">>))
    ,?assertEqual({error, [{scheme, <<"https">>}]}
                 ,check(<<"https://httprelay.com">>))
    ,?assertEqual({error, [{hostname, <<"httprelay_!.com">>}]}
                 ,check(<<"wss://httprelay_!.com">>))
    ].
     
%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
-spec check_hostname() -> iodata().
check_hostname() ->
    Pattern = <<"(?=^.{4,253}$)(^((?!-)[a-zA-Z0-9-]{0,62}[a-zA-Z0-9]\.)+[a-zA-Z]{2,63}$)">>,
    {ok, Regex} = re:compile(Pattern),
    Regex.

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
-spec check_hostname(Hostname) -> Return when
      Hostname :: bitstring(),
      Return :: {ok, Hostname}
              | {error, proplists:proplists()}.
check_hostname(Hostname) ->
    Regex = check_hostname(),
    case re:run(Hostname, Regex) of
        {match, _} ->
            {ok, Hostname};
        _ ->
            {error, [{hostname, Hostname}]}
    end.
    

