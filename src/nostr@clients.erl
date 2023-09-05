%%%===================================================================
%%% @author Mathieu Kerjouan <contact at erlang-punch.com>
%%% @doc THIS FILE IS A DRAFT.
%%% @endf
%%%===================================================================
-module('nostr@clients').
-export([create_table/0]).
-export([create_client/1]).
-export([get_client/1]).
-export([delete_client/1]).
-export([list_clients/0]).

-record(?MODULE, { target = "" :: string()
                 , connection = undefined :: pid()
                 , created_at = erlang:system_time() :: integer()
                 , state = undefined :: any()
                 , users = 0 :: integer()
                 }).

-spec create_table() -> any().
create_table() ->
    Opts = [{attributes, record_info(fields, ?MODULE)}],
    mnesia:create_table(?MODULE, Opts).

-spec create_client(map()) -> {atomik, ok}.
create_client(#{ target := Target }) ->
    Transaction = fun() -> mnesia:write(#?MODULE{ target = Target })
                  end,
    mnesia:transaction(Transaction).

-spec get_client(map()) -> {atomic, any()}.
get_client(#{ target := Target }) ->
    Transaction = fun() ->
                          mnesia:match_object(?MODULE, #?MODULE{ target = Target, _ = '_' }, read)
                  end,
    mnesia:transaction(Transaction).

-spec list_clients() -> {atomic, list()}.
list_clients() ->
    Transaction = fun() ->
                         mnesia:select(?MODULE, [{'$1', [], ['$1']}])
                 end,
    mnesia:transaction(Transaction).
                               
-spec delete_client(map()) -> {atomic, ok}.
delete_client(#{ target := Target }) ->
    Transaction = fun() ->
                          mnesia:delete(?MODULE, Target, write)
                  end,
    mnesia:transaction(Transaction).

    
