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
-export([list_clients/0, list_clients_by_host/1]).

-record(?MODULE, { target = "" :: {string(), pos_integer(), tls, string()}
                 , controller = undefined :: pid()
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
create_client(#{ target := Target 
               , controller := Controller
               , connection := Connection }) ->
    Transaction = fun() -> mnesia:write(#?MODULE{ target = Target
                                                , connection = Connection 
                                                , controller = Controller
                                                })
                  end,
    mnesia:transaction(Transaction).

-spec get_client(map()) -> {atomic, any()}.
get_client(#{ target := Target }) ->
    Transaction = fun() ->
                          mnesia:match_object(?MODULE, #?MODULE{ target = Target, _ = '_' }, read)
                  end,
    mnesia:transaction(Transaction).

% @todo: this code is quite dirty, cleanup needed!
-spec list_clients_by_host(string()) -> {atomic, any()}.
list_clients_by_host(Host) ->
    Filter = fun(Client) -> 
                     case Client of 
                         #?MODULE{ target = {Host,_,_,_} } -> true;
                         _ -> false
                     end
             end,
    Transaction = fun() ->
                          case mnesia:select(?MODULE, [{'$1', [], ['$1']}]) of
                              [] ->
                                  [];
                              List ->
                                  lists:filter(Filter, List)
                          end
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
