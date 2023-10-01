%%%===================================================================
%%% @author Mathieu Kerjouan <contact at erlang-punch.com>
%%% @doc THIS FILE IS A DRAFT.
%%% get rid of the annoying list of certificates. Now they will
%%% be stored in mnesia.
%%% @end
%%%===================================================================
-module('nostr@certificates').
-export([create_table/0, load/0, get/0]).
-include_lib("kernel/include/logger.hrl").
-record(?MODULE, { id = <<>>
                 , certificate = undefined
                 }).

%%-------------------------------------------------------------------
%% @doc create table and load certificates.
%% @end
%%-------------------------------------------------------------------
-spec create_table() -> any().

create_table() ->
    mnesia:create_table(?MODULE, [{attributes, record_info(fields, ?MODULE)}]),
    load().

%%-------------------------------------------------------------------
%% @doc load certificates manually.
%% @end
%%-------------------------------------------------------------------
-spec load() -> any().
load() -> mnesia:transaction(fun create_certificates/0).

%%--------------------------------------------------------------------
%% @doc get all certificates stored in mnesia.
%% @end
%%--------------------------------------------------------------------
-spec get() -> any().

get() ->
    case mnesia:transaction(fun get_certificates/0) of
        {atomic, Certificates} ->
            [ {cert, Id, Cert} || {?MODULE, Id, Cert} <- Certificates ];
        _ ->
            []
    end.

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
get_certificates() ->
    mnesia:select(?MODULE, [{'$1', [], ['$1']}]).    

%%-------------------------------------------------------------------
%%
%%-------------------------------------------------------------------
create_certificates() ->
    [ mnesia:write(#?MODULE{ id = Id, certificate = Cert }) ||
        {cert, Id, Cert} <- public_key:cacerts_get() ].
