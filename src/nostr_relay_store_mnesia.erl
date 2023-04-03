%%%===================================================================
%%% @doc handler module to deal with Mnesia database.
%%% @end
%%%===================================================================
-module(nostr_relay_store_mnesia).
-export([init/1, terminate/1, insert/1, lookup/1, delete/1, select/3]).
-include("nostrlib.hrl").

%%--------------------------------------------------------------------
%% @doc initialize the database.
%% @end
%%--------------------------------------------------------------------
-spec init(Args) -> Return when
      Args :: proplists:proplists(),
      Return :: ok | {error, any()}.
init(_Args) ->
    mnesia:start(),
    mnesia:create_table(event, [{attributes, record_info(fields, event)}]),
    mnesia:create_table(subscription, [{attributes, record_info(fields, subscription)}]),
    ok.

%%--------------------------------------------------------------------
%% @doc terminate the database (drop everything).
%% @end
%%--------------------------------------------------------------------
-spec terminate(Args) -> Return when
      Args :: proplists:proplists(),
      Return :: ok | {error, any()}.
terminate(_Args) ->
    mnesia:delete_table(event),
    mnesia:delete_table(subscription).

%%--------------------------------------------------------------------
%% @doc insert a new element into the database.
%% @end
%%--------------------------------------------------------------------
-spec insert(Entry) -> Return when
      Entry :: term(),
      Return :: ok | {ok, any()} | {error, any()}.
insert(#event{} = Event) ->
    Fun = fun() -> mnesia:write(Event) end,
    mnesia:transaction(Fun).

%%--------------------------------------------------------------------
%% @doc lookup for an entry in the database.
%% @end
%%--------------------------------------------------------------------
-spec lookup(Entry) -> Return when
      Entry :: term(),
      Return :: {ok, any()} | {error, any()}.
lookup(#event{} = Event) ->
    Fun = fun() ->
                  Guard = {'=:=', '$1', {const, Event}},
                  mnesia:select(event, [{'$1',[Guard],['$1']}])
          end,
    mnesia:transaction(Fun).


%%--------------------------------------------------------------------
%% @doc 
%% @end
%%--------------------------------------------------------------------
-spec select(Entry, Guards, Result) -> Return when 
      Entry :: term(),
      Guards :: [term(), ...],
      Result :: [term(), ...],
      Return :: {ok, term()} | {error, any()}.
select(_Entry, _Guards, _Result) ->
    ok.

%%--------------------------------------------------------------------
%% @doc delete an event from the database
%% @end
%%--------------------------------------------------------------------
-spec delete(Entry) -> Return when
      Entry :: term(),
      Return :: ok | {ok, any()} | {error, any()}.
delete(#event{} = Event) ->
    Fun = fun() ->
                  mnesia:delete_object(Event)
          end,
    mnesia:transaction(Fun).
