%%%===================================================================
%%% @author Mathieu Kerjouan <contact at erlang-punch.com>
%%% @doc handler module to deal with Mnesia database.
%%%
%%% Here a bit of documentation to help people to understand/use
%%% mnesia with nostr_relay
%%%
%%% ```
%%% rr(nostrlib).
%%% mnesia:start().
%%%
%%% % create a new table
%%% mnesia:create_table(event, [{attributes, record_info(fields, event)}]).
%%%
%%% % create a new event
%%% {ok, E2}
%%%   = nostrlib:encode(#event{content = <<"test">>, kind = text_note},
%%%      [{private_key, <<1:256>>}, {as_record, true}]).
%%%
%%% % insert this event in the table
%%% _ = mnesia:transaction(fun() -> mnesia:write(E2) end).
%%%
%%% % search the value by the id of the event
%%% % returns only the content of this event
%%% mnesia:transaction(fun() ->
%%%   Search = #event{ id = E2#event.id
%%%                  , content = '$1'
%%%                  , created_at = '_'
%%%                  , signature = '_'
%%%                  , kind = '_'
%%%                  , public_key = '_' }
%%%   mnesia:select(event, [{Search,[],['$1']}])
%%% end)
%%%
%%% % search the value by the id of the event
%%% % returns only the content of this event
%%% mnesia:transaction(fun() ->
%%%   Search = #event{ id = E2#event.id
%%%                  , content = '$1'
%%%                  , created_at = '_'
%%%                  , signature = '_'
%%%                  , kind = '_'
%%%                  , public_key = '$2' }
%%%   mnesia:select(event, [{Search,[],[['$1', '$2']]}])
%%% end)
%%%
%%% Prefix =:= erlang:binary_part(E2#event.id, {0, 8}).
%%%
%%% % convert a fun to a match spec
%%% ets:fun2ms(
%%%    fun(#event{ id = Id
%%%              , content = Content
%%%              , created_at = _
%%%              , signature = _
%%%              , kind = _, public_key = _})
%%%      when Prefix =:= binary_part(Id, {0,2}) ->
%%%        Content
%%%    end).
%%%
%%% % Pattern Spec to extract the whole content
%%% % [{#event{id = '$1', public_key = '_', created_at = '_',
%%% %          kind = '_', tags = '_', content = '$2', signature = '_'},
%%% %  [{'=:=',{const,<<202,244,196,122,235,105,92,155>>},
%%% %          {binary_part,'$1',{{0,8}}}
%%% %   }],
%%%
%%% % lambda function to extract a part of an id.
%%% fun(Prefix) ->
%%%   mnesia:transaction(fun() ->
%%%     Event = #event{ id = '$2', content = '$1', created_at = '_'
%%%                   , signature = '_', kind = '_', public_key = '_'},
%%%     Guard = [{'=:=',{const, Prefix}
%%%                    ,{binary_part,'$2',{{0,8}}}
%%%             }],
%%%     Select = ['$1'],
%%%     mnesia:select(event, [{Event, Guard, Select}])
%%%   end)
%%% end.
%%% '''
%%%
%%% @end
%%%===================================================================
-module(nostr_relay_store_mnesia).
-export([init/1, terminate/1, insert/1, lookup/1, delete/1, select/3]).
-include_lib("nostr/include/nostrlib.hrl").

% those records should be exported in another external files
% to be easily reused by other modules present in the project.
% for the moment, we will just set them here to check if what
% we are doing is working correctly.
-record(subscripter_id, { subscription_id = undefined
                        , connection = undefined
                        }).
-record(subscripter, { subscripter_id = #subscripter_id{}
                     , filters = [] % reference to another table
                     }).

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
    index(event),
    mnesia:create_table(subscripter, [{attributes, record_info(fields, subscripter)}]),
    index(subscripter),
    ok.

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
index(event = Table) ->
    Fun = fun() ->
                  mnesia:add_table_index(Table, id),
                  mnesia:add_table_index(Table, created_at)
          end,
    mnesia:transaction(Fun);
index(subscripter = Table) ->
    Fun = fun() ->
                  mnesia:add_table_index(Table, subscripter_id)
          end,
    mnesia:transaction(Fun).

%%--------------------------------------------------------------------
%% @doc terminate the database (drop everything).
%% @end
%%--------------------------------------------------------------------
-spec terminate(Args) -> Return when
      Args :: proplists:proplists(),
      Return :: ok | {error, any()}.
terminate(_Args) ->
    mnesia:delete_table(event),
    mnesia:delete_table(subscripter).

%%--------------------------------------------------------------------
%% @doc insert a new element into the database.
%% @end
%%--------------------------------------------------------------------
-spec insert(Entry) -> Return when
      Entry :: term(),
      Return :: ok | {ok, any()} | {error, any()}.
insert(#event{} = Event) ->
    Fun = fun() ->
                  mnesia:write(Event)
          end,
   mnesia:transaction(Fun);

% when we create a new subscripter, we should also probably create the
% filter(s) if it/they is/are not present in the database.
insert(#subscripter{ subscripter_id = #subscripter_id{} } = Subscripter) ->
    Fun = fun() ->
                  % 1. @TODO check the filters
                  % 2. @TODO create the filter if necessary
                  % 3. then insert the new subscripter
                  mnesia:write(Subscripter)
          end,
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
%% @doc This function is a big draft. The idea is to create an
%% abstraction (or a wrapper) around the select function you can find
%% in mnesia, but also available on other databases.
%%
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
    mnesia:transaction(Fun);

% When a subscripter is delete, the filters must also be removed as
% well. But, the filters are shared with other clients sometime,
% then, we should be sure no one else is using the filter created
% by the client, and only remove the unused filters.
delete(#subscripter{} = Subscripter) ->
    Fun = fun() ->
                  % 1. @TODO check if the filters are used by someone else
                  % 2. @TODO delete the unused filters
                  % 3. delete the subscripter from the database
                  mnesia:delete_object(Subscripter)
          end,
    mnesia:transaction(Fun).
