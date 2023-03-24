%%%===================================================================
%%% @doc DRAFT
%%%
%%% ```
%%% nostr_manager_relay_sup:start_relay_sup([{port, 4000}]).
%%% nostr_client:connect("localhost", [{port, 4000}, {tls, false}]).
%%% rr(nostrlib).
%%% nostr_client:request("localhost", #filter{limit = 1}).
%%% '''
%%%
%%% @end
%%% @author Mathieu Kerjouan <contact at erlang-punch.com>
%%%===================================================================
-module(nostr_relay_handler).
-export([init/2, websocket_init/1, websocket_handle/2, terminate/3]).
-include_lib("kernel/include/logger.hrl").

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
-spec init(Req, State) -> Return when
      Req :: term(),
      State :: term(),
      Return :: any().
init(Req, State) ->
    ?LOG_DEBUG("~p",[{?MODULE, self(), init, Req, State}]),
    {cowboy_websocket, Req, State, #{ idle_timeout => 60000*5}}.

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
-spec websocket_init(State) -> Return when
      State :: term(),
      Return :: any().
websocket_init(State) ->
    ?LOG_DEBUG("~p",[{?MODULE, self(), websocket_init, State}]),
    {ok, State}.

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
-spec websocket_handle(Frame, State) -> Return when
      Frame :: any(),

      State :: term(),
      Return :: cowboy_websocket:commands().

websocket_handle({text, Frame}, State) ->
    ?LOG_DEBUG("~p",[{?MODULE, self(), websocket_handle, Frame, State}]),
    case nostrlib:decode(Frame) of
        {ok, Decoded, Labels} ->
            nostr_relay_store:add(Decoded, Labels, State),
            {[{active, true}], State};
        _Elsewise ->
            Encoded = thoas:encode([<<"ERROR">>]),
            {[{text, Encoded}], State}
    end.

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
-spec terminate(Reason, Req, State) -> Return when
      Reason :: any(),
      Req :: any(),
      State :: any(),
      Return :: ok.
terminate(Reason, Req, State) ->
    ?LOG_DEBUG("~p",[{?MODULE, self(), terminate, Reason, Req, State}]),
    ok.
