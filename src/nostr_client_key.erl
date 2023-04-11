%%%===================================================================
%%% @doc 
%%%
%%% @end
%%%===================================================================
-module(nostr_client_key).
-behavior(gen_server).
-export([start/1]).
-export([init/1, terminate/2]).
-export([handle_cast/2, handle_call/3, handle_info/2]).
-include_lib("kernel/include/logger.hrl").
-include("nostrlib.hrl").
-record(state, { store_path = undefined :: binary()
               , name :: undefined | binary()
               , private_key_path :: undefined | binary()
               , private_key :: undefined | binary()
               , public_key :: undefined | binary()
               }).
-define(KEY_FILENAME, <<"id_secp256k1">>).

-spec start(Args) -> Return when
      Args :: proplists:proplists(),
      Return :: {ok, pid()}.
start(Args) ->
    gen_server:start(?MODULE, Args, []).

%%--------------------------------------------------------------------
%% @doc Here the init steps:
%%
%% 1. get the name of the user (identifier) if not -> crash
%% 2. get the store path if not -> crash
%% 3. check if the path exist if not -> create it or crash
%% 4. ensure the directory has correct right and access (0700)
%% 5. if a file called `id_secp256k1' exist -> load it or goto 6
%% 6. generate a new private key and store it in `id_secp256k1'
%% 7. start the gen server with the correct states
%%
%% @end
%%--------------------------------------------------------------------
-spec init(Args) -> Return when
      Args :: proplists:proplists(),
      Return :: {ok, #state{}}.
init(Args) ->
    State = #state{},
    Name = proplists:get_value(name, Args, undefined),
    init_name(Name, State).

init_name(undefined, _State) ->
    {stop, [{message, "name undefined"}]};
init_name(Name, State) 
  when is_binary(Name)->
    NewState = State#state{ name = Name },
    init_path(NewState);
init_name(Name, _State) ->
    {stop, [{message, "wrong name"}, {name, Name}]}.

init_path(#state{ name = Name } = State) ->
    case store_dir() of
        {ok, StoreDir} ->
            StorePath = filename:join(StoreDir, Name),
            NewState = State#state{ store_path = StorePath
                                  , name = Name 
                                  },
            init_directory(NewState);
        {error, Reason} ->
            {store, [{message, Reason}]}
    end.

init_directory(#state{ store_path = StorePath } = State) ->
    case filelib:ensure_path(StorePath) of
        ok ->
            init_private_key(State);
        {error, eacces} ->
            % @todo update with 0700 mode
            case file:make_dir(StorePath) of
                ok ->
                    init_private_key(State);
                Error -> {stop, Error}
            end
    end.

init_private_key(#state{ store_path = StorePath } = State) ->
    KeyPath = filename:join(StorePath, ?KEY_FILENAME),
    NewState = State#state{ private_key_path = KeyPath },
    case filelib:is_regular(KeyPath) of
        true -> 
            init_private_key_load(NewState);
        false -> 
            init_private_key_generate(NewState)
    end.

init_private_key_generate(State) ->
    {ok, PrivateKey} = nostrlib_schnorr:new_privatekey(),
    {ok, PublicKey} = nostrlib_schnorr:new_publickey(PrivateKey),
    NewState = State#state{ private_key = PrivateKey
                          , public_key = PublicKey
                          },
    {ok, NewState}.

init_private_key_load(#state{ private_key_path = PrivateKeyPath} = State) ->
    {ok, Base64} = file:read_file(PrivateKeyPath),
    PrivateKey = base64:decode(Base64),
    {ok, PublicKey} = nostrlib_schnorr:new_publickey(PrivateKey),
    NewState = State#state{ private_key = PrivateKey
                          , public_key = PublicKey
                          },
    {ok, NewState}.    

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec terminate(any(), any()) -> ok.
terminate(_,_) ->
    ok.

%%--------------------------------------------------------------------
%% @doc
%%
%% @todo create the revocation process, if a new key is generated 
%%       over a new one, it should be stored somewhere and not 
%%       reused.
%% @end
%%--------------------------------------------------------------------
-spec handle_cast(Message, State) -> Return when
      Message :: generate | store | load,
      State :: #state{},
      Return :: {noreply, State}.
%% perhaps not the best thing to do if we don't have a way to store
%% the old keys.
%% handle_cast(generate, State) ->
%%     {ok, PrivateKey} = nostrlib_schnorr:new_privatekey(),
%%     {ok, PublicKey} = nostrlib_schnorr:new_publickey(PrivateKey),
%%     NewState = State#state{ private_key = PrivateKey
%%                           , public_key = PublicKey
%%                           },
%%     {noreply, NewState};
handle_cast(store, #state{ private_key_path = Path 
                         , private_key = PrivateKey 
                         } = State) ->
    Base64 = base64:encode(PrivateKey),
    file:write_file(Path, Base64),
    {noreply, State};
handle_cast(_Message, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec handle_call(Message, From, State) -> Return when
      Message :: export,
      From :: any(),
      State :: #state{},
      Return :: {reply, ReplyExport, State},
      ReplyExport :: {ok, PrivateKey},
      PrivateKey :: binary().
handle_call({get, private_key}, _From, #state{ private_key = PrivateKey} = State) ->
    {reply, {ok, PrivateKey}, State};
handle_call({get, public_key}, _From, #state{ public_key = PublicKey} = State) ->
    {reply, {ok, PublicKey}, State};
handle_call({get, private_key_path}, _From, #state{ private_key_path = Path} = State) ->
    {reply, {ok, Path}, State};
handle_call(_Message, _From, State) ->
    {reply, ok, State}.

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec handle_info(Message, State) ->  Return when
      Message :: any(),
      State :: #state{},
      Return :: {noreply, State}.
handle_info(_Message, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec store_dir() -> Return when
      Return :: string().
store_dir() ->
    case os:getenv("HOME") of
        false ->
            {error, [{message, "no HOME defined"}]};
        Home ->
            {ok, filename:join(Home, "nostr")}
    end.
