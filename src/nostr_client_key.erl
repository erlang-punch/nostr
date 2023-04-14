%%%===================================================================
%%% @author Mathieu Kerjouan <contact at erlang-punch dot com>
%%% @doc `nostr_client_key' module manages users keys. An user can
%%% have more than one key and can use them on one or many relays.
%%% This module is also in charge to store user's information.
%%%
%%% This module is in charge of:
%%%
%%% <ul>
%%%   <li>create/delete new keys based on an unique name</li>
%%%   <li>load/store the private key(s) and its metadata on the filesystem</li>
%%%   <li>encrypt/decrypt the private key(s) and its metadata locally</li>
%%%   <li>generate the public key based on the private key</li>
%%%   <li>manage metadata associated with the key(s)</li>
%%% </ul>
%%%
%%% This module will create different kind of files on the filesystem:
%%%
%%% <ul>
%%%   <li>`${HOME}/.nostr' directory containing configurations and key
%%%   store</li>
%%%   <li>`${HOME}/.nostr/${name}' directory containing the key and
%%%   metadata associated with `${name}' account</li>
%%%   <li>`${HOME}/.nostr/${name}/id_secp256k1' file containing the
%%%   private key and its metadata in encrypted or decrypted format</li>
%%% </ul>
%%%
%%% == Usage ==
%%%
%%% ```
%%% % 1. (ok) create or open `user_name' key
%%% {ok, Pid} = nostr_client_key:start([{name, <<"user_name">>}]).
%%%
%%% % 2. (ok) get the private key
%%% {ok, PrivateKey} = nostr_client_key:private_key(Pid).
%%%
%%% % 3. (ok) get the public key
%%% {ok, PublicKey} = nostr_client_key:public_key(Pid).
%%%
%%% % 4. (ok) set the "name" field associated with the key
%%% ok = nostr_client_key:set_metadata(Pid, name, <<"my_new_name">>).
%%% {ok, <<"my_new_name>>} = nostr_client_key:get_metadata(Pid, name).
%%%
%%% % 5. (ok) set the "about" field associated with the key
%%% ok = nostr_client_key:set_metadata(Pid, about, <<"This is about me">>).
%%% {ok, <<"This is about me">>} = nostr_client_key:get_metadata(Pid, about).
%%%
%%% % 6. (ok) set the "picture" field associated with the key
%%% ok = nostr_client_key:set_metadata(Pid, picture, <<"https://...">>
%%% {ok, <<"https://...">>} = nostr_client_key:get_metadata(Pid, picture).
%%%
%%% % 7. export metadata
%%% {ok, #event{} = Event} = nostr_client_key:export(Pid).
%%% '''
%%%
%%% == Notes ==
%%%
%%% When encrypted, the key store SHALL NOT BE DECRYPTED outside the
%%% BEAM. When metadata changes when encrypted, exported data MUST BE
%%% encrypted as well.
%%%
%%% @todo instead of converting raw Erlang data into base64, using 
%%% an encrypted DETS could be a good solution.
%%% @todo creates a way to generate automatically the `set_metadata'
%%% event as an export to a server
%%%
%%% @end
%%%===================================================================
-module(nostr_client_key).
-behavior(gen_server).
-export([start/1]).
-export([start_link/1]).
-export([public_key/1, private_key/1]).
-export([set_metadata/3, get_metadata/2]).
-export([init/1, terminate/2]).
-export([handle_cast/2, handle_call/3, handle_info/2]).
-include_lib("kernel/include/logger.hrl").
-include_lib("kernel/include/file.hrl").
-include("nostrlib.hrl").
-record(state, { store_path = undefined :: binary()
               , name :: undefined | binary()
               , private_key_path :: undefined | binary()
               , private_key :: undefined | binary()
               , public_key :: undefined | binary()
               , metadata = #{} :: #{}
               , sync = undefined :: undefined | integer()
               }).
-define(DEFAULT_DIRECTORY_NAME, <<".nostr">>).
-define(DEFAULT_KEY_FILENAME, <<"id_secp256k1">>).
-define(DEFAULT_DIRECTORY_MODE, 16832).
% @todo change DEFAULT_FILE_MODE to 0600 
-define(DEFAULT_FILE_MODE, 33024).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec start(Args) -> Return when
      Args :: proplists:proplists(),
      Return :: {ok, pid()}.
start(Args) ->
    gen_server:start(?MODULE, Args, []).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec start_link(Args) -> Return when
      Args :: proplists:proplists(),
      Return :: {ok, pid()}.
start_link(Args) ->
    gen_server:start_link(?MODULE, Args, []).

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

%%--------------------------------------------------------------------
%% @doc internal. init_name/2 function check if the name given is 
%% valid or not.
%% @end
%%--------------------------------------------------------------------
init_name(undefined, State) ->
    ?LOG_DEBUG("~p", [{?MODULE, self(), init_name, undefined, State}]),
    {stop, [{message, "name undefined"}]};
init_name(Name, State) 
  when is_binary(Name)->
    ?LOG_DEBUG("~p", [{?MODULE, self(), init_name, Name, State}]),
    NewState = State#state{ name = Name },
    init_path(NewState);
init_name(Name, State) ->
    ?LOG_DEBUG("~p", [{?MODULE, self(), init_name, Name, State}]),
    {stop, [{message, "wrong name"}, {name, Name}]}.

%%--------------------------------------------------------------------
%% @doc internal. `init_path/1' function will modify the state to
%% configure the valid store path.
%% @end
%%--------------------------------------------------------------------
init_path(#state{ name = Name } = State) ->
    ?LOG_DEBUG("~p", [{?MODULE, self(), init_path, State}]),
    case store_dir() of
        {ok, StoreDir} ->
            StorePath = filename:join(StoreDir, Name),
            NewState = State#state{ store_path = StorePath
                                  , name = Name 
                                  },
            init_directory_check(NewState);
        {error, Reason} = Error->
            ?LOG_DEBUG("~p", [{?MODULE, self(), init_path, Error}]),
            {store, [{message, Reason}]}
    end.

%%--------------------------------------------------------------------
%% @doc internal. `init_directory_check/1' checks ONLY if the
%% directory exists or not.
%% @end
%%--------------------------------------------------------------------
init_directory_check(#state{ store_path = StorePath } = State) ->
    case filelib:ensure_path(StorePath) of
        ok ->
            init_directory_mode(State);
        {error, eacces} ->
            init_directory_create(State);
        {error, Error} ->
            {stop, Error}
    end.

%%--------------------------------------------------------------------
%% @doc internal. `init_directory_create/1' creates store directory.
%% @end
%%--------------------------------------------------------------------
init_directory_create(#state{ store_path = StorePath } = State) ->
    case file:make_dir(StorePath) of
        ok ->
            init_directory_mode(State);
        Error -> 
            {stop, Error}
    end.

%%--------------------------------------------------------------------
%% @doc internal. `init_directory_mode/1' ensures store directory was
%% created with the correct mode, if not, it will fix it.
%% @end
%%--------------------------------------------------------------------
init_directory_mode(#state{ store_path = StorePath } = State) ->
    {ok, #file_info{ mode = Mode }} = file:read_file_info(StorePath),
    case Mode of
        ?DEFAULT_DIRECTORY_MODE -> 
            init_private_key(State);
        _ when is_integer(Mode) -> 
            ?LOG_WARNING("Wrong mode for ~p. correct it.",[StorePath]),
            % @todo check if mode was correctly configured
            file:change_mode(StorePath, ?DEFAULT_DIRECTORY_MODE),
            init_private_key(State);
        Elsewise ->
            ?LOG_ERROR("~p", [{?MODULE, self(), init_directory_mode, Elsewise}]),
            {store, Elsewise}
    end.

init_private_key(#state{ store_path = StorePath } = State) ->
    KeyPath = filename:join(StorePath, ?DEFAULT_KEY_FILENAME),
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
    init_private_key_store(NewState).

init_private_key_store(State) ->
    case store_private_key(State) of
        {ok, NewState} -> {ok, NewState};
        Elsewise -> {stop, Elsewise}
    end.

init_private_key_load(State) ->
    case load_private_key(State) of
        {ok, NewState} -> {ok, NewState};
        Elsewise -> {stop, Elsewise}
    end.

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
handle_cast(store, State) ->
    case store_private_key(State) of
        {ok, NewState} -> {noreply, NewState};
        {error, NewState} -> {noreply, NewState}
    end;
handle_cast({set_metadata, name, Value}, #state{ metadata = Metadata } = State)
  when is_binary(Value) ->
    NewMetadata = maps:put(name, Value, Metadata),
    NewState = State#state{metadata = NewMetadata},
    {noreply, NewState};
handle_cast({set_metadata, about, Value}, #state{ metadata = Metadata } = State)
  when is_binary(Value) ->
    NewMetadata = maps:put(about, Value, Metadata),
    NewState = State#state{metadata = NewMetadata},
    {noreply, NewState};
handle_cast({set_metadata, picture, Value}, #state{ metadata = Metadata } = State)
  when is_binary(Value) ->
    NewMetadata = maps:put(picture, Value, Metadata),
    NewState = State#state{metadata = NewMetadata},
    {noreply, NewState};
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
handle_call({get_metadata, Key}, _From, #state{ metadata = Metadata } = State) ->
    {reply, {ok, maps:get(Key, Metadata, undefined)}, State};
handle_call({get, private_key}, _From, #state{ private_key = PrivateKey} = State) ->
    {reply, {ok, PrivateKey}, State};
handle_call({get, public_key}, _From, #state{ public_key = PublicKey} = State) ->
    {reply, {ok, PublicKey}, State};
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
            {ok, filename:join(Home, ?DEFAULT_DIRECTORY_NAME)}
    end.

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
store_private_key(#state{ private_key_path = Path
                        , private_key = PrivateKey
                        } = State) ->
    Base64 = base64:encode(PrivateKey),
    case file:write_file(Path, Base64) of
        ok ->
            % @todo ensure the file is using correct mode
            file:change_mode(Path, ?DEFAULT_FILE_MODE),
            NewState = State#state{ sync = erlang:system_time() },
            {ok, NewState};
        {error, _Reason} = Error ->
            NewState = State#state{ sync = Error },
            {error, NewState}
    end.

%%--------------------------------------------------------------------
%% @doc internal.
%%
%% @todo ensure the key is using the correct right before loading
%%       or correct that.
%% @end
%%--------------------------------------------------------------------
load_private_key(#state{ private_key_path = PrivateKeyPath} = State) ->
    case file:read_file(PrivateKeyPath) of
        {ok, Base64} ->
            PrivateKey = base64:decode(Base64),
            {ok, PublicKey} = nostrlib_schnorr:new_publickey(PrivateKey),
            NewState = State#state{ private_key = PrivateKey
                                  , public_key = PublicKey
                                  , sync = erlang:system_time()
                                  },
            {ok, NewState};
        Elsewise ->
            Elsewise
    end.

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
-spec private_key(Pid) -> Return when
      Pid :: pid(),
      Return :: {ok, binary()}.
private_key(Pid) ->
    gen_server:call(Pid, {get, private_key}).

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
-spec public_key(Pid) -> Return when
      Pid :: pid(),
      Return :: {ok, binary()}.
public_key(Pid) ->
    gen_server:call(Pid, {get, public_key}).

%%--------------------------------------------------------------------
%% @doc `set_metadata/3' add or overwrite a key present in the
%% metadata
%%
%% @end
%%--------------------------------------------------------------------
-spec set_metadata(Pid, Key, Value) -> Return when
      Pid :: pid(),
      Key :: name | about | picture,      
      Value :: binary(),
      Return :: ok.
set_metadata(Pid, Key, <<Value/binary>>) 
  when Key =:= name orelse Key =:= about orelse Key =:= picture ->
    gen_server:cast(Pid, {set_metadata, Key, Value}).

%%--------------------------------------------------------------------
%% @doc `get_metadata/2' gets the value from the metadata.
%%
%% @end
%%--------------------------------------------------------------------
-spec get_metadata(Pid, Key) -> Return when
      Pid :: pid(),
      Key :: name | about | picture,      
      Return :: {ok, undefined | binary()}.
get_metadata(Pid, Key) when is_atom(Key) ->
    gen_server:call(Pid, {get_metadata, Key}).
