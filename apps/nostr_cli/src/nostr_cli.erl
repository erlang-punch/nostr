%%%===================================================================
%%% @copyright (c) 2023 Erlang Punch
%%% @author Mathieu Kerjouan
%%% @doc 
%%%
%%% This module implement nostr command line interface to deal with
%%% nostr applications (client, server, database) and with nostr utils
%%% (bech32, segwit, websocket client/server...). 
%%%
%%% == Bech32 Encoder/Decoder ==
%%%
%%% ```
%%% $ nostr_cli bech32 encode -hrp test -data test
%%% test1w3jhxaq3pvlcg
%%%
%%% $ nostr_cli bech32 encode -hrp test -data "abcd" -from hex
%%% test140xs04c67l
%%%
%%% $ nostr_cli bech32 encode -hrp test -data 3yARIxdnWjJN+uys -from base64
%%% test1muspzgchvadryn06ajkqcfyqsk
%%% '''
%%%
%%% == Websocket Client/Server ==
%%%
%%% It's always useful to have a simple websocket client/server to
%%% debug weird behaviors or to generate connections.
%%%
%%% ```
%%% $ nostr_cli websocket server -port 8081
%%% '''
%%%
%%% ```
%%% $ nostr_cli websocket client -url ws://localhost:8081 -text test
%%% 
%%% $ nostr_cli websocket client -url ws://localhost:8081
%%% data: test
%%% data: test
%%% ^C
%%% '''
%%%
%%% @end
%%%===================================================================
-module(nostr_cli).
-export([main/1]).

%%--------------------------------------------------------------------
%% @hidden
%% @doc
%% @end
%%--------------------------------------------------------------------
usage() ->
    io:format("Usage: nostr [bech32|websocket]~n").

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec main([string(), ...]) -> any().
main([]) ->
    usage();
main(["bech32"|Rest]) ->
    bech32(Rest);
main(["websocket"|Rest]) ->
    websocket(Rest);
main(_) ->
    usage().

%%--------------------------------------------------------------------
%% @hidden
%% @doc
%% @end
%%--------------------------------------------------------------------
bech32(["encode"|Rest]) ->
    bech32_encode(Rest, #{ format => bech32, from => string });
bech32(_) ->
    io:format("usage: nostr bech32 encode~n").

%%--------------------------------------------------------------------
%% @hidden
%% @doc
%% @end
%%--------------------------------------------------------------------
bech32_encode([], Params) 
  when map_size(Params) =:= 0 ->
    io:format("Usage: nostr bech32 encode [options]~n"),
    io:format("  -help~n"),
    io:format("  -format [bech32|bech32m]~n"),
    io:format("  -hrp Hrp~n"),
    io:format("  -data Data~n"),
    io:format("  -from [string|hex|base64] Data~n");
bech32_encode([], Params) ->
    bech32_encode_cli(Params);
bech32_encode(["-help"|_], _) ->
    bech32_encode([], #{});
bech32_encode(["-format", "bech32"|Rest], Buffer) ->
    bech32_encode(Rest, Buffer#{ format => bech32 });
bech32_encode(["-format", "bech32m"|Rest], Buffer) ->
    bech32_encode(Rest, Buffer#{ format => bech32m });
bech32_encode(["-hrp", HRP|Rest], Buffer) ->
    bech32_encode(Rest, Buffer#{ hrp => HRP });
bech32_encode(["-data", Data|Rest], Buffer) ->
    bech32_encode(Rest, Buffer#{ data => Data });
bech32_encode(["-from", "string"|Rest], Buffer) ->
    bech32_encode(Rest, Buffer#{ from => string });
bech32_encode(["-from", "hex"|Rest], Buffer) ->
    bech32_encode(Rest, Buffer#{ from => hex });
bech32_encode(["-from", "base64"|Rest], Buffer) ->
    bech32_encode(Rest, Buffer#{ from => base64 }).


%%--------------------------------------------------------------------
%% @hidden
%% @doc encode bech32 from string, hexadecimal or base64 data.
%% @end
%%--------------------------------------------------------------------
bech32_encode_cli(#{ format := Format, hrp := Hrp, data := Data, from := string }) ->
    case bech32:encode(Hrp, Data, [{format, Format}, {indexed, false}]) of
        {ok, Bech32} ->
            io:format("~s~n", [Bech32]);
        {error, Reason} ->
            io:format("error: ~p~n", [Reason])
    end;
bech32_encode_cli(#{ format := Format, hrp := Hrp, data := Data, from := hex }) ->
    try
        HexData = binary:decode_hex(list_to_binary(Data)),
        case bech32:encode(Hrp, HexData, [{format, Format}, {indexed, false}]) of
            {ok, Bech32} ->
                io:format("~s~n", [Bech32]);
            {error, Reason} ->
                io:format("error: ~p~n", [Reason])
        end
    catch
        _:_ -> io:format("error: invalid hexadecimal data~n")
    end;
bech32_encode_cli(#{ format := Format, hrp := Hrp, data := Data, from := base64 }) ->
    try
        Base64Data = binary_to_list(base64:decode(Data)),
        case bech32:encode(Hrp, Base64Data, [{format, Format}, {indexed, false}]) of
            {ok, Bech32} ->
                io:format("~s~n", [Bech32]);
            {error, Reason} ->
                io:format("error: ~p~n", [Reason])
        end
    catch
        _:_ -> io:format("error: invalid base64 data")
    end;
bech32_encode_cli(_) ->
    bech32_encode([], #{}).

%%--------------------------------------------------------------------
%% @hidden
%% @doc
%% @end
%%--------------------------------------------------------------------
websocket(["server"|Rest]) ->
    websocket_server(Rest, #{});
websocket(["client"|Rest]) ->
    websocket_client(Rest, #{});
websocket(_) ->
    io:format("usage: nostr websocket [client|server]~n").

%%--------------------------------------------------------------------
%% @hidden
%% @doc
%% @end
%%--------------------------------------------------------------------
websocket_server([], Params) 
  when map_size(Params) =:= 0 ->
    io:format("Usage: nostr websocket server [options]~n"),
    io:format("  -help~n"),
    io:format("  -port Port~n");
websocket_server([], Params) ->
    websocket_server_cli(Params);
websocket_server(["-help"|_], _) ->
    websocket_server([], #{});
websocket_server(["-port", Port|Rest], Buffer) ->
    try IntegerPort = erlang:list_to_integer(Port),
         websocket_server(Rest, Buffer#{ port => IntegerPort })
    catch
        _:Reason:S -> 
            io:format("error ~p: ~p~n", [Reason, S]),
            websocket_server([], #{})
    end.

%%--------------------------------------------------------------------
%% @hidden
%% @doc
%% @end
%%--------------------------------------------------------------------
websocket_server_cli(#{ port := Port }) ->
    {ok, Pid} = websocket_server:start("localhost", Port),
    websocket_server_cli_loop(Pid).

%%--------------------------------------------------------------------
%% @hidden
%% @doc
%% @end
%%--------------------------------------------------------------------
websocket_server_cli_loop(Pid) ->
    receive 
        _ -> websocket_server_cli_loop(Pid)
    end.

%%--------------------------------------------------------------------
%% @hidden
%% @doc
%% @end
%%--------------------------------------------------------------------
websocket_client([], Params) 
  when map_size(Params) =:= 0 ->
    io:format("Usage: nostr websocket client [options]~n"),
    io:format("  -help~n"),
    io:format("  -url Url~n"),
    io:format("  -text Text~n"),
    io:format("  -binary Binary~n"),
    io:format("  -as [text|binary]~n");
websocket_client([], Params) ->
    websocket_client_cli(Params);    
websocket_client(["-help"|_], _) ->
    websocket_client([], #{});    
websocket_client(["-as", "text"|Rest], Buffer) ->
    websocket_client(Rest, Buffer#{ as => text });
websocket_client(["-as", "binary"|Rest], Buffer) ->
    websocket_client(Rest, Buffer#{ as => binary });
websocket_client(["-text", Text|Rest], Buffer) ->
    websocket_client(Rest, Buffer#{ text => Text });
websocket_client(["-binary", Binary|Rest], Buffer) ->
    websocket_client(Rest, Buffer#{ binary => list_to_binary(Binary) });
websocket_client(["-url", Url|Rest], Buffer) ->
    websocket_client(Rest, Buffer#{ url => Url }).

%%--------------------------------------------------------------------
%% @hidden
%% @doc
%% @end
%%--------------------------------------------------------------------
websocket_client_cli(#{ url := Url } = Params) -> 
    application:ensure_all_started(gun),
    {ok, Pid} = websocket_client:start(Url),
    case Params of
        #{ text := Text} ->
            websocket_client:send(Pid, Text);
        #{ binary := Binary } ->
            websocket_client:send(Pid, Binary);
        _ ->
            websocket_client_cli_loop(Pid, Params)
    end.

%%--------------------------------------------------------------------
%% @hidden
%% @doc
%% @end
%%--------------------------------------------------------------------
websocket_client_cli_loop(Pid, Params) ->
    Data = io:get_line("data: "),
    case maps:get(as, Params, text) of
        text ->
            websocket_client:send(Pid, Data),
            websocket_client_cli_loop(Pid, Params);
        binary ->
            websocket_client:send(Pid, list_to_binary(Data)),
            websocket_client_cli_loop(Pid, Params)
    end.
