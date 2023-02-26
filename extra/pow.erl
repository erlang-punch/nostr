%%%===================================================================
%%% @doc This script parse the output of the `check_pow.py' module
%%% present in `extra' directory. The `nostrlib:pow/2' function must
%%% be exported and this script must be executed at the root of the
%%% project.
%%%
%%% @end
%%%===================================================================
-module(pow).
-export([check/0, check/1]).
-define(PYTHON, "python3").
-define(CHECK_SCRIPT, "check_pow.py").

%%--------------------------------------------------------------------
%% @doc `check/0' executes the script using python and check its
%% output.
%%
%% @end
%% --------------------------------------------------------------------
check() ->
    Path = filename:join("extra", ?CHECK_SCRIPT),
    Command = string:join([?PYTHON, Path], " "),
    Return = os:cmd(Command),
    Content = list_to_bitstring(Return),
    check_content(Content).

%%--------------------------------------------------------------------
%% @doc `check/1' read a file containing the output of check_pow.py.
%% @end
%%--------------------------------------------------------------------
check(File) ->
    {ok, Content} = file:read_file(File),
    check_content(Content).

%%--------------------------------------------------------------------
%% @doc `check_content/1' returns true if the results are valid,
%% return false if something was wrong
%%
%% @end
%%--------------------------------------------------------------------
check_content(Content) ->
    Lines = re:split(Content, "\n"),
    Splitted = lists:map(fun(X) -> re:split(X, ",") end, Lines),
    Result = [ { binary_to_integer(I)
               , binary_to_integer(P) =:=
                 nostrlib_schnorr:pow(binary_to_integer(A)
                                     ,binary_to_integer(B)
                                     ,binary_to_integer(M))}
             || [I,A,B,M,P] <- Splitted
             ],
    Filter = lists:filter(fun({_,false}) -> true;
                             (_) -> false
                          end, Result),
    case Filter of
        _ when Filter =:= [] -> {ok, length(Result)};
        Elsewise -> {error, Elsewise}
    end.
