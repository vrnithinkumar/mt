-module(dm).

%% PRINT Debugging macro%%
-ifndef(PRINT).
-define(PRINT(Var), io:format("DEBUG: ~p:~p - ~p~n~n ~p~n~n", [?MODULE, ?LINE, ??Var, Var])).
-endif.

-export([check_deps/1, get_module_paths/2, save_env/2]).

get_module_paths(Modules, Path) ->
    Base = string:join(lists:droplast(string:tokens(Path, "/")), "/"),
    lists:map(fun(M) -> Base ++ "/" ++ atom_to_list(M) ++ ".erl" end, Modules).

check_deps(Modules)->
    lists:map(fun(M) ->
        main_spec([M])
     end, Modules).

spawn_and_wait(M,F,Arg) ->
    process_flag(trap_exit, true),
    {_Pid, MonitorRef} = spawn_monitor(M, F, [[Arg]]),
    ?PRINT(_Pid),
    receive
        {_Tag, MonitorRef, _Type, _Object, _Info} -> ok
    after 9000 ->
        timeout
    end.

spawn_and_trap(M,F,Arg) ->
    Pid = spawn_link(M, F, [[Arg]]),
    ?PRINT(Pid),
    receive
	{'EXIT', Pid, Reason} -> Reason
    end.

main_spec(Args0) ->
    Args = ["+{parse_transform, tidy}"] ++ 
    case lists:member("+noti",Args0) of
        true -> [];
        false -> ["+{parse_transform, etc}"]
    end ++ 
    case lists:member("+pe",Args0) of
        true -> ["+{parse_transform, pe}"];
        false -> []
    end ++ Args0,
    erl_compile2:compile(Args).

save_env(File, Env) ->
    ErlTypeFile = re:replace(File, "\\.erl", "\\.erltypes", [{return,list}]),
    save_to_file(ErlTypeFile, Env).

save_to_file(FileName, Data) ->
    BD = erlang:term_to_binary(Data),
    file:write_file(FileName, io_lib:fwrite("~p.", [BD])).

open_from_file(FileName) ->
    {ok, [Data]} = file:consult(FileName),
    erlang:binary_to_term(Data).

search_stdlib(ModueName) ->
    code:lib_dir(stdlib, ModueName).