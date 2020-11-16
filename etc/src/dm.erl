-module(dm).

%% PRINT Debugging macro%%
-ifndef(PRINT).
-define(PRINT(Var), io:format("DEBUG: ~p:~p - ~p~n~n ~p~n~n", [?MODULE, ?LINE, ??Var, Var])).
-endif.

-export([check_deps/1, get_module_paths/2]).


get_module_paths(Modules, Path) ->
    Base = string:join(lists:droplast(string:tokens(Path, "/")), "/"),
    lists:map(fun(M) -> Base ++ "/" ++ atom_to_list(M) ++ ".erl" end, Modules).

check_deps(Modes)->
    % io:format("Process: ~p~n", [registered()]),
    Sid = self(),
    ?PRINT(Sid),
    lists:map(fun(M) -> 
        spawn_and_trap(etc, main, M)
     end, Modes).


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
