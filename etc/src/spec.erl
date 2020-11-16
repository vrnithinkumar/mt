-module(spec).
-import(erl_syntax,[
    function_clauses/1,
    fun_expr_clauses/1,
    clause_patterns/1,
    clause_body/1,
    clause_guard/1,
    type/1
]).

%% PRINT Debugging macro%%
-ifndef(PRINT).
-define(PRINT(Var), io:format("DEBUG: ~p:~p - ~p~n~n ~p~n~n", [?MODULE, ?LINE, ??Var, Var])).
-endif.

-export([parse_transform/2]).

parse_transform(Forms,_Opt) ->
    % Pid = self(),
    % ?PRINT(Pid),
    Mods_ = pp:getImprtdMods(Forms),
    % ?PRINT(Mods_),
    pp:eraseAnn(Forms).
