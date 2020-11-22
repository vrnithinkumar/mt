-module(env).
-export([empty/0,lookup/2,extend/3,extendConstr/3,free/2
        ,is_bound/2,fmapV/2,lookupConstrs/2,default/0
        ,freeInEnv/1,length/1
        ,dumpModuleBindings/2,readModuleBindings/1
        ,lookupRemote/3,extendRecord/4,lookupRecord/2
        ,isPatternInf/1,setPatternInf/1,addGuard/3,checkGuard/2
        ,enableGuardExprEnv/1,disableGuardExprEnv/1
        ,isGuardExprEnabled/1 %,addModuleBindings/2
        ,addExtModuleBindings/2,lookup_ext_binding/2, printExtBindings/1]).

-export_type([env/0]).

%% PRINT Debugging macro%%
-ifndef(PRINT).
-define(PRINT(Var), io:format("DEBUG: ~p:~p - ~p~n~n ~p~n~n", [?MODULE, ?LINE, ??Var, Var])).
-endif.

% Type checker ENvironment
-record(ten, 
    {   bindings        = [],
        ext_bindings    = [],
        specs_bindings  = [],
        constructors    = [],
        recFieldMap     = [],
        guardExpr       = [],
        isPattern       = false,
        isGuardExpr     = false
    }).

-type env() :: ten.

empty() -> #ten{}.

default() -> rt:defaultEnv().

% lookup :: (Var, [Var,Type])  -> Type
lookup(X,Env) -> proplists:get_value(X, Env#ten.bindings).

lookup_ext_binding(X,Env) -> proplists:get_value(X, Env#ten.ext_bindings).

is_bound(X,Env) -> proplists:is_defined(X,Env#ten.bindings).

extend(X,A,Env) -> Env#ten{bindings = [{X,A} | Env#ten.bindings]}.

addGuard(X,A,Env) -> Env#ten{guardExpr = [{X,A} | Env#ten.guardExpr]}.

checkGuard(X,Env) -> proplists:get_value(X, Env#ten.guardExpr).

enableGuardExprEnv(Env) -> Env#ten{isGuardExpr = true}.

disableGuardExprEnv(Env) -> Env#ten{isGuardExpr = false}.

isGuardExprEnabled(Env) -> Env#ten.isGuardExpr.

extendConstr(X,A,Env) -> Env#ten{constructors = [{X,A} | Env#ten.constructors]}.

free(X,Env) -> Env#ten{bindings = proplists:delete(X,Env#ten.bindings)}.

fmapV(F,Env) -> Env#ten{bindings = lists:map(fun ({Var,Type}) -> {Var,F(Type)} end, Env#ten.bindings)}.

lookupConstrs(X,Env) -> proplists:get_all_values(X,Env#ten.constructors).

%%%%%%%%% Records

extendRecord(R,A,RecFieldMap,Env) -> 
    extendRecFieldMap(R,RecFieldMap,extendConstr(R,A,Env)).

extendRecFieldMap(R,FieldMap,Env) -> 
    Env#ten{recFieldMap = [{R,FieldMap} | Env#ten.recFieldMap]}.

lookupRecord(X,Env) -> 
    case lookupConstrs(X,Env) of
        [A] -> {A,lookupRecFieldMap(X,Env)};
        []  -> undefined
    end.

lookupRecFieldMap(X,Env) -> 
    proplists:get_value(X, Env#ten.recFieldMap).

-spec freeInEnv(hm:env()) -> set:set(hm:tvar()).
freeInEnv (Env) ->
    lists:foldr(
            fun sets:union/2,
            sets:new(),
            lists:map(fun({_,T}) -> hm:free(T)end, Env#ten.bindings)).

length(Env) -> erlang:length(Env#ten.bindings).

dumpModuleBindings(Env,Module) ->
    InterfaceFile = lists:concat([Module,".ei"]),
    ModuleBindings = Env#ten.bindings -- ((env:default())#ten.bindings ++ Env#ten.ext_bindings),
    file:write_file(InterfaceFile,erlang:term_to_binary(ModuleBindings)).

dumpModuleSpecs(Env,Module) ->
    InterfaceFile = lists:concat([Module,".ei"]),
    ModuleBindings = Env#ten.bindings -- ((env:default())#ten.bindings ++ Env#ten.ext_bindings),
    file:write_file(InterfaceFile,erlang:term_to_binary(ModuleBindings)).

readModuleBindings(Module) ->
    InterfaceFile = lists:concat([Module,".ei"]),
    {ok, Data} = file:read_file(InterfaceFile),
    erlang:binary_to_term(Data).

addExtModuleBindings(Env, Module) ->
    Bindings = readModuleBindings(Module),
    Env#ten{ext_bindings = Env#ten.ext_bindings ++ Bindings}.

addModuleBindings(Env, Module) ->
    Bindings = readModuleBindings(Module),
    Env#ten{bindings = Env#ten.bindings ++ Bindings}.

lookupRemote(Module,X,_Env) ->
    case ets:lookup(compile_config, main_file) of 
        [] -> io:fwrite("No base file found ~n");
        [{main_file, File}] -> lookupModule(Module, File)
    end,
    InterfaceFile = lists:concat([Module,".ei"]),
    case filelib:is_regular(InterfaceFile)of
        true -> lookup(X,#ten{bindings = readModuleBindings(Module)});
        false -> na
    end.

lookupModule(Module, File) ->
    case isAlreadyChecked(Module) of 
        true -> na;
        false ->
            io:fwrite("Running etc for dependent module ~p ~n",[Module]),
            case getLibModulePath(Module) of 
                no_lib_module -> dm:type_check_mods([Module], File);
                Path -> registerAsLib(Module), dm:check_modules([Path])
            end
    end.

getLibModulePath(Module) ->
    LibDir = code:lib_dir(),
    ModString = atom_to_list(Module),
    WC = LibDir ++ "/*/src/" ++ ModString ++ ".erl",
    case filelib:wildcard(WC) of
        [] -> no_lib_module;
        [Path | _] -> Path
    end.

isAlreadyChecked(Module) -> 
    case ets:lookup(compile_config, Module) of 
        [] ->  false;
        _  -> true
    end.

registerAsLib(Module) -> 
    ets:insert(compile_config, {Module, stdlib}).


printExtBindings(Env) ->
    Ext = Env#ten.ext_bindings,
    ?PRINT(Ext).
%%%%%%%%%%%%%%%%%%%
isPatternInf(Env) -> 
    Env#ten.isPattern.

setPatternInf(Env) ->
    Env#ten{isPattern = true}.
