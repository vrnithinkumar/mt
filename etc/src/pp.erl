-module(pp).
% Pre-processor
-export([eraseAnn/1,getUDTs/1,getFns/1
        ,fmapPEFns/2,getModule/1,getRecs/1, getSpecs/1,getImprtdMods/1, getFile/1]).

eraseAnn(Forms) ->
    lists:filter(fun(F) ->
        case F of
            {attribute,_,etc,_}    -> false;
            _                           -> true
        end
    end, Forms).

getUDTs(Forms) ->
    getAttributes(Forms,'type').

getRecs(Forms) ->
    getAttributes(Forms,'record').

getSpecs(Forms) ->
        getAttributes(Forms,'spec').

getAttributes(Forms,Attribute) ->
    lists:filter(fun (Node) -> 
            (erl_syntax:type(Node) == 'attribute') andalso
            (element(3,Node) == Attribute)
    end, Forms).

getFns([]) -> 
    [];
getFns([{attribute,_,etc,skip}|[_|Forms]]) -> 
    getFns(Forms);
getFns([F={function,_,_,_,_}|Forms]) -> 
    [F|getFns(Forms)];
getFns([_|Forms]) -> 
    getFns(Forms).

fmapPEFns(Fun,[]) -> [];
fmapPEFns(Fun,[{attribute,_,etc,pe}|[F|Forms]]) -> [Fun(F) | fmapPEFns(Fun,Forms)];
fmapPEFns(Fun,[F|Forms]) -> [F | fmapPEFns(Fun,Forms)].

getModule(Forms) -> {attribute,_,module,Name} = lists:nth(2,Forms),Name.

getImprtdMods(Forms) ->
    MAs = getAttributes(Forms, import),
    lists:map(fun({attribute, _, import, {Name, _}}) -> Name end, MAs).

getFile(Forms) ->
    [{attribute,1,file,{File, _}}] = getAttributes(Forms, file),
    File.