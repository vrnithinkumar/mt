-module(pp).
% Pre-processor
-export([eraseAnn/1,getUDTs/1,getFns/1]).

eraseAnn(Forms) ->
    lists:filter(fun(F) ->
        case F of
            {attribute,_,etc,_}    -> false;
            _                           -> true
        end
    end, Forms).

getUDTs(Forms) ->
    lists:filter(fun (Node) -> 
            (erl_syntax:type(Node) == 'attribute') andalso
            (element(3,Node) == 'type')
    end, Forms).

getFns([]) -> 
    [];
getFns([{attribute,_,etc,skip}|[_|Forms]]) -> 
    getFns(Forms);
getFns([F={function,_,_,_,_}|Forms]) -> 
    [F|getFns(Forms)];
getFns([_|Forms]) -> 
    getFns(Forms).