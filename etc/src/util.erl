-module (util).
-export([to_string/1,intersperse/2,
    interFoldEffect/4,pairwiseChunk/1,
    getFnName/1,getFnArgLen/1,getFnClauses/1,
    getLn/1,getFnQName/1,eqLists/3,
    keysIntersection/1,allElemEq/2,find/2, list_and/1]).
-export_type([maybe/1]).

-type maybe(A) :: {nothing} | {just,A}.

to_string(X) -> lists:flatten(io_lib:format("~p",[X])).

intersperse(_,[]) -> [];
intersperse(_,[X]) -> [X];
intersperse(A,[X|Xs]) -> [X|[A|intersperse(A,Xs)]].

% a fold with an "effect intersperse"
interFoldEffect(_,_,B,[])         -> B;
interFoldEffect(F,_,B,[X])        -> F(X,B);
interFoldEffect(F,I,B,[X|Xs])     ->
    B_ = F(X,B), 
    I(), 
    interFoldEffect(F,I,B_,Xs).

pairwiseChunk ([])              -> {[],{nothing}};
pairwiseChunk ([X])             -> {[],{just,X}};
pairwiseChunk ([X|[Y|Tail]])    -> 
    {TailChunks, Rem} = pairwiseChunk(Tail),
    {[{X,Y} | TailChunks], Rem}.

getFnName (Fun) -> element(3,Fun).
getFnArgLen (Fun) -> element(4,Fun).
getFnClauses (Fun) -> element(5,Fun).
getLn(Node) -> element(2,Node).

getFnQName(Fun) -> {getFnName(Fun),getFnArgLen(Fun)}.

eqLists(Comp,Xs,Ys) ->
    (length(Xs) == length (Ys))
    andalso
    lists:all(
        fun({X,Y}) -> Comp(X,Y) end, lists:zip(Xs,Ys)).

% [maps:map()] -> sets:set("keys")
keysIntersection(Maps) -> 
    KeySets = lists:map(fun(S) ->
        sets:from_list(maps:keys(S))
    end, Maps),
    case length(KeySets) of
        0 -> sets:new();
        _ -> sets:intersection(KeySets)
    end.

allElemEq(_,[]) -> true;
allElemEq(_,[_]) -> true;
allElemEq(Comp,[X|[Y|Rem]]) -> 
    Comp(X,Y) and allElemEq(Comp,[Y|Rem]).

find(_,[]) -> {nothing};
find(Fun,[X|Xs]) -> 
    case Fun(X) of
        true -> {just,X};
        false -> find(Fun,Xs)
    end.

list_and(List) -> 
    not lists:member(false, List).