-module(g3).
-compile(export_all).

description() -> "Comparison expressions".

ceq(X,Y) -> X == Y.

cne(X,Y) -> X /= Y.

clte(X,Y) -> X =< Y.

clt(X,Y) -> X < Y.

cgte(X,Y) -> X >= Y.

cgt (X,Y) -> X > Y.

cee (X,Y) -> X =:= Y.

cene (X,Y) -> X =/= Y.
