-module(b4).

foo5(X,Y) -> X div Y.

foo4() -> 1.5;
foo4() -> 3;
foo4() -> foo5(1,2).