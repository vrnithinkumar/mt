-module(g9).

% different ways to call a function


foo(X,Y) -> X + Y.

bar() -> X = fun foo/2, X(1,2) div foo(1,2).

bar2() -> X = fun bar/0, X().