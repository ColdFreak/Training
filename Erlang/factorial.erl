-module(factorial).
-export([fac/1]).

fac(N) when N >= 1 ->
	fac(N, 1).

fac(1, Acc) ->
	Acc;
fac(N, Acc) ->
	fac(N-1, N*Acc).

