-module(printN).
-export([printN/1]).

printN(N) ->
	printN(N, 1).

printN(N,M) when M =< N ->
	io:format("Number:~p~n",[M]),
	printN(N,M+1);
% 上記以外の場合、つまりM > Nになったら、とまる。
% okを返す
printN(_,_) -> 
	ok.
	
