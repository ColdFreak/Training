-module(reverse).
-export([tail_reverse/1]).

tail_reverse(L) ->
	
	tail_reverse(L, []).

tail_reverse([], Acc) ->
	
	Acc;

tail_reverse([H|T], Acc) ->

	tail_reverse(T, [H|Acc]).

