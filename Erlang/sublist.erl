-module(sublist).
-export([tail_sublist/2]).

tail_sublist(L, N) ->

	tail_sublist(L, N, []).

tail_sublist(_, 0, SubList) ->

	SubList;

tail_sublist([],_ , SubList) ->

	SubList;

tail_sublist([H|T], N, SubList) when N > 0 ->
	
	tail_sublist(T,N-1, [H|SubList]).

