-module(sum_list).
-export([sum_list/1]).

sum_list([]) ->
	0;
sum_list([H|T]) ->
	H + sum_list(T).
