-module(shop1).
-export([total/1]).

total([]) -> 0;
total([{What, N}|T]) ->
	shop:cost(What)*N + total(T).
