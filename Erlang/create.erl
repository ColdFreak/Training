-module(create).
-export([create/1,reverse/1,reverse_create/1]).
create(N) ->
	create(N, []).

create(-1, Acc) ->
	Acc;
create(N, Acc) ->
	create(N-1, [N|Acc]).

reverse(L) ->
	reverse(L, []).
reverse([],Acc) ->
	Acc;
reverse([H|T],Acc) ->
	reverse(T, [H|Acc]).
	
reverse_create(N) ->
	reverse(create(N)).

