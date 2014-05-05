-module(foreach).
-compile(export_all).

test(L) ->
	lists:foreach(fun addOne/1, L).

addOne(N) ->
	io:format("~p ",[N+1]).
% 4> foreach:test([1,2,3,4]).
% 2 3 4 5 ok
