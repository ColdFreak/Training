-module(hhfuns).
-compile(export_all).

increment([]) ->
	[];
increment([H|T]) ->
	[H+1|increment(T)].

decrement([]) ->
	[];
decrement([H|T]) ->
	[H-1|decrement(T)].


map(_, []) -> 
	[];
map(F, [H|T]) ->
	[F(H) | map(F,T)].

incr(X) ->
	X+1.
decr(X) ->
	X-1.


a() ->
	Secret = "pony",
	fun() ->
		Secret end.

b(F) ->
	"a/0's password is " ++F().
