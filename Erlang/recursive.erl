-module(recursive).
-export([reverse/1, tail_zip/2, tail_lenient_zip/2]).

% reverse
reverse([]) ->
	[];
reverse([H|T]) ->
	reverse(T) ++ [H].


%% tail recursive version of zip/2
tail_zip(X,Y) -> 
	reverse(tail_zip(X,Y, [])).

tail_zip([],[], Acc) ->
	Acc;

tail_zip([X|Xs], [Y|Ys], Acc) ->
	tail_zip(Xs, Ys, [{X,Y}|Acc]).



tail_lenient_zip(X,Y) ->

	reverse(tail_lenient_zip(X,Y,[])).

tail_lenient_zip([],_,Acc) ->
	Acc;
tail_lenient_zip(_,[],Acc) ->
	Acc;

tail_lenient_zip([X|Xs], [Y|Ys], Acc) ->
	
	tail_lenient_zip(Xs, Ys, [{X,Y}|Acc]).

