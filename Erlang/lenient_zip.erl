-module(lenient_zip).
-export([lenient_zip/2]).

lenient_zip([],_) ->
	[];

lenient_zip(_,[]) ->
	[];

lenient_zip([X|Xs], [Y|Ys]) ->
	[{X,Y} | lenient_zip(Xs, Ys)].

