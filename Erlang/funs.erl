-module(funs).
-compile(export_all).

map(_, []) ->
	[];
map(F,[H|T]) -> 
	[F(H)|map(F,T)].


%only keep even numbers
even(L) -> 
	lists:reverse(even(L,[])).

even([], Acc) ->
	Acc;
even([H|T], Acc) when H rem 2 == 0 ->
	even(T, [H|Acc]);
even([_|T], Acc) ->
	even(T, Acc).


%% only keep men older than 60
old_man(L) ->
	lists:reverse(old_man(L,[])).

old_man([], Acc) ->
	Acc;
old_man([Person={male, Age}|People],Acc) when Age > 60 ->
	old_man(People, [Person|Acc]);
old_man([_|People], Acc) ->
	old_man(People, Acc).


filter(Pred, L) ->
	lists:reverse(filter(Pred, L, [])).
filter(_, [], Acc) ->
	Acc;
filter(Pred, [H|T], Acc) ->
	case Pred(H) of
		true -> filter(Pred, T, [H|Acc]);
		false -> filter(Pred, T, Acc)
	end.


% find the max number
max([H|T]) ->
	max2(T,H).
max2([], Max) ->
	Max;
max2([H|T], Max) when H > Max ->
	max2(T, H);
max2([_|T], Max) ->
	max2(T, Max).

% find the minimum number
min([H|T]) ->
	min2([H|T],H).
min2([], Min) ->
	Min;
min2([H|T], Min) when H < Min ->
	min2(T,H);
min2([_|T], Min) ->
	min2(T, Min).

% sum of all the elements of a list
sum(L) -> 
	sum(L, 0).
sum([], Sum) ->
	Sum;
sum([H|T], Sum) ->
	sum(T, H+Sum).

% build the fold abstraction
fold(_, Start, []) ->
	Start;
fold(F, Start, [H|T]) ->
	fold(F, F(H,Start), T).


