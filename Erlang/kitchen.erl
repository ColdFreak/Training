-module(kitchen).
-export([fridge/1]).

% store and take food from the fridge
% so two methods, take, and store
% after some take and store, 
% don't forget to use flush() function.

fridge(FoodList) ->
	
	receive 
		{From, {store, Food}} ->
			From ! {self(), ok},
			fridge([Food|FoodList]);

		{From, {take, Food}} ->
			
			case lists:member(Food, FoodList) of
				true ->
					From ! {self(), {ok, food}},
					fridge(lists:delete(Food, FoodList));
				false ->
					From ! {self(), not_found},
					fridge(FoodList)
			end;
		terminate ->
			ok
	end.

	
store(Pid, Food) ->
	Pid ! {self(), {store, Food}},
	receive
		{Pid, Msg} -> Msg
	end.

take(Pid, Food) ->
	Pid ! {self(), {take, Food}},
	receive
		{Pid, Msg} -> Msg
	end.


