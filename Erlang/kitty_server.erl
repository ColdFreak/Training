%%Naive version

-module(kitty_server).
-export([start_link/0, order_cat/4, return_cat/2, close_shop/1]).

-record(cat, {name, color=green, description}).

%%%Client API
start_link() -> spawn_link(fun init/0).

%% Synchronous call
order_cat(Pid, Name, Color, Description) ->
	Ref = erlang:monitor(process, Pid),
	% send to server's Pid message
	Pid ! {self(), Ref, {order, Name, Color, Description}},
	receive 
		{Ref, Cat} ->
			erlang:demonitor(Ref, [flush]),
			Cat;
		{'DOWN', Ref, process, Pid, Reason} ->
			erlang:error(Reason)
	after 5000 ->
		erlang:error(timeout)
	end.

%% This call is asynchronous
return_cat(Pid, Cat=#cat{}) ->
	Pid ! {return, Cat},
	ok.

close_shop(Pid) ->
	Ref = erlang:monitor(process, Pid),
	Pid ! {self(), Ref, teminate},
	receive
		{Ref, ok} ->
			erlang:demonitor(Ref, [flush]),
			ok;
		{'DOWN', Ref, process, Pid, Reason} ->
			erlang:error(Reason)
	after 5000 ->
		erlang:error(timeout)
	end.
	
%% Server func
init() ->
	loop([]).

loop(Cats) ->
	receive
		% receive from client, this pattern is the same as the 14th line
		% this Pid is 15th line's self(), that is client's Pid,
		{Pid, Ref, {order, Name, Color, Description}} ->
			if Cats =:= [] ->
				% received a cat request, then make_cat()
				Pid ! { Ref, make_cat(Name, Color, Description)},
				loop(Cats);

			Cats =/= [] -> % got to empty the stock 
				Pid ! {Ref, hd(Cats)},
				loop(tl(Cats))


			end; % here is ';', not '.'

		{return, Cat=#cat{}} ->
			loop([Cat|Cats]);
		{Pid, Ref, terminate} ->
			Pid ! {Ref, ok},
			terminate(Cats);

		Unknown ->
			%% do some loggin here too
			io:format("Unknown message:~p~n", [Unknown]),
			loop(Cats)
	end.

make_cat(Name, Col, Desc) ->
	% use #cat{} to make a cat
	#cat{name=Name, color=Col, description=Desc}.

terminate(Cats) ->
	[io:format("~p was set free.~n", [C#cat.name]) || C <- Cats],
	ok.

