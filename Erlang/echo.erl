-module(echo).
-export([go/0, loop/0]).

go() ->
	register(echo, spawn(echo, loop, [])),
	echo ! {self(), 'Hello world'},
	receive
		{_Pid, Msg} ->
			io:format("~w~n", [Msg])
	end,
	echo ! stop.


loop() ->
	receive 
		{From, Msg} ->
			From ! {self(), Msg},
			loop();
		stop ->
			true
	end.

%	 11> echo:go().
%	 'Hello world'
%	 stop
