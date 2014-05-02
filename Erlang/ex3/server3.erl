-module(server3).
-export([start/2, rpc/2, swap_code/2]).

start(Name, Mod) ->
	register(Name, spawn(fun() -> loop(Name, Mod, Mod:init()) end)).


% you can upgrade a server code withou
% shutting it down, 
% or fall back to former implementation
% if sth does not work as expected
swap_code(Name, Mod) ->
	rpc(Name, {swap_code, Mod}).

rpc(Name, Request) ->
	Name ! {self(), Request},
	receive
		{Name, crash} ->
			exit(rpc);
		{Name, ok, Response} ->
			Response
	end.

loop(Name, Mod, OldState) ->
	receive
		{From, {swap_code, NewCallBackMod}} ->
			From ! {Name, ack},
			loop(Name, NewCallBackMode, OldState);
		{From, Request} ->
			{Response, NewState} = Mod:handle(Request, OldState),
			From ! {Name, Response},
			loop(Name, Mod, NewState)
	end.



