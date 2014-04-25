-module(area_server2).
-export([loop/0, rpc/2]).

% rpc関数に入るとPidには値が束縛される。そのため、パターン{Pid, Response}
% のPidは束縛されているが、Responseは束縛されていない状態になる.
%
rpc(Pid, Request) ->
	Pid ! {self(), Request},
	receive
		{Pid, Response} ->
			Response
	end.

loop() ->
	% プロセスは受信するとマッチングしたものの面積を返す
	receive
		{From , {rectangle, Width, Ht}} ->
			From ! {self(), Width*Ht},
			loop();
		{From ,{circle, R}} ->
			From ! {self(),3.14159*R*R},
			loop();
		{From, Other} ->
			From ! {self(), {error, Other}},
			loop()
	end.
