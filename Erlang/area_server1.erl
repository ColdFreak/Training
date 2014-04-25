-module(area_server1).
-export([loop/0, rpc/2]).

rpc(Pid, Request) ->
	Pid ! {self(), Request},
	receive
		Response ->
			Response
	end.

loop() ->
	% 問題点
	% このクライアントがサーバーからの応答を待っている時に何か
	% 別のプロセスがクライアントにメッセージを送ると
	% クライアントはそのメッセージをサーバからの応答と取り違えてしまう 
	receive
		{From , {rectangle, Width, Ht}} ->
			From ! Width*Ht,
			loop();
		{From ,{circle, R}} ->
			From ! 3.14159*R*R,
			loop();
		{From, Other} ->
			From ! {error, Other},
			loop()
	end.
