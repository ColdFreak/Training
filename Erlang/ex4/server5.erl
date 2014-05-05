-module(server5).
-compile(export_all).

start() ->
	spawn(fun() -> wait() end).

wait() ->
	receive
		% ここのFは別のファイルのなかのloop関数かも
		{become, F} ->
			F()
	end.

rpc(Pid, Q) ->
	Pid ! {self(), Q},
	receive
		{Pid, Reply} ->
			Reply
	end.
