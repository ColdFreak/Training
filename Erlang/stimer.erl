-module(stimer).
-compile(export_all).

% Time/1000秒以上経つとFun関数実行される
start(Time, Fun) ->
	spawn(fun() -> timer(Time, Fun) end).

cancel(Pid) ->
	Pid ! cancel.

timer(Time, Fun) ->
	receive
		cancel ->
			void
	after Time ->
		Fun()
	end.

% 8> Pid = stimer:start(6000, fun() -> io:format("time out\n") end).
% <0.54.0>
% 9> Pid ! cancel.
% cancel
% 16> stimer:cancel(Pid).
% cancel
