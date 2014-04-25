% `使い方
% Pid = sawn(fun area_server0:loop/0).
% Pid ! {rectangle, 34, 25}.
-module(area_server0).
-export([loop/0]).

loop() ->
	% プロセスは受信するとマッチングしたものの面積を返す
	receive
		{rectangle, Width, Ht} ->
			io:format("Area of rectangle is ~p~n", [Width*Ht]),
			loop();
		{circle, R} ->
			io:format("Area of circle is ~p~n", [3.14159*R*R]),
			loop();
		Other ->
			io:format("I dont' know what the area of a ~p~n", [Other]),
			loop()
	end.
