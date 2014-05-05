-module(sum).
-export([loop/0,sum/1,sum/2]).
loop() ->
	receive
		{From, {sum, N}} ->
			From ! {self(), sum(N)},
			loop();
		{become, Something} ->
			Something()
	end.

% calculate N+(N+1)+...+M
sum(N) ->
	sum(1,N).
sum(N, M) when is_integer(N), is_integer(M), N =<M ->
	% 結果の初期値はMに設定する
	% 例えば、sum(1,3)の時、もし初期値は0に設定したら
	% 1+2までしか足さないから、結果は3になる
	sum(N, M, M ).
	
% 11行目の結果はMに設定する理由は
% NがMになるまで足していくが
% Mになると、ただ足してきた値Accを返すだけで
% Mを足さないから、だから、初期値はMに事前に設定する
sum(M, M, Acc) ->
	Acc;
sum(N,M,Acc) ->
	sum(N+1,M, N+Acc).
