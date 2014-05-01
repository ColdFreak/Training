-module(dog_fsm).
-export([start/0, squirrel/1, pet/1]).

% 1> Pid = dog_fsm:start().
% Pidを取得することがpet(Pid)とsquirrel(Pid)
% 関数につながる
%　犬はまずbarkの状態から始まる
start() ->
	spawn(fun() -> bark() end).

squirrel(Pid) -> Pid ! squirrel.
pet(Pid) -> Pid ! pet.

bark() ->
	% 犬はとりあえず吠える
	io:format("Dog says: BARK! BARK!~n"),
	receive 
		% 吠えている時にもし撫でてくれたら
		% 尻尾を振る
		pet ->
			wag_tail();

		_ 	->
			io:format("Dog is confused~n"),
			bark()
	after 2000 ->
		% 2秒後何もしてくれないと
		% 吠えつづける
		bark()
	end.

wag_tail() ->
	io:format("Dog wags its tail~n"),
	receive
		pet ->
			sit();
		% 撫でるの以外のことしてくれても拒否する
		% そして尻尾を振り続ける。
		_ ->
			io:format("Dog is confused~n"),
			wag_tail()
	% 何もしてくれなくて、30秒たったら吠える
	after 30000 ->
		bark()
	end.

sit() ->
	io:format("Dog is sitting. Goooood boy!~n"),
	receive
		% 座ってる時にリスを見たら吠える
		squirrel ->
			bark();
		_ ->
			io:format("Dog is confused~n"),
			sit()
	end.


