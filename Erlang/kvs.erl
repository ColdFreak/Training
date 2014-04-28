-module(kvs).

-export([start/0,store/2, lookup/1]). % start()はloop()を呼び出すから、exportしない

start() ->
	register(kvs, spawn(fun() -> loop() end)).

% クライアントはstoreを呼び出すと、つまりサーバーのkvsに
% {store, Key, Value}のようなパターンを送る.
% storeはrpc()関数がstore()関数をラップした後つけたもの
store(Key, Value) ->
	rpc({store, Key, Value}).

lookup(Key) ->
	rpc({lookup, Key}).


% rpc()関数はQのクライアントからのrequestを受け取る
rpc(Q) ->
	kvs ! {self(), Q}, % kvsに送信、自分のPIDを書き込む

	receive
		{kvs, Reply} -> % サーバーからこの形の返事をもらいたい
			Reply % storeに対する返信は {kvs, true},
	end.

loop() -> % サーバー側
	receive
		{From, {store, Key, Value}} ->
			put(Key, {ok, Value}),
			From ! {kvs, true}, % Keyをprocess dictionaryに追加したら、クライアントに返事する
			loop();
		{From, {lookup, Key}} ->
			From ! {kvs,get(Key)}, 
			loop()  % 関数の最後なので、endの後ろに'.'をつけるだけ、loo()の後ろ'.'いらない
	end.

	
	

