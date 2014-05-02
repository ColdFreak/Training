-module(server1).
-export([start/2, rpc/2]).

% この基本サーバserver1はひとつのプロセスを生成する
% このプロセスはNameという変数によって各サーバと
% やりとりできる,たとえば、name_server
start(Name, Mod) ->
	register(Name, spawn(fun() -> loop(Name, Mod, Mod:init()) end)).

% > server1:start(name_server, name_server).
% このコマンドでname_serverのなかの操作i(add, whereis)のために
% プロセスを生成した。

% > name_server:add(joe, "at home").
% コマンドを打って、server1のrpcが呼び出される。
% rpcからloopにリクエストを送信する
% loopはパターンマッチングを行った後、
% name_serverの中の具体的な関数を呼び出して
% 処理する
rpc(Name, Request) ->
	Name ! {self(), Request},
	receive
		{Name, Response} ->
			Response
	end.

loop(Name, Mod, State) ->
	receive
		{From, Request} ->
			% 処理したら、最初のdict:new()という
			% Stateが変わるので（一つレコードを追加した)
			% loopの中のStateと違うState1 を使う
			{Response, State1} = Mod:handle(Request, State),
			% Responseという結果もらってきたら
			% Nameというプロセスに返す（rpcに返す）
			% つまり、rpcはリクエストと返事の中枢
			% となっている
			From ! {Name, Response},
			loop(Name, Mod, State1)
	end.


