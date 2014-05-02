%このモジュールはコールバックには並行処理やプロセス
% 生成、メッセージ送受信、登録に関するコードは一つ
% もない、コールバックは純粋な逐次コードで、それ以外
% には何もない
% これはベースになっている並行処理モデルについて何も
% 何も分かっていなくてもクライアントサーバモデルを
% 書けるということだ。
-module(name_server).
-export([init/0, add/2, whereis/1, handle/2]).
-import(server2, [rpc/2]).

% クライアントルーチン
add(Name, Place) ->
	% rpcは統一管理のような気がする
	% 誰に何を送るか
	rpc(name_server, {add, Name, Place}).
% add, whereisのような問い合わせは
% 全部server1のなかのrpcを呼び出して
% server1から、だれに問い合わせしたらいいか決める
whereis(Name) ->
	rpc(name_server, {whereis, Name}).

init() ->
	dict:new().

handle({add, Name, Place}, Dict) ->
	{ok, dict:store(Name, Place, Dict)};
handle({whereis, Name}, Dict) ->
	{dict:find(Name, Dict), Dict}.
