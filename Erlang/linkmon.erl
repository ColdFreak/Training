-module(linkmon).
-compile(export_all).

% この関数を呼び出すと
% restarterという監視するプロセスを作る
start_critic() ->
	spawn(?MODULE, restarter, []).

% この監視プロセスは自分をシステムプロセスに変更する
% そしてcritic2プロセスを作る。
%  作ったあと自分とcritic2をリンクする。
restarter() ->
	process_flag(trap_exit, true),
	% restarterプロセスはcriticとリンクする
	Pid = spawn_link(?MODULE, critic,[]),
	register(critic, Pid),
	receive
		{'EXIT', Pid, normal} -> % not a crash
			ok;
		{'EXIT', Pid, shutdown} -> % manual termination, not a crash
			ok;
		% criticはnormalとshutdown以外の原因で
		% 死んで、restarterにメッセージを送って
		% きたら、改めてcriticをspawn_link()する
		{'EXIT', Pid, _} ->
			restarter()
	end.


% judge2関数はメッセージごとにリファレンスをつくる
% そしてメッセージをcritic2プロセスに送る。
judge(Band, Album) ->
	Ref = make_ref(),
	% we use references as unique value to identify
	critic ! {self(),Ref, {Band, Album}},
	receive
		% Pidでメッセージを特定するのではなく
		% Referenceでメッセージを特定する
		% なぜかというと一つのPidでもたくさん
		% のメッセージを送る可能性が高い
		% Refでメッセージと紐付けるべき
		{Ref, Criticism} ->
			Criticism
	after 2000 ->
		timeout
	end.

critic() ->
	receive 
		{From, Ref, {"Rage Against the Turing Machine", "Unit Testify"}} ->
			From ! {Ref , "They are great"};
		{From, Ref, {"System of a Downltime", "Memorize"}} ->
			From ! {Ref, "They're not Johnny Crash but they're good."};
		{From, Ref, {_Band, _Album}} ->
			From ! {Ref, "They are terrible!"}
	end,
	critic().
