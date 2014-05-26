-module(lib_misc).
-compile(export_all).

% Pidは死んだら、Fun関数で処理する
on_exit(Pid, Fun) ->
	% on_exitはシグナルを受け取って、処理するため
	% プロセスを生成して、システムプロセスに変更する
	% そしてPidにリンクする。
	spawn(fun() ->
				process_flag(trap_exit, true),
				link(Pid),
				receive
					{'EXIT', Pid, Why} ->
						Fun(Why)
				end
			end).

		
