%% file about the x,y,z process events
-module(event).
-export([start/2, start_link/2, cancel/1]).
-export([init/3, loop/1]).
-record(state, {server,
                name="",
                to_go=0}).

%%% Public interface
% event:start("Event", 500).
% サーバからイベントを追加する要求来たら、
% イベントを作成する、そのために、新しいプロセスを
% spawnで作って、initを呼び出して、loop()を実行する
% サーバのほうからイベントにサーバのPidも送る(self()).
start(EventName, DateTime) ->
    spawn(?MODULE, init, [self(), EventName, DateTime]).

start_link(EventName, DateTime) ->
    spawn_link(?MODULE, init, [self(), EventName, DateTime]).

% event:cancel(Pid).
% サーバーからキャンセルのリクエストがくる。
% どういう形でくるかというと
% サーバはまずイベントプロセスを監視する(死んでるか確認)
cancel(Pid) ->
    %% Monitor in case the process is already dead
    Ref = erlang:monitor(process, Pid),
	% イベントにメッセージを送る
	% 送る内容としてはサーバ自身のPid(self())とRefをいれる 
	% なぜRefをいれるかというとRefはmake_ref()で作られている
	% サーバはRefを入れて、Xイベントにキャンセルのリクエストを送ったら
	% Xから{Ref, ok}のメッセージが帰ってくる
	% つまり、このメッセージはXイベントから帰ってきた。
	% Y,Zイベントから帰ってきたじゃない

    Pid ! {self(), Ref, cancel},
    receive
        {Ref, ok} ->
            erlang:demonitor(Ref, [flush]),
            ok;
        {'DOWN', Ref, process, Pid, _Reason} ->
            ok
    end.

%%% Event's innards
init(Server, EventName, DateTime) ->
    loop(#state{server=Server,
                name=EventName,
                to_go=time_to_go(DateTime)}).

%% Loop uses a list for times in order to go around the ~49 days limit
%% on timeouts.
% イベントが作成されたら、サーバーからキャンセルのリクエストを待つか、Timeoutしたら、メッセージを送るだけ
loop(S = #state{server=Server, to_go=[T|Next]}) ->
    receive
		% サーバからのキャンセルリクエスト
        {Server, Ref, cancel} ->
			% okを送ったり、死ぬ
            Server ! {Ref, ok}
    after T*1000 ->
        if Next =:= [] ->
			% timeoutしたら、event processはメッセージを送って死ぬ
			% event serverにメッセージを送るため、event serverのPidを知る必要があって、
			% state.nameという名前のイベントがもう終わったというメッセージを送るため
            Server ! {done, S#state.name};
           Next =/= [] ->
            loop(S#state{to_go=Next})
        end
    end.

%%% private functions
time_to_go(TimeOut={{_,_,_}, {_,_,_}}) ->
    Now = calendar:local_time(),
    ToGo = calendar:datetime_to_gregorian_seconds(TimeOut) -
           calendar:datetime_to_gregorian_seconds(Now),
    Secs = if ToGo > 0  -> ToGo;
              ToGo =< 0 -> 0
           end,
    normalize(Secs).

%% Because Erlang is limited to about 49 days (49*24*60*60*1000) in
%% milliseconds, the following function is used
normalize(N) ->
    Limit = 49*24*60*60,
    [N rem Limit | lists:duplicate(N div Limit, Limit)].
