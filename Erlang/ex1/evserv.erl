%% Event server
-module(evserv).
-compile(export_all).

% レコードstateはclientsという属性があって
% このclientsなかにRefとpidのKey-Valueは複数ある
% つまり複数のクライアントのこと
-record(state, {events,    %% list of #event{} records
                clients}). %% list of Pids

-record(event, {name="",
                description="",
                pid,
                timeout={{1970,1,1},{0,0,0}}}).

%%% User Interface

start() ->
    register(?MODULE, Pid=spawn(?MODULE, init, [])),
    Pid.

start_link() ->
    register(?MODULE, Pid=spawn_link(?MODULE, init, [])),
    Pid.

terminate() ->
    ?MODULE ! shutdown.

init() ->
    %% Loading events from a static file could be done here.
    %% You would need to pass an argument to init (maybe change the functions
    %% start/0 and start_link/0 to start/1 and start_link/1) telling where the
    %% resource to find the events is. Then load it from here.
    %% Another option is to just pass the event straight to the server
    %% through this function.
    loop(#state{events=orddict:new(),
                clients=orddict:new()}).

% クライアントはまずイベントサーバにsubscribeする
% クライアントは自分のPidとRefw
subscribe(Pid) ->
    Ref = erlang:monitor(process, whereis(?MODULE)),
    ?MODULE ! {self(), Ref, {subscribe, Pid}},
    receive
        {Ref, ok} ->
            {ok, Ref};
        {'DOWN', Ref, process, _Pid, Reason} ->
            {error, Reason}
    after 5000 ->
        {error, timeout}
    end.

% クライアントはsubscribeをした後、add_eventを呼び出すことができる
% 一つのクライアントPid一つしかないが、subscribe、addなといろんな操作ができるから
% その操作ごとにユニークのRefを送って、返信をもらって来たときに、Refの照合で
% 確かにその操作の返事ということを確認できる
add_event(Name, Description, TimeOut) ->
    Ref = make_ref(),
    ?MODULE ! {self(), Ref, {add, Name, Description, TimeOut}},
    receive
        {Ref, Msg} -> Msg
	% 5秒後返信がないと、add_eventが不成功で,timeout
    after 5000 ->
        {error, timeout}
    end.

add_event2(Name, Description, TimeOut) ->
    Ref = make_ref(),
    ?MODULE ! {self(), Ref, {add, Name, Description, TimeOut}},
    receive
        {Ref, {error, Reason}} -> erlang:error(Reason);
        {Ref, Msg} -> Msg
    after 5000 ->
        {error, timeout}
    end.

cancel(Name) ->
    Ref = make_ref(),
    ?MODULE ! {self(), Ref, {cancel, Name}},
    receive
        {Ref, ok} -> ok
    after 5000 ->
        {error, timeout}
    end.

listen(Delay) ->
    receive
        M = {done, _Name, _Description} ->
            [M | listen(0)]
    after Delay*1000 ->
        []
    end.

%%% The Server itself
% イベントサーバはクライアントからの注文を処理する
% 
loop(S=#state{}) ->
    receive
		% サーバはクライアントからsubscribeのリクエストを受ける
		% クライアントを監視する,つまりクライアントとサーバーお互いに監視しあう
		% 
        {Pid, MsgRef, {subscribe, Client}} ->
            Ref = erlang:monitor(process, Client),
			% orddict:store(Key, Value, Orddict1) -> Orddict2
			% RefとClient(Pid)を関連する！subscribeが来たら、新しいクライアントがきたの
			% と一緒で、そのクライアントのRefとClient(Pid)を辞書に入れる
            NewClients = orddict:store(Ref, Client, S#state.clients),
			% 新しいクライアントを登録したら、そのクライアントにokのメッセージを送る
            Pid ! {MsgRef, ok},
			% そして、このサーバはこの新しいクライアントのために動いている
            loop(S#state{clients=NewClients});
		% add_eventが呼び出されて、まずはTimeOutの形は正しいかはチェックする

        {Pid, MsgRef, {add, Name, Description, TimeOut}} ->
            case valid_datetime(TimeOut) of

                true ->
					% イベントを登録するという操作は
					% サーバevent:start_linkを呼び出して、
					% start_link()の中にspawn_linkを呼び出す。
					% spawn_linkはイベントのPidを生成する
					% サーバは自分のPid(self())をとイベントのPidとリンクする
                    EventPid = event:start_link(Name, TimeOut),
					% そして、イベントの情報はすでに持っているので
					% store関数を呼び出して、NameとNameに関する情報
					% を登録する
					% この登録は新規クライアントの登録の内容と少し
					% 違う、クライアントの登録はRefとクライアントのPidを関連づけで

					%  レコードstateのeventsの属性の中に
					% イベントを登録する
					% このアプリは一人のユーザしかイベントを登録できないみたい
                    NewEvents = orddict:store(Name,
                                              #event{name=Name,
                                                     description=Description,
                                                     pid=EventPid,
                                                     timeout=TimeOut},
                                              S#state.events),
					% 登録したら,add_eventにメッセージを送る
                    Pid ! {MsgRef, ok},

                    loop(S#state{events=NewEvents});
                false ->
                    Pid ! {MsgRef, {error, bad_timeout}},
                    loop(S)
            end;
		% クライアントがイベント登録のリクエストを送ったあと、キャンセルことができる
		% クライアントがキャンセルフォーマットのメッセージを送ったあと、
		% サーバー側が受け取って、イベント辞書の中にNameという名前のイベントを探す
		% find(Key, Orddict) -> {ok, Value} | error
	    % つまりorddict:find関数の結果は二種類,ValueはRefと関連したPidのことである 	
        {Pid, MsgRef, {cancel, Name}} ->
            Events = case orddict:find(Name, S#state.events) of
                         {ok, E} ->
							 %キャンセルは何のことがというと
							 % サーバがイベントに対して,
							 % 当初monitorで作ったRefに
							 % erlang:demonitorを呼び出す
							 % これ以上監視しない
							 % そのイベントのloop()がcancelを受け取って、ループ終了する。イベントのループ終了すると
							 % このイベントは消滅する。
							 % つまり、イベントのループを終了させたい
								
                             event:cancel(E#event.pid),
							 % そしてeventの辞書からNameというイベントを削除する
                             orddict:erase(Name, S#state.events);
                         error ->
                             S#state.events
                     end,
            Pid ! {MsgRef, ok},
            loop(S#state{events=Events});
        {done, Name} ->
            case orddict:find(Name, S#state.events) of
                {ok, E} ->
                    send_to_clients({done, E#event.name, E#event.description},
                                    S#state.clients),
                    NewEvents = orddict:erase(Name, S#state.events),
                    loop(S#state{events=NewEvents});
                error ->
                    %% This may happen if we cancel an event and
                    %% it fires at the same time
                    loop(S)
            end;
        shutdown ->
            exit(shutdown);
        {'DOWN', Ref, process, _Pid, _Reason} ->
            loop(S#state{clients=orddict:erase(Ref, S#state.clients)});
        code_change ->
            ?MODULE:loop(S);
        {Pid, debug} -> %% used as a hack to let me do some unit testing
            Pid ! S,
            loop(S);
        Unknown ->
            io:format("Unknown message: ~p~n",[Unknown]),
            loop(S)
    end.


%%% Internal Functions
send_to_clients(Msg, ClientDict) ->
    orddict:map(fun(_Ref, Pid) -> Pid ! Msg end, ClientDict).

valid_datetime({Date,Time}) ->
    try
        calendar:valid_date(Date) andalso valid_time(Time)
    catch
        error:function_clause -> %% not in {{Y,M,D},{H,Min,S}} format
            false
    end;
valid_datetime(_) ->
    false.

%% calendar has valid_date, but nothing for days.
%% This function is based on its interface.
%% Ugly, but ugh.
valid_time({H,M,S}) -> valid_time(H,M,S).

valid_time(H,M,S) when H >= 0, H < 24,
                       M >= 0, M < 60,
                       S >= 0, S < 60 -> true;
valid_time(_,_,_) -> false.
