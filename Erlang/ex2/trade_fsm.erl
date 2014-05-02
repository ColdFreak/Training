-module(trade_fsm).
-behaviour(gen_fsm).

-record(state, {name="",
				other,
				ownitems=[],
				otheritems=[],
				monitor,
				from}).

% このプログラムのメインのことは
% メッセージを送るのではなく
% 一つのプロセスのステータスが
% どんどん変わっていく
start(Name) ->
	% Create a stand-alone gen_fsm process, i.e. a gen_fsm which 
	% is not part of a supervision tree and thus 
	% has no supervisor
	gen_fsm:start(?MODULE, [Name], []).

% Nameは例えば"Carl"
start_link(Name) ->
	% Creates a gen_fsm process as part of a supervision tree.
	% The function should be called, directly or indirectly,
	% by the supervisor.
	% It will among other things, ensure that the gen_fsm is linked to 
	% the supervisor
	% この関数は{ok, Pid}のような結果が戻ってくる。
	gen_fsm:start_link(?MODULE, [Name], []).

% ask for a begin session. Returns when if the other accepts.
% other cannot reject
trade(OwnPid, OtherPid) ->
	% sync_send_event(FsmRef, Event, Timeout) -> Reply
	% sends an event to the gen_fsm FsmRef and 
	% waits until a reply arrives or a timeout occurs
	% OwnPidはOtherPidとnegotiateしたい、
	% 自分のFSMに送った
	gen_fsm:sync_send_event(OwnPid, {negotiate, OtherPid}, 30000).

% Accept someone's trade offer.
% send_event(FsmRef, Event) -> ok
% Sends an event asynchronously to 
% the gen_fsm FsmRef and returns ok immediately. 
accept_trade(OwnPid) ->
	gen_fsm:sync_send_event(OwnPid, accept_negotiate).

% send an item on the table to be traded
make_offer(OwnPid, Item) ->
	% send_event(FsmRef, Event) -> ok
	% Sends an event asynchronously to the gen_fsm 
	% FsmRef and returns ok immediately. 
	% The gen_fsm will call Module:StateName/2 to 
	% handle the event, where StateName is the name
    % of the current state of the gen_fsm.
	
	% Module:StateName(Event, From, StateData) -> Result
	% 
	gen_fsm:send_event(OwnPid, {make_offer, Item}).

% Cancel trade offer
retract_offer(OwnPid, Item) ->

	gen_fsm:send_event(OwnPid, {retract_offer, Item}).

% Mention that you're ready for a trade. 
% When the other player also declares being ready
% the trade is done
ready(OwnPid) ->
	gen_fsm:sync_send_event(OwnPid, ready, infinity).


% Cancel the transaction
cancel(OwnPid) ->
	gen_fsm:sync_send_all_state_event(OwnPid, cancel).


init(Name) ->
	{ok, idle, #state{name=Name}}.


% Ask the other FSM's Pid for a trade session
% As soon as the ask_negotiate message are sent,
% FSM switch to idle_wait state.
ask_negotiate(OtherPid, OwnPid) ->
	gen_fsm:send_event(OtherPid, {ask_negotiate, OwnPid}).

% Forward the client message accepting the transaction
accept_negotiate(OtherPid, OwnPid) ->
	gen_fsm:send_event(OtherPid, {accept_negotiate, OwnPid}).

% forward a client's offer
do_offer(OtherPid, Item) ->
	gen_fsm:send_event(OtherPid, {do_offer, Item}).

% forward a client's offer cancellation
undo_offer(OtherPid, Item) ->
	gen_fsm:send_event(OtherPid, {undo_offer, Item}).


% Ask the other size if he's ready to trade
are_you_ready(OtherPid) ->
	gen_fsm:send_event(OtherPid, are_you_ready).

% Reply that the side is not ready to trade
% i.e. is not in 'wait' state.
not_yet(OtherPid) ->
	gen_fsm:send_event(OtherPid, not_yet).

% Tells the other fsm that the user is currently waiting
% for the ready state. State should transition to 'ready'
am_ready(OtherPid) ->
	gen_fsm:send_event(OtherPid, 'ready!').

% Acknowledge that the fsm is in a ready state.
ack_trans(OtherPid) ->
	gen_fsm:send_event(OtherPid,ack).

% ask if ready to commit
ask_commit(OtherPid) ->
	gen_fsm:sync_send_event(OtherPid, ask_commit).

% begin the synchronous commit
do_commit(OtherPid) ->
	gen_fsm:sync_send_event(OtherPid, do_commit).

% Send players a notices. This could be messages 
% to their clients
% but for our purposes, outputting to the shell
% is enough.
notice(#state{name=N}, Str, Args) ->
	io:format("~s: "++Str++"~n", [N|Args]).

% log unexpected messages
unexpected(Msg, State) ->
	io:format("~p received unknown event ~p while in state ~p~n", [self(), Msg, State]).


idle({ask_negotiate, OtherPid}, S=#state{}) ->
	Ref = monitor(process, OtherPid),
	notice(S, "~p asked for a trade negotiation", [OtherPid]),
	{next_state, idle_wait, S#state{other=OtherPid, monitor=Ref}};

idle(Event, Data) ->
	unexpected(Event, idle),
	{next_state, idle, Data}.

% our own client asks the FSM to contact another player for a trade
% it will send a synchronous event. 
idle({negotiate, OtherPid}, From, S=#state{}) ->
	ask_negotiate(OtherPid, self()),
	notice(S, "asking user ~p for a trade", [OtherPid]),
	Ref = monitor(process, OtherPid),
	{next_state, idle_wait, S#state{other=OtherPid, monitor=Ref, from=From}};

idle(Event, _From, Data) ->
	unexpected(Event, idle),
	{next_state, idle, Data}.

idle_wait({ask_negotiate, OtherPid}, S=#state{other=OtherPid}) ->
	gen_fsm:reply(S#state.from,ok),
	notice(S, "starting negotiation", []),
	{next_state, negotiate,S};

%The other side has acceted our offer. 
% Move to negotiate state
idle_wait({accet_negotiate, OtherPid}, S=#state{other=OtherPid}) ->
	gen_fsm:reply(S#state.from, ok),
	notice(S, "starting negotiation", []),
	{next_state, negotiate, S};
idle_wait(Event, Data) ->
	unexpected(Event, idle_wait),
	{next_state, idle_wait, Data}.

idle_wait(accept_negotiate, _From, S=#state{other=OtherPid}) ->
	accept_negotiate(OtherPid,self()),
	notice(S, "accepting negotiation", []),
	{reply, ok, negotiate, S};
idle_wait(Event, _From, Data) ->
	unexpected(Event, idle_wait),
	{next_state, idle_wait, Data}.

% adds an item to an item list
add(Item, Items) ->
	[Item | Items].

% remove an item from an item list
remove(Item, Items) ->
	Items -- [Item].

% offering and removing items
negotiate({make_offer, Item}, S=#state{ownitems=OwnItems}) ->
	do_offer(S#state.other, Item),
	notice(S, "offering ~p", [Item]),
	{next_state, negotiate, S#state{ownitems=add(Item,OwnItems)}};

% Own side retracting an item offer
negotiate({retract_offer, Item},S=#state{ownitems=OwnItems}) ->
	undo_offer(S#state.other, Item),
	notice(S, "cancelling offer on ~p", [Item]),
	{next_state, negotiate, S#state{ownitems=remove(Item, OwnItems)}};

% other side offering an item
negotiate({do_offer, Item}, S=#state{otheritems=OtherItems}) ->
	notice(S, "other player offring ~p", [Item]),
	{next_state, negotiate, S#state{otheritems=add(Item, OtherItems)}};

% other side retracting an item ofer
negotiate({undo_offer, Item}, S=#state{otheritems=OtherItems}) ->
	notice(S, "Other player cancelling offer on ~p", [Item]),
	{next_state, negotiate, S#state{otheritems=remove(Item, OtherItems)}};

negotiate(are_you_ready, S=#state{other=OtherPid}) ->
	io:format("Other user ready to trade.~n"),
	notice(S, "Other user ready to transfer goods:~nYou get ~p, The other side gets ~p", [S#state.otheritems, S#state.ownitems]),
	not_yet(OtherPid),
	{next_state, negotiate, S};
negotiate(Event, Data) ->
	unexpected(Event, negotiate),
	{next_state, negotiate, Data}.


code_change(_OldVsn, StateName, Data, _Extra) ->
	{ok, StateName, Data}.

% transaction completed
terminate(normal, ready, S=#state{}) ->
	notice(S, "FSM leaving.", []);
terminate(_Reason, _StateName, _StateData) ->
	ok.
