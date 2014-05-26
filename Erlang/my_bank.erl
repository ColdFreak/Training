-module(my_bank).
-behaviour(gen_server).
-compile(export_all).

start() -> gen_server:start_link({local,?MODULE}, ?MODULE, [],[]).
stop() -> gen_server:call(?MODULE, stop).

new(Who) -> gen_server:call(?MODULE, {new, Who}).
deposit(Who, Amount) -> gen_server:call(?MODULE, {add, Who, Amount}).
withdraw(Who, Amount) -> gen_server:call(?MODULE, {remove, Who, Amount}).

init([]) -> {ok, ets:new(?MODULE,[])}.

	
