-module(eircd).
-compile(export_all).
-export([init/1,init/0]).
-behaviour(supervisor).

init() ->
	db:init(),
	start().

start() ->
	start_link(),
	db:start(),
	{ok, ListenSocket} = gen_tcp:listen(settings_manager:get_setting(serverport), [list, {active, false}, {packet, line}]),
	server(ListenSocket).

server(LS) ->
	case gen_tcp:accept(LS) of
		{ok,S} ->
		{ok, Pid} = start_child(),
		gen_tcp:controlling_process(S, Pid),
		gen_server:cast(Pid, S),
		server(LS);
	Other ->
		io:format("accept returned ~w - goodbye!~n",[Other]),
		ok
	end.

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init(Args) ->
    {ok, {{simple_one_for_one, 1, 60},
          [{connection, {connection, start, []},
            permanent, 10000, worker, [connection]}]}}.

start_child() -> 
	supervisor:start_child(?MODULE, []).


	

