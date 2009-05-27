-module(connection).
-compile(export_all).
-behaviour(gen_server).
-define(settings_file, "eircd.conf").
-define(message_file, "messages.conf").

start() ->
	gen_server:start_link(?MODULE, [], []).

stop() ->
	gen_server:cast(?MODULE, stop).

init(Args) ->
	{ok, initialized}.

handle_cast(Socket, State) ->
	loop(Socket).

loop(Socket) ->
inet:setopts(Socket,[{active,once}]),
        receive
		{tcp, Socket, Data} ->
			%io:format("Received: ~p", [Data]),
			ListData = string:tokens(Data, ":\r\n "),
			parse_message(Socket, ListData),
			loop(Socket);
		{internal, Data} ->
			%io:format("Received internal: ~p", [Data]),
			ListData = string:tokens(Data, ":\r\n "),
			parse_internal_message(Socket, ListData),
			loop(Socket)
        end.

parse_message(Socket, ["NICK", Nick|_]) ->
	%io:format("~p",[Nick]),
	case db:add_temp_user(Nick, self()) of
		{atomic, ok} -> ok;
		_ -> gen_tcp:send(Socket, ":" ++ settings_manager:get_setting(servername) ++ settings_manager:get_message("ERR_NICKNAMEINUSE") ++ " * " ++ Nick ++ " :Nickname is already in use.\r\n")
	end;

parse_message(Socket, ["USER", Nick, User, Host, RealName|_]) ->
	%io:format("~p",[RealName]),	
	case db:get_user_from_pid(self()) of
		Nick -> io:format("~p",[RealName]), gen_tcp:send(Socket, ":" ++ settings_manager:get_setting(servername) ++ " " ++ settings_manager:get_message("RPL_WELCOME") ++ " " ++ Nick ++ " : " ++ settings_manager:get_setting(serverdesc) ++ ", " ++ Nick ++ "!" ++ User ++ "@" ++ Host ++ "\r\n"), db:add_user({self(),Nick, Host, RealName, User});
		_ -> gen_tcp:close(Socket)
	end;

parse_message(Socket, ["PRIVMSG", Receiver|Msg]) ->
	%io:format("~p",[Msg]),
	Message = parse_message_text(Msg),
	case db:get_user_pid(Receiver) of
		{fail, no_such_user} -> gen_tcp:send(Socket, "No such user\r\n");
		Pid ->			
			[{client,SenderPid, SenderNick, SenderHost, SenderName, SenderUser}] = db:get_user(self()), 
			Pid ! {internal, lists:concat([":" , SenderNick , "!" , SenderUser , "@" , SenderHost , " PRIVMSG " , Receiver , " :" , Message , "\r\n"])}
	end;

parse_message(Socket, ["QUIT", QuitMsg|_]) ->
	[{client,_Pid, Nick, _Host, _Name, _User}] = db:get_user(self()),
	db:remove_temp_user(self()),
	db:delete_user(Nick),
	gen_tcp:close(Socket);

parse_message(Socket, ["JOIN", Channel|_]) ->
	[{client,_Pid, Nick, _Host, _Name, _User}] = db:get_user(self()),
		case db:check_channel(Channel) of
			{fail, no_such_channel} -> 
				db:add_channel({Channel, "", "", Nick}),
				db:add_chanuser({Nick, Channel});
			_ -> 	
				db:add_chanuser({Nick, Channel})
		end,
	Users = parse_message_text(db:get_chanusers(Channel)),
	gen_tcp:send(Socket, ":" ++ settings_manager:get_setting(servername) ++ " " ++ settings_manager:get_message("RPL_NAMREPLY") ++ " " ++ Nick ++ " = " ++ Channel ++ " : " ++ Users ++ "\r\n");

parse_message(Socket, Rest) -> ok.

parse_internal_message(Socket, [SenderHost, "PRIVMSG", Receiver|Msg]) ->
	Message = parse_message_text(Msg),
	gen_tcp:send(Socket, lists:concat([":" , SenderHost , " PRIVMSG " , Receiver , " :" , Message , "\r\n"])).

parse_message_text([FirstWord|[]]) ->
	FirstWord;
parse_message_text([FirstWord|Rest]) ->
	FirstWord ++ " " ++ parse_message_text(Rest).

