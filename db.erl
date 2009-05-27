-module(db).
-compile(export_all).

-include_lib("stdlib/include/qlc.hrl").

-record(temp_user, {pid, nick}).
-record(client, {pid, nick, host, real_name, username}).
-record(channel, {chname, topic, chmodes, chowner}).
-record(chanuser, {pid, channel, usermode = null}).
-record(banlist, {nick, channel, host}).


start() ->
	mnesia:start().

init() ->
	mnesia:create_schema([node()]),
	mnesia:start(),
	mnesia:create_table(temp_user, [{ram_copies, [node()]}, {type, set}, {attributes, record_info(fields, temp_user)}]),
	mnesia:create_table(client, [{ram_copies, [node()]}, {type, set}, {attributes, record_info(fields, client)}]),
	mnesia:create_table(channel, [{ram_copies, [node()]}, {type, set}, {attributes, record_info(fields, channel)}]),
        mnesia:create_table(chanuser, [{ram_copies, [node()]}, {type, bag}, {attributes, record_info(fields, chanuser)}]),
        mnesia:create_table(banlist, [{ram_copies, [node()]}, {type, set}, {attributes, record_info(fields, banlist)}]).
	
stop() ->
	mnesia:stop().

get_user_pid(Nick) ->
	Result = mnesia:dirty_match_object(temp_user, {temp_user, '_', Nick}),
	if
		Result == [] ->
			{fail, no_such_user};
		true ->
			[{temp_user,Pid,Nick}] = Result,
			Pid	
	end.

get_user_from_pid(Pid) ->
	Result = mnesia:dirty_match_object(temp_user, {temp_user, Pid, '_'}),
	if	
		Result == [] ->
			{fail, no_such_user};
		true ->
			[{temp_user,Pid,Nick}] = Result,
			Nick
	end.	

get_temp_user(Nick) ->
	Result = mnesia:dirty_match_object(temp_user, {temp_user, '_', Nick}),
	if
		Result == [] ->
			{fail, no_such_user};
		true ->
			[{temp_user,_Pid,Nick}] = Result,
			Nick
	end.

add_temp_user(Nick, Pid) ->
	TempNick = get_temp_user(Nick),
	if
		TempNick == {fail, no_such_user} ->
			Row = #temp_user{nick = Nick, pid = Pid},
			write_row(Row);
		true -> 
			TempNick
	end.

remove_temp_user(Pid) ->
	TempNick = get_user_from_pid(Pid),
	if
		TempNick == [] ->
			{fail, no_such_nick};
		true ->
			delete({temp_user, Pid})
	end.

%% add_user: Use to add new users to the database
add_user(Pid, Nick, Host, RealName, Username) ->
	Row = #client{pid = Pid, nick = Nick, host = Host, real_name = RealName, username = Username},
	write_row(Row).

%% add_channel: Use to add new channels to the databse
add_channel(Chname, Topic, Chmodes, Chowner) ->
	Row = #channel{chname = Chname, topic = Topic, chmodes = Chmodes, chowner = Chowner},
	write_row(Row).

%% add_chanuser: Use to add new users to a channel in the database.
add_chanuser(Pid, Channel) ->
	Row = #chanuser{pid = Pid, channel = Channel},
	write_row(Row).

%% add_ban: Use to add a new banned user to the database.
add_ban(Nick, Channel, Host) ->
	Row = #banlist{nick = Nick, channel = Channel, host = Host},
	write_row(Row).

%% get_user: Use to get the user information from the database.
%% Reformat return data in this.
get_user(Pid) ->
	Result = do(qlc:q([X || X <- mnesia:table(client), X#client.pid == Pid])),
	if
		Result == [] ->
			{fail, no_such_nick};
		true ->
			Result
	end.

%% get_chanuser: Use to get the channels one user belongs to.
%% Reformat return data in this.
get_chanuser(Pid) ->
	Result = do(qlc:q([X || X <- mnesia:table(chanuser), X#chanuser.pid == Pid])),
	if
		Result == [] ->
			{fail, no_such_nick};
		true ->
			Result
	end.

get_chanusers(Channel) ->
	Result = do(qlc:q([Y#client.nick || X <- mnesia:table(chanuser), X#chanuser.channel == Channel, Y <- mnesia:table(client), X#chanuser.pid == Y#client.pid])),
	if
		Result == [] ->
			{fail, no_such_channel};
		true ->
			Result
	end.

get_chanuser_pids(Channel) ->
	Result = do(qlc:q([X#chanuser.pid || X <- mnesia:table(chanuser), X#chanuser.channel == Channel])),
        if
                Result == [] ->
                        {fail, no_such_channel};
                true ->
                        Result
        end.


check_channel(Channel) ->
	Result = do(qlc:q([X || X <- mnesia:table(channel), X#channel.chname == Channel])),
	if
		Result == [] ->
			{fail, no_such_channel};
		true ->
			Channel
	end.

%% get_channelnames: Use to get ALL channelnames present in the databse.
%% Reformat return data in this.
get_channelnames() ->
	Result = do(qlc:q([X#channel.chname || X <- mnesia:table(channel)])),
	if
		Result == [] ->
			{fail, no_channels_exist};
		true ->
			Result
	end.

%% delete_user: Use to delete a user from the database.
delete_user(Nick) ->
	delete({client, Nick}).

%% delete_channel: Use to delete a channel from the database.
delete_channel(Chname) ->
	delete({channel, Chname}).

%% delete_chanuser: Use to delete a channeluser from the database.
delete_chanuser(Pid, Channel, Usermode) ->
	R = #chanuser{pid = Pid, channel = Channel, usermode = Usermode},
	delete_object(R).

%% delete_object: Use to delete selective data from a table. Supply a complete record.
delete_object(Record) ->
	F = fun() ->
		mnesia:delete_object(Record)
	end,
	
	case mnesia:transaction(F) of
	{atomic, ok} -> {atomic, ok};
	_ -> {fail, could_not_delete_object_from_database}
	end.

%% write_row: Use to write a row to the database. Supply a complete record.
write_row(Row) ->
	F = fun() ->
		mnesia:write(Row)
	end,

	case mnesia:transaction(F) of
	{atomic, ok} -> {atomic, ok};
	_ -> {fail, could_not_write_to_database}
	end.

%% delete: Use to delete a row from a Table. Supply table name and key.
delete({Tab, Key}) ->
	F = fun() ->
		mnesia:delete({Tab, Key})
	end,

	case mnesia:transaction(F) of
	{atomic, ok} -> {atomic, ok};
	_ -> {fail, could_not_delete_from_database}
	end.

%% do: Use to execute queries to the database.
do(Q) ->
    F = fun() -> qlc:e(Q) end,
    {atomic, Val} = mnesia:transaction(F),
    Val.
