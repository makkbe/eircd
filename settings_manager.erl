-module(settings_manager).
-export([get_message/1, get_setting/1]).
-define(settings_file, "eircd.conf").
-define(message_file, "messages.conf").

get_message(Key) ->
	[H|T] = read_file(?message_file),
	get_setting(Key, H, T).

get_setting(Key) ->
	[H|T] = read_file(?settings_file),
	get_setting(Key, H, T).

get_setting(Key, {Key,Value}, SettingsList) ->
	Value;

get_setting(Key, _List, [H|T]) ->
	get_setting(Key, H, T);

get_setting(_Key, _List, []) ->
	{fail, setting_not_found}.

read_file(File) ->
	case file:consult(File) of
		{ok, SettingsList} -> SettingsList;
		_ -> {fail, could_not_read_file}
	end.
