%%
%% Read wether station data
%%

-module(hid_weather_station).

-compile(export_all).

-define(END_MARK, 16#20).
-define(READ_COMMAND, 16#A1).
-define(WRITE_COMMAND, 16#A0).
-define(WRITE_COMMAND_WORD, 16#A2).

open() ->
    hid:open(16#1941, 16#8021).

close(Port) ->
    hid:close(Port).

read_block(Port, Address) ->
    case hid:write(Port, <<?READ_COMMAND,Address:16,?END_MARK,
			   ?READ_COMMAND,Address:16,?END_MARK>>) of
	{ok,8} ->
	    hid:read(Port, 32);
	Error ->
	    Error
    end.
