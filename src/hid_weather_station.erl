%%
%% Read wether station data
%%

-module(hid_weather_station).

-compile(export_all).

-define(END_MARK, 16#20).
-define(READ_COMMAND, 16#A1).
-define(WRITE_COMMAND, 16#A0).
-define(WRITE_COMMAND_WORD, 16#A2).
-define(DATA_START, 16#0100).

open() ->
    hid:open(16#1941, 16#8021).

close(Port) ->
    hid:close(Port).

%% Read a 32 byte block of data
read_block(Port, Address) ->
    case hid:write(Port, <<?READ_COMMAND,Address:16,?END_MARK,
			   ?READ_COMMAND,Address:16,?END_MARK>>) of
	{ok,8} ->
	    hid:read(Port, 32);
	Error ->
	    Error
    end.

read_data(Port, Address) ->
    BlockAddress = Address band (bnot 31),
    {ok, Block} = read_block(Port, BlockAddress),
    Offset = Address - BlockAddress,
    <<_:Offset/binary, Data:16/binary, _/binary>> = Block,
    {ok, Data}.

%% read a sample
read(Port, Address) ->
    {ok,Data} = read_data(Port, Address),
    decode_WH1080(Data).

read_current_pos(Port) ->
    {ok,Data} = read_data(Port, 16),
    decode_current_pos(Data).

read_fixed_16(Port) ->
    {ok,Data} = read_data(Port, 16),
    decode_fixed_16(Data).

%% The data is stored in little endian (and lsb bit order!!!)
decode_WH1080(<<Delay:8, HumIn:8, TempIn:16/little-signed, 
		HumOut:8, TempOut:16/little-signed, AbsPressure:16/little,
		WindAveL:8, WindGustL:8, WindGustH:4, WindAveH:4, WindDir:8,
		Rain:16/little, Status:8>>) ->
    WindAve = WindAveL + (WindAveH bsl 8),    %% lsb bit order
    WindGust = WindGustL + (WindGustH bsl 8), %% lsb bit order
    [{delay,Delay},{hum_in,HumIn},{temp_in,TempIn*0.1},
     {hum_out,HumOut}, {temp_out, TempOut*0.1}, {abs_pressure,AbsPressure*0.1},
     {wind_ave, WindAve*0.1}, {wind_gust,WindGust*0.1}, {wind_dir,WindDir},
     {rain, Rain*0.3}, {status,Status}].

decode_WH3080(<<Delay:8, HumIn:8, TempIn:16/little-signed, 
		HumOut:8, TempOut:16/signed, AbsPressure:16/little,
		WindAveL:8, WindGustL:8, WindGustH:4, WindAveH:4, WindDir:8,
		Rain:16/little,Status:8,
		Illumninace:24/little,Uv:8>>) ->
    WindAve = WindAveL + (WindAveH bsl 8),
    WindGust = WindGustL + (WindGustH bsl 8),
    [{delay,Delay},{hum_in,HumIn},{temp_in,TempIn*0.1},
     {hum_out,HumOut}, {TempOut*0.1}, {abs_pressure,AbsPressure*0.1},
     {wind_ave, WindAve*0.1}, {wind_gust,WindGust*0.1}, {wind_dir,WindDir},
     {rain, Rain*0.3}, {status,Status}, {illuminance,Illumninace*0.1},
     {uv, Uv}].

decode_fixed_16(<<ReadPeriod:8, Settings1:8,Settings2:8,
		  Display1:8, Display2:8,
		  Alarm1:8, Alarm2:8, Alarm3:8,
		  TimeZone:8/signed, _:8,
		  DataChanged:8,
		  DataCount:16/little,
		  Display3:8,
		  CurrentPos:16/little>>) ->
    [{read_period,ReadPeriod},
     {settings1,Settings1},{settings2,Settings2},
     {display1,Display1},{display2,Display2},{display3,Display3},
     {alarm1,Alarm1},{alarm2,Alarm2},{alarm3,Alarm3},
     {time_zone,TimeZone}, {data_changed,DataChanged},
     {data_count,DataCount}, {current_pos, CurrentPos}].

decode_current_pos(<<_:14/binary, CurrentPos:16/little>>) ->
    [{current_pos, CurrentPos}].
