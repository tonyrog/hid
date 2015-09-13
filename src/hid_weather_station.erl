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

read_blocks(_Port, _Address, 0) ->
    {ok, <<>>};
read_blocks(Port, Address, Length) ->
    read_blocks(Port, Address, Length, <<>>).

read_blocks(Port, Address, Length, Acc) when Length > 0 ->
    case read_block(Port, Address) of
	{ok, Block} ->
	    read_blocks(Port, Address+32, Length-32, 
			<<Acc/binary, Block/binary>>);
	Error ->
	    Error
    end;
read_blocks(_Port, _Address, _Length, Acc) ->
    {ok, Acc}.
    

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

%% read fixed data 32 .. 256
read_fixed(Port) ->
    {ok, Data} = read_blocks(Port, 32, 224),
    io:format("fixed len=~w, data=~p\n", [byte_size(Data), hex(Data)]),
    decode_fixed(Data).

%% convert to hex
hex(Bin) when is_binary(Bin) ->
    [ element(I+1,{$0,$1,$2,$3,$4,$5,$6,$7,$8,$9,
		   $a,$b,$c,$d,$e,$f}) || <<I:4>> <= Bin];
hex(List) when is_list(List) ->
    hex(list_to_binary(List));
hex(Int) when is_integer(Int) ->
    integer_to_list(Int, 16).


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

decode_fixed_16(<<ReadPeriod:8, 
		  %% Settings-1: Pressure / Rain / Temp
		  Setting1:1/binary,
		  
		  %% Settings-2: Wind
		  Setting2:1/binary,

		  Display1:8, Display2:8,
		  Alarm1:8, Alarm2:8, Alarm3:8,
		  TimeZone:8/signed, _:8,
		  DataChanged:8,
		  DataCount:16/little,
		  Display3:8,
		  CurrentPos:16/little>>) ->


    <<PressureMMHg:1,PressureInHg:1, PressureHPa:1,
      _:1, _:1,
      RainIn:1, TempOutF:1, TempInF:1>> = Setting1,

    <<_:1, _:1, _:1, WindBft:1, WindMph:1, WindKnot:1,
      WindKmph:1, WindMps:1>> = Setting2,

    WindUnit = if WindBft =:= 1 -> "bft";
		  WindMph =:= 1 -> "mph";
		  WindKnot =:= 1 -> "knot";
		  WindKmph =:= 1 -> "km/h";
		  WindMps =:= 1 -> "mps";
		  true -> ""
	       end,
    TempInUnit = if TempInF =:= 1 -> "F";
		    true -> "C"
		 end,
    TempOutUnit = if TempOutF =:= 1 -> "F";
		    true -> "C"
		 end,

    PressureUnit = if PressureMMHg =:= 1 -> "mmhg";
		      PressureInHg =:= 1 -> "inhg";
		      PressureHPa =:= 1 -> "hPa";
		      true -> ""
		   end,
    RainUnit = if RainIn -> "in";
		  true -> "mm"
	       end,
    [{read_period,ReadPeriod},
     {temp_in_unit, TempInUnit}, {temp_out_unit, TempOutUnit},
     {pressure_unit, PressureUnit}, {wind_unit, WindUnit},
     {rain_unit, RainUnit},
     {display1,Display1},{display2,Display2},{display3,Display3},
     {alarm1,Alarm1},{alarm2,Alarm2},{alarm3,Alarm3},
     {time_zone,TimeZone}, {data_changed,DataChanged},
     {data_count,DataCount}, {current_pos, CurrentPos}].

decode_current_pos(<<_:14/binary, CurrentPos:16/little>>) ->
    [{current_pos, CurrentPos}].

%% read from address 32 .. 256
decode_fixed(<<RelPressure:16/little,  %% 32
	       AbsPressure:16/little,  %% 34
	       LuxWm2Coeff:16/little,  %% 36
	       _:5/binary,             %% 38,39,40,41,42
	       Y1:4,Y2:4,              %% 43
	       M1:4,M2:4,              %% 44
	       D1:4,D2:4,              %% 45
	       H1:4,H2:4,              %% 46
	       Mi1:4,Mi2:4,            %% 47
	       _Alarm:49/binary,       %% 48 .. 96 - fixme
	       _:8,                    %% 97
	       %% More to add
	       _/binary>>) ->
    [{rel_pressure, RelPressure*0.1},
     {abs_pressure, AbsPressure*0.1},
     {lux_wm2_coeff, LuxWm2Coeff*0.1},
     {date_time, {{Y1*10+Y2+2000, M1*10+M2, D1*10+D2},
		  {H1*10+H2,Mi1*10+Mi2,0}}}].

		   
		  
		  
