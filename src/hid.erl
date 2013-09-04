%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2013, Tony Rogvall
%%% @doc
%%%    HIDAPI interface
%%% @end
%%% Created : 31 Aug 2013 by Tony Rogvall <tony@rogvall.se>

-module(hid).

-export([enumerate/0, enumerate/2]).
-export([open/2, open/3, open_path/1]).	 
-export([close/1]).
-export([get_manufacturer_string/1]).
-export([get_product_string/1]).
-export([get_serial_number_string/1]).
-export([get_indexed_string/2]).
-export([read/2, read/3, write/2]).
-export([get_feature_report/3, send_feature_report/3]).
-export([setopts/2]).
-export([get_report_descriptor/1]).

-define(CMD_ENUMERATE,                1).
-define(CMD_OPEN,                     2).
-define(CMD_OPEN_PATH,                3).
-define(CMD_WRITE,                    4).
-define(CMD_SET_BUFFER,               5).
-define(CMD_SET_ACTIVE,               6).
-define(CMD_SEND_FEATURE_REPORT,      7).
-define(CMD_GET_FEATURE_REPORT,       8).
-define(CMD_CLOSE,                    9).
-define(CMD_GET_MANUFACTURER_STRING,  10).
-define(CMD_GET_PRODUCT_STRING,       11).
-define(CMD_GET_SERIAL_NUMBER_STRING, 12).
-define(CMD_GET_INDEXED_STRING,       13).
-define(CMD_SET_DEBUG,                14).
-define(CMD_GET_REPORT_DESCRIPTOR,    15).

-define(is_uint8(X), (((X) band (bnot 16#ff)) =:= 0)).
-define(is_uint16(X), (((X) band (bnot 16#ffff)) =:= 0)).
-define(is_uint32(X), (((X) band (bnot 16#ffffffff)) =:= 0)).

-define(MAX_BUFFER, 16#ffffffff).  %% just a big buffer

enumerate() ->
    enumerate(0,0).

enumerate(Vid, Pid) when ?is_uint16(Vid), ?is_uint16(Pid) ->
    case open_() of
	{ok,Port} ->
	    try call(Port, ?CMD_ENUMERATE, <<Vid:16, Pid:16>>) of
		{ok,CallRef} ->
		    receive
			{enumeration,Port,CallRef,List} ->
			    {ok,List}
		    after 0 ->
			    {error,internal}
		    end;
		Error -> Error
	    catch
		error:Reason -> {error,Reason}
	    after
		close_(Port)
	    end;
	Error ->
	    Error
    end.

get_manufacturer_string(Port) when is_port(Port) ->
    call(Port, ?CMD_GET_MANUFACTURER_STRING, []).

get_product_string(Port) when is_port(Port) ->
    call(Port, ?CMD_GET_PRODUCT_STRING, []).

get_serial_number_string(Port) when is_port(Port) ->
    call(Port, ?CMD_GET_SERIAL_NUMBER_STRING, []).

get_report_descriptor(Port) when is_port(Port) ->
    call(Port, ?CMD_GET_REPORT_DESCRIPTOR, []).

get_indexed_string(Port, Ix) when  is_port(Port), is_integer(Ix) ->
    call(Port, ?CMD_GET_INDEXED_STRING, <<Ix:32/signed>>).

get_feature_report(Port, Report, Size) when is_port(Port),
					    ?is_uint8(Report),
					    ?is_uint32(Size) ->
    call(Port, ?CMD_GET_FEATURE_REPORT, <<Report,Size:32>>).

send_feature_report(Port, Report, Data) when is_port(Port),
					     ?is_uint8(Report),
					     is_binary(Data) ->
    call(Port, ?CMD_SEND_FEATURE_REPORT, <<Report,Data/binary>>).

open(Vid, Pid) when ?is_uint16(Vid), ?is_uint16(Pid) ->
    open(Vid, Pid, "").

open(Vid, Pid, Serial) when ?is_uint16(Vid), ?is_uint16(Pid), is_list(Serial) ->
    case open_() of
	{ok, Port} ->
	    Args = <<Vid:16, Pid:16, (list_to_binary(Serial))/binary>>,
	    case call(Port, ?CMD_OPEN, Args) of
		ok -> {ok,Port};
		Error ->
		    close_(Port),
		    Error
	    end;
	Error ->
	    Error
    end.

open_path(Path) when is_list(Path) ->
    case open_() of
	{ok, Port} ->
	    Args = list_to_binary(Path),
	    case call(Port, ?CMD_OPEN_PATH, Args) of
		ok -> {ok,Port};
		Error -> 
		    close_(Port),
		    Error
	    end;
	Error ->
	    Error
    end.    

close(Port) when is_port(Port) ->
    call(Port, ?CMD_CLOSE, []),
    close_(Port).

read(Port, Size) when is_port(Port), is_integer(Size), Size > 0 ->
    setopts(Port, [{buffer,Size},{active,once}]),
    receive
	{hid, Port, Data} ->
	    setopts(Port, [{buffer,?MAX_BUFFER}]),
	    {ok, Data}
    end.

read(Port, Size, infinity) when is_port(Port), ?is_uint32(Size) ->
    read(Port,Size);
read(Port, Size, Tmo) when is_port(Port), ?is_uint32(Size),
			   ?is_uint32(Tmo) ->
    setopts(Port, [{buffer,Size},{active,once}]),
    receive
	{hid, Port, Data} ->
	    setopts(Port, [{buffer,?MAX_BUFFER}]),
	    {ok, Data}
    after
	Tmo ->
	    setopts(Port, [{buffer,?MAX_BUFFER},{active,false}]),
	    receive
		{hid,Port,Data} -> %% got some at the last moment
		    {ok,Data}
	    after 0 ->
		    {error, timeout}
	    end
    end.

write(Port, Data) when is_port(Port), is_binary(Data) ->
    write_(Port, Data).

setopts(Port, [{active,true}|Opts]) when is_port(Port) ->
    call(Port, ?CMD_SET_ACTIVE, <<1>>),
    setopts(Port, Opts);
setopts(Port, [{active,false}|Opts]) when is_port(Port) ->
    call(Port, ?CMD_SET_ACTIVE, <<0>>),
    setopts(Port, Opts);
setopts(Port, [{active,once}|Opts]) when is_port(Port) ->
    call(Port, ?CMD_SET_ACTIVE, <<2>>),
    setopts(Port, Opts);
setopts(Port, [{buffer,Size}|Opts]) when is_port(Port), is_integer(Size),
					 Size > 0 ->
    call(Port, ?CMD_SET_BUFFER, <<Size:32>>),
    setopts(Port, Opts);
setopts(Port, [{debug,Level}|Opts]) when is_port(Port), is_integer(Level) ->
    call(Port, ?CMD_SET_DEBUG, <<Level:32>>),
    setopts(Port, Opts);

setopts(_Port, []) ->
    ok.

write_(Port, Data) ->
    call(Port, ?CMD_WRITE, Data).

open_() ->
    Driver = "hid_drv", 
    case erl_ddll:load_driver(code:priv_dir(hid), Driver) of
	ok ->
	    Port = erlang:open_port({spawn_driver, Driver},[binary]),
	    {ok,Port};
	E={error,Reason} ->
	    io:format("~s\n", [erl_ddll:format_error(Reason)]),
	    E
    end.

close_(Port) when is_port(Port) ->
    erlang:port_close(Port).

call(Port, Cmd, Data) ->
    case erlang:port_control(Port, Cmd, Data) of
	<<0>> ->
	    ok;
	<<255,E/binary>> -> 
	    {error, erlang:binary_to_atom(E, latin1)};
	<<1,Y>> -> {ok,Y};
	<<2,Y:16/native-unsigned>> -> {ok, Y};
	<<4,Y:32/native-unsigned>> -> {ok, Y};
	<<8,Y:64/native-unsigned>> -> {ok, Y};
	<<3,Return/binary>> -> {ok,[C || <<C:32>> <= Return]};
	<<5,Return/binary>> -> {error,[C || <<C:32>> <= Return]};
	<<7,Reply/binary>> ->   {ok, Reply}
    end.
