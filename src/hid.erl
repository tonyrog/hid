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
-define(DEFAULT_TIMEOUT, 100).

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
    read_(Port, Size, <<>>, ?DEFAULT_TIMEOUT).

read(Port, Size, infinity) when
      is_port(Port), is_integer(Size), Size > 0 ->
    read_(Port, Size, <<>>, infinity);
read(Port, Size, Timeout) when
      is_port(Port), is_integer(Size), Size > 0, 
      is_integer(Timeout), Timeout >= 0 ->
    read_(Port, Size, <<>>, Timeout).
    
read_(Port, Size, Acc, Timeout) ->
    setopts(Port, [{buffer,Size},{active,once}]),
    receive
	{hid, Port, Data} ->
	    Size1 = Size - byte_size(Data),
	    Acc1  = <<Acc/binary,Data/binary>>,
	    if Size1 =< 0 ->
		    {ok,Acc1};
	       true ->
		    read_(Port,Size1,Acc1,Timeout)
	    end
    after Timeout ->
	    {error,timeout}
    end.

write(Port, Data) when is_port(Port), is_binary(Data) ->
    write_(Port, Data).

setopts(Port, [{active,true}|Opts]) when is_port(Port) ->
    call(Port, ?CMD_SET_ACTIVE, <<-1:32>>),
    setopts(Port, Opts);
setopts(Port, [{active,false}|Opts]) when is_port(Port) ->
    call(Port, ?CMD_SET_ACTIVE, <<0:32>>),
    setopts(Port, Opts);
setopts(Port, [{active,once}|Opts]) when is_port(Port) ->
    call(Port, ?CMD_SET_ACTIVE, <<1:32>>),
    setopts(Port, Opts);
setopts(Port, [{active,N}|Opts]) when is_port(Port), is_integer(N), N >= -1 ->
    call(Port, ?CMD_SET_ACTIVE, <<N:32>>),
    setopts(Port, Opts);
setopts(Port, [{buffer,Size}|Opts]) when is_port(Port), is_integer(Size),
					 Size > 0 ->
    call(Port, ?CMD_SET_BUFFER, <<Size:32>>),
    setopts(Port, Opts);
setopts(Port, [{debug,Level}|Opts]) when is_port(Port), is_integer(Level) ->
    call(Port, ?CMD_SET_DEBUG, <<Level:32>>),
    setopts(Port, Opts);
setopts(Port, [{debug,Level}|Opts]) when is_port(Port) ->
    if is_integer(Level), Level >= -1, Level =< 7 ->
	    call(Port, ?CMD_SET_DEBUG,<<Level:32>>);
       true ->
	    case Level of
		true       -> call(Port, ?CMD_SET_DEBUG,<<7:32>>);
		false      -> call(Port, ?CMD_SET_DEBUG,<<-1:32>>);
		debug      -> call(Port, ?CMD_SET_DEBUG,<<7:32>>);
		info       -> call(Port, ?CMD_SET_DEBUG,<<6:32>>);
		notice     -> call(Port, ?CMD_SET_DEBUG,<<5:32>>);
		warning    -> call(Port, ?CMD_SET_DEBUG,<<4:32>>);
		error      -> call(Port, ?CMD_SET_DEBUG,<<3:32>>);
		critical   -> call(Port, ?CMD_SET_DEBUG,<<2:32>>);
		alert      -> call(Port, ?CMD_SET_DEBUG,<<1:32>>);
		emergency ->  call(Port, ?CMD_SET_DEBUG,<<0:32>>);
		none      ->  call(Port, ?CMD_SET_DEBUG,<<-1:32>>);
		_ -> ignore
	    end
    end,
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
