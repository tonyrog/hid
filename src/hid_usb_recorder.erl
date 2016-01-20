%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2016, Tony Rogvall
%%% @doc
%%%    Interface to USB Vellman K8047 4-channel recorder
%%% @end
%%% Created : 20 Jan 2016 by Tony Rogvall <tony@rogvall.se>

-module(hid_usb_recorder).

-compile(export_all).

%% [{collection,application,
%% [{input,
%%      [variable],
%%      {hid_item,
%%          {hid_global,65280,0,255,undefined,undefined,undefined,
%%              undefined,8,undefined,8},
%%          [1],
%%          1,8,undefined,undefined,undefined,undefined,undefined,
%%          undefined,[]}},
%%  {output,
%%      [variable],
%%      {hid_item,
%%          {hid_global,65280,0,255,undefined,undefined,undefined,
%%              undefined,8,undefined,8},
%%          [],1,8,undefined,undefined,undefined,undefined,undefined,
%%          undefined,[]}}]}]

-define(VENDOR, 16#10cf).
-define(PRODUCT, 16#8047).
-define(VSCALE, (12/101)).

start() ->
    spawn_link(fun() ->
		       logger_start()
	       end).

start_file() ->
    start_file("valleman.csv").

start_file(Filename) ->
    spawn_link(fun() ->
		       logger_file_start(Filename)
	       end).

open() ->
    hid:open(?VENDOR, ?PRODUCT).

close(Port) ->
    hid:close(Port).

%% Read a 8 byte block of data
read(Port) ->
    hid:read(Port, 8).

read_samples(Port) ->
    case read(Port) of
	{ok, <<T:16/little, V1, V2, V3, V4, _, _>>} ->
	    {ok, {T, V1*?VSCALE, V2*?VSCALE, V3*?VSCALE, V4*?VSCALE}};
	Error ->
	    Error
    end.

logger_file_start() ->
    logger_file_start("velleman.csv").

logger_file_start(Filename) ->
    Tstart = erlang:system_time(milli_seconds),
    case file:open(Filename, [write]) of
	{ok, Fd} ->
	    {ok, Port} = open(),
	    try logger_loop(Port, Fd, Tstart, 1000) of
		ok -> ok
	    after
		file:close(Fd),
		close(Port)
	    end;
	Error ->
	    Error
    end.

logger_start() ->
    {ok, Port} = open(),
    Tstart = erlang:system_time(milli_seconds),
    logger_loop(Port, user, Tstart, 1000),
    close(Port).

logger_loop(Port, Fd, Tstart, Delay) ->
    logger_loop(Port, Fd, Tstart, undefined, 0, Delay).

logger_loop(Port, Fd, Tstart, Tlast, Tack, Delay) ->
    case read_samples(Port) of
	{ok, {Tnew, Ch1, Ch2, Ch3, Ch4}} ->
	    if is_integer(Tlast) ->
		    Tdiff = (Tnew - Tlast) band 16#ffff,
		    Tack1 = Tack + Tdiff*10,
		    Tnow = Tstart + Tack1,
		    io:format(Fd, "\"~s\",~w, ~.2f, ~.2f, ~.2f, ~.2f\n",
			      [format_abs_time(Tnow), Tack1, 
			       Ch1, Ch2, Ch3, Ch4]),
		    logger_loop_wait(Port, Fd, Tstart, Tnew, Tack1, Delay);
	       true ->
		    logger_loop_wait(Port, Fd, Tstart, Tnew, Tack, Delay)
	    end;
	Error ->
	    io:format("logger_loop: error = ~p\n", [Error]),
	    logger_loop_wait(Port, Fd, Tstart, Tlast, Tack, Delay)
    end.

logger_loop_wait(Port, Fd, Tstart, Tlast, Tack, Delay) ->
    receive
	stop -> ok
    after Delay ->
	    logger_loop(Port, Fd, Tstart, Tlast, Tack, Delay)
    end.



format_abs_time() ->
    format_abs_time(milli_seconds_from_0000()).

milli_seconds_from_1970() ->
    timer:now_diff(os:timestamp(), {0,0,0}) div 1000.

milli_seconds_from_0000() ->
    milli_seconds_from_1970() + (719528*86400*1000).

format_abs_time(Tms) ->
    Ts = Tms div 1000,
    _Ms = Tms rem 1000,
    {{Year,Mon,Day},{H,M,S}} = calendar:gregorian_seconds_to_datetime(Ts),
    io_lib:format("~4..0w-~2..0w-~2..0wT~2..0w:~2..0w:~2..0w",
		  [Year,Mon,Day,H,M,S]).
