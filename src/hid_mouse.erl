%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2013, Tony Rogvall
%%% @doc
%%%    Simple mouse tracking example
%%% @end
%%% Created :  4 Sep 2013 by Tony Rogvall <tony@rogvall.se>

-module(hid_mouse).

-compile(export_all).

start(Path) when is_list(Path) ->
    spawn_link(fun() ->
		       case hid:open_path(Path) of
			   {ok,D} ->
			       hid:setopts(D, [{active,once}]),
			       loop(D, 0, 0, 0, 0, 0);
			   Error ->
			       io:format("open usb device failed: ~p\n", 
					 [Error]),
			       Error
		       end
	       end).

stop(Pid) ->
    call(Pid, stop).

get_position(Pid) ->
    call(Pid, get_position).


call(Pid,Req) ->
    Ref = make_ref(),
    Pid ! {call,[Ref|self()],Req},
    receive
	{Ref,Result} ->
	    Result
    end.

reply([Ref|Caller],Result) ->
    Caller ! {Ref,Result}.

%% FIXME: check mouse flavour by inspecting the report_descriptor!
loop(D, B, X, Y, Wx, Wy) ->
    receive
	{hid,D,<<B1,Xd:8/signed,Yd:8/signed>>} ->
	    %% No wheel mouse
	    mouse_input(D, B, X, Y, Wx, Wy, B1,Xd,Yd,0,0);
	{hid,D,<<B1,Xd:8/signed,Yd:8/signed,Wxd:8/signed>>} ->
	    %% One wheel mouse
	    mouse_input(D, B, X, Y, Wx, Wy, B1,Xd,Yd,Wxd,0);
	{hid,D,<<B1,Xd:8/signed,Yd:8/signed,Wxd:8/signed,Wyd:8/signed,
		 _Ignore/binary>>} ->
	    %% Two axis wheel mouse
	    mouse_input(D, B, X, Y, Wx, Wy, B1,Xd,Yd,Wxd,Wyd);
	{call,From,get_position} ->
	    reply(From,{X,Y}),
	    loop(D, B, X, Y, Wx, Wy);
	{call,From,stop} ->
	    reply(From, ok),
	    hid:close(D),
	    ok;
	%% RFID - keybord is reported as this over and over
	{hid,D,<<0,0,0,0,0,0,0,0>>} ->
	    hid:setopts(D, [{active,once}]),
	    loop(D, B, X, Y, Wx, Wy);
	{hid,D,Data} ->
	    io:format("HID: ~p\n", [Data]),
	    hid:setopts(D, [{active,once}]),
	    loop(D, B, X, Y, Wx, Wy);
	Other ->
	    io:format("Other: ~p\n", [Other]),
	    loop(D, B, X, Y, Wx, Wy)
    end.


mouse_input(D, 
	    B, X, Y, Wx, Wy,
	    B1,Xd,Yd,Wxd,Wyd) ->
    %% Two axis wheel mouse
    Pressed  = B1 band (bnot B),
    Released = (bnot B1) band B,
    if Pressed =/= 0 -> io:format("pressed: ~w\n", [Pressed]);
       Released =/= 0 -> io:format("released: ~w\n", [Released]);
       true -> ok
    end,
    X1 = X+Xd,
    Y1 = Y+Yd,
    if Xd =/= 0; Yd =/= 0 -> 
	    io:format("position: (~w,~w)\n", [X1,Y1]);
       true -> ok
    end,
    hid:setopts(D, [{active,once}]),
    ?MODULE:loop(D, B1, X1, Y1, Wx+Wxd, Wy+Wyd).

    
    
