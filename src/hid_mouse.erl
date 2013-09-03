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

loop(D, B, X, Y, Wx, Wy) ->
    receive
	{hid,D,<<B1,Xr:8/signed,Yr:8/signed,Wrx:8/signed,Wry:8/signed,_Z>>} ->
	    if _Z =/= 0 -> io:format("Z = ~w\n", [_Z]);
	       true -> ok
	    end,
	    Pressed  = B1 band (bnot B),
	    Released = (bnot B1) band B,
	    if Pressed =/= 0 -> io:format("pressed: ~w\n", [Pressed]);
	       Released =/= 0 -> io:format("released: ~w\n", [Released]);
	       true -> ok
	    end,
	    X1 = X+Xr,
	    Y1 = Y+Yr,
	    if Xr =/= 0; Yr =/= 0 -> io:format("position: (~w,~w)\n", [X1,Y1]);
	       true -> ok
	    end,
	    hid:setopts(D, [{active,once}]),
	    loop(D, B1, X1, Y1, Wx+Wrx, Wy+Wry);
	{call,From,get_position} ->
	    reply(From,{position,X,Y}),
	    loop(D, B, X, Y, Wx, Wy);
	{call,From,stop} ->
	    reply(From, ok),
	    hid:close(D),
	    ok;
	Other ->
	    io:format("Other: ~p\n", [Other]),
	    loop(D, B, X, Y, Wx, Wy)
    end.


	
    
