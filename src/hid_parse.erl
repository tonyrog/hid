%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2013, Tony Rogvall
%%% @doc
%%%     hid description parser
%%% @end
%%% Created :  3 Sep 2013 by Tony Rogvall <tony@rogvall.se>

-module(hid_parse).

-include("../include/hid.hrl").

-compile(export_all).


decode(Descriptor) when is_binary(Descriptor) ->
    decode(Descriptor, [], [], #hid_global{}, #hid_item{ }).

%% decode
decode(<<?ITEM_LONG, Size, Tag, Data:Size/binary, Tail/binary>>,
       Cs, Gs, G, L) ->
    decode(Tail, [{Tag,Data} | Cs], Gs, G, L);
decode(<<?ITEM_SHORT(Tag,Type,Size), Data:Size/binary, Tail/binary>>,
       Cs, Gs, G, L) ->
    case decode_tag(decode_type(Type),Tag) of
	undefined ->
	    <<Tagi>> = <<?ITEM_SHORT(Tag,Type,Size)>>,
	    %% I guess that preserveing the size in the numeric tag 
	    %% will render the least surprise.
	    decode(Tail, [{Tagi,Data} | Cs], Gs, G, L);
	T ->
	    decode_item(Tail,T,Data,Cs,Gs,G,L)
    end;
decode(<<>>, Cs, _Gs, _G, _L) ->
    lists:reverse(Cs).

%% main items
decode_item(Tail,collection,Data,Cs,Gs,G,L) ->
    Type = decode_collection(decode_unsigned(Data)),
    decode(Tail,[{collection,Type}|Cs],Gs,G,L);
decode_item(Tail,end_collection,_Data,Cs,Gs,G,L) ->
    decode(Tail,pop_collection(Cs),Gs,G,L);
decode_item(Tail,input,Data,Cs,Gs,G,L) ->
    Bits = decode_parts(decode_unsigned(Data)),
    E = {input,Bits,L#hid_item { global = G }},
    decode(Tail,[E|Cs],Gs,G,#hid_item{ });  %% clear L?
decode_item(Tail,output,Data,Cs,Gs,G,L) ->
    Bits = decode_parts(decode_unsigned(Data)),
    E = {output,Bits,L#hid_item { global = G }},
    decode(Tail,[E|Cs],Gs,G,#hid_item{ });  %% clear L?
decode_item(Tail,feature,Data,Cs,Gs,G,L) ->
    Bits = decode_parts(decode_unsigned(Data)),
    E = {feature,Bits,L#hid_item { global = G }},
    decode(Tail,[E|Cs],Gs,G,#hid_item{ });  %% clear L?

%% Global items
decode_item(Tail,push,_Data,Cs,Gs,G,L) ->
    decode(Tail,Cs,[G|Gs],G,L);
decode_item(Tail,pop,_Data,Cs,[G|Gs],_G,L) ->
    decode(Tail,Cs,Gs,G,L);

decode_item(Tail,usage_page,Data,Cs,Gs,G,L) ->
    V = decode_usage_page(decode_unsigned(Data)),
    G1=G#hid_global { usage_page = V },
    decode(Tail,Cs,Gs,G1,L);
decode_item(Tail,logical_minimum,Data,Cs,Gs,G,L) ->
    V = decode_signed(Data),
    G1 = G#hid_global { logical_minimum = V },
    decode(Tail,Cs,Gs,G1,L);
decode_item(Tail,logical_maximum,Data,Cs,Gs,G,L) ->
    V = decode_signed(Data),
    G1 = G#hid_global { logical_maximum = V },
    decode(Tail,Cs,Gs,G1,L);
decode_item(Tail,pysical_minimum,Data,Cs,Gs,G,L) ->
    V = decode_signed(Data),
    G1 = G#hid_global { pysical_minimum = V },
    decode(Tail,Cs,Gs,G1,L);
decode_item(Tail,pysical_maximum,Data,Cs,Gs,G,L) ->
    V = decode_signed(Data),
    G1 = G#hid_global { pysical_maximum = V },
    decode(Tail,Cs,Gs,G1,L);
decode_item(Tail,unit_exponent,Data,Cs,Gs,G,L) ->
    V = decode_signed(Data),
    G1 = G#hid_global { unit_exponent = V },
    decode(Tail,Cs,Gs,G1,L);
decode_item(Tail,unit,Data,Cs,Gs,G,L) ->
    V = decode_unit(decode_unsigned(Data),byte_size(Data)*2),
    G1 = G#hid_global { unit = V },
    decode(Tail,Cs,Gs,G1,L);
decode_item(Tail,report_size,Data,Cs,Gs,G,L) ->
    V = decode_unsigned(Data),
    G1 = G#hid_global { report_size = V },
    decode(Tail,Cs,Gs,G1,L);
decode_item(Tail,report_id,Data,Cs,Gs,G,L) ->
    V = decode_unsigned(Data),
    G1 = G#hid_global { report_id = V },
    decode(Tail,Cs,Gs,G1,L);
decode_item(Tail,report_count,Data,Cs,Gs,G,L) ->
    V = decode_unsigned(Data),
    G1 = G#hid_global { report_count = V },
    decode(Tail,Cs,Gs,G1,L);
%% Local items
decode_item(Tail,usage,Data,Cs,Gs,G,L) ->
    V  = decode_usage(G#hid_global.usage_page,decode_unsigned(Data)),
    L1 = L#hid_item { usage = [V|L#hid_item.usage]},
    decode(Tail,Cs,Gs,G,L1);
decode_item(Tail,usage_minimum,Data,Cs,Gs,G,L) ->
    V  = decode_unsigned(Data),
    L1 = L#hid_item { usage_minimum = V },
    decode(Tail,Cs,Gs,G,L1);
decode_item(Tail,usage_maximum,Data,Cs,Gs,G,L) ->
    V  = decode_unsigned(Data),
    L1 = L#hid_item { usage_maximum = V },
    decode(Tail,Cs,Gs,G,L1);
decode_item(Tail,designator_index,Data,Cs,Gs,G,L) ->
    V  = decode_unsigned(Data),
    L1 = L#hid_item { designator_index = V },
    decode(Tail,Cs,Gs,G,L1);
decode_item(Tail,designator_minimum,Data,Cs,Gs,G,L) ->
    V  = decode_unsigned(Data),
    L1 = L#hid_item { designator_minimum = V },
    decode(Tail,Cs,Gs,G,L1);
decode_item(Tail,designator_maximum,Data,Cs,Gs,G,L) ->
    V  = decode_unsigned(Data),
    L1 = L#hid_item { designator_maximum = V },
    decode(Tail,Cs,Gs,G,L1);
decode_item(Tail,string_index,Data,Cs,Gs,G,L) ->
    V  = decode_unsigned(Data),
    L1 = L#hid_item { string_index = V },
    decode(Tail,Cs,Gs,G,L1);
decode_item(Tail,string_minimum,Data,Cs,Gs,G,L) ->
    V  = decode_unsigned(Data),
    L1 = L#hid_item { string_minimum = V },
    decode(Tail,Cs,Gs,G,L1);
decode_item(Tail,string_maximum,Data,Cs,Gs,G,L) ->
    V  = decode_unsigned(Data),
    L1 = L#hid_item { string_maximum = V },
    decode(Tail,Cs,Gs,G,L1).
		      
pop_collection(Stack) ->
    pop_collection(Stack, []).

pop_collection([{collection,Type}|Stack], Collection) ->
    [{collection,Type,Collection} | Stack];
pop_collection([E|Stack], Collection) ->
    pop_collection(Stack, [E|Collection]).

decode_unsigned(<<>>) -> 0;
decode_unsigned(<<X:8>>) ->  X;
decode_unsigned(<<X:16/little>>) -> X;
decode_unsigned(<<X:24/little>>) -> X;
decode_unsigned(<<X:32/little>>) -> X.

decode_signed(<<>>) -> 0;
decode_signed(<<X:8/little-signed>>) ->  X;
decode_signed(<<X:16/little-signed>>) -> X;
decode_signed(<<X:24/little-signed>>) -> X;
decode_signed(<<X:32/little-signed>>) -> X.

-define(ite(C,T,E), if (C) -> T; true -> E end).

decode_parts(Bits) ->
    lists:append(
      [?ite(Bits band ?CONSTANT =/= 0, [constant], []),
       ?ite(Bits band ?VARIABLE =/= 0, [variable], []),
       ?ite(Bits band ?RELATIVE =/= 0, [relative], []),
       ?ite(Bits band ?WRAP =/= 0, [wrap], []),
       ?ite(Bits band ?NONE_LINEAR =/= 0, [none_linear], []),
       ?ite(Bits band ?NO_PREFERED =/= 0, [no_prefered], []),
       ?ite(Bits band ?NULL_STATE =/= 0, [null_state], []),
       ?ite(Bits band ?VOLATILE =/= 0, [volatile], []),
       ?ite(Bits band ?BUFFERED_BYTES =/= 0, [buffered_bytes], [])]).

decode_collection(Collection) ->    
    case Collection of
	?PHYSICAL       -> physical;
	?APPLICATION    -> application;
	?LOGICAL        -> logical;
	?REPORT         -> report;
	?NAMED_ARRAY    -> named_array;
	?USAGE_SWITCH   -> usage_switch;
	?USAGE_MODIFIER -> usage_modifier;
	_ -> Collection
    end.

decode_usage_page(UsagePage) ->
    case UsagePage of
	?GENERIC_DESKTOP -> generic_desktop;
	?SIMULATION_CONTROLS -> simulation_controls;
	?VR_CONTROLS -> vr_controls;
	?SPORT_CONTROLS -> sport_controls;
	?GAME_CONTROLS -> game_controls;
	?GENERIC_DEVICE -> generic_device;
	?KEYBOARD_KEYPAD -> keyboard_keypad;
	?LEDS -> leds;
	?BUTTON -> button;
	?ORDINAL -> ordinal;
	?TELEPHONY -> telephony;
	?CONSUMER_DEVICES -> consumer_devices;
	?DIGITIZER -> digitizer;
	_ -> UsagePage
    end.

%% smart unit coding!
%% Joule can be expressed as 16#E121
%% hid_parse:decode_unit(16#E121,4) -> [{centimeter,2},{gram,1},{seconds,-2}]
%% unit_exponent = 7 (meters and kilograms)
%%
units() ->
    { {none, si_linear,  si_rotation, english_linear, english_rotation},
      {none, centimeter, radians,     inch,           degrees},
      {none, gram,       gram,        slug,           slug},
      {none, seconds,    seconds,     seconds,        seconds},
      {none, kelvin,     kelvin,      fahrenheit,     fahrenheit},
      {none, ampere,     ampere,      ampere,         ampere},
      {none, candela,    candela,     candela,        candela},
      {none, none,       none,        none,           none} }.

decode_unit(Unit,Sz) ->
    Sys = Unit band 16#f,
    if Sys =< 16#04 ->
	    decode_unit(Unit bsr 4, 2, Sz, Sys+1, []);
       Sys =< 16#0E ->
	    {reserved,Unit};
       true -> 
	    {vendor_defined,Unit}
    end.

decode_unit(Unit, I, Sz, Sys, Acc) ->
    if I =< Sz ->
	    N = Unit band 16#f,
	    if N =:= 0 -> decode_unit(Unit bsr 4,I+1,Sz,Sys,Acc);
	       true ->
		    Exp = if N > 7 -> N - 16; true -> N end,
		    U = element(Sys,element(I,units())),
		    decode_unit(Unit bsr 4,I+1,Sz,Sys,[{U,Exp}|Acc])
	    end;
       true ->
	    lists:reverse(Acc)
    end.


decode_usage(generic_desktop,Usage) ->
    case Usage of
	?POINTER -> pointer;
	?MOUSE -> mouse;
	%% ?Reserved -> reserved;
	?JOYSTICK -> joystick;
	?GAME_PAD -> game_pad;
	?KEYBOARD -> keyboard;
	?KEYPAD -> keypad;
	?MULTI_AXIS_CONTROLLER -> multi_axis_controller;
	?X -> x;
	?Y -> y;
	?Z -> z;
	?Rx -> rx;
	?Ry -> ry;
	?Rz -> rz;
	?SLIDER -> slider;
	?DIAL -> dial;
	?WHEEL -> wheel;
	?HAT_SWITCH -> hat_switch;
	?COUNTED_BUFFER -> counted_buffer;
	?BYTE_COUNT -> byte_count;
	?MOTION_WAKEUP -> motion_wakeup;
	?START -> start;
	?SELECT -> select;
	?Vx -> vx;
	?Vy -> vy;
	?Vz -> vz;
	?Vbrx -> vbrx;
	?Vbry -> vbry;
	?Vbrz -> vbrz;
	?Vno -> vno;
	?FEATURE_NOTIFICATION -> feature_notification;
	?RESOLUTION_MULTIPLIER -> resolution_multiplier;
	_ -> Usage
    end;
decode_usage(_UsagePage,Usage) ->
    Usage.
    

decode_type(T) ->
    element(T+1, {main,global,local,undefined}).

decode_tag(main,T) ->
    element(T+1, 
	    {undefined,undefined,undefined,undefined,
	     undefined,undefined,undefined,undefined,
	     input,output,collection,feature,end_collection,
	     undefined,undefined,undefined,undefined});
decode_tag(global,T) ->
    element(T+1,
	    {usage_page,logical_minimum,logical_maximum,
	     pysical_minimum,pysical_maximum,
	     unit_exponent, unit, report_size,
	     report_id, report_count, push, pop,
	     undefined, undefined, undefined, undefined});
decode_tag(local,T) ->
    element(T+1,
	    {usage,usage_minimum,usage_maximum,
	     designator_index,designator_minimum,
	     designator_maximum,
	     undefined,
	     string_index,string_minimum,string_maximum,
	     delimiter,
	     undefined,undefined,undefined,undefined,
	     undefined,undefined});
decode_tag(_,_T) ->
    undefined.

    
%%
%% Example:
%%

raw_mouse_wheel() ->
<<
    16#05, 16#01,        %% USAGE_PAGE (Generic Desktop)
    16#09, 16#02,        %% USAGE (Mouse)
    16#a1, 16#01,        %% COLLECTION (Application)
    16#09, 16#02,        %%   USAGE (Mouse)
    16#a1, 16#02,        %%   COLLECTION (Logical)
    16#09, 16#01,        %%     USAGE (Pointer)
    16#a1, 16#00,        %%     COLLECTION (Physical)
  %% ------------------------------  Buttons
    16#05, 16#09,        %%       USAGE_PAGE (Button)
    16#19, 16#01,        %%       USAGE_MINIMUM (Button 1)
    16#29, 16#05,        %%       USAGE_MAXIMUM (Button 5)
    16#15, 16#00,        %%       LOGICAL_MINIMUM (0)
    16#25, 16#01,        %%       LOGICAL_MAXIMUM (1)
    16#75, 16#01,        %%       REPORT_SIZE (1)
    16#95, 16#05,        %%       REPORT_COUNT (5)
    16#81, 16#02,        %%       INPUT (Data,Var,Abs)
  %% ------------------------------  Padding
    16#75, 16#03,        %%       REPORT_SIZE (3)
    16#95, 16#01,        %%       REPORT_COUNT (1)
    16#81, 16#03,        %%       INPUT (Cnst,Var,Abs)
  %% ------------------------------  X,Y position
    16#05, 16#01,        %%       USAGE_PAGE (Generic Desktop)
    16#09, 16#30,        %%       USAGE (X)
    16#09, 16#31,        %%       USAGE (Y)
    16#15, 16#81,        %%       LOGICAL_MINIMUM (-127)
    16#25, 16#7f,        %%       LOGICAL_MAXIMUM (127)
    16#75, 16#08,        %%       REPORT_SIZE (8)
    16#95, 16#02,        %%       REPORT_COUNT (2)
    16#81, 16#06,        %%       INPUT (Data,Var,Rel)
    16#a1, 16#02,        %%       COLLECTION (Logical)
  %% ------------------------------  Vertical wheel res multiplier
    16#09, 16#48,        %%         USAGE (Resolution Multiplier)
    16#15, 16#00,        %%         LOGICAL_MINIMUM (0)
    16#25, 16#01,        %%         LOGICAL_MAXIMUM (1)
    16#35, 16#01,        %%         PHYSICAL_MINIMUM (1)
    16#45, 16#04,        %%         PHYSICAL_MAXIMUM (4)
    16#75, 16#02,        %%         REPORT_SIZE (2)
    16#95, 16#01,        %%         REPORT_COUNT (1)
    16#a4,              %%         PUSH
    16#b1, 16#02,        %%         FEATURE (Data,Var,Abs)
  %% ------------------------------  Vertical wheel
    16#09, 16#38,        %%         USAGE (Wheel)
    16#15, 16#81,        %%         LOGICAL_MINIMUM (-127)
    16#25, 16#7f,        %%         LOGICAL_MAXIMUM (127)
    16#35, 16#00,        %%         PHYSICAL_MINIMUM (0)        - reset physical
    16#45, 16#00,        %%         PHYSICAL_MAXIMUM (0)
    16#75, 16#08,        %%         REPORT_SIZE (8)
    16#81, 16#06,        %%         INPUT (Data,Var,Rel)
    16#c0,              %%       END_COLLECTION
    16#a1, 16#02,        %%       COLLECTION (Logical)
  %% ------------------------------  Horizontal wheel res multiplier
    16#09, 16#48,        %%         USAGE (Resolution Multiplier)
    16#b4,              %%         POP
    16#b1, 16#02,        %%         FEATURE (Data,Var,Abs)
  %% ------------------------------  Padding for Feature report
    16#35, 16#00,        %%         PHYSICAL_MINIMUM (0)        - reset physical
    16#45, 16#00,        %%         PHYSICAL_MAXIMUM (0)
    16#75, 16#04,        %%         REPORT_SIZE (4)
    16#b1, 16#03,        %%         FEATURE (Cnst,Var,Abs)
  %% ------------------------------  Horizontal wheel
    16#05, 16#0c,        %%         USAGE_PAGE (Consumer Devices)
    16#0a, 16#38, 16#02,  %%         USAGE (AC Pan)
    16#15, 16#81,        %%         LOGICAL_MINIMUM (-127)
    16#25, 16#7f,        %%         LOGICAL_MAXIMUM (127)
    16#75, 16#08,        %%         REPORT_SIZE (8)
    16#81, 16#06,        %%         INPUT (Data,Var,Rel)
    16#c0,              %%       END_COLLECTION
    16#c0,              %%     END_COLLECTION
    16#c0,              %%   END_COLLECTION
    16#c0               %% END_COLLECTION
>>.

%%
%% typedef struct{
%%    char    throttle;
%%    char    x;
%%    char    y;
%%    uchar    hatSwitchAndButtons;
%% }report_t;

raw_joystick() ->
<<
    16#05, 16#01,                    %% USAGE_PAGE (Generic Desktop)
    16#15, 16#00,                    %% LOGICAL_MINIMUM (0)
    16#09, 16#04,                    %% USAGE (Joystick)
    16#a1, 16#01,                    %% COLLECTION (Application)
    16#05, 16#02,                    %%   USAGE_PAGE (Simulation Controls)
    16#09, 16#bb,                    %%   USAGE (Throttle)
    16#15, 16#81,                    %%   LOGICAL_MINIMUM (-127)
    16#25, 16#7f,                    %%   LOGICAL_MAXIMUM (127)
    16#75, 16#08,                    %%   REPORT_SIZE (8)
    16#95, 16#01,                    %%   REPORT_COUNT (1)
    16#81, 16#02,                    %%   INPUT (Data,Var,Abs)
    16#05, 16#01,                    %%   USAGE_PAGE (Generic Desktop)
    16#09, 16#01,                    %%   USAGE (Pointer)
    16#a1, 16#00,                    %%   COLLECTION (Physical)
    16#09, 16#30,                    %%     USAGE (X)
    16#09, 16#31,                    %%     USAGE (Y)
    16#95, 16#02,                    %%     REPORT_COUNT (2)
    16#81, 16#02,                    %%     INPUT (Data,Var,Abs)
    16#c0,                          %%   END_COLLECTION
    16#09, 16#39,                    %%   USAGE (Hat switch)
    16#15, 16#00,                    %%   LOGICAL_MINIMUM (0)
    16#25, 16#03,                    %%   LOGICAL_MAXIMUM (3)
    16#35, 16#00,                    %%   PHYSICAL_MINIMUM (0)
    16#46, 16#0e, 16#01,              %%   PHYSICAL_MAXIMUM (270)
    16#65, 16#14,                    %%   UNIT (Eng Rot:Angular Pos)
    16#75, 16#04,                    %%   REPORT_SIZE (4)
    16#95, 16#01,                    %%   REPORT_COUNT (1)
    16#81, 16#02,                    %%   INPUT (Data,Var,Abs)
    16#05, 16#09,                    %%   USAGE_PAGE (Button)
    16#19, 16#01,                    %%   USAGE_MINIMUM (Button 1)
    16#29, 16#04,                    %%   USAGE_MAXIMUM (Button 4)
    16#15, 16#00,                    %%   LOGICAL_MINIMUM (0)
    16#25, 16#01,                    %%   LOGICAL_MAXIMUM (1)
    16#75, 16#01,                    %%   REPORT_SIZE (1)
16#95, 16#04,                    %%   REPORT_COUNT (4)
16#55, 16#00,                    %%   UNIT_EXPONENT (0)
16#65, 16#00,                    %%   UNIT (None)
16#81, 16#02,                    %%   INPUT (Data,Var,Abs)
16#c0                           %% END_COLLECTION
>>.



hid_ReportDescriptor_sym() ->
<<
  ?USAGE_PAGE(?GENERIC_DESKTOP),
  ?USAGE(?MOUSE),
  ?COLLECTION(?APPLICATION),
  ?USAGE(?MOUSE),
  ?COLLECTION(?LOGICAL),
  ?USAGE(?POINTER),  
  ?COLLECTION(?PHYSICAL),
  %% --- buttons ---
  ?USAGE_PAGE(?BUTTON),
  ?USAGE_MINIMUM(1),
  ?USAGE_MAXIMUM(5),
  ?LOGICAL_MINIMUM(0),
  ?LOGICAL_MAXIMUM(1),
  ?REPORT_SIZE(1),
  ?REPORT_COUNT(5),
  ?INPUT(?DATA bor ?VARIABLE bor ?ABSOLUTE),
  %% --- padding ---
  ?REPORT_SIZE(3),
  ?REPORT_COUNT(1),
  ?INPUT(?CONSTANT bor ?VARIABLE bor ?ABSOLUTE),  
  %% -- X,Y position
  ?USAGE_PAGE(?GENERIC_DESKTOP),
  ?USAGE(?X),
  ?USAGE(?Y),
  ?LOGICAL_MINIMUM(-127),
  ?LOGICAL_MAXIMUM(127),
  ?REPORT_SIZE(8),
  ?REPORT_COUNT(2),
  ?INPUT(?DATA bor ?VARIABLE bor ?RELATIVE),
  ?COLLECTION(?LOGICAL),
  ?USAGE(?RESOLUTION_MULTIPLIER),
  ?LOGICAL_MINIMUM(0),
  ?LOGICAL_MAXIMUM(1),
  ?PYSICAL_MINIMUM(1),
  ?PYSICAL_MAXIMUM(4),
  ?REPORT_SIZE(2),
  ?REPORT_COUNT(1),
  ?PUSH(),
  ?FEATURE(?DATA bor ?VARIABLE bor ?ABSOLUTE),
  ?USAGE(?WHEEL),
  ?LOGICAL_MINIMUM(-127),
  ?LOGICAL_MAXIMUM(127),
  ?PYSICAL_MINIMUM(0),
  ?PYSICAL_MAXIMUM(0),
  ?REPORT_SIZE(8),
  ?INPUT(?DATA bor ?VARIABLE bor ?RELATIVE),
  ?END_COLLECTION(),
  ?COLLECTION(?LOGICAL),
  ?USAGE(?RESOLUTION_MULTIPLIER),
  ?POP(),
  ?FEATURE(?DATA bor ?VARIABLE bor ?ABSOLUTE),
  ?PYSICAL_MINIMUM(0),
  ?PYSICAL_MAXIMUM(0),
  ?REPORT_SIZE(4),
  ?FEATURE(?CONSTANT bor ?VARIABLE bor ?ABSOLUTE),
  ?USAGE_PAGE(?CONSUMER_DEVICES),
  ?USAGE2(16#238),   %% AC_PAN
  ?LOGICAL_MINIMUM(-127),
  ?LOGICAL_MAXIMUM(127),
  ?REPORT_SIZE(8),
  ?INPUT(?DATA bor ?VARIABLE bor ?RELATIVE),
  ?END_COLLECTION(),
  ?END_COLLECTION(),
  ?END_COLLECTION(),
  ?END_COLLECTION()  
>>.


      
      
