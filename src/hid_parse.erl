%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2013, Tony Rogvall
%%% @doc
%%%     hid description parser
%%% @end
%%% Created :  3 Sep 2013 by Tony Rogvall <tony@rogvall.se>

-module(hid_parse).

-include("../include/hid.hrl").

-compile(export_all).

%%
%% Example:
%%

hid_ReportDescriptor() ->
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


      
      
