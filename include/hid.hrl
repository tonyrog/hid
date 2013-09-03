%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2013, Tony Rogvall
%%% @doc
%%%    HID api
%%% @end
%%% Created :  1 Sep 2013 by Tony Rogvall <tony@rogvall.se>

-ifndef(__HID_HRL__).
-define(__HID_HRL__, true).

-record(hid_device_info, {
	  %% Platform-specific device path
	  path  :: string(),
	  %% Device Vendor ID
	  vendor_id :: integer(),
	  %% Device Product ID
	  product_id :: integer(),
	  %% Serial Number
	  serial_number :: string(),
	  %% Device Release Number in binary-coded decimal,
	  %% also known as Device Version Number */
	  release_number :: integer(),
	  %% Manufacturer String
	  manufacturer_string :: string(),
	  %% Product string,
	  product_string :: string(),
	  %% Usage Page for this Device/Interface
	  %% (Windows/Mac only). */
	  usage_page :: integer(),
	  %% Usage for this Device/Interface
	  %% (Windows/Mac only).*/
	  usage :: integer(),
	  %% The USB interface which this logical device
	  %% represents. Valid on both Linux implementations
	  %% in all cases, and valid on the Windows implementation
	  %% only if the device contains more than one interface. */
	  interface_number :: integer()
	 }).

%% constant needed building HID report descriptors
-define(ITEM_SHORT(Tag,Type,Size), 
	(((Tag) bsl 4) bor ((Type) bsl 2) bor (Size))).

%% <<?ITEM_LONG, Size, Tag, Data:Size/binary>>
-define(ITEM_LONG,          2#11111110).

%% <<Tag:4, Type:2, Size:2>>
-define(TYPE_MAIN,     2#00).
-define(TYPE_GLOBAL,   2#01).
-define(TYPE_LOCAL,    2#10).
-define(TYPE_RESERVED, 2#11).

-define(SIZE_0,        2#00).
-define(SIZE_1,        2#01).
-define(SIZE_2,        2#10).
-define(SIZE_4,        2#11).

%% Main items
-define(TAG_INPUT,          2#1000).
-define(TAG_OUTPUT,         2#1001).
-define(TAG_FEATURE,        2#1011).
-define(TAG_COLLECTION,     2#1010).
-define(TAG_END_COLLECTION, 2#1100).

%% collection types
-define(PHYSICAL,       16#00).
-define(APPLICATION,    16#01).
-define(LOGICAL,        16#02).
-define(REPORT,         16#03).
-define(NAMED_ARRAY,    16#04).
-define(USAGE_SWITCH,   16#05).
-define(USAGE_MODIFIER, 16#06).

%% Field bits for input/output/feature
-define(DATA,             2#000000000).
-define(CONSTANT,         2#000000001).
-define(ARRAY,            2#000000000).
-define(VARIABLE,         2#000000010).
-define(ABSOLUTE,         2#000000000).
-define(RELATIVE,         2#000000100).
-define(NO_WRAP,          2#000000000).
-define(WRAP,             2#000001000).
-define(LINEAR,           2#000000000).
-define(NONE_LINEAR,      2#000010000).
-define(PREFERED_STATE,   2#000000000).
-define(NO_PREFERED,      2#000100000).
-define(NO_NULL_POSITION, 2#000000000).
-define(NULL_STATE,       2#001000000).
-define(NON_VOLATILE,     2#000000000).
-define(VOLATILE,         2#010000000).
-define(BIT_FIELD,        2#000000000).
-define(BUFFERED_BYTES,   2#100000000).

%% Global items
-define(TAG_USAGE_PAGE,      2#0000).
-define(TAG_LOGICAL_MINIMUM, 2#0001).
-define(TAG_LOGICAL_MAXIMUM, 2#0010).
-define(TAG_PYSICAL_MINIMUM, 2#0011).
-define(TAG_PYSICAL_MAXIMUM, 2#0100).
-define(TAG_UNIT_EXPONENT,   2#0101).
-define(TAG_UNIT,            2#0110).
-define(TAG_REPORT_SIZE,     2#0111).
-define(TAG_REPORT_ID,       2#1000).
-define(TAG_REPORT_COUNT,    2#1001).
-define(TAG_PUSH,            2#1010).
-define(TAG_POP,             2#1011).

%% Local items
-define(TAG_USAGE,              2#0000).
-define(TAG_USAGE_MINIMUM,      2#0001).
-define(TAG_USAGE_MAXIMUM,      2#0010).
-define(TAG_DESIGNATOR_INDEX,   2#0011).
-define(TAG_DESIGNATOR_MINIMUM, 2#0100).
-define(TAG_DESIGNATOR_MAXIMUM, 2#0101).
-define(TAG_STRING_INDEX,       2#0111).
-define(TAG_STRING_MINIMUM,     2#1000).
-define(TAG_STRING_MAXIMUM,     2#1001).
-define(TAG_DELIMITER,          2#1010).

%% Usage page 
-define(GENERIC_DESKTOP, 16#01).
-define(SIMULATION_CONTROLS,      16#02).
-define(VR_CONTROLS,16#03).
-define(SPORT_CONTROLS,16#04).
-define(GAME_CONTROLS,16#05).
-define(GENERIC_DEVICE,16#06).
-define(KEYBOARD_KEYPAD,16#07).
-define(LEDS,16#08).
-define(BUTTON,16#09).
-define(ORDINAL,16#0A).
-define(TELEPHONY,16#0B).
-define(CONSUMER_DEVICES,16#0C).
-define(DIGITIZER,16#0D).
%% ...

%% GENERIC_DESKTOP: usage
-define(POINTER,   16#01).
-define(MOUSE,     16#02).
%% -define(Reserved, 16#03).
-define(JOYSTICK,  16#04).
-define(GAME_PAD,  16#05).
-define(KEYBOARD,  16#06).
-define(KEYPAD,    16#07).
-define(MULTI_AXIS_CONTROLLER,16#08).
-define(X,         16#30).
-define(Y,         16#31).
-define(Z,         16#32).
-define(Rx,        16#33).
-define(Ry,        16#34).
-define(Rz,        16#35).
-define(SLIDER,    16#36).
-define(DIAL,      16#37).
-define(WHEEL,     16#38).
-define(HAT_SWITCH,16#39).
-define(COUNTED_BUFFER, 16#3a).
-define(BYTE_COUNT, 16#3b).
-define(MOTION_WAKEUP, 16#3c).
-define(START, 16#3d).
-define(SELECT, 16#3e).
-define(Vx, 16#40).
-define(Vy, 16#41).
-define(Vz, 16#42).
-define(Vbrx, 16#43).
-define(Vbry, 16#44).
-define(Vbrz, 16#45).
-define(Vno, 16#46).
-define(FEATURE_NOTIFICATION, 16#47).
-define(RESOLUTION_MULTIPLIER, 16#48).

%% Bu

%% macros used to parse and generate descriptors
%% MAIN - "normal" sized
-define(INPUT(Bits),
	?ITEM_SHORT(?TAG_INPUT,?TYPE_MAIN,?SIZE_1),(Bits)).
-define(OUTPUT(Bits),
	?ITEM_SHORT(?TAG_OUTPUT,?TYPE_MAIN,?SIZE_1),(Bits)).
-define(FEATURE(Bits),
	?ITEM_SHORT(?TAG_FEATURE,?TYPE_MAIN,?SIZE_1),(Bits)).
-define(COLLECTION(Type), 
	?ITEM_SHORT(?TAG_COLLECTION,?TYPE_MAIN,?SIZE_1),(Type)).
-define(END_COLLECTION(),
	?ITEM_SHORT(?TAG_END_COLLECTION,?TYPE_MAIN,?SIZE_0)).

%% Global items
-define(USAGE_PAGE(UsagePage),
	?ITEM_SHORT(?TAG_USAGE_PAGE,?TYPE_GLOBAL,?SIZE_1),(UsagePage)).
-define(LOGICAL_MINIMUM(Value),
	?ITEM_SHORT(?TAG_LOGICAL_MINIMUM,?TYPE_GLOBAL,?SIZE_1),(Value)).
-define(LOGICAL_MAXIMUM(Value),
	?ITEM_SHORT(?TAG_LOGICAL_MAXIMUM,?TYPE_GLOBAL,?SIZE_1),(Value)).
-define(PYSICAL_MINIMUM(Value),
	?ITEM_SHORT(?TAG_PYSICAL_MINIMUM,?TYPE_GLOBAL,?SIZE_1),(Value)).
-define(PYSICAL_MAXIMUM(Value),
	?ITEM_SHORT(?TAG_PYSICAL_MAXIMUM,?TYPE_GLOBAL,?SIZE_1),(Value)).
-define(UNIT_EXPONENT(Exp),
	?ITEM_SHORT(?TAG_UNIT_EXPONENT,?TYPE_GLOBAL,?SIZE_1),(Exp)).
-define(UNIT(Unit),
	?ITEM_SHORT(?TAG_UNIT,?TYPE_GLOBAL,?SIZE_1),(Unit)).
-define(REPORT_SIZE(Size),
	?ITEM_SHORT(?TAG_REPORT_SIZE,?TYPE_GLOBAL,?SIZE_1),(Size)).
-define(REPORT_ID(ID),
	?ITEM_SHORT(?TAG_REPORT_ID,?TYPE_GLOBAL,?SIZE_1),(ID)).
-define(REPORT_COUNT(Count),
	?ITEM_SHORT(?TAG_REPORT_COUNT,?TYPE_GLOBAL,?SIZE_1),(Count)).
-define(PUSH(),
	?ITEM_SHORT(?TAG_PUSH,?TYPE_GLOBAL,?SIZE_0)).
-define(POP(),
	?ITEM_SHORT(?TAG_POP,?TYPE_GLOBAL,?SIZE_0)).
	
%% local
-define(USAGE(Usage), 
	?ITEM_SHORT(?TAG_USAGE,?TYPE_LOCAL,?SIZE_1),(Usage)).
-define(USAGE2(Usage), 
	?ITEM_SHORT(?TAG_USAGE,?TYPE_LOCAL,?SIZE_2),(Usage):16/little).
-define(USAGE_MINIMUM(Min),
	?ITEM_SHORT(?TAG_USAGE_MINIMUM,?TYPE_LOCAL,?SIZE_1),(Min)).
-define(USAGE_MAXIMUM(Max),
	?ITEM_SHORT(?TAG_USAGE_MAXIMUM,?TYPE_LOCAL,?SIZE_1),(Max)).
-define(DESIGNATOR_INDEX(Index),
	?ITEM_SHORT(?TAG_DESIGNATOR_INDEX,?TYPE_LOCAL,?SIZE_1),(Index)).
-define(DESIGNATOR_MINIMUM(Min),
	?ITEM_SHORT(?TAG_DESIGNATOR_MINIMUM,?TYPE_LOCAL,?SIZE_1),(Min)).
-define(DESIGNATOR_MAXIMUM(Max),
	?ITEM_SHORT(?TAG_DESIGNATOR_MAXIMUM,?TYPE_LOCAL,?SIZE_1),(Max)).
-define(STRING_INDEX(Index),
	?ITEM_SHORT(?TAG_STRING_INDEX,?TYPE_LOCAL,?SIZE_1),(Index)).
-define(STRING_MINIMUM(Min),
	?ITEM_SHORT(?TAG_STRING_MINIMUM,?TYPE_LOCAL,?SIZE_1),(Min)).
-define(STRING_MAXIMUM(Max),
	?ITEM_SHORT(?TAG_STRING_MAXIMUM,?TYPE_LOCAL,?SIZE_1),(Max)).
-define(DELIMITER_OPEN(),
	?ITEM_SHORT(?TAG_DELIMITER,?TYPE_LOCAL,?SIZE_1),(1)).
-define(DELIMITER_CLOSE(),
	?ITEM_SHORT(?TAG_DELIMITER,?TYPE_LOCAL,?SIZE_1),(0)).

-endif.
