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
	  %% unsigned short release_number;
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

-endif.
