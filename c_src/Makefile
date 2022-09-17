#@BEGIN-C_SRC-DEFAULT-RULES@
ERL=$(shell readlink -f `which erl`)
ERL_TOP=$(ERL:%/bin/erl=%)
OS=$(shell uname -s)
CFLAGS=-MMD -MP -MF .$<.d -I$(ERL_TOP)/usr/include
#@END-C_SRC-DEFAULT-RULES@

APP=hid
CFLAGS+=-std=c99 -Wall -O3 -I/usr/local/include/hidapi
LDFLAGS+=-shared

OBJS = hid_drv.o
# LIBS = -L/usr/local/lib -lhidapi-hidraw 
LIBS = -L/usr/local/lib -lusb-1.0 -lhidapi-libusb

all: ../priv/$(APP)_drv.so

../priv/$(APP)_drv.so: $(OBJS)
	$(CC) $(LDFLAGS) $(OBJS) $(LIBS) -o ../priv/$(APP)_nif.so

%.o:	%.c
	$(CC) $(CFLAGS) -c -fPIC $<

.%.d:	;

-include .*.d
