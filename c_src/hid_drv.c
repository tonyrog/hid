//
// hid_drv.c
//
// require: hidapi 
//

#include <stdio.h>
#include <stdarg.h>
#include <stdint.h>
#include <stdlib.h>
#include <wchar.h>
#include <memory.h>
#include <unistd.h>
#include <errno.h>

#include "erl_driver.h"
// #include "dthread.h"
#include "hidapi.h"

#define ATOM(NAME) am_ ## NAME
#define INIT_ATOM(NAME) am_ ## NAME = driver_mk_atom(#NAME)

// Hack to handle R15 driver used with pre R15 driver
#if ERL_DRV_EXTENDED_MAJOR_VERSION == 1
typedef int  ErlDrvSizeT;
typedef int  ErlDrvSSizeT;
#endif

#if (ERL_DRV_EXTENDED_MAJOR_VERSION > 2) || ((ERL_DRV_EXTENDED_MAJOR_VERSION == 2) && (ERL_DRV_EXTENDED_MINOR_VERSION >= 1))
#define OUTPUT_TERM(ctx, message, len) erl_drv_output_term((ctx)->dport,(message),(len))
#define SEND_TERM(ctx, to, message, len) erl_drv_send_term((ctx)->dport,(to),(message),(len))

#else
#define OUTPUT_TERM(ctx, message, len) driver_output_term((ctx)->port,(message),(len))
#define SEND_TERM(ctx, to, message, len) driver_send_term((ctx)->port,(to),(message),(len))
#endif


#define PORT_CONTROL_BINARY

#define INT_EVENT(e) ((int)((long)(e)))

#define CMD_ENUMERATE                1
#define CMD_OPEN                     2
#define CMD_OPEN_PATH                3
#define CMD_WRITE                    4
#define CMD_SET_BUFFER               5
#define CMD_SET_ACTIVE               6
#define CMD_SEND_FEATURE_REPORT      7
#define CMD_GET_FEATURE_REPORT       8
#define CMD_CLOSE                    9
#define CMD_GET_MANUFACTURER_STRING  10
#define CMD_GET_PRODUCT_STRING       11
#define CMD_GET_SERIAL_NUMBER_STRING 12
#define CMD_GET_INDEXED_STRING       13
#define CMD_SET_DEBUG                14

#define MAX_STRING      1024
#define MAX_PATH        1024
#define MAX_SERIAL      1024
#define MAX_REPORT      1024
#define MAX_READ_LENGTH 4096

static inline uint32_t get_uint32(uint8_t* ptr)
{
    uint32_t value = (ptr[0]<<24) | (ptr[1]<<16) | (ptr[2]<<8) | (ptr[3]<<0);
    return value;
}

static inline int32_t get_int32(uint8_t* ptr)
{
    return (int32_t) get_uint32(ptr);
}

static inline uint16_t get_uint16(uint8_t* ptr)
{
    uint16_t value = (ptr[0]<<8) | (ptr[1]<<0);
    return value;
}

static inline uint8_t get_uint8(uint8_t* ptr)
{
    uint8_t value = (ptr[0]<<0);
    return value;
}

static inline void put_uint16(uint8_t* ptr, uint16_t v)
{
    ptr[0] = v>>8;
    ptr[1] = v;
}

static inline void put_uint32(uint8_t* ptr, uint32_t v)
{
    ptr[0] = v>>24;
    ptr[1] = v>>16;
    ptr[2] = v>>8;
    ptr[3] = v;
}


typedef struct _hid_ctx_t
{
    ErlDrvPort     port;
    ErlDrvTermData dport;
    hid_device*    dev;
    ErlDrvEvent    event;    // input event handle
    ErlDrvTermData receiver; // receiver of input
    uint32_t       ref;
    int            active;
    int            buflen;
} hid_ctx_t;

static int  hid_drv_init(void);
static void hid_drv_finish(void);
static void hid_drv_stop(ErlDrvData);
static void hid_drv_output(ErlDrvData, char*, ErlDrvSizeT);
static void hid_drv_ready_input(ErlDrvData, ErlDrvEvent);
static void hid_drv_ready_output(ErlDrvData data, ErlDrvEvent event);
static ErlDrvData hid_drv_start(ErlDrvPort, char* command);
static ErlDrvSSizeT hid_drv_ctl(ErlDrvData,unsigned int,char*,ErlDrvSizeT,char**, ErlDrvSizeT);
static void hid_drv_timeout(ErlDrvData);
static void hid_drv_stop_select(ErlDrvEvent, void*);

ErlDrvTermData ATOM(ok);
ErlDrvTermData ATOM(error);
ErlDrvTermData ATOM(undefined);
ErlDrvTermData ATOM(hid_device_info);
ErlDrvTermData ATOM(enumeration);
ErlDrvTermData ATOM(hid);

static ErlDrvEntry hid_drv_entry;

#define DLOG_DEBUG     7
#define DLOG_INFO      6
#define DLOG_NOTICE    5
#define DLOG_WARNING   4
#define DLOG_ERROR     3
#define DLOG_CRITICAL  2
#define DLOG_ALERT     1
#define DLOG_EMERGENCY 0
#define DLOG_NONE     -1

#ifndef DLOG_DEFAULT
#define DLOG_DEFAULT DLOG_NONE
#endif

#define DLOG(level,file,line,args...) do { \
	if (((level) == DLOG_EMERGENCY) ||				\
	    ((debug_level >= 0) && ((level) <= debug_level))) { \
	    emit_log((level),(file),(line),args);		\
	}								\
    } while(0)

#define DEBUGF(args...) DLOG(DLOG_DEBUG,__FILE__,__LINE__,args)
#define INFOF(args...)  DLOG(DLOG_INFO,__FILE__,__LINE__,args)
#define NOTICEF(args...)  DLOG(DLOG_NOTICE,__FILE__,__LINE__,args)
#define WARNINGF(args...)  DLOG(DLOG_WARNING,__FILE__,__LINE__,args)
#define ERRORF(args...)  DLOG(DLOG_ERROR,__FILE__,__LINE__,args)
#define CRITICALF(args...)  DLOG(DLOG_CRITICAL,__FILE__,__LINE__,args)
#define ALERTF(args...)  DLOG(DLOG_ALERT,__FILE__,__LINE__,args)
#define EMERGENCYF(args...)  DLOG(DLOG_EMERGENCY,__FILE__,__LINE__,args)

static int debug_level = DLOG_DEFAULT;

static void emit_log(int level, char* file, int line, ...)
{
    va_list ap;
    char* fmt;

    if ((level == DLOG_EMERGENCY) ||
	((debug_level >= 0) && (level <= debug_level))) {
	va_start(ap, line);
	fmt = va_arg(ap, char*);
	fprintf(stderr, "%s:%d: ", file, line); 
	vfprintf(stderr, fmt, ap);
	fprintf(stderr, "\r\n");
	va_end(ap);
    }
}

/* general control reply function */
static ErlDrvSSizeT ctl_reply(int rep, char* buf, ErlDrvSizeT len,
			      char** rbuf, ErlDrvSizeT rsize)
{
    char* ptr;

    if ((len+1) > rsize) {
#ifdef PORT_CONTROL_BINARY
	ErlDrvBinary* bin = driver_alloc_binary(len+1);
	if (bin == NULL) 
	    return -1;
	ptr = bin->orig_bytes;	
	*rbuf = (char*) bin;
#else
	if ((ptr = driver_alloc(len+1)) == NULL)
	    return -1;
	*rbuf = ptr;
#endif
    }
    else
	ptr = *rbuf;
    *ptr++ = rep;
    memcpy(ptr, buf, len);
    return len+1;
}


static size_t device_info_size(struct hid_device_info* ptr)
{
    size_t sz = 0;

    while(ptr != NULL) {
	sz += 2;  // ERL_DRV_ATOM: atom (hid_device_info)
	sz += 3;  // ERL_DRV_STRING: path
	sz += 2;  // ERL_DRV_UINT: vendor_id
	sz += 2;  // ERL_DRV_UINT: product_id
	sz += 2+1+2*wcslen(ptr->serial_number);        // ERL_DRV_LIST<n1>
	sz += 2;  // ERL_DRV_UINT: release_number
	sz += 2+1+2*wcslen(ptr->manufacturer_string);  // ERL_DRV_LIST<n2>
	sz += 2+1+2*wcslen(ptr->product_string);       // ERL_DRV_LIST<n3>
	sz += 2;  // ERL_DRV_UINT: usage_page
	sz += 2;  // ERL_DRV_UINT: usage
	sz += 2;  // ERL_DRV_UINT: interace_number
	sz += 2;  // ERL_DRV_TUPLE: 11
	ptr = ptr->next;
    }
    return sz + 2;  // ERL_DRV_LIST: <n>
}

static int build_wstring(ErlDrvTermData* data, int j, wchar_t* wptr)
{
    if (wptr == NULL)
	data[j++] = ERL_DRV_NIL;
    else {
	int len = 0;
	while(*wptr != 0) {
	    data[j++] = ERL_DRV_UINT;
	    data[j++] = (ErlDrvTermData) *wptr++;
	    len++;
	}
	data[j++] = ERL_DRV_NIL;
	data[j++] = ERL_DRV_LIST;
	data[j++] = (ErlDrvTermData) len+1;
    }
    return j;
}


static int device_info_build(ErlDrvTermData* data, int j, struct hid_device_info* ptr)
{
    int i = 0;

    while(ptr) {
	data[j++] = ERL_DRV_ATOM;
	data[j++] = (ErlDrvTermData) ATOM(hid_device_info);
	if (ptr->path == NULL)
	    data[j++] = ERL_DRV_NIL;
	else {
	    data[j++] = ERL_DRV_STRING;
	    data[j++] = (ErlDrvTermData) ptr->path;
	    data[j++] = (ErlDrvTermData) strlen(ptr->path);
	}

	data[j++] = ERL_DRV_UINT;
	data[j++] = (ErlDrvTermData) ptr->vendor_id;

	data[j++] = ERL_DRV_UINT;
	data[j++] = (ErlDrvTermData) ptr->product_id;

	j = build_wstring(data, j, ptr->serial_number);

	data[j++] = ERL_DRV_UINT;
	data[j++] = (ErlDrvTermData) ptr->release_number;

	j = build_wstring(data, j, ptr->manufacturer_string);
	j = build_wstring(data, j, ptr->product_string);

	data[j++] = ERL_DRV_UINT;
	data[j++] = (ErlDrvTermData) ptr->usage_page;

	data[j++] = ERL_DRV_UINT;
	data[j++] = (ErlDrvTermData) ptr->usage;

	data[j++] = ERL_DRV_UINT;
	data[j++] = (ErlDrvTermData) ptr->interface_number;

	data[j++] = ERL_DRV_TUPLE;
	data[j++] = (ErlDrvTermData) 11;
	ptr = ptr->next;
	i++;
    }

    data[j++] = ERL_DRV_LIST;
    data[j++] = (ErlDrvTermData) i;
    return j;
}


static ErlDrvSSizeT ctl_string_reply(hid_ctx_t* ctx, int rep,
				     wchar_t* string,
				     char** rbuf, ErlDrvSizeT rsize)
{
    char     reply[4*MAX_STRING];
    uint8_t* ptr = (uint8_t*) reply;
    int      i = 0;

    if (string != NULL) {
	while(string[i] != 0) {
	    put_uint32(ptr, string[i]);
	    ptr += 4;
	    i++;
	}
    }
    return ctl_reply(rep, reply, (ptr - (uint8_t*)reply), rbuf, rsize);
}

// setup global object area
// load atoms etc.

static int hid_drv_init(void)
{
    debug_level = DLOG_DEFAULT;
    DEBUGF("hid_driver_init");
    hid_init();
    INIT_ATOM(ok);
    INIT_ATOM(error);
    INIT_ATOM(undefined);
    INIT_ATOM(hid_device_info);
    INIT_ATOM(enumeration);
    INIT_ATOM(hid);
    return 0;
}

// clean up global stuff
static void hid_drv_finish(void)
{
    hid_exit();
}

static ErlDrvData hid_drv_start(ErlDrvPort port, char* command)
{
    (void) command;
    hid_ctx_t* ctx;

    if ((ctx = (hid_ctx_t*) 
	 driver_alloc(sizeof(hid_ctx_t))) == NULL) {
	errno = ENOMEM;
	return ERL_DRV_ERROR_ERRNO;
    }
    ctx->port   = port;
    ctx->dport  = driver_mk_port(port);
    ctx->dev    = NULL;
    ctx->ref    = 1;
    ctx->active = 0;
    ctx->buflen = MAX_READ_LENGTH;
    ctx->receiver = driver_connected(port);

    DEBUGF("hid_drv: start (%s)", command);
#ifdef PORT_CONTROL_BINARY
    set_port_control_flags(port, PORT_CONTROL_FLAG_BINARY);
#endif
    return (ErlDrvData) ctx;
}

static void hid_drv_stop(ErlDrvData d)
{
    hid_ctx_t* ctx = (hid_ctx_t*) d;
    if (ctx->dev != NULL) {
	if (ctx->active)
	    driver_select(ctx->port, ctx->event, ERL_DRV_READ, 0);
	hid_close(ctx->dev);
	ctx->dev = NULL;
    }
    driver_free(ctx);
}

static ErlDrvSSizeT hid_drv_ctl(ErlDrvData d, 
				unsigned int cmd, char* buf0, ErlDrvSizeT len,
				char** rbuf, ErlDrvSizeT rsize)
{
    hid_ctx_t* ctx = (hid_ctx_t*) d;
    uint8_t* buf = (uint8_t*) buf0;

    DEBUGF("hid_drv: ctl: cmd=%u, len=%d", cmd, len);
    switch(cmd) {
    case CMD_ENUMERATE: {
	uint16_t vid;
	uint16_t pid;
	struct hid_device_info* info;
	ErlDrvTermData* data;
	size_t sz;
	int j;
	uint32_t ref;

	if (len != 4) goto badarg;
	vid = get_uint16(buf);
	pid = get_uint16(buf+2);
	info = hid_enumerate(vid, pid);
	ref  = ctx->ref++;
	
	sz = device_info_size(info)+2*4;
	DEBUGF("hid_drv: enumerate size = %d", sz);
	data = driver_alloc(sz*sizeof(ErlDrvTermData));
	// {enumeration, Port, Ref, List}
	j = 0;
	data[j++] = ERL_DRV_ATOM;
	data[j++] = ATOM(enumeration);

	data[j++] = ERL_DRV_PORT;
	data[j++] = ctx->dport;

	data[j++] = ERL_DRV_UINT;
	data[j++] = (ErlDrvTermData) ref;

	j = device_info_build(data, j, info);

	data[j++] = ERL_DRV_TUPLE;
	data[j++] = 4;

	SEND_TERM(ctx, driver_caller(ctx->port), data, j);

	driver_free(data);
	hid_free_enumeration(info);

	return ctl_reply(4, (void*) &ref, sizeof(ref), rbuf, rsize);
    }

    case CMD_OPEN: {
	uint16_t vid;
	uint16_t pid;
	wchar_t  serial[MAX_SERIAL+1];
	wchar_t* serp = NULL;
	int i;
	int j;

	if ((len < 4) || (len & 3) != 0)
	    goto badarg;
	if (ctx->dev != NULL)
	    goto badarg;
	vid = get_uint16(buf);
	pid = get_uint16(buf+2);
	for (i = 4, j=0; (i < len) && (j < MAX_SERIAL); i += 4, j++)
	    serial[j] = get_uint32(buf+i);
	if (j > 0) {
	    serial[j++] = 0;
	    serp = serial;
	}
	if ((ctx->dev = hid_open(vid, pid, serp)) == NULL)
	    goto werror;
	ctx->event = (ErlDrvEvent) hid_get_event_handle(ctx->dev);
	goto ok;
    }

    case CMD_OPEN_PATH: {
	char path[MAX_PATH+1];
	if ((len == 0) || (len > MAX_PATH))
	    goto error;
	if (ctx->dev != NULL)
	    goto badarg;
	memcpy(path, buf, len);
	path[len] = 0;
	if ((ctx->dev = hid_open_path(path)) == NULL)
	    goto werror;
	ctx->event = (ErlDrvEvent) hid_get_event_handle(ctx->dev);
	goto ok;
    }

    case CMD_WRITE: {
	int32_t n;
	if (ctx->dev == NULL) 
	    goto badarg;
	DEBUGF("hid: write %d bytes\n", len);
	if ((n = hid_write(ctx->dev, buf, len)) < 0)
	    goto werror;
	return ctl_reply(4, (void*) &n, sizeof(n), rbuf, rsize);
    }

    case CMD_SET_BUFFER: {
	uint32_t length;
	if (len != 4)
	    goto badarg;
	if ((length = get_uint32(buf)) > MAX_READ_LENGTH)
	    length = MAX_READ_LENGTH;
	ctx->buflen = length;
	goto ok;
    }

    case CMD_SEND_FEATURE_REPORT: {
	uint8_t  data[MAX_REPORT+1];
	int n;

	if ((len == 0) || (len >= MAX_REPORT) || (ctx->dev == NULL))
	    goto badarg;
	memcpy(data, buf, len);
	if ((n = hid_send_feature_report(ctx->dev, data, len)) < 0)
	    goto werror;
	goto ok;
    }

    case CMD_GET_FEATURE_REPORT: {
	uint8_t  data[MAX_REPORT+1];
	uint8_t  report;
	uint32_t length;
	int n;

 	if ((len != 5) || (ctx->dev == NULL))
	    goto badarg;
	report = buf[0];
	length = get_uint32(buf+1);
	if (length > MAX_REPORT)
	    goto badarg;
	length++;
	data[0] = report;
	if ((n=hid_get_feature_report(ctx->dev, data, length)) < 0)
	    goto werror;
	return ctl_reply(7, (void*)data, n, rbuf, rsize);
    }

    case CMD_CLOSE: {
	if (ctx->dev != NULL) {
	    if (ctx->active)
		driver_select(ctx->port, ctx->event, ERL_DRV_READ, 0);
	    hid_close(ctx->dev);
	    ctx->dev = NULL;
	}
	goto ok;
    }

    case CMD_GET_MANUFACTURER_STRING: {
	wchar_t  string[MAX_STRING+1];

	if (ctx->dev == NULL)
	    goto badarg;
	if (hid_get_manufacturer_string(ctx->dev, string, MAX_STRING) < 0)
	    goto werror;
	return ctl_string_reply(ctx, 3, string, rbuf, rsize);
    }
	
    case CMD_GET_PRODUCT_STRING: {
	wchar_t  string[MAX_STRING+1];

	if (ctx->dev == NULL)
	    goto badarg;
	if (hid_get_product_string(ctx->dev, string, MAX_STRING) < 0)
	    goto werror;
	return ctl_string_reply(ctx, 3, string, rbuf, rsize);
    }

    case CMD_GET_SERIAL_NUMBER_STRING: {
	wchar_t  string[MAX_STRING+1];

	if (ctx->dev == NULL)
	    goto badarg;
	if (hid_get_serial_number_string(ctx->dev, string, MAX_STRING) < 0)
	    goto werror;
	return ctl_string_reply(ctx, 3, string, rbuf, rsize);
    }

    case CMD_GET_INDEXED_STRING: {
	wchar_t  string[MAX_STRING+1];
	int string_index;
	
	if ((len != 4) || (ctx->dev == NULL))
	    goto badarg;
	string_index = get_int32(buf);
	if (hid_get_indexed_string(ctx->dev,string_index,string,MAX_STRING)<0)
	    goto werror;
	return ctl_string_reply(ctx, 3, string, rbuf, rsize);
    }

    case CMD_SET_ACTIVE: {
	int active;
	if ((ctx->dev == NULL) || (len != 1))
	    goto badarg;
	active = buf[0];
	if (active != ctx->active) {
	    if (active) {
		ctx->receiver = driver_caller(ctx->port);
		driver_select(ctx->port, ctx->event, ERL_DRV_READ, 1);
	    }
	    else
		driver_select(ctx->port, ctx->event, ERL_DRV_READ, 0);
	    ctx->active = active;
	}
	goto ok;
    }

    case CMD_SET_DEBUG: {
	if (len != 4)
	    goto badarg;
	debug_level = get_int32(buf);
	goto ok;
    }
    default:
	goto badarg;
    }
ok:
    return ctl_reply(0, NULL, 0, rbuf, rsize);
badarg:
    errno = EINVAL;
    goto error;

werror: {
	const wchar_t* err_wstr;
	if (ctx->dev)
	    err_wstr = hid_error(ctx->dev);
	else
	    err_wstr = L"unknown";

	DEBUGF("hid: werror %ls\n", err_wstr);

	return ctl_string_reply(ctx, 5, (wchar_t*) err_wstr, rbuf, rsize);
    }


error: {
        char* err_str = erl_errno_id(errno);
	return ctl_reply(255, err_str, strlen(err_str), rbuf, rsize);
    }
}


static void hid_drv_output(ErlDrvData d, char* buf, ErlDrvSizeT len)
{
    (void) d;
    (void) buf;
    (void) len;
    // hid_ctx_t*   ctx = (hid_ctx_t*) d;
    DEBUGF("hid_drv: output");
}

static void hid_drv_outputv(ErlDrvData d, ErlIOVec *ev)
{
    (void) d;
    (void) ev;
//  hid_ctx_t*   ctx = (hid_ctx_t*) d;
    DEBUGF("hid_drv: outputv");
}

static void hid_drv_event(ErlDrvData d, ErlDrvEvent e,
				  ErlDrvEventData ed)
{
    (void) d;
    (void) e;
    (void) ed;
//  hid_ctx_t* ctx = (hid_ctx_t*) d;
    DEBUGF("hid_drv: event called");
}

static void hid_drv_ready_input(ErlDrvData d, ErlDrvEvent e)
{
    uint8_t  buf[MAX_READ_LENGTH];
    hid_ctx_t* ctx = (hid_ctx_t*) d;

    DEBUGF("hid_drv: ready_input called");
    if (ctx->event == e) {
	ErlDrvTermData msg[16];
	int n;
	int i = 0;
	int len = ctx->buflen;

	if (len > MAX_READ_LENGTH)
	    len = MAX_READ_LENGTH;

	if ((n = hid_read_timeout(ctx->dev, buf, len, 0)) < 0) {
	    DEBUGF("hid_drv: hid_read_timeout failed");
	    return;
	}
	if (n == 0)  // no data
	    return;
	// send {hid, <port>, <<data>>}
	msg[i++] = ERL_DRV_ATOM;
	msg[i++] = ATOM(hid);

	msg[i++] = ERL_DRV_PORT;
	msg[i++] = ctx->dport;

	msg[i++] = ERL_DRV_BUF2BINARY;
	msg[i++] = (ErlDrvTermData) buf;
	msg[i++] = n;

	msg[i++] = ERL_DRV_TUPLE;
	msg[i++] = 3;
	
	SEND_TERM(ctx, ctx->receiver, msg, i); 

	if (ctx->active == 2) {
	    driver_select(ctx->port, ctx->event, ERL_DRV_READ, 0);
	    ctx->active = 0;
	}
    }
}

static void hid_drv_ready_output(ErlDrvData d, ErlDrvEvent e)
{
    (void) d;
    (void) e;
//  hid_ctx_t* ctx = (hid_ctx_t*) d;
    DEBUGF("hid_drv: ready_output called");
}

// operation timed out
static void hid_drv_timeout(ErlDrvData d)
{
    (void) d;
    DEBUGF("hid_drv: timeout");
}

static void hid_drv_stop_select(ErlDrvEvent event, void* arg)
{    
    (void) arg;
    DEBUGF("hid_drv: stop_select event=%d", INT_EVENT(event));
    close(INT_EVENT(event));
}

DRIVER_INIT(hid_drv)
{
    ErlDrvEntry* ptr = &hid_drv_entry;

    DEBUGF("hid DRIVER_INIT");

    ptr->driver_name = "hid_drv";
    ptr->init  = hid_drv_init;
    ptr->start = hid_drv_start;
    ptr->stop  = hid_drv_stop;
    ptr->output = hid_drv_output;
    ptr->ready_input  = hid_drv_ready_input;
    ptr->ready_output = hid_drv_ready_output;
    ptr->finish = hid_drv_finish;
    ptr->control = hid_drv_ctl;
    ptr->timeout = hid_drv_timeout;
    ptr->outputv = hid_drv_outputv;
    ptr->ready_async = 0;
    ptr->flush = 0;
    ptr->call = 0;
    ptr->event = hid_drv_event;
    ptr->extended_marker = ERL_DRV_EXTENDED_MARKER;
    ptr->major_version = ERL_DRV_EXTENDED_MAJOR_VERSION;
    ptr->minor_version = ERL_DRV_EXTENDED_MINOR_VERSION;
    ptr->driver_flags = ERL_DRV_FLAG_USE_PORT_LOCKING;
    ptr->process_exit = 0;
    ptr->stop_select = hid_drv_stop_select;
    return ptr;
}
