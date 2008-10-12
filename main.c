/*
 * Copyright (c) 1998  Dustin Sallings
 *
 * $Id: main.c,v 1.3 2003/06/12 20:18:13 dustin Exp $
 */

#include <config.h>

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <time.h>
#include <assert.h>
#include <errno.h>
#include <sysexits.h>

#include "waitforsocket.h"

static void
usage(const char *name)
{
	fprintf(stderr, "Usage:\n  %s hostname service|port\n", name);
}

static char *
unknown_error(int status)
{
	/* This is basically a programming error, but it should at least
	be able to tell us *what* programming error. */
	static char buf[32];
	snprintf(buf, sizeof(buf)-1, "unknown: %d", (int)status);
	return buf;
}

int 
main(int argc, char **argv)
{
	char   *hostname=0x00;
	char   *svc=0x00;
	time_t  t=0, status;

	if (argc < 3) {
		usage(argv[0]);
		exit(EX_USAGE);
	}

	hostname = argv[1];
	svc=argv[2];

	while((status=attemptConnection(hostname, svc)) != RV_SUCCESS) {
		t=time(NULL);
		char *err="unknown";
		switch(status) {
			case ERR_ERRNO:   err=strerror(errno);       break;
			case ERR_TIMEOUT: err="timeout";             break;
			case ERR_DNS:     err="getaddrinfo error";   break;
			default:          err=unknown_error(status);
		}
		assert(err != NULL);
		fprintf(stderr, "Failed to connect (%s) at %s", err, ctime(&t));
		sleep(1);
	}
	t=time(NULL);
	fprintf(stderr, "Connected at %s", ctime(&t));

	return(0);
}
