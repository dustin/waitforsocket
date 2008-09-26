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

#include "waitforsocket.h"

static requested_socket *reqs;

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

static int
parse_commands(int argc, char **argv)
{
	int rv = 0;
	reqs = calloc(argc, sizeof(requested_socket));

	if(argc < 3) {
		return -1;
	}

	reqs[0]=mk_req(argv[1], argv[2]);

	return rv;
}

int 
main(int argc, char **argv)
{
	time_t  t=0, status;

	if (parse_commands(argc, argv) < 0) {
		usage(argv[0]);
		exit(0);
	}

	while((status=attemptConnection(&reqs[0])) != RV_SUCCESS) {
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
