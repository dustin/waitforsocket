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
	int rv = 0, current_arg = 1;
	reqs = calloc(argc, sizeof(requested_socket));

	for(current_arg=1; current_arg<argc; current_arg++) {
		if(strchr(argv[current_arg], ':') == NULL) {
			if(current_arg == argc-1) {
				fprintf(stderr, "Need a port number for %s\n",
					argv[current_arg]);
				return -1;
			}
			reqs[rv++]=mk_req(argv[current_arg], argv[current_arg+1]);
			current_arg++;
		} else {
			char *host=strdup(argv[current_arg]);
			*(strchr(host, ':')) = 0x00;
			reqs[rv++]=mk_req(host, host + strlen(host)+1);
		}
	}

	return rv;
}

int 
main(int argc, char **argv)
{
	time_t  t=0, status;

	if (parse_commands(argc, argv) < 1) {
		usage(argv[0]);
		exit(0);
	}

	while((status=attemptConnection(reqs)) != RV_SUCCESS) {
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
