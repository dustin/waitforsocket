/*
 * Copyright (c) 1997 Dustin Sallings
 *
 * $Id: sockets.c,v 1.4 2003/06/12 20:20:30 dustin Exp $
 */
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <fcntl.h>
#include <signal.h>
#include <time.h>
#include <sys/time.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <errno.h>
#include <sys/errno.h>
#include <netinet/in.h>
#include <netdb.h>
#include <syslog.h>
#include <netinet/tcp.h>

#include "waitforsocket.h"

requested_socket mk_req(char *host, char *svc)
{
	requested_socket rv;

	rv.host=host;
	rv.svc=svc;
	rv.success=0;
	rv.socket=-1;

	return rv;
}

static enum returnvalues waitForConnect(int s)
{
	int selected=0;
	fd_set rset;
	fd_set wset;
	fd_set eset;
	struct timeval tv;
	enum returnvalues rv=ERR_ERRNO;

	FD_ZERO(&rset);
	FD_ZERO(&wset);
	FD_ZERO(&eset);
	FD_SET(s, &rset);
	FD_SET(s, &wset);
	FD_SET(s, &eset);

	/* Wait up to five seconds */
	tv.tv_sec=5;
	tv.tv_usec=0;

	selected=select(s+1, &rset, &wset, &eset, &tv);
	if(selected > 0) {
		if(FD_ISSET(s, &rset)) {
			char buf[1];
			/* Make sure we can read a byte */
			if(read(s, &buf, 1) == 1) {
				rv=RV_SUCCESS;
			}
		} else if(FD_ISSET(s, &wset)) {
			rv=RV_SUCCESS;
		} else {
			rv=ERR_ERRNO;
		}
	} else {
		rv=ERR_TIMEOUT;
	}
	
	/* True if there was at least one thing that hinted as being available */
	return(rv);
}

enum returnvalues
attemptConnection(requested_socket *req)
{
	struct addrinfo hints, *res, *res0;
	enum returnvalues rv=ERR_ERRNO;
	struct linger l;
	int fflags =0;
	char *cause=NULL;
	int err=0;

	if (req->host == NULL || req->svc == NULL) {
		return (0);
	}

	memset(&hints, 0, sizeof(hints));
	hints.ai_family = PF_UNSPEC;
	hints.ai_socktype = SOCK_STREAM;
	err=getaddrinfo(req->host, req->svc, &hints, &res0);
	if(err != 0) {
		fprintf(stderr, "Error looking up %s:%s:  %s\n",
			req->host, req->svc, gai_strerror(err));
		return(ERR_DNS);
	}

	cause="no addresses";
	for (res = res0; res; res = res->ai_next) {

		if ((req->socket = socket(res->ai_family, res->ai_socktype,
			res->ai_protocol)) < 0) {

			cause="socket";
			continue;
		}

		l.l_onoff = 1;
		l.l_linger = 60;
		setsockopt(req->socket, SOL_SOCKET, SO_LINGER, (char *) &l, sizeof(l));

		/* Configure non-blocking IO */
		fflags = fcntl(req->socket, F_GETFL);
		if(fcntl(req->socket, F_SETFL, fflags | O_NONBLOCK) < 0) {
			perror("fcntl");
		}

		if (connect(req->socket, res->ai_addr, res->ai_addrlen) < 0) {
			if(errno==EINPROGRESS) {
				rv = RV_SUCCESS;
				break;
			} else {
				cause="connect";
			}
		} else { 
			rv = RV_SUCCESS;
			break;
		}
	}
	freeaddrinfo(res0);

	/* If we got this far, wait for data */
	if(rv == RV_SUCCESS) {
		rv=waitForConnect(req->socket);
	} else {
		perror(cause);
	}

	if(req->socket>=0) {
		close(req->socket);
	}

	return rv;
}
