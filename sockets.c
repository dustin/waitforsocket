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

#define MAX(a, b) (a > b ? a : b)

requested_socket mk_req(char *host, char *svc)
{
	requested_socket rv;

	rv.host=host;
	rv.svc=svc;
	rv.success=0;
	rv.socket=-1;

	return rv;
}

static enum returnvalues waitForConnect(requested_socket *req)
{
	int selected=0, i=0, maxs=-1, seen=0;
	time_t now=0, then=0;
	fd_set rset;
	fd_set wset;
	fd_set eset;
	struct timeval tv;
	enum returnvalues rv=ERR_TIMEOUT;;

	for(i=0; req[i].host; i++) {
		req[i].success=0;
	}

	then=time(NULL) + 5;
	now=time(NULL);

	while(now < then && seen == 0) {

		maxs=-1;

		FD_ZERO(&rset);
		FD_ZERO(&wset);
		FD_ZERO(&eset);

		for(i=0; req[i].host; i++) {
			if(req[i].socket >= 0 && req[i].success == 0) {
				FD_SET(req[i].socket, &rset);
				FD_SET(req[i].socket, &wset);
				FD_SET(req[i].socket, &eset);
				maxs=MAX(req[i].socket, maxs);
			}
		}

		if(maxs < 0) {
			break;
		}

		/* Wait up to five seconds */
		tv.tv_sec=(then-now);
		tv.tv_usec=0;

		selected=select(maxs+1, &rset, &wset, &eset, &tv);
		if(selected > 0) {
			for(i=0; req[i].host && rv != RV_SUCCESS; i++) {
				if(req[i].socket >= 0) {
					if(FD_ISSET(req[i].socket, &rset)) {
						char buf[1];
						/* Make sure we can read a byte */
						if(read(req[i].socket, &buf, 1) == 1) {
							req[i].success=1;
							seen++;
						} else {
							rv=ERR_ERRNO;
							req[i].success=-1;
						}
					} else if(FD_ISSET(req[i].socket, &wset)) {
						req[i].success=1;
						seen++;
					} else {
						/* I don't quite understand why it's not OK to consider
						 * this a failure, but in practice, not ignoring this
						 * gives me a lot of false negatives. */
						rv=ERR_ERRNO;
					}
				}
			}
		}

		now=time(NULL);
	}

	rv = seen > 0 ? RV_SUCCESS : rv;

	/* True if there was at least one thing that hinted as being available */
	return(rv);
}

enum returnvalues
setupOneConnection(requested_socket *req)
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

	if(rv != RV_SUCCESS && req->socket >= 0) {
		perror(cause);
		close(req->socket);
		req->socket = -1;
	}

	return rv;
}

enum returnvalues
attemptConnection(requested_socket *reqs)
{
	int i=0, any_success=0;
	enum returnvalues rv = ERR_ERRNO;

	for(i=0; reqs[i].host; i++) {
		any_success |= (setupOneConnection(&reqs[i]) == RV_SUCCESS);
	}

	rv=waitForConnect(reqs);

	for(i=0; reqs[i].host; i++) {
		if(reqs[i].socket>=0) {
			close(reqs[i].socket);
		}
	}

	return rv;
}
