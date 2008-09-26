/*
 * Copyright (c) 2005  Dustin Sallings <dustin@spy.net>
 */

typedef struct {
	char *host;
	char *svc;
	int success;
} requested_socket;

enum returnvalues { ERR_DNS=-3, ERR_TIMEOUT=-2, ERR_ERRNO=-1, RV_SUCCESS=0 };

enum returnvalues attemptConnection(requested_socket);

requested_socket mk_req(char *host, char *svc);
