This is a simple go program that waits for a remote socket to become available.
It's useful to avoid guessing when a service comes back up, etc...

## Install

Assuming you have a recent [go][go] compiler installed and working
correctly, you can install `waitforsocket` with the following command:

    go get github.com/dustin/waitforsocket


## Example

    waitforsocket somehost:22 ; ssh somehost

You can wait for multiple hosts in a similar way:

    waitforsocket somehost:22 otherhost:22

If you want to be fancy, you can look for a variety of hosts, and be
satisfied when any one of them works (e.g. am I on the internet?):

    waitforsocket -required=1 www.{yahoo,google,microsoft}.com:80

## Experimental

HTTP support is currently experimental (i.e. it's useful, but too easy
to add a billion new options I didn't want to add, so it may not be
perfect enough).  You can supply a URL instead of a host:port in that
case.  e.g.:

    waitforsocket http://www.{yahoo,google,microsoft}.com/

And it will wait until these all return HTTP responses indicating
success.

[go]: http://golang.org/
