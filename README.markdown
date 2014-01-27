This is a simple go program that waits for a remote socket to become available.
It's useful to avoid guessing when a service comes back up, etc...

## Example:

    waitforsocket somehost:22 ; ssh somehost

You can wait for multiple hosts in a similar way:

    waitforsocket somehost:22 otherhost:22

If you want to be fancy, you can look for a variety of hosts, and be
satisfied when any one of them works (e.g. am I on the internet?):

    waitforsocket -required=1 www.{yahoo,google,microsoft}.com:80
