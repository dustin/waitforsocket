# waitforsocket

`waitforsocket` is a tool that lets you wait for a service or multiple
services to become available.  It understands `http`, `https`, and raw
TCP sockets.

`waitforsocket` will continue to try all non-connected sockets until
the absolute timeout is reached, or enough connections are made.  If
enough connections are established before the absolute timeout, the
process will exit 0, otherwise, the process will exit with a non-zero
status code indicating the required connections were not made.

# Usage

```
Usage: waitforsocket [--absTimeout ARG] [--required ARG] [--timeout ARG]
                     targets...
  Wait for network things.

Available options:
  --absTimeout ARG         absolute timeout (default: 0)
  --required ARG           how many connections required (0 = all) (default: 0)
  --timeout ARG            connect/retry timeout (ms) (default: 5000)
  -h,--help                Show this help text
```

When waiting for services, there are three configuration parameters to
consider:

1. How long are we willing to wait for any results at all?
2. How long are we willing to wait for any given connection to be
   established?
3. How many connections are required in order to claim success?
