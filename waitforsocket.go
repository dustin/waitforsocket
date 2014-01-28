package main

import (
	"flag"
	"fmt"
	"log"
	"net"
	"os"
	"time"
)

var (
	timeout    = flag.Duration("timeout", 5*time.Second, "connection/retry timeout")
	absTimeout = flag.Duration("absTimeout", 0, "absolute timeout (0 == never)")
	required   = flag.Int("required", 0, "How many connections required (0 == all)")
)

func init() {
	flag.Usage = func() {
		fmt.Fprintf(os.Stderr,
			"Usage: waitforsocket [opts] host:service|port [...]\n")
		fmt.Fprintf(os.Stderr, "Options:\n")
		flag.PrintDefaults()
	}
}

type res struct {
	addr       string
	started, t time.Time
}

func wait(addr string, ch chan res) {
	ticker := time.Tick(*timeout)
	started := time.Now()
	for {
		c, err := net.DialTimeout("tcp", addr, *timeout)
		if err == nil {
			c.Close()
			ch <- res{addr, started, time.Now()}
			return
		}
		log.Printf("%v", err)
		<-ticker
	}

}

func main() {
	flag.Parse()

	if flag.NArg() < 1 {
		flag.Usage()
		os.Exit(64)
	}

	if *required == 0 {
		*required = flag.NArg()
	}

	ch := make(chan res)

	for _, hp := range flag.Args() {
		go wait(hp, ch)
	}

	var absTo <-chan time.Time
	if *absTimeout > 0 {
		absTo = time.After(*absTimeout)
	}

	responses := 0
	for responses < *required {
		select {
		case r := <-ch:
			responses++
			log.Printf("Connected to %v after %v", r.addr, r.t.Sub(r.started))
		case <-absTo:
			log.Printf("Timed out")
			os.Exit(1)
		}
	}
}
