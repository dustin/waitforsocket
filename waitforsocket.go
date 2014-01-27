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
	timeout  = flag.Duration("timeout", 5*time.Second, "connection/retry timeout")
	required = flag.Int("required", 0, "How many connections required (0 == all)")
)

func init() {
	flag.Usage = func() {
		fmt.Fprintf(os.Stderr,
			"Usage: waitforsocket [opts] host:service|port [...]\n")
		fmt.Fprintf(os.Stderr, "Options:\n")
		flag.PrintDefaults()
	}
}

func try(addr string) error {
	c, err := net.DialTimeout("tcp", addr, *timeout)
	if err == nil {
		c.Close()
	}
	return err
}

type res struct {
	addr       string
	started, t time.Time
}

func wait(addr string, ch chan res) {
	ticker := time.Tick(*timeout)
	started := time.Now()
	for {
		if err := try(addr); err == nil {
			ch <- res{addr, started, time.Now()}
			return
		} else {
			log.Printf("%v", err)
			<-ticker
		}
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

	responses := 0
	for responses < *required {
		r := <-ch
		responses++
		log.Printf("Connected to %v after %v", r.addr, r.t.Sub(r.started))
	}
}
