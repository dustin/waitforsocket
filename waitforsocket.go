package main

import (
	"context"
	"flag"
	"fmt"
	"log"
	"net"
	"net/http"
	"os"
	"strings"
	"time"

	"github.com/dustin/httputil"
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
	addr               string
	started, connected time.Time
}

func tryURL(ctx context.Context, url string) error {
	req, err := http.NewRequest("GET", url, nil)
	if err != nil {
		return err
	}
	req = req.WithContext(ctx)

	res, err := http.DefaultClient.Do(req)
	if err != nil {
		return err
	}

	defer res.Body.Close()
	if res.StatusCode >= 200 && res.StatusCode < 300 {
		return nil
	}
	return httputil.HTTPError(res)
}

func waitURL(ctx context.Context, addr string, ch chan res) {
	ticker := time.Tick(*timeout)
	started := time.Now()
	for {
		ctxt, cancel := context.WithTimeout(ctx, *timeout)
		err := tryURL(ctxt, addr)
		cancel()
		if err == nil {
			ch <- res{addr, started, time.Now()}
			return
		}
		log.Printf("%v", err)
		<-ticker
	}
}

func wait(ctx context.Context, addr string, ch chan res) {
	if strings.Contains(addr, "/") {
		waitURL(ctx, addr, ch)
		return
	}

	ticker := time.Tick(*timeout)
	started := time.Now()
	d := net.Dialer{}
	for {
		ctxt, cancel := context.WithTimeout(ctx, *timeout)
		c, err := d.DialContext(ctxt, "tcp", addr)
		cancel()
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

	ctx := context.Background()

	if *absTimeout > 0 {
		time.AfterFunc(*absTimeout, func() { log.Fatalf("Timed out") })
	}

	if *required == 0 {
		*required = flag.NArg()
	}

	ch := make(chan res)

	for _, hp := range flag.Args() {
		go wait(ctx, hp, ch)
	}

	responses := 0
	for responses < *required {
		r := <-ch
		responses++
		log.Printf("Connected to %v after %v", r.addr, r.connected.Sub(r.started))
	}
}
