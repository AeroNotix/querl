package main

import (
	"fmt"
	"io/ioutil"
	"math/rand"
	"net/http"
	"runtime"
	"strings"
	"time"
)

func main() {
	runtime.GOMAXPROCS(runtime.NumCPU())
	for {
		resp, err := http.Post("http://localhost:8080/pop/", "application/json", strings.NewReader(`{"queue": "tt"}`))
		if err != nil {
			fmt.Println(err)
			continue
		}
		b, err := ioutil.ReadAll(resp.Body)
		if err != nil {
			fmt.Println(err)
			continue
		}
		resp.Body.Close()
		if string(b) != "" {
			fmt.Println(string(b))
		}
		<-time.After(time.Duration(rand.Intn(2500)) * time.Millisecond)
	}

}
