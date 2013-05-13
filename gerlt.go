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
	go func() {
		for {
			_, err := http.Post("http://localhost:8080/push/", "application/json", strings.NewReader(`{"queue": "tt", "id":123}`))
			if err != nil {
				fmt.Println(err)
				continue
			}
			<-time.After(time.Duration(rand.Intn(2500)) * time.Millisecond)
		}
	}()
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
		fmt.Println(string(b))
		<-time.After(time.Duration(rand.Intn(2500)) * time.Millisecond)
	}
	select {}
}
