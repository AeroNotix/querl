package main

import (
	"fmt"
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
	select {}
}
