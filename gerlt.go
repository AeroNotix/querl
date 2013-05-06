package main

import (
	"fmt"
	"net/http"
	"strings"
	"time"
)

func main() {
	for {
		_, err := http.Post("http://localhost:8080/push/", "application/json", strings.NewReader(`{"id":123}`))
		if err != nil {
			fmt.Println(err)
		}
		<-time.After(50 * time.Millisecond)
	}
}
