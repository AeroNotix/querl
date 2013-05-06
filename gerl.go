package main

import (
	"bytes"
	"encoding/json"
	"io/ioutil"
	"log"
	"net/http"
	"time"
)

var EntryChannel chan *JobEntry

type JobEntry struct {
	ID        int64
	EntryDate time.Time
}

type Jobs []*JobEntry

func (j Jobs) Serialize() []byte {
	var b bytes.Buffer
	b.WriteString(`{"jobs":[`)
	for idx, job := range j {
		b.Write(job.Serialize())
		if idx+1 != len(j) {
			b.WriteString(",")
		}
	}
	b.WriteString("]}")
	return b.Bytes()
}

func (j JobEntry) Serialize() []byte {
	b, _ := json.Marshal(j)
	return b
}

func Pop(w http.ResponseWriter, req *http.Request) {
	var jobs Jobs
	for {
		select {
		case job := <-EntryChannel:
			jobs = append(jobs, job)
		default:
			if len(jobs) < 1 {
				w.WriteHeader(http.StatusNoContent)
				return
			}
			w.Write(jobs.Serialize())
			return
		}
	}
}

func Push(w http.ResponseWriter, req *http.Request) {
	if req.Body == nil {
		w.WriteHeader(http.StatusBadRequest)
		return
	}
	defer req.Body.Close()
	b, err := ioutil.ReadAll(req.Body)
	if err != nil {
		w.WriteHeader(http.StatusBadRequest)
		w.Write([]byte(`{"success": false}`))
		log.Println(err)
		return
	}
	e := &JobEntry{}
	err = json.Unmarshal(b, e)
	if err != nil {
		w.WriteHeader(http.StatusBadRequest)
		w.Write([]byte(`{"success": false}`))
		log.Println(err)
		return
	}
	go func() {
		EntryChannel <- e
	}()
	w.Write([]byte(`{"success": true}`))
}

func main() {
	EntryChannel = make(chan *JobEntry)
	http.HandleFunc("/push/", Push)
	http.HandleFunc("/pop/", Pop)
	log.Fatal(http.ListenAndServe(":8080", nil))
}
