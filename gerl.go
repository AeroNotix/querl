package main

import (
	"bytes"
	"encoding/json"
	"errors"
	"io"
	"io/ioutil"
	"log"
	"net/http"
	"runtime"
	"sync"
	"time"
)

var Queues map[string]Queue

type JobEntry struct {
	QueueName string `json:"queue"`
	ID        int64
	EntryDate time.Time
}

type Jobs []*JobEntry

type Queue struct {
	ondisk  bool
	m       sync.Mutex
	j       Jobs
	entries chan *JobEntry
}

func NewQueue() Queue {
	return Queue{entries: make(chan *JobEntry)}
}

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
	job, err := readbody(req.Body)
	if err != nil {
		writefail(w)
		return
	}
	queue, ok := Queues[job.QueueName]
	if !ok {
		writefail(w)
		return
	}
	var jobs Jobs
	for {
		select {
		case job := <-queue.entries:
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
	job, err := readbody(req.Body)
	if err != nil {
		writefail(w)
		return
	}
	go func() {
		if val, ok := Queues[job.QueueName]; ok {
			val.entries <- job
		}
	}()
	writesuccess(w)
}

func readbody(b io.ReadCloser) (*JobEntry, error) {
	if b == nil {
		return nil, errors.New("Nil body supplied!")
	}
	defer b.Close()
	body, err := ioutil.ReadAll(b)
	if err != nil {
		return nil, err
	}
	e := &JobEntry{}
	err = json.Unmarshal(body, e)
	return e, err
}

func writesuccess(w http.ResponseWriter) {
	w.Write([]byte(`{"success": true}`))
}

func writefail(w http.ResponseWriter) {
	w.WriteHeader(http.StatusBadRequest)
	w.Write([]byte(`{"success": false}`))
}

func main() {
	runtime.GOMAXPROCS(runtime.NumCPU())
	Queues = make(map[string]Queue)
	Queues["tt"] = NewQueue()
	http.HandleFunc("/push/", Push)
	http.HandleFunc("/pop/", Pop)
	log.Fatal(http.ListenAndServe(":8080", nil))
}
