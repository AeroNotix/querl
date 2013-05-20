package main

import (
	"bytes"
	"encoding/json"
	"errors"
	"io"
	"io/ioutil"
	"log"
	"net/http"
	"os"
	"os/signal"
	"sync"
	"time"
)

var TIMEOUTDURATION = time.Duration(10 * time.Second)
var Queues map[string]*Queue

type JobEntry struct {
	QueueName string `json:"queue"`
	ID        int64
	EntryDate time.Time
}

type Jobs []*JobEntry
type JobFile struct {
	Jobs Jobs `json:"jobs"`
}

type Queue struct {
	entries    chan *JobEntry
	j          Jobs
	m          sync.Mutex   // Whole queue mutex
	t          sync.Mutex   // just the timer mutex
	rw         sync.RWMutex // For reading and setting fields
	ondisk     bool
	ondiskfile string
	timer      *time.Timer
}

// Initialize a new queue using the filepath as it's disk storage.
func NewQueue(filepath string) *Queue {
	q := &Queue{
		ondiskfile: filepath,
		entries:    make(chan *JobEntry),
		timer:      time.NewTimer(TIMEOUTDURATION),
	}
	go MonitorQueue(q)
	return q
}

// SaveToDisk will write any unused entries to disk
//
// This is not thread safe and external locking should be used.
func (q *Queue) SaveToDisk() {
	var jobfile JobFile
	if q.HasOnDisk() {
		q.PopOnDiskJobs(&jobfile)
	}
	for {
		select {
		case job := <-q.entries:
			jobfile.Jobs = append(jobfile.Jobs, job)
		default:
			if len(jobfile.Jobs) < 1 {
				return
			}
			w, err := os.OpenFile(q.ondiskfile, os.O_CREATE|os.O_RDWR, 0666)
			if err != nil {
				panic(err)
			}
			w.Write(jobfile.Jobs.Serialize())
			q.rw.Lock()
			defer q.rw.Unlock()
			q.ondisk = true
			return
		}
	}
}

// HasOnDisk returns the ondisk state
func (q Queue) HasOnDisk() bool {
	q.rw.RLock()
	defer q.rw.RUnlock()
	return q.ondisk
}

// Reset the underlying timer. Used when we interact with the queue so
// we can organize the writes to disk into low-activity periods.
func (q *Queue) Reset() bool {
	q.t.Lock()
	defer q.t.Unlock()
	return q.timer.Reset(TIMEOUTDURATION)
}

// Pop off all the on-disk jobs into the supplied JobFile.
func (q *Queue) PopOnDiskJobs(j *JobFile) {
	if q.ondiskfile == "" {
		panic("Queue's job file is inaccessible.")
	}
	q.m.Lock()
	defer q.m.Unlock()
	b, err := ioutil.ReadFile(q.ondiskfile)
	if err != nil {
		log.Println(err)
		return
	}
	err = json.Unmarshal(b, j)
	if err != nil {
		log.Println(err)
		return
	}
	q.ondisk = false
	if err := os.Remove(q.ondiskfile); err != nil {
		log.Println(err)
	}
}

// Monitors a queue for inactivity, if there is inactivity for the
// specified timeout duration, we write to disk.
func MonitorQueue(q *Queue) {
	log.Printf("Monitoring %s\n", q.ondiskfile)
	for {
		<-q.timer.C
		q.m.Lock()
		q.SaveToDisk()
		q.Reset()
		q.m.Unlock()
	}
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

	// We're interacting with the queue so we'll reset the timer to
	// show that
	queue.Reset()
	var jobfile JobFile

	// Get all the pending jobs from disk
	if queue.HasOnDisk() {
		queue.PopOnDiskJobs(&jobfile)
	}

	// Doing it this way allows other readers to take jobs out the
	// queue without duplicating our jobs.
	for {
		select {
		case job := <-queue.entries:
			jobfile.Jobs = append(jobfile.Jobs, job)
		default:
			if len(jobfile.Jobs) < 1 {
				w.WriteHeader(http.StatusNoContent)
				return
			}
			w.Write(jobfile.Jobs.Serialize())
			return
		}
	}
}

// Pushes a new job into the queue.
func Push(w http.ResponseWriter, req *http.Request) {
	job, err := readbody(req.Body)
	if err != nil {
		writefail(w)
		return
	}
	go func() {
		if queue, ok := Queues[job.QueueName]; ok {
			queue.Reset()
			queue.entries <- job
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

	// Save to disk in the case of horrible failure.
	defer func() {
		if err := recover(); err != nil {
			log.Println(err)
			for _, queue := range Queues {
				queue.SaveToDisk()
			}
		}
	}()

	go func() {
		sigchan := make(chan os.Signal)
		signal.Notify(sigchan, os.Interrupt)
		<-sigchan
		log.Println("Caught SIGINT, saving state")
		for _, queue := range Queues {
			queue.SaveToDisk()
		}
		os.Exit(0)
	}()

	Queues = make(map[string]*Queue)
	Queues["tt"] = NewQueue("tt.q")
	http.HandleFunc("/push/", Push)
	http.HandleFunc("/pop/", Pop)
	log.Fatal(http.ListenAndServe(":8080", nil))
}
