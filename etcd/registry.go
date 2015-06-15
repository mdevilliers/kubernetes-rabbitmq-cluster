package etcd

import (
	"errors"
	"fmt"
	etcdclient "github.com/coreos/go-etcd/etcd"
	"sync"
)

var (
	RegistryIsSealedError          = errors.New("Registry is sealed - you cannot add or remove callbacks")
	ItemNotFoundError              = errors.New("Item not found")
	ItemOfSameKeyAlreadyAddedError = errors.New("Item of same key already added")
)

type Registry struct {
	sync.RWMutex
	sealed     bool
	callbacks  map[string]RegistryCallback
	connection *Connection
	paths      *Paths
}

// TODO : return a object with Path, Value, NewValue
type RegistryCallback func(*etcdclient.Response) (bool, error)

func NewRegistry(connection *Connection, paths *Paths) *Registry {
	return &Registry{
		callbacks:  make(map[string]RegistryCallback),
		connection: connection,
		paths:      paths,
	}
}

func (r *Registry) Put(path string, f RegistryCallback) error {
	r.Lock()
	defer r.Unlock()

	if r.sealed {
		return RegistryIsSealedError
	}

	_, exists := r.callbacks[path]

	if exists {
		return ItemOfSameKeyAlreadyAddedError
	}

	r.callbacks[path] = f
	return nil
}

func (r *Registry) Get(path string) (RegistryCallback, error) {
	r.Lock()
	defer r.Unlock()

	cb, ok := r.callbacks[path]
	if !ok {
		return nil, ItemNotFoundError
	}

	return cb, nil
}

func (r *Registry) Seal() {
	r.Lock()
	defer r.Unlock()
	r.sealed = true
}

func (r *Registry) StartWatches() {
	r.Lock()
	defer r.Unlock()

	ch := make(chan *etcdclient.Response, 10)
	stop := make(chan bool, 1)

	go r.simpleReceiver(ch)
	go r.connection.Watch(r.paths.ClusterRoot(), ch, stop)
}

func (r *Registry) simpleReceiver(cs chan *etcdclient.Response) {
	fmt.Println("receiving...")

	for response := range cs {
		// TODO delegate to callback
		fmt.Printf("Received in watch : %s \n", PrettyPrintResponse(response))
	}
}
