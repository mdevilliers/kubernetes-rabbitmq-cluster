package sidekick

import (
	"errors"
	"fmt"
	etcdclient "github.com/coreos/go-etcd/etcd"
	"github.com/mdevilliers/kubernetes-rabbitmq-cluster/pkg/etcd"
	"github.com/mdevilliers/kubernetes-rabbitmq-cluster/pkg/paths"
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
	connection *etcd.Connection
	paths      *paths.Paths
}

type Response struct {
	Path      string
	NewValue  string
	OldValue  string
	WatchPath string
}

// TODO : return a object with Path, Value, NewValue
type RegistryCallback func(*Response) (bool, error)

func NewRegistry(connection *etcd.Connection, paths *paths.Paths) *Registry {
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

//TODO : wire up initial state provider
func (r *Registry) StartWatches() {
	r.Lock()
	defer r.Unlock()

	// TODO : wire up stop correctly
	stop := make(chan bool, 1)

	for key, value := range r.callbacks {

		fmt.Println("Key:", key, "Value:", value)
		ch := make(chan *etcdclient.Response, 10)

		go r.simpleReceiver(ch, key, value)
		go r.connection.Watch(key, ch, stop)
	}
}

func (r *Registry) simpleReceiver(cs chan *etcdclient.Response, watchPath string, cb RegistryCallback) {

	for response := range cs {

		fmt.Printf("Received in watch : %s \n", etcd.PrettyPrintResponse(response))

		callbackResponse := &Response{
			Path:      response.Node.Key,
			NewValue:  response.Node.Value,
			OldValue:  response.PrevNode.Value,
			WatchPath: watchPath,
		}

		fmt.Printf("Received in watch : Path : %s, Old Value : %s, New Value %s \n", callbackResponse.Path, callbackResponse.OldValue, callbackResponse.NewValue)

		cb(callbackResponse)
	}
}
