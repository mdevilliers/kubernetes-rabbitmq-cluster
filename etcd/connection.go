package etcd

import (
	"fmt"
	etcdclient "github.com/coreos/go-etcd/etcd"
)

type Connection struct {
	client *etcdclient.Client
}

func NewConnection(etcdConnectionStrings []string) *Connection {

	c := etcdclient.NewClient(etcdConnectionStrings)

	return &Connection{
		client: c,
	}
}

// watches for changes in the cluster
func (c *Connection) Watch(etcdPath string, ch chan *etcdclient.Response, stop chan bool) {

	fmt.Println("watching : ", etcdPath)

	_, err := c.client.Watch(etcdPath, 0, true, ch, stop)

	if err != nil {
		fmt.Println(err.Error())
	}
}

func (c *Connection) Get(path string) (*etcdclient.Response, error) {
	return c.client.Get(path, true, true)
}

func (c *Connection) SetDir(path string, ttl uint64) (*etcdclient.Response, error) {
	return c.client.SetDir(path, ttl)
}

func (c *Connection) Set(key, value string, ttl uint64) (*etcdclient.Response, error) {
	return c.client.Set(key, value, ttl)
}
