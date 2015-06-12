package etcd

import (
	"fmt"
	etcdclient "github.com/coreos/go-etcd/etcd"
	"github.com/mdevilliers/kubernetes-rabbitmq-cluster/util"
	"time"
)

type Client struct {
	client *etcdclient.Client
}

func NewClient(etcdConnectionStrings []string) (*Client, error) {

	c := etcdclient.NewClient(etcdConnectionStrings)

	return &Client{
		client: c,
	}, nil
}

/*
/registry/rabbitmq-cluster/{LOGICAL_NAME}/idAddress_10.1.1.1 "alive"
/registry/rabbitmq-cluster/{LOGICAL_NAME}/idAddress_10.1.1.2 "alive"
/registry/rabbitmq-cluster/{LOGICAL_NAME}/state/users/....
/registry/rabbitmq-cluster/{LOGICAL_NAME}/state/xxxx/....
*/

func (c *Client) InitOrPanic(clusterName string) {

	clusterNodeName := getEtcdClusterRoot(clusterName)

	if err := c.ensureDirExists(clusterNodeName); err != nil {
		message := "Error creating key :" + clusterNodeName + err.Error()
		panic(message)
	}

	go c.loop(clusterNodeName)
	go c.watch(clusterNodeName)
}

// pings etcd on a schedule setting the ipadress
func (c *Client) loop(clusterName string) {

	ipAddress, err := util.GetIPAddress()

	if err != nil {
		panic("Error retreiving ipAddress : " + err.Error())
	}

	fmt.Println(clusterName, ipAddress)

	_, err = c.client.Set(clusterName+"/ipAddress_"+ipAddress, "alive", 30)

	if err != nil {
		fmt.Println(err.Error())
	}

	util.ScheduleInNewGoRoutine(func() { c.loop(clusterName) }, time.Second*20)

}

// watches for changes in the cluster
func (c *Client) watch(clusterName string) {

	ch := make(chan *etcdclient.Response, 10)
	stop := make(chan bool, 1)

	go c.simpleReceiver(ch)

	fmt.Println("watching : ", clusterName)

	_, err := c.client.Watch(getEtcdClusterRoot(clusterName), 0, true, ch, stop)

	if err != nil {
		fmt.Println(err.Error())
	}
}

func (c *Client) simpleReceiver(cs chan *etcdclient.Response) {
	fmt.Println("receiving...")

	for s := range cs {
		fmt.Println("Received in watch : ", s)
	}
}

func (c *Client) ensureDirExists(path string) error {

	_, err := c.client.Get(path, true, true)

	if err != nil {

		if isKeyNotFoundError(err) {

			_, err := c.client.SetDir(path, 0)
			if err != nil {
				return err
			}
		} else {
			return err
		}
	}
	return nil
}

func getEtcdClusterRoot(clusterName string) string {
	return "/registry/rabbitmq-cluster/" + clusterName
}

//https://github.com/coreos/etcd/blob/master/Documentation/errorcode.md
func isKeyNotFoundError(err error) bool {
	return isEtcdError(err, 100)
}

func isEtcdError(err error, code int) bool {
	eerr, ok := err.(*etcdclient.EtcdError)
	if ok {
		return eerr.ErrorCode == code
	}
	return false
}
