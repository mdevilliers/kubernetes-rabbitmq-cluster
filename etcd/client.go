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
/registry/rabbitmq-cluster/{LOGICAL_NAME}/nodes/.....
/registry/rabbitmq-cluster/{LOGICAL_NAME}/node/users/....
/registry/rabbitmq-cluster/{LOGICAL_NAME}/node/plugins/....
*/

func (c *Client) InitOrPanic(clusterName string) {

	clusterNodeName := "/registry/rabbitmq-cluster/" + clusterName

	_, err := c.client.Get(clusterNodeName, true, true)

	if err != nil {

		if isKeyNotFoundError(err) {

			_, err := c.client.SetDir(clusterNodeName, 0)
			if err != nil {
				message := "Error creating key :" + clusterNodeName + err.Error()
				panic(message)
			}

		} else {
			panic(err.Error())
		}
	}
	go c.loop(clusterNodeName)
}

func (c *Client) loop(clusterNodeName string) {

	ipAddress, err := util.GetIPAddress()

	if err != nil {
		panic("Error retreiving ipAddress : " + err.Error())
	}

	fmt.Println(clusterNodeName, ipAddress)

	_, err = c.client.Set(clusterNodeName+"/"+ipAddress, "alive", 30)

	if err != nil {
		fmt.Println(err.Error())
	}
	// pings etcd on a schedule setting its alive key in the nodes key!
	util.Schedule(func() { c.loop(clusterNodeName) }, time.Second*20)

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
