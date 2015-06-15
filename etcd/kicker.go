package etcd

import (
	"fmt"
	"github.com/mdevilliers/kubernetes-rabbitmq-cluster/util"
	"time"
)

var (
	KickInterval = time.Second * 20
	KeyLifeTime  = 25
)

type Kicker struct {
	connection *Connection
	paths      *Paths
	ipAddress  string
}

func NewKicker(connection *Connection, paths *Paths) *Kicker {

	ipAddress, err := util.GetIPAddress()

	if err != nil {
		panic("Error retreiving ipAddress : " + err.Error())
	}

	kicker := &Kicker{
		connection: connection,
		paths:      paths,
		ipAddress:  ipAddress,
	}

	if err := kicker.ensureDirExists(paths.ClusterRoot()); err != nil {
		panic(err.Error())
	}

	return kicker
}

func (k *Kicker) StartKicking() {

	_, err := k.connection.Set(k.getIPAddressKey(), "alive", KeyLifeTime)

	if err != nil {
		fmt.Println(err.Error())
	}

	util.ScheduleInNewGoRoutine(func() { k.StartKicking() }, KickInterval)
}

func (k *Kicker) ensureDirExists(path string) error {

	_, err := k.connection.Get(path)

	if err != nil {

		if isKeyNotFoundError(err) {

			_, err := k.connection.SetDir(path, 0)
			if err != nil {
				return err
			}
		} else {
			return err
		}
	}
	return nil
}

func (k *Kicker) getIPAddressKey() string {
	return k.paths.NodeIpAddressKey() + k.ipAddress
}
