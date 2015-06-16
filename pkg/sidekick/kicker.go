package sidekick

import (
	"fmt"
	"github.com/mdevilliers/kubernetes-rabbitmq-cluster/pkg/etcd"
	"github.com/mdevilliers/kubernetes-rabbitmq-cluster/pkg/paths"
	"github.com/mdevilliers/kubernetes-rabbitmq-cluster/pkg/util"
	"time"
)

var (
	KickInterval = time.Second * 20
	KeyLifeTime  = uint64(25)
)

type Kicker struct {
	connection *etcd.Connection
	paths      *paths.Paths
	ipAddress  string
}

func NewKicker(connection *etcd.Connection, paths *paths.Paths, ipAddress string) *Kicker {

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

	_, err := k.connection.Set(k.paths.NodeIpAddressKey(k.ipAddress), "alive", KeyLifeTime)

	if err != nil {
		fmt.Println(err.Error())
	}

	util.ScheduleInNewGoRoutine(func() { k.StartKicking() }, KickInterval)
}

func (k *Kicker) ensureDirExists(path string) error {

	_, err := k.connection.Get(path)

	if err != nil {

		if etcd.IsKeyNotFoundError(err) {

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
