package main

import (
	"flag"
	"github.com/mdevilliers/kubernetes-rabbitmq-cluster/pkg/etcd"
	"github.com/mdevilliers/kubernetes-rabbitmq-cluster/pkg/logger"
	"github.com/mdevilliers/kubernetes-rabbitmq-cluster/pkg/paths"
	"github.com/mdevilliers/kubernetes-rabbitmq-cluster/pkg/rabbitmq"
	"github.com/mdevilliers/kubernetes-rabbitmq-cluster/pkg/sidekick"
	"github.com/mdevilliers/kubernetes-rabbitmq-cluster/pkg/util"
)

func main() {

	/*
		1. configure cluster with random node not me
		2. sets key on a loop with a ttl - KICKER - DONE
		3. has super set of all the rabbitmq nodes for a LOGICAL_NAME
			if one is missing for a LOST_PERIOD - remove from the local cluster
			if it reappears add to the cluster
		4. replicates state from etcd to the nodes e.g. users, permissions, plugins etc
	*/

	logger.Info.Println("rabbitmq-cluster-sidekick")

	flag.Parse()

	_, err := rabbitmq.StartNode("golang@localhost", "xxx")

	if err != nil {
		panic("Error connecting Erlang node : " + err.Error())
	}

	connection := etcd.NewConnection([]string{"http://127.0.0.1:2379"})
	pathManager := paths.NewPathManager("astana")

	ipAddress, err := util.GetIPAddress()

	if err != nil {
		panic("Error retreiving ipAddress : " + err.Error())
	}

	kicker := sidekick.NewKicker(connection, pathManager, ipAddress)
	kicker.StartKicking()

	registry := sidekick.NewRegistry(connection, pathManager)

	cb := func(r *sidekick.Response) (bool, error) {
		logger.Info.Printf("Recieved in callback - Path : %s, Watching: %s, Old Value : %s, New Value %s \n", r.Path, r.WatchPath, r.OldValue, r.NewValue)
		return true, nil
	}

	registry.Put(pathManager.NodeIpAddressKey(ipAddress), cb)
	registry.Put("/registry/rabbitmq-cluster", cb)

	registry.Seal()
	registry.StartWatches()

	var ch chan bool
	<-ch // blocks forever
}
