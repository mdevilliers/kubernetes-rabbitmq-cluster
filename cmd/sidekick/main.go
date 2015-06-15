package main

import (
	"fmt"
	etcdclient "github.com/coreos/go-etcd/etcd"
	"github.com/mdevilliers/kubernetes-rabbitmq-cluster/etcd"
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

	fmt.Printf("rabbitmq-cluster-sidekick\n")

	connection := etcd.NewConnection([]string{"http://127.0.0.1:2379"})
	pathManager := etcd.NewPathManager("astana")

	kicker := etcd.NewKicker(connection, pathManager)
	kicker.StartKicking()

	registry := etcd.NewRegistry(connection, pathManager)

	cb := func(r *etcdclient.Response) (bool, error) {
		fmt.Printf("Recieved : ", etcd.PrettyPrintResponse(r))
		return true, nil
	}

	registry.Put("/registry/rabbitmq-cluster", cb)
	registry.Seal()
	registry.StartWatches()

	var ch chan bool
	<-ch // blocks forever
}
