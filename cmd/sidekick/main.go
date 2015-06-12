package main

import (
	"fmt"
	"github.com/mdevilliers/kubernetes-rabbitmq-cluster/etcd"
)

func main() {
	fmt.Printf("rabbitmq-cluster-sidekick\n")

	client, _ := etcd.NewClient([]string{"http://127.0.0.1:2379"})
	client.InitOrPanic("astana")
	/*
		1. configure cluster with random node not me
		2. sets key on a loop with a ttl
		3. has super set of all the rabbitmq nodes for a LOGICAL_NAME
			if one is missing for a LOST_PERIOD - remove from the local cluster
			if it reappears add to the cluster
		4. replicates state from etcd to the nodes e.g. users, permissions, plugins etc
	*/

	var ch chan bool
	<-ch // blocks forever

}
