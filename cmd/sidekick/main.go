package main

import (
	"github.com/mdevilliers/kubernetes-rabbitmq-cluster/etcd"
	"fmt"
)

func main(){
	fmt.Printf("rabbitmq-cluster-sidekick\n")

	client ,_ := etcd.NewClient([]string{"http://127.0.0.1:2379"})
	client.InitOrPanic("astana")
/*
	- sidekick 1
			- configure cluster with random node not me
			- sidekick 2
				- sets key on a loop with a ttl
			- sidekick 3
				- has super set of all the rabbitmq nodes for a LOGICAL_NAME
				- if one is missing for a LOST_PERIOD - remove from the local cluster
			- sidekick 4
				- starts rabbitmq
*/

	var ch chan bool
	<-ch // blocks forever

}