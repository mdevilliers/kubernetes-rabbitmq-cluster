package paths

type Paths struct {
	clusterName string
}

func NewPathManager(clusterName string) *Paths {
	return &Paths{
		clusterName: clusterName,
	}
}

/*
/registry/rabbitmq-cluster/{LOGICAL_NAME}/idAddress_10.1.1.1 "alive"
/registry/rabbitmq-cluster/{LOGICAL_NAME}/idAddress_10.1.1.2 "alive"
/registry/rabbitmq-cluster/{LOGICAL_NAME}/state/users/....
/registry/rabbitmq-cluster/{LOGICAL_NAME}/state/xxxx/....
*/

func (p *Paths) ClusterRoot() string {
	return "/registry/rabbitmq-cluster/" + p.clusterName
}

func (p *Paths) ClusterStateRoot() string {
	return p.ClusterRoot() + "/state"
}
func (p *Paths) NodeIpAddressKey(ipAddress string) string {
	return p.ClusterRoot() + "/ipAddress_" + ipAddress
}
