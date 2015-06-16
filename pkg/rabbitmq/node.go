package rabbitmq

import (
	"github.com/goerlang/etf"
	"github.com/goerlang/node"
	"github.com/mdevilliers/kubernetes-rabbitmq-cluster/pkg/logger"
)

type Node struct {
	node *node.Node
}

type rpcGenServer struct {
	node.GenServerImpl
	completeChan chan bool
}

func StartNode(nodeName, erlangCookie string) (*Node, error) {

	erlnode := node.NewNode(nodeName, erlangCookie)

	// Allow node be available on 5588 port
	err := erlnode.Publish(5888)

	if err != nil {
		return nil, err
	}

	// Create channel to receive message when main process should be stopped
	completeChan := make(chan bool)
	server := &rpcGenServer{}
	erlnode.Spawn(server, completeChan)

	return &Node{
		node: erlnode,
	}, nil

}

func (gs *rpcGenServer) Init(args ...interface{}) {

	logger.Info.Printf("rpcGenServer: Init: %#v", args)
	gs.Node.Register(etf.Atom("rpcGenServer"), gs.Self)

	// Store first argument as channel
	gs.completeChan = args[0].(chan bool)
}

func (gs *rpcGenServer) HandleCast(message *etf.Term) {
	logger.Info.Printf("rpcGenServer: HandleCast: %#v", *message)
}

func (gs *rpcGenServer) HandleCall(message *etf.Term, from *etf.Tuple) (reply *etf.Term) {
	logger.Info.Printf("rpcGenServer: HandleCall: %#v, From: %#v", *message, *from)
	rep := etf.Term(etf.Tuple{etf.Atom("ok")})
	return &rep
}

func (gs *rpcGenServer) HandleInfo(message *etf.Term) {
	logger.Info.Printf("rpcGenServer: HandleInfo: %#v", *message)
}

func (gs *rpcGenServer) Terminate(reason interface{}) {
	logger.Info.Printf("rpcGenServer: Terminate: %#v", reason.(int))
}
