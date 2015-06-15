package etcd

import (
	"fmt"
	etcdclient "github.com/coreos/go-etcd/etcd"
)

func PrettyPrintResponse(r *etcdclient.Response) string {
	return fmt.Sprintf("Action : %s,  %+v", r.Action, r.Node)
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
