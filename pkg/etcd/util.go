package etcd

import (
	etcdclient "github.com/coreos/go-etcd/etcd"
)

//https://github.com/coreos/etcd/blob/master/Documentation/errorcode.md
func IsKeyNotFoundError(err error) bool {
	return isEtcdError(err, 100)
}

func isEtcdError(err error, code int) bool {
	eerr, ok := err.(*etcdclient.EtcdError)
	if ok {
		return eerr.ErrorCode == code
	}
	return false
}
