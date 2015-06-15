package etcd

import (
	etcdclient "github.com/coreos/go-etcd/etcd"
	"testing"
)

func TestNewRegistry(t *testing.T) {

	registry := NewRegistry(nil, nil)

	called := false

	cb := func(_ *etcdclient.Response) (bool, error) {
		called = true
		return true, nil
	}

	registry.Put("/a/b/c", cb)
	cb2, _ := registry.Get("/a/b/c")

	ok, _ := cb2(nil)

	if !ok {
		t.Error("Incorrect function passed back!")
	}

	if !called {
		t.Error("Incorrect function passed back!")
	}
}

func TestItemDoesNotExistInRegistry(t *testing.T) {
	registry := NewRegistry(nil, nil)
	_, err := registry.Get("/a/b/c")

	if err != ItemNotFoundError {
		t.Error("Should have thrown an ItemNotFoundError")
	}
}

func TestItemAddedTwiceErrors(t *testing.T) {
	registry := NewRegistry(nil, nil)

	cb := func(_ *etcdclient.Response) (bool, error) {
		return true, nil
	}

	err := registry.Put("/a/b/c", cb)

	if err != nil {
		t.Error("Adding an item once shouldn't error")
	}

	err = registry.Put("/a/b/c", cb)

	if err != ItemOfSameKeyAlreadyAddedError {
		t.Error("Adding an item twice should error")
	}
}

func TestUnableToAddIfSealed(t *testing.T) {
	registry := NewRegistry(nil, nil)

	cb := func(_ *etcdclient.Response) (bool, error) {
		return true, nil
	}

	err := registry.Put("/a/b/c", cb)

	if err != nil {
		t.Error("Adding an item once shouldn't error")
	}

	registry.Seal()

	err = registry.Put("/a/b/c", cb)

	if err != RegistryIsSealedError {
		t.Error("Adding an item when sealed should error")
	}

}
