kubernetes-rabbitmq-cluster
---------------------------

[![Build Status](https://travis-ci.org/mdevilliers/kubernetes-rabbitmq-cluster.svg)](https://travis-ci.org/mdevilliers/kubernetes-rabbitmq-cluster)

Experimment to produce a "kubernentes first" rabbitmq cluster manager.


Aims
----

- zero user effort high availablity and fault tolerence.


Components
----------

- rabbitmq-cluster-sidekick - handles dynamic cluster managment. 
- rabbitmq-cluster-admin - a clusterwide ui for managment of the cluster(s).
- docker images
- kubernetes yaml configuration

Docker Images
-------------

rabbitmq-cluster-node - hosts a rabbitmq instance and rabbitmq-cluster-sidekick instance


rabbitmq-cluster-admin - hosts a rabbitmq-cluster-admin instance

Kubernentes files
-----------------

rabbitmq-cluster-rc.yaml


rabbitmq-cluster-service.yaml


rabbitmq-cluster-admin-rc.yaml


rabbitmq-cluster-admin-service.yaml


Requirements
------------

- kubernentes
- docker 
- etcd
- golang





