kubernetes-rabbitmq-cluster
---------------------------

PLEASE NOTE THIS PROJECT IS NO LONGER MAINTIANED 
------------------------------------------------

[![Build Status](https://travis-ci.org/mdevilliers/kubernetes-rabbitmq-cluster.svg)](https://travis-ci.org/mdevilliers/kubernetes-rabbitmq-cluster)

Experiment to produce a "kubernentes first" rabbitmq cluster manager.

Aims
----

- zero user effort high availablity and fault tolerence.

RabbitMQ Healthcheck Test
-------------------------

Http request to 

http://localhost:15672/api/vhosts to collect list of vhosts

then for each vhost -

http://localhost:15672/api/aliveness-test/%2F 

from http://hg.rabbitmq.com/rabbitmq-management/raw-file/rabbitmq_v2_2_0/priv/www-api/help.html

Failure means sidekick dies, then pod dies and is replicated somewhere else

This will need an admin user's credentials which will need to bootstrapped in the rabbitmq docker image.


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





