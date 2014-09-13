erl-web-zookeeper
=================

Sample app demonstrating use of Erlang supervision, release with multiple apps, web interface, 
and quorum election via Zookeeper.

**This is very early days for this project, it's not quite ready to use for anything.  Just 
wanted to get it up on github.**

#Prerequisites

[http://zookeeper.apache.org/](Apache Zookeeper) is one of the keystones for this project.
* At least one zookeeper instance should be available.  The src/ez.app.src file contains the configuration for zookeeper.  The `{hosts {...}}` tuple should be modified to match up with the zookeeper installation.
