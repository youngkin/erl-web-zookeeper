# erl-web-zookeeper

Sample app demonstrating use of Erlang supervision, release with multiple apps, web interface, 
and quorum election via Zookeeper.  What does this app do?
   1. Uses zookeeper for configuration data.  May also use for quorum election, not sure yet.
   2. Accepts weather info via ReST interface
      a. This data is stored in Mnesia
   3. Accepts requests for weather info via the ReST interface
   4. Demonstrates reactive concepts via exclusive use of async message passing and an Erlang
      implementation of "futures" 


**NOTE:** This is very early days for this project, it's not quite ready to use for anything.  Just 
wanted to get it up on github.

# Prerequisites

[http://zookeeper.apache.org/](Apache Zookeeper) is one of the cornerstones for this project. Most all other external dependencies are handled by rebar.
* At least one zookeeper instance should be available.  The src/ez.app.src file contains the configuration for Zookeeper.  The `{hosts {...}}` tuple should be modified to match up with the Zookeeper installation.

# Building
This could be better, i.e., with make handling all this.
'''$ ./rebar get-deps'''
'''$ ./rebar compile

# Running
'''$ ./start'''
'''$ ./start_in_shell''' starts the service in an Erlang shell'''

# Take it for a spin
1. Retrieve service status in JSON, HTTP, or plain text formats
'''curl -i -H "Accept: application/json" http://localhost:8080/status'''
'''curl -i -H "Accept: text/html" http://localhost:8080/status'''
'''curl -i -H "Accept: text/plain" http://localhost:8080/status'''
2. GET data (weather) from service. It currently returns a hard-coded response.
'''curl -i -H "Accept: application/json" http://localhost:8080/weather'''
3. POST data (weather) to service. It currently only echoes what was POSTed.
'''curl -i -X POST -H "Content-Type: application/json" -d 'here is some weather info' http://localhost:8080/weather'''

# Setup - TODO
## Release - TODO
## Zookeeper - TODO
## Cowboy - TODO

# TODO:
1. Package as a release and distribute. Document how to do this.
2. Integrate with Zookeeper

