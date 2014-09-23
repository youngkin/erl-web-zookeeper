# erl-web-zookeeper

"ez" is an application that demonstrates the usage of:
1. Rebar - see the rebar.config file in the project root directory for details.
2. Starting multiple, dependent, applications. For example, this project depends on lager and zookeeper.  See the start.sh file for more details, particularly the application:ensure_all_started() directive.  This starts all the applications listed in the application resource file's, src/ez.app.src, {applications, [...]} tuple for the complete set of applications.
3. Lager as a logging replacement for the standard Erlang logger. See the erl-zookeeper.config file for Lager configuration settings.
4. Cowboy to enable support for a ReSTful interface to the application. n this application the interface is used to get and set weather data.  
5. Zookeeper is used as the data store for weather data. 


# Prerequisites

[http://zookeeper.apache.org/](Apache Zookeeper) is one of the cornerstones for this project. Most all other external dependencies are handled by rebar.
* At least one zookeeper instance should be available.  The src/ez.app.src file contains the configuration for Zookeeper.  The `{hosts {...}}` tuple should be modified to match up with the Zookeeper installation.

# Building
This could be better, i.e., with make handling all this.

1. `$ ./rebar get-deps`
2. `$ ./rebar compile`

# Running
1. `$ ./start`
2. Or `$ ./start_in_shell` to start the service in an Erlang shell

# Take it for a spin
1. Retrieve service status in JSON, HTTP, or plain text formats

`curl -i -H "Accept: application/json" http://localhost:8080/status`

`curl -i -H "Accept: text/html" http://localhost:8080/status`

`curl -i -H "Accept: text/plain" http://localhost:8080/status`

2. GET data (weather) from service. It currently returns a hard-coded response.

`curl -i -H "Accept: application/json" http://localhost:8080/weather`

3. POST data (weather) to service. It currently only echoes what was POSTed.

`curl -i -X POST -H "Content-Type: application/json" -d 'here is some weather info' http://localhost:8080/weather`

# Setup - TODO
## Release - TODO
## Zookeeper - TODO
## Cowboy - TODO

# TODO:
1. Package as a release and distribute. Document how to do this.
2. Integrate with Zookeeper

