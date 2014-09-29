**Table of Contents**  *generated with [DocToc](http://doctoc.herokuapp.com/)*

- [erl-web-zookeeper](#user-content-erl-web-zookeeper)
- [Prerequisites](#user-content-prerequisites)
- [Building](#user-content-building)
- [Building an Erlang release](#user-content-building-an-erlang-release)
    - [Rebar & reltool](#user-content-rebar--reltool)
    - [relx](#user-content-relx)
- [Running](#user-content-running)
- [Take it for a spin](#user-content-take-it-for-a-spin)
- [REST API](#user-content-rest-api)
    - [http://example.com/status](#user-content-httpexamplecomstatus)
        - [GET](#user-content-get)
        - [PUT](#user-content-put)
        - [POST](#user-content-post)
        - [DELETE](#user-content-delete)
    - [http://example.com/weather/cities](#user-content-httpexamplecomweathercities)
        - [GET](#user-content-get-1)
        - [PUT](#user-content-put-1)
        - [POST](#user-content-post-1)
        - [DELETE](#user-content-delete-1)
    - [http://example.com/weather/city/some_city_name](#user-content-httpexamplecomweathercitysome_city_name)
        - [GET](#user-content-get-2)
    - [PUT](#user-content-put-2)
        - [POST](#user-content-post-2)
        - [DELETE](#user-content-delete-2)
    - [Example output](#user-content-example-output)
        - [Status](#user-content-status)
        - [Weather JSON](#user-content-weather-json)
- [TODO:](#user-content-todo)

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

# Building an Erlang release
## Rebar & reltool
`rel/reltool.config` contains the directives that specify how the release is to be built. Create a 
tarball of `rel/ez` to create a distributable release that can be unzipped on the target machine.

1. `$ ./rebar get-deps`
2. `$ ./rebar clean compile`
3. `$ ./rebar generate`

The last step creates the release in `rel/ez`. When rebuilding a release it is best to remove the
`ez` directory before running the above steps.

## relx
`relx.config` contains the directives that specify how the release is to be built using `relx`. This
file is much simpler than the corresponding `rel/reltool.config` as it gets much of the dependency
information from `src/ez.app.src`. 

1. `$ ./rebar get-deps`
2. `$ ./rebar clean compile`
3. `$ relx release tar`

The last step, `relx release tar`, creates the 
release and creates the tarball for distribution. This saves a step over `reltool`. When rebuilding a 
release it is best to remove the `_rel` directory before running the above steps.



# Running
Before running the app you'll need to take a look at the src/ez.app.sr file. For example, the `{hosts, [...]}`
tuple needs to be modified to point to the Zookeeper instance that supports the app. The values included
should be sufficient for cases where Zookeeper is running locally using the Zookeeper configuration defaults.

The `{env, [{http_host, xxx}, {http_port, xxx}]}` needs to reflect your local runtime environment for the
application. The defaults will be sufficient in most cases.

If these configuration settings have been changed you'll need to rebuild the app as described above.

Do one of the following to start the app:

1. `$ ./start`
2. Or `$ ./start_in_shell` to start the service in an Erlang shell

# Take it for a spin
1. Retrieve service status in JSON, HTTP, or plain text formats

`curl -i -H "Accept: application/json" http://localhost:8080/status`

`curl -i -H "Accept: text/html" http://localhost:8080/status`

`curl -i -H "Accept: text/plain" http://localhost:8080/status`

2. POST data (weather) to service (for Denver). Change the value of the last node to whatever city desired to set the weather conditions for that city.

`curl -i -X POST -H "Content-Type: application/json" -d 'clear' http://localhost:8080/weather/city/denver`

3. GET data (weather) from service (for Denver).  Change the value of the last node to whatever city desired to get the weather conditions for that city.

`curl -i -H "Accept: text/plain" http://localhost:8080/weather/city/denver`

3. GET data (weather) from service for all available cities with associated weather conditions.

`curl -i -H "Accept: text/plain" http://localhost:8080/weather/cities`

# REST API
## http://example.com/status
### GET

Returns the current status of the application

Success: 200

Returns application/json, text/plain, text/html formatted data (see below)

### PUT

Not supported

Error: 405, Method not allowed, no retry

### POST

Not supported

Error: 405, Method not allowed, no retry

### DELETE

Not supported

Error: 405, Method not allowed, no retry

## http://example.com/weather/cities
### GET

Returns the URLs of all currently defined cities.  

Success: 200
Error: 404 NOT FOUND

Error: 400 if the URL has a bad path element (e.g., /weather/sites vs. /weather/cities or /weather/city)

Returns application/json, text/plain, text/html formatted data (see below)

### PUT

Not supported

Error: 405, Method not allowed, no retry

### POST

Not supported

Error: 405, Method not allowed, no retry

### DELETE

Not supported

Error: 405, Method not allowed, no retry

## http://example.com/weather/city/some_city_name
### GET

Returns the weather data of the specified city or a 404 NOT FOUND

Error: 400 if the URL has a bad path element (e.g., /weather/sites vs. /weather/cities or /weather/city)

Returns application/json, text/plain, text/html formatted data (see below)

## PUT

Not supported

Error: 405, Method not allowed, no retry

### POST

Adds/updates the specified city

Success: 204, Created or updated;
Error: 400, bad request, don’t retry
Error: 500, server error, retry

Accepts only application/json formatted data (see below)


### DELETE

Not supported

Error: 405, Method not allowed, no retry

## Example output
### Status 

`{ �Cowboy port�: number }`

```
application/json GET Example (status)
{ �Cowboy port�: 8080 }
```

```
text/plain GET Example (status)
{ �Cowboy port�: 8080 }
```

```
text/html GET Example (status)
<html>
<head>
   <meta charset="utf-8">
   <title>EZ Status</title>
</head>
<body>
    <p><em><center>EZ Status</center></em></p>
    <p>Cowboy Port: 8080</p>
</body>
</html>
```

### Weather JSON
```
{ �city�: string,
  �weather�: string}
```

PUT Example (weather/city/city_name)
```
{ �city� : �denver�,
  �weather� : �clear� }
```

GET Example (weather/cities)
```
{ �city� : �http://example.com/weather/city/denver�,
  �city� : �http://example.com/weather/city/seattle�, 
  �
}
```

GET Example (weather/city)
```
{ �city� : �denver�,
  �weather� : �clear� }
```


# TODO:
1. Implement better support for accepting actual JSON content on POST requests
2. Protect against GET requests for non-existent city weather data. Should return a 404 NOT FOUND. It currently returns 500.
3. Get cities from Zookeeper instead of being hardcoded like it currently is. As is, it returns a
hard-coded city list of [denver, tucson, seattle].
4. Add `make` support

