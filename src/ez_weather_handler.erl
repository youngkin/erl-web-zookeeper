%% ===================================================================
%% @author youngkin
%% @doc This is the Cowboy callback module for city weather information. 
%%      ez_weather_handler supports all weather related requests
%%      including:
%%          1. Creating/updating weather information,
%%          2. Getting weather information for a specific city
%%          3. Getting URIs for all cities with weather information.
%%             These URIs can each be used to get the weather data
%%             for the specified city. 
%%
%%      Note that most string variables, are represented as binaries. 
%%      Much, but not all, of the string manipulation done in this module
%%      requires conversion to/from binary.
%% 
%% ===================================================================

-module(ez_weather_handler).

%% ====================================================================
%% Callback functions
%% ====================================================================

%%
%% Cowboy defined callbacks
%%
-export(
  [ init/3
  , rest_init/2
  , allowed_methods/2
  , content_types_provided/2
  , content_types_accepted/2
  , terminate/3
  ]).

%%
%% Application defined callbacks for GET/POST processing to be used by Cowboy. 
%% See content_types_provided/2 and content_types_accepted/2 for the actual
%% reference declarations used by Cowboy.
%%
-export(
  [to_html_weather/2, 
   to_json_weather/2, 
   to_text_weather/2,
   from_json_weather_update/2]).

%%
%% Modifies (upgrades )a standard http request handler into a ReST request handler.
%%
init(_Transport, _Req, _Opts) ->
  {upgrade, protocol, cowboy_rest}.

%%
%% @doc Creates the initial state to be used during the handling of a request.
%% For now, this capability isn't leveraged. It's left here as an example of
%% how this might be accomplished when/if it's eventually needed.
%%
rest_init(_Req, _Opts) ->
    %% TODO: It's expected that the base URL is just one item that will need to
    %% be retrieved from the app resource file and stored as State that can be
    %% referenced at runtime.
    %% TODO: is this even needed? get_base url/0 does this and needs to get it from config as fqdn 
    %% might be a proxy
    State = "http://example.com",
    {ok, _Req, State}.

%%
%% @doc As the name implies, this function tells Cowboy what HTTP methods are supported.
%% Cowboy will return an HTTP status of 405, Method not allowed for any attempts to use
%% other HTTP methods.
%%
allowed_methods(Req, State) -> {[<<"GET">>, <<"POST">>], Req, State}.

%%
%% @doc Indicates to Cowboy which content types are accepted in GET requests and the internal
%% functions to be called for each defined content type. Note that all of the referenced
%% functions must be exported from the module.
%%
content_types_provided(Req, State) ->
    {[
        {<<"text/html">>, to_html_weather},
        {<<"application/json">>, to_json_weather},
        {<<"text/plain">>, to_text_weather}
    ], Req, State}.

%%
%% @doc Indicates to Cowboy which content types are accepted in POST requests and the internal
%% function(s) to be called for each defined content type. In this instance, the only acceptable
%% type is application/json. Note that all of the referenced functions must be exported from 
%% the module.
%%
content_types_accepted(Req, State) ->
    {[{<<"application/json">>, from_json_weather_update}], Req, State}.


%%
%% @doc Handles HTML GET requests for city weather. It handles both the weather/cities and 
%% weather/city/??? resource routes. It uses the :resource and [:city] route bindings
%% declared in ez_app.erl where the resource routes are defined.
%%
%%
%% TODO: Refactor code duplicated with to_json_weather and to_text_weather into a common helper
%% TODO: function.
%%
to_html_weather(Req, State) ->
    %% Get the value associated with the ":resource" binding (i.e., "city" or "cities")
    {RequestedResourceBinary, Req2} = get_resource_from_url(Req),
    %% Get the value associated with the ":city" binding (i.e., the value of city in the request)
    {RequestedCityBinary, Req3} = get_city_from_url(Req2),
    
    RequestedResource = binary_to_list(RequestedResourceBinary),
    RequestedCity = case is_atom(RequestedCityBinary) of
                        false -> binary_to_list(RequestedCityBinary);
                        %% RequestedCity will only be an atom if it wasn't part of the URL
                        %% (e.g., in requests to weather/cities). In this case it's value 
                        %% is never used.
                        true -> undefined
                    end,

    lager:debug("Binding values [resource, city]: [~p, ~p]", [RequestedResource, RequestedCity]),
    ResourceValid = validate_resource(RequestedResource),
    lager:debug("Is the URL valid? ~p", [ResourceValid]),
    
    %% ReplyOrHalt will contain a complete response body if the URI is valid. Otherwise
    %% Cowboy will be directed to return a 400 BAD REQUEST and processing will be halted.
    ReplyOrHalt = case ResourceValid of
        ok -> 
            ReturnBody = case RequestedResource of
                "city" -> build_city_data_html(RequestedCity);
                "cities" -> build_cities_data_html()
            end,
            
            lager:debug("City weather as HTML: ~p", [ReturnBody]),
            HtmlStart = <<"<html>
                <head>
                    <meta charset=\"utf-8\">
                    <title>EZ Weather Service</title>
                </head>
                <body>">>,
                    
            HtmlEnd = <<"</body>\n</html>">>,
            
            <<HtmlStart/binary, ReturnBody/binary, HtmlEnd/binary>>;
        _ ->
            cowboy_req:reply(400, Req3),
            halt
    end,
    
    %% ReplyOrHalt contains HTML if validation passed, halt otherwise
    {ReplyOrHalt, Req3, State}.


%%
%% @doc Handles JSON GET requests for city weather. It handles both the weather/cities and 
%% weather/city/??? resource routes. It uses the :resource and [:city] route bindings
%% declared in ez_app.erl where the resource routes are defined.
%%
%%
%% TODO: Refactor code duplicated with to_html_weather and to_text_weather into a common helper
%% TODO: function.
%%
to_json_weather(Req, State) ->
    %% Get the value associated with the ":resource" binding (i.e., "city" or "cities")
    {RequestedResourceBinary, Req2} = get_resource_from_url(Req),
    %% Get the value associated with the ":city" binding (i.e., the value of city in the request)
    {RequestedCityBinary, Req3} = get_city_from_url(Req2),
    
    RequestedResource = binary_to_list(RequestedResourceBinary),
    RequestedCity = case is_atom(RequestedCityBinary) of
                        false -> binary_to_list(RequestedCityBinary);
                        %% RequestedCity will only be an atom (undefined) if it wasn't part of the
                        %% URL. In this case it's value is never used.
                        true -> undefined
                    end,

    lager:debug("Binding values [resource, city]: [~p, ~p]", [RequestedResource, RequestedCity]),
    ResourceValid = validate_resource(RequestedResource),
    lager:debug("Is the URL valid? ~p", [ResourceValid]),

    %% ReplyOrHalt will contain a complete response body if the URI is valid. Otherwise
    %% Cowboy will be directed to return a 400 BAD REQUEST and processing will be halted.
    ReplyOrHalt = case ResourceValid of
        ok -> 
            ReturnBody = case RequestedResource of
                "city" -> build_city_data_json(RequestedCity);
                "cities" -> build_cities_data_json()
            end,
            lager:debug("City weather as JSONs: ~p", [ReturnBody]),
            ReturnBody;
        _ ->
            cowboy_req:reply(400, Req3),
            halt
    end,
            
    %% ReplyOrHalt contains HTML if validation passed, halt otherwise
    {ReplyOrHalt, Req3, State}.

%%
%% @doc Handles text/plain GET requests for city weather.
%%
%%
%%TODO: to_text_weather/2 is currently implemented in terms of to_json_weather/2. At some point
%%TODO: it should be implemented to return plain text instead of JSON.
%%
to_text_weather(Req, State) ->
    to_json_weather(Req, State).

%%
%% @doc Handles JSON POST requests to create or update weather conditions for the specified city.
%%
from_json_weather_update(Req, State) ->
    lager:debug("Enter"),

    %% TODO: Validate request so nothing nasty gets in ZK
    %% {ok, Req2} = validate_request(Req),
    {ok, Body, Req3} = cowboy_req:body(Req),
    lager:debug("Body contents: ~p", [Body]),
    
    %% TODO: putting the Body into ZK as-is.  Need to extract city and weather from Body and
    %% TODO: use city to create the resource (e.g., denver) and the weather to create the
    %% TODO: associated data (e.g., "clear").
    ez_data:create("/ez/weather/cities/denver", Body),
    ez_data:set("/ez/weather/cities/denver", Body),
    {true, Req3, State}.

terminate(_Reason, _Req, _State) -> ok.

%% ====================================================================
%% Internal functions
%% ====================================================================

%%
%% Returns the value for the URI node represented in the :resource part of the Cowboy
%% route specification. It is expected to be "cities" or "city".
%%
get_resource_from_url(Req) ->
    cowboy_req:binding(resource, Req).

%%
%% Returns the value for the URI node represented in the optional :city part of the Cowboy
%% route specification. It is expected to be some string representing a city name (e.g., denver).
%% Since the :city part of the URI is optional this function may return the atom "undefined".
%%
get_city_from_url(Req) ->
    cowboy_req:binding(city, Req).

build_city_data_html(RequestedCity) ->
    CityWeather = get_city_weather(RequestedCity),
    CityPrefix = <<"<p>{\"city\"   : \"">>,
    WeatherPrefix = <<"\"</p>
       <p>\"weather\" : \"">>,
    End = <<"\"}</p>">>,
    RequestedCityBinary = list_to_binary(RequestedCity),
    <<CityPrefix/binary, RequestedCityBinary/binary, WeatherPrefix/binary, CityWeather/binary, End/binary>>.

build_cities_data_html() ->
    BaseUrl = get_base_url(),
    CityList = get_cities(),
    
    %% "http://example.com/weather/city/"
    QualifiedUrl = lists:flatten([BaseUrl, "/weather/city/"]),
    
    %% [ ["http://example.com/weather/city/", "denver"], [...], [...] ]
    CityUrlListDeep = [[QualifiedUrl, X] ||  X <- CityList],
    
    %% [ ["http://example.com/weather/city/denver"], [...], [...] ]
    CityUrlListFlat = [[lists:flatten(X)] || X <- CityUrlListDeep],
   
    %% [ ["\"city\" : \"", "http://example.com/weather/city/denver", "\","], [...], [...] ]
    DisplayListDeep = [ ["<p>\"city\" : \"", X,"\"</p>"] || X <- CityUrlListFlat ],

    %% The following is a string shown over 3 lines, in reality it's all on one line
    %% "<p>\"city\" : \"http://example.com/weather/city/denver\"</p>
    %% <p>\"city\" : \"http://example.com/weather/city/tucson\"</p>
    %% <p>\"city\" : \"http://example.com/weather/city/denver\"</p>"
    DisplayListFlat = [ [lists:flatten(X)] || X <- DisplayListDeep],
    list_to_binary(DisplayListFlat).

build_city_data_json(RequestedCity) ->
    CityWeather = get_city_weather(RequestedCity),
    CityPrefix = <<"{\"city\"   : \"">>,
    WeatherPrefix = <<"\", \"weather\" : \"">>,
    End = <<"\"}">>,
    RequestedCityBinary = list_to_binary(RequestedCity),
    <<CityPrefix/binary, RequestedCityBinary/binary, WeatherPrefix/binary, CityWeather/binary, End/binary>>.

build_cities_data_json() ->
    BaseUrl = get_base_url(),
    
    CityList = get_cities(),
    
    %% "http://example.com/weather/city/"
    QualifiedUrl = lists:flatten([BaseUrl, "/weather/city/"]),
    
    %% [ ["http://example.com/weather/city/", "denver"], [...], [...] ]
    CityUrlListDeep = [[QualifiedUrl, X] ||  X <- CityList],
    
    %% [ ["http://example.com/weather/city/denver"], [...], [...] ]
    CityUrlListFlat = [[lists:flatten(X)] || X <- CityUrlListDeep],
   
    %% [ ["\"city\" : \"", "http://example.com/weather/city/denver", "\","], [...], [...] ]
    DisplayListDeep = [ ["\"city\" : \"", X,"\","] || X <- CityUrlListFlat ],
    
    %% [ ["\"city\" : \"http://example.com/weather/city/denver\,"],
    %% [\"city\" : \"http://example.com/weather/city/tucson\,"],
    %% [\"city\" : \"http://example.com/weather/city/denver\,"],"]
    DisplayListFlat = [ [lists:flatten(X)] || X <- DisplayListDeep],

    %% The following is a string shown over 3 lines, in reality it's all on one line;
    %% Note the commas (",") between the "city" entries
    %% "\"city\" : \"http://example.com/weather/city/denver\",
    %% \"city\" : \"http://example.com/weather/city/tucson\",
    %% \"city\" : \"http://example.com/weather/city/denver\","
    DisplayString = lists:flatten(DisplayListFlat),
    
    %% Remove the extra trailing "," from the flattened list
    DisplayListTrimmed = string:strip(DisplayString, right, $,),
    BinaryContent = list_to_binary(DisplayListTrimmed),
    << <<"{">>/binary, BinaryContent/binary, <<"}">>/binary >>.

validate_resource(Resource) ->
    case Resource of
        "city" -> ok;
        "cities" -> ok;
        _ -> error
    end.

%% validate_request(_Req) ->
%%     %% TODO: This may need to be implemented if ZK is susceptible to SQL-injection type attacks.
%%     %% It doesn't seem to be an issue though.
%%     finish_this.

%% TODO: Should these methods return binaries?
get_cities() ->
    %% TODO: get from Zookeeper
    ["denver","tuscon", "seattle"].

get_city_weather(City) ->
    lager:debug("City: ~p", [City]),
    PathList = ["/ez/weather/cities/", City],
    Path = lists:flatten(PathList),
    lager:debug("Query path: ~p", [Path]),
    {ok, {CityWeatherBinary, _}} = ez_data:get(Path),
    lager:debug("~p weather(binary): ~p", [City, CityWeatherBinary]),
    CityWeatherBinary.

get_base_url() ->
    HttpHost = case application:get_env(ez, http_host) of
        {ok, HostName} -> lager:debug("Got http_host from application resource file."),
                          HostName;
        undefined -> "localhost";
        {_, _} -> "localhost"
    end,
    lager:debug("HttpHost for resource URL: ~p", [HttpHost]),
    HttpHost.

