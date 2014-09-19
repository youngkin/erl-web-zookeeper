%% @author uyounri
%% @doc @todo Add description to ez_weather_handler.


-module(ez_weather_handler).

%% ====================================================================
%% API functions
%% ====================================================================

%% Cowboy callbacks
-export(
  [ init/3
  , rest_init/2
  , allowed_methods/2
  , content_types_provided/2
  , content_types_accepted/2
  , terminate/3
  ]).

%% Callbacks for GET/POST processing, will be used by Cowboy
-export(
  [to_html_weather/2, 
   to_json_weather/2, 
   to_text_weather/2,
   from_json_weather_update/2]).

init(_Transport, _Req, _Opts) ->
  {upgrade, protocol, cowboy_rest}.

rest_init(_Req, _Opts) ->
    %% TODO: It's expected that the base URL is just one item that will need to
    %% be retrieved from the app resource file and stored as State that can be
    %% referenced at runtime.
%% TODO: is this even needed? get_base url/0 does this and needs to get it from config as fqdn 
%% might be a proxy
    State = "http://example.com",
    {ok, _Req, State}.

allowed_methods(Req, State) -> {[<<"GET">>, <<"POST">>], Req, State}.

content_types_provided(Req, State) ->
    {[
        {<<"text/html">>, to_html_weather},
        {<<"application/json">>, to_json_weather},
        {<<"text/plain">>, to_text_weather}
    ], Req, State}.

content_types_accepted(Req, State) ->
    {[{<<"application/json">>, from_json_weather_update}], Req, State}.


to_html_weather(Req, State) ->
    %% Get the value associated with the ":resource" binding (i.e., "city" or "cities")
    {RequestedResourceBinary, Req2} = get_resource_from_url(Req),
    %% Get the value associated with the ":city" binding (i.e., the value of city in the request)
    {RequestedCityBinary, Req3} = get_city_from_url(Req2),
    
    RequestedResource = binary_to_list(RequestedResourceBinary),
    RequestedCity = case is_atom(RequestedCityBinary) of
                        false -> binary_to_list(RequestedCityBinary);
                        %% RequestedCity will only be an atom if it wasn't part of the URL.
                        %% In this case it's value is never used.
                        true -> undefined
                    end,

    lager:debug("Binding values [resource, city]: [~p, ~p]", [RequestedResource, RequestedCity]),
    ResourceValid = validate_resource(RequestedResource),
    lager:debug("Is the URL valid? ~p", [ResourceValid]),
    
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

to_text_weather(Req, State) ->
    to_json_weather(Req, State).

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

get_resource_from_url(Req) ->
    cowboy_req:binding(resource, Req).

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
    %% ["denver", "tucson", "seattle"]
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
    
    %% ["denver", "tucson", "seattle"]
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

