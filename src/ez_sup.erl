%% ===================================================================
%% @author youngkin
%% @doc This is the main (and only) supervisor for the ez application. 
%%
%% It starts the gen_servers needed to communicate with Zookeeper and
%% initializing the Zookeeper znodes (data structures) used by the app. 
%%
%% ===================================================================
-module(ez_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, reconfigure/0]).

%% Supervisor callbacks
-export([init/1]).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% TODO: What's this for???
reconfigure() ->
    superman:reconfigure_supervisor_init_args(?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    %% ez_data is a gen_server used to communicate with Zookeeper.
    %% ez_config is a gen_server that sets up the initial Zookeeper
    %% znodes used by the application.
   {ok, {{rest_for_one, 10, 10}, 
         [
          {ez_data,{ez_data,start_link,[[{localhost, 2181,30000,10000},
    		{ localhost, 2182,30000,10000},
    		{ localhost, 2183,30000,10000}],"",[]]},permanent,5000,worker,[ez_data]},
          {ez_config, {ez_config, start_link, []}, permanent, 5000, worker, [ez_config]}
         ] 
        }
   }.

%% ===================================================================
%% Internal functions
%% ===================================================================

%% TODO: Add back so that the values for the Zookeeper servers can be substituted at runtime
%%
%% specs() ->
%%     case application:get_all_key(ez) of
%%         {ok, ZK} ->
%%             [
%% 		        {
%% 			        ez,
%% 			        {
%% 				        ez,
%% 				        start_link,
%% 				        [
%% 					        [{Host, Port, 30000, 10000} || [Host, Port] <- proplists:get_value(hosts, ZK)],
%% 					        proplists:get_value(chroot, ZK),
%% 					        []
%% 				        ]
%% 			        },
%% 			        permanent, 10000, worker, [ez]
%% 		        }
%% 	        ];
%%         _ -> lager:error("Config is not available, starting empty"),[]
%%     end.

