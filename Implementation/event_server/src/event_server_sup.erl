%%--------------------------------------------------------------------
%% @doc
%% Supervisor module for the Event Server Nodes.
%%
%% It spawns the storage, base and coordinator processes.
%% The possible supervision strategy are
%%   - one_for_one: If a child process terminates, only that process is restarted.
%%   - one_for_all: If one child terminates, all children are restarted.
%%--------------------------------------------------------------------
-module(event_server_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% supervisor callbacks
-export([init/1]).

%%--------------------------------------------------------------------
%% API functions
%%--------------------------------------------------------------------

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%%--------------------------------------------------------------------
%% Supervisor callbacks
%%--------------------------------------------------------------------
init([]) ->

    %% Supervisor flags
    SupFlags = #{
                    strategy => one_for_one, 
                    intensity => 5, 
                    period => 10
                },

    %% Child specifications
    %% storage, base and coordinator export a start_link/0 
    %% function that starts them as gen_servers
    ChildSpecs = [
        #{
            id => storage,
            start => {storage, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [storage]
        },
        #{
            id => base,
            start => {base, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [base]
        },
        #{
            id => coordinator,
            start => {coordinator, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [coordinator]
        }
    ],

    {ok, {SupFlags, ChildSpecs}}.
