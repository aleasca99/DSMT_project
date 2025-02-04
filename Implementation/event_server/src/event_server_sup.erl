%%--------------------------------------------------------------------
%% @doc
%% Supervisor module for the Event Server Nodes.
%%
%% It spawns the base, coordinator and storage processes.
%% The supervision strategy is one_for_all: if one child terminates,
%% all children are restarted.
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
    %% one_for_all strategy: if any child fails, all children are restarted.
    SupFlags = #{
                    strategy => one_for_all, 
                    intensity => 5, 
                    period => 10
                },

    %% Child specifications
    %% base, coordinator and storage export a start_link/0 
    %% function that starts them as gen_servers
    ChildSpecs = [
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
        },
        #{
            id => storage,
            start => {storage, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [storage]
        }
    ],

    {ok, {SupFlags, ChildSpecs}}.
