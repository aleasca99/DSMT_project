%%--------------------------------------------------------------------
%% @doc
%% Supervisor for the backend application.
%%
%% Starts the erlang_backend_api process.
%%--------------------------------------------------------------------
-module(erlang_backend_sup).

-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    SupFlags = #{
                    strategy => one_for_one, 
                    intensity => 5, period => 10
                },

    ChildSpecs = [
        #{
            id => erlang_backend_api,
            start => {erlang_backend_api, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [erlang_backend_api]
        }
    ],

    {ok, {SupFlags, ChildSpecs}}.
