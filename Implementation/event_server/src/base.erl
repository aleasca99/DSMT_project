%%--------------------------------------------------------------------
%% @doc
%% Base module for the Event Server.
%%
%% Receives all requests from the erlang backend.
%% - For event creation it calls the coordinator.
%% - For constraint submissions it retrieves the current partial solution
%%   from storage (if any), computes the new intersection using calculator,
%%   and updates storage.
%%--------------------------------------------------------------------
-module(base).

-behaviour(gen_server).

%% API
-export([start_link/0,
         create_event/3,
         add_constraint/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {}).

%%%===================================================================
%%% API Functions
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% create_event(EventId, Deadline, Constraints)
%% Deadline is a Unix timestamp (in seconds). Constraints is a list of intervals.
create_event(EventId, Deadline, Constraints) ->
    gen_server:call(?MODULE, {create_event, EventId, Deadline, Constraints}).

%% add_constraint(EventId, NewConstraints)
%% NewConstraints is a list of intervals.
add_constraint(EventId, NewConstraints) ->
    gen_server:call(?MODULE, {add_constraint, EventId, NewConstraints}).

%%%===================================================================
%%% gen_server Callbacks
%%%===================================================================

init([]) ->
    %% Get Backend node from configuration.
    BackendNode = config_reader:get_backend_node(),
    %% Connect to the Backend node using net_adm:ping/1.
    net_adm:ping(BackendNode),
    io:format("Base initialized.~n"),
    {ok, #state{}}.

handle_call({create_event, EventId, Deadline, Constraints}, _From, State) ->
    %% Forward to coordinator for event creation.
    coordinator:create_event(EventId, Deadline, Constraints),
    {reply, ok, State};

handle_call({add_constraint, EventId, NewConstraints}, _From, State) ->
    %% Get the current partial solution from storage.
    case storage:get_solution(EventId) of
        undefined ->
            %% No stored partial solution yet, so store the new constraints as the initial solution.
            storage:store_solution(EventId, NewConstraints),
            {reply, ok, State};
        no_solution ->
            %% Already determined that no solution is possible; ignore further constraints.
            {reply, ok, State};
        expired -> 
            %% The deadline has passed; ignore further constraints.
            {reply, ok, State};
        ExistingSolution ->
            %% Compute the new partial solution.
            NewSolution = calculator:intersect(ExistingSolution, NewConstraints),
            storage:store_solution(EventId, NewSolution),
            {reply, ok, State}
    end;

%% Catch-all for synchronous messages not explicitly handled.
handle_call(_Request, _From, State) ->
    {reply, error, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
