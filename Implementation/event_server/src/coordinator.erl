%%--------------------------------------------------------------------
%% @doc
%% Coordinator module.
%%
%% Handles event creation requests by scheduling a deadline expiration.
%% When the deadline is reached, it gathers partial solutions from all nodes,
%% computes the final solution, sends it to the backend and deletes the event data.
%%--------------------------------------------------------------------
-module(coordinator).

-behaviour(gen_server).

%% API
-export([start_link/0, create_event/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {
    %% Map of EventId -> TimerRef for scheduled deadlines
    timers = #{},
    %% List of other node names (atoms) to gather partial solutions from.
    nodes = []
}).

%%%===================================================================
%%% API Functions
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% create_event(EventId, Deadline, Constraints)
create_event(EventId, Deadline, Constraints) ->
    gen_server:call(?MODULE, {create_event, EventId, Deadline, Constraints}).

%%%===================================================================
%%% gen_server Callbacks
%%%===================================================================

init([]) ->
    %% Read the list of node names from config. For example, assume the config
    %% has a key <<"nodes">> which is a JSON array of node names.
    Config = config_reader:load_config(),
    %% Assume the nodes are provided as a list of binaries under the key <<"nodes">>.
    Nodes = case maps:get(<<"nodes">>, Config, undefined) of
                undefined -> [];
                List when is_list(List) ->
                    [ binary_to_atom(NodeBin, utf8) || NodeBin <- List ]
            end,
    %% Register self in the global registry (if desired).
    global:register_name(coordinator, self()),
    {ok, #state{nodes = Nodes}}.

handle_call({create_event, EventId, Deadline, Constraints}, _From, State) ->
    %% On event creation, store the initial constraints in storage.
    %% Store the intial constraints only if they are not an empty list.
    case Constraints of
        [] ->
            ok;
        _ ->
            storage:store_solution(EventId, Constraints)
    end,
    %% Compute delay (in milliseconds) until the deadline.
    Now = erlang:system_time(second),
    DelaySecs = Deadline - Now,
    Delay = if DelaySecs > 0 -> DelaySecs * 1000; true -> 0 end,
    %% Schedule the deadline timeout.
    TimerRef = erlang:send_after(Delay, self(), {deadline, EventId}),
    NewTimers = maps:put(EventId, TimerRef, State#state.timers),
    {reply, ok, State#state{timers = NewTimers}};

handle_call(_Request, _From, State) ->
    {reply, error, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({deadline, EventId}, State) ->
    io:format("Deadline reached for event ~p. Initiating final solution computation.~n", [EventId]),
    %% Gather partial solutions from all nodes.
    %% We use the list of nodes from our state.
    PartialSolutions = gather_solutions(EventId, State#state.nodes),
    %% Also include our own local storage.
    LocalSolution = storage:get_solution(EventId),
    AllSolutions = [LocalSolution | PartialSolutions],
    FinalSolution = calculator:final_intersection(AllSolutions),
    io:format("Final solution for event ~p is: ~p~n", [EventId, FinalSolution]),
    %% Send the final solution to the backend.
    BackendNode = config_reader:get_backend_node(),
    send_final_solution(BackendNode, EventId, FinalSolution),
    %% Delete the event data from storage on all nodes.
    delete_event_all(EventId, State#state.nodes),
    %% Remove the timer from our state.
    NewTimers = maps:remove(EventId, State#state.timers),
    {noreply, State#state{timers = NewTimers}};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal Functions
%%%===================================================================

%% Gather partial solutions from the nodes listed.
gather_solutions(EventId, Nodes) ->
    gather_solutions(EventId, Nodes, []).

gather_solutions(_EventId, [], Acc) ->
    Acc;
gather_solutions(EventId, [Node | Rest], Acc) ->
    %% Use RPC to call storage:get_solution/1 on Node.
    %% We assume that all nodes run the same code.
    case rpc:call(Node, storage, get_solution, [EventId]) of
        {'badrpc', _} ->
            io:format("Warning: Could not gather solution from node ~p for event ~p~n", [Node, EventId]),
            gather_solutions(EventId, Rest, Acc);
        Solution ->
            gather_solutions(EventId, Rest, [Solution | Acc])
    end.

%% Sends the final solution to the backend.
send_final_solution(undefined, EventId, FinalSolution) ->
    io:format("No backend configured; final solution for event ~p: ~p~n", [EventId, FinalSolution]);
send_final_solution(BackendNode, EventId, FinalSolution) ->
    io:format("Sending final solution for event ~p to backend on node ~p~n", [EventId, BackendNode]). %, 
    %% In a real system, you might send a message or use an RPC.
    %% Here we simply call a function in the backend module.
    % backend:final_solution(EventId, FinalSolution).

%% Deletes event data from local storage and from nodes.
delete_event_all(EventId, Nodes) ->
    storage:delete_event(EventId),
    lists:foreach(
      fun(Node) ->
              rpc:call(Node, storage, delete_event, [EventId])
      end,
      Nodes).
