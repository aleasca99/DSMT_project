%%--------------------------------------------------------------------
%% @doc
%% Coordinator module.
%%
%% Handles event creation requests by scheduling a deadline expiration.
%% When the deadline is reached, it gathers partial solutions from all nodes,
%% computes the final solution and sends it to the backend.
%%--------------------------------------------------------------------
-module(coordinator).

-behaviour(gen_server).

%% API
-export([start_link/0, create_event/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {
    %% List of node names (atoms) to gather partial solutions from.
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
    %% Delay the initialization of the coordinator to 
    %% be sure all the other processes are started.
    timer:sleep(1000),
    %% Read the configuration to get the list of nodes.
    Nodes = config_reader:get_nodes(),
    %% Get all stored deadlines (if any) and schedule the corresponding timers.
    Deadlines = storage:get_all_deadlines(),
    io:format("Coordinator starting with deadlines: ~p~n", [Deadlines]),
    Now = erlang:system_time(second),
    lists:foreach(
        fun({EventId, Deadline}) ->
                DelaySecs = Deadline - Now,
                Delay = if DelaySecs > 0 -> 
                            DelaySecs * 1000; 
                            true -> 0 
                        end,
                erlang:send_after(Delay, self(), {deadline, EventId})
        end,
        Deadlines),
    io:format("Coordinator started with nodes: ~p~n", [Nodes]),
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
    %% Store the event deadline.
    storage:store_deadline(EventId, Deadline),
    %% Compute delay (in milliseconds) until the deadline.
    Now = erlang:system_time(second),
    DelaySecs = Deadline - Now,
    Delay = if DelaySecs > 0 -> 
                DelaySecs * 1000; 
                true -> 0 
            end,
    %% Schedule the deadline timeout.
    erlang:send_after(Delay, self(), {deadline, EventId}),
    {reply, ok, State};

handle_call(_Request, _From, State) ->
    {reply, error, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({deadline, EventId}, State) ->
    io:format("Deadline reached for event ~p. Initiating final solution computation.~n", [EventId]),
    %% Gather partial solutions from all nodes.
    %% Using the list of nodes from the State.
    PartialSolutions = gather_solutions(EventId, State#state.nodes),
    %% Also include the local solution.
    % LocalSolution = storage:get_solution(EventId),
    % AllSolutions = [LocalSolution | PartialSolutions],
    FinalSolution = calculator:final_intersection(PartialSolutions),
    io:format("Final solution for event ~p is: ~p~n", [EventId, FinalSolution]),
    %% Send the final solution to the backend.
    BackendNode = config_reader:get_backend_node(),
    %% send_final_solution(BackendNode, EventId, FinalSolution),
    send_final_solution(undefined, EventId, FinalSolution),
    %% Store on all nodes that the event has expired.
    event_expired(EventId, State#state.nodes),
    %% Delete event deadline from storage.
    storage:delete_deadline(EventId),
    {noreply, State};

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
    io:format("Sending final solution for event ~p to backend on node ~p~n", [EventId, BackendNode]), 
    %% TO BE IMPLEMENTED: Send the final solution to the backend.
    %% In a real system, you might send a message or use an RPC.
    %% Here we simply call a function in the backend module.
    % backend:final_solution(EventId, FinalSolution).
    io:format("Please implement sending the final solution to the backend.~n").

%% Stores on all nodes that the event has expired.
event_expired(EventId, Nodes) ->
    % storage:store_solution(EventId, expired),
    lists:foreach(
      fun(Node) -> 
              rpc:call(Node, storage, store_solution, [EventId, expired])
      end,
      Nodes).

%% Deletes event data from all nodes.
delete_event_all(EventId, Nodes) ->
    % storage:delete_event(EventId),
    lists:foreach(
      fun(Node) ->
              rpc:call(Node, storage, delete_event, [EventId])
      end,
      Nodes).
