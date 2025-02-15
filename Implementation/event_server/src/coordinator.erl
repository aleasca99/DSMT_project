%%--------------------------------------------------------------------
%% @doc
%% Coordinator module.
%%
%% Handles event creation requests by scheduling a deadline expiration.
%% When the deadline is reached, it gathers partial solutions from all nodes,
%% computes the final solution and sends it to the erlang backend.
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
    io:format("Event Server nodes: ~p~n", [Nodes]),
    %% Get all stored deadlines (if any) and schedule the corresponding timers.
    %% This is a recovery mechanism in case the coordinator was restarted.
    Deadlines = storage:get_all_deadlines(),
    io:format("Deadlines: ~p~n", [Deadlines]),
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
    io:format("Coordinator initialized.~n"),
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
    %% Compute the final solution.
    FinalSolution = calculator:final_intersection(PartialSolutions),
    io:format("Computed final solution for event ~p: ~p~n", [EventId, FinalSolution]),
    %% Ensure the final solution is sent to the backend 
    %% only if it hasn't been handled already.
    case FinalSolution of
        expired ->
            io:format("Event Solution already computed when current node was down.~n");
        _ ->
            %% Send the final solution to the backend.
            BackendNode = config_reader:get_backend_node(),
            send_final_solution(BackendNode, EventId, FinalSolution)
    end,
    %% Store on all nodes that the event has expired 
    %% (i.e., the final solution has been computed).
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
    io:format("No backend configured. Final solution for event ~p: ~p~n", [EventId, FinalSolution]);
send_final_solution(BackendNode, EventId, FinalSolution) ->
    io:format("Sending final solution to backend on node ~p~n", [BackendNode]),
    %% Normalizing the FinalSolution so it always follows the list-of-tuples format.
    %% In this way the Java Backend can always expect a list of tuples.
    FinalSolutionToSend = case FinalSolution of
                                undefined -> [{undefined, 0}];
                                no_solution -> [{no_solution, 0}];
                                _ -> FinalSolution
                          end,
    %% Send the final solution to the erlang backend.
    rpc:call(BackendNode, erlang_backend_api, final_solution, [EventId, FinalSolutionToSend]).

%% Stores on all nodes that the event has expired.
event_expired(EventId, Nodes) ->
    lists:foreach(
      fun(Node) -> 
              rpc:call(Node, storage, store_solution, [EventId, expired])
      end,
      Nodes).

%% Deletes event data from all nodes.
delete_event_all(EventId, Nodes) ->
    lists:foreach(
      fun(Node) ->
              rpc:call(Node, storage, delete_event, [EventId])
      end,
      Nodes).
