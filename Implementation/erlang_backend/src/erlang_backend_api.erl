%%--------------------------------------------------------------------
%% @doc
%% Erlang Backend API module.
%%
%% This module provides the following functionalities:
%% - Exposes APIs to send create event and constraint requests
%%   to the specified Erlang Event Server Node.
%% - Monitors the nodes and notifies Java when a node goes down.
%% - Receives final solutions (sent by the coordinator) and forwards
%%   them to the Java backend.
%%--------------------------------------------------------------------
-module(erlang_backend_api).

-behaviour(gen_server).

%% API exports
-export([start_link/0,
         final_solution/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {}).

%%%===================================================================
%%% API Functions
%%%===================================================================

%% Starts the erlang backend API process.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% Creation of an event:
%% {create_event, Node, EventId, Deadline, Constraints}
%%
%% - Node: atom representing the target event server node.
%% - EventId: unique identifier for the event.
%% - Deadline: Unix timestamp (seconds).
%% - Constraints: list of initial constraints (list of intervals).

%% Addition of constraints to an event:
%% {add_constraint, Node, EventId, NewConstraints}
%%
%% - Node: atom representing the target event server node.
%% - EventId: unique identifier for the event.
%% - NewConstraints: list of new constraints (intervals).

%% Called remotely from an Event Server Node when a final solution is computed.
final_solution(EventId, FinalSolution) ->
    gen_server:call(?MODULE, {final_solution, EventId, FinalSolution}).

%%%===================================================================
%%% gen_server Callbacks
%%%===================================================================

init([]) ->
    %% Enable node monitoring so we get notified of nodeup/nodedown events.
    net_kernel:monitor_nodes(true),
    %% Read the configuration to get the list of event server nodes.
    Nodes = config_reader:get_nodes(),
    io:format("Backend API started and monitoring nodes: ~p~n", [Nodes]),
    %% Connecting to the event server nodes via net_adm:ping/1
    lists:foreach(fun(Node) -> net_adm:ping(Node) end, Nodes),
    {ok, #state{}}.

handle_call({final_solution, EventId, FinalSolution}, _From, State) ->
    io:format("Backend API: Final solution for event ~p is: ~p~n", [EventId, FinalSolution]),
    %% Notify the Java backend about the final solution.
    JavaBackendMailbox = config_reader:get_java_backend_mailbox(),
    JavaBackendNode = config_reader:get_java_backend_node(),
    {JavaBackendMailbox, JavaBackendNode} ! {final_solution, EventId, FinalSolution},
    {reply, ok, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({create_event, Node, EventId, Deadline, Constraints}, State) ->
    io:format("Backend API: Creating event ~p with deadline ~p and constraints ~p on node ~p~n", [EventId, Deadline, Constraints, Node]),
    %% RPC call to the base module on the specified node.
    rpc:call(Node, base, create_event, [EventId, Deadline, Constraints]),    
    {noreply, State};

handle_info({add_constraint, Node, EventId, NewConstraints}, State) ->
    io:format("Backend API: Adding constraints ~p to event ~p on node ~p~n", [NewConstraints, EventId, Node]),
    %% RPC call to the base module on the specified node.
    rpc:call(Node, base, add_constraint, [EventId, NewConstraints]),
    {noreply, State};

handle_info({nodeup, Node}, State) ->
    io:format("Backend API: Node up ~p~n", [Node]),
    %% Notify Java about the node-up event.
    JavaBackendMailbox = config_reader:get_java_backend_mailbox(),
    JavaBackendNode = config_reader:get_java_backend_node(),
    {JavaBackendMailbox, JavaBackendNode} ! {node_status, Node, up},
    {noreply, State};

handle_info({nodedown, Node}, State) ->
    io:format("Backend API: Node down ~p~n", [Node]),
    %% Notify Java that Node is down.
    JavaBackendMailbox = config_reader:get_java_backend_mailbox(),
    JavaBackendNode = config_reader:get_java_backend_node(),
    {JavaBackendMailbox, JavaBackendNode} ! {node_status, Node, down},
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
