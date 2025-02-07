%%--------------------------------------------------------------------
%% @doc
%% Storage module using Mnesia.
%%
%% Provides APIs to get, store and delete event partial solutions.
%%--------------------------------------------------------------------
-module(storage).

%% Include the record definition.
-include("storage.hrl").

-behaviour(gen_server).

%% API
-export([start_link/0,
         get_solution/1,
         store_solution/2,
         delete_event/1,
         get_deadline/1,
         get_all_deadlines/0,
         store_deadline/2,
         delete_deadline/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {}).

-define(PARTIAL_SOLUTIONS_TABLE, partial_solutions).
-define(DEADLINES_TABLE, deadlines).

%%%===================================================================
%%% API Functions
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% get_solution(EventId) returns:
%%  - undefined if no partial solution is stored for the event yet.
%%  - no_solution if the event has no solution.
%%  - expired if the deadline has passed.
%%  - the existing partial solution otherwise.
get_solution(EventId) ->
    gen_server:call(?MODULE, {get_solution, EventId}).

%% store_solution(EventId, PartialSolution) stores or updates the solution.
store_solution(EventId, PartialSolution) ->
    gen_server:call(?MODULE, {store_solution, EventId, PartialSolution}).

%% delete_event(EventId) deletes the event record.
delete_event(EventId) ->
    gen_server:call(?MODULE, {delete_event, EventId}).

%% get_deadline(EventId) returns the deadline for the event.
get_deadline(EventId) ->
    gen_server:call(?MODULE, {get_deadline, EventId}).

%% get_all_deadlines() returns all the stored deadlines.
%% It returns a list of {EventId, Deadline} tuples.
get_all_deadlines() ->
    gen_server:call(?MODULE, get_all_deadlines).

%% store_deadline(EventId, Deadline) stores the deadline for the event.
store_deadline(EventId, Deadline) ->
    gen_server:call(?MODULE, {store_deadline, EventId, Deadline}).

%% delete_deadline(EventId) deletes the deadline for the event.
delete_deadline(EventId) ->
    gen_server:call(?MODULE, {delete_deadline, EventId}).

%%%===================================================================
%%% gen_server Callbacks
%%%===================================================================

init([]) ->
    %% Start Mnesia and create the tables if they don't exist.
    mnesia:create_schema([node()]),
    ok = mnesia:start(),
    %% Create the Partial Solution table with disc_copies on the local node.
    TableDefPartialSolution = [{attributes, record_info(fields, event_record)},
                               {disc_copies, [node()]},
                               {record_name, event_record}],
    mnesia:create_table(?PARTIAL_SOLUTIONS_TABLE, TableDefPartialSolution),
    %% Create the Deadline table with disc_copies on the local node.
    TableDefDeadline = [{attributes, record_info(fields, event_deadline)},
                        {disc_copies, [node()]},
                        {record_name, event_deadline}],
    mnesia:create_table(?DEADLINES_TABLE, TableDefDeadline),
    {ok, #state{}}.

handle_call({get_solution, EventId}, _From, State) ->
    F = fun() ->
                case mnesia:read(?PARTIAL_SOLUTIONS_TABLE, EventId, read) of
                    [] -> undefined;
                    [#event_record{partialsolution = Sol}] -> Sol
                end
        end,
    %% Execute the transaction and process its result
    Result = case mnesia:transaction(F) of
                {atomic, Sol} -> Sol;
                {aborted, _Reason} -> undefined
             end,
    {reply, Result, State};

handle_call({store_solution, EventId, PartialSolution}, _From, State) ->
    F = fun() ->
                Record = #event_record{eventid = EventId, partialsolution = PartialSolution},
                mnesia:write(?PARTIAL_SOLUTIONS_TABLE, Record, write)
        end,
    _ = mnesia:transaction(F),
    {reply, ok, State};

handle_call({delete_event, EventId}, _From, State) ->
    F = fun() ->
                mnesia:delete(?PARTIAL_SOLUTIONS_TABLE, EventId, write)
        end,
    _ = mnesia:transaction(F),
    {reply, ok, State};

handle_call({get_deadline, EventId}, _From, State) ->
    F = fun() ->
                case mnesia:read(?DEADLINES_TABLE, EventId, read) of
                    [] -> undefined;
                    [#event_deadline{deadline = Deadline}] -> Deadline
                end
        end,
    %% Execute the transaction and process its result
    Result = case mnesia:transaction(F) of
                {atomic, Deadline} -> Deadline;
                {aborted, _Reason} -> undefined
             end,
    {reply, Result, State};

handle_call(get_all_deadlines, _From, State) ->
    F = fun() ->
                mnesia:select(?DEADLINES_TABLE, [{'_', [], ['$_']}])
        end,
    %% Execute the transaction and process its result
    Result = case mnesia:transaction(F) of
                {atomic, Deadlines} -> 
                    %% The transaction returns a list of: {event_deadline, EventId, Deadline} tuples.
                    %% This function however has to return a list of {EventId, Deadline} tuples.
                    [{EventId, Deadline} || {event_deadline, EventId, Deadline} <- Deadlines];
                {aborted, _Reason} -> []
             end,
    {reply, Result, State};

handle_call({store_deadline, EventId, Deadline}, _From, State) ->
    F = fun() ->
                Record = #event_deadline{eventid = EventId, deadline = Deadline},
                mnesia:write(?DEADLINES_TABLE, Record, write)
        end,
    _ = mnesia:transaction(F),
    {reply, ok, State};

handle_call({delete_deadline, EventId}, _From, State) ->
    F = fun() ->
                mnesia:delete(?DEADLINES_TABLE, EventId, write)
        end,
    _ = mnesia:transaction(F),
    {reply, ok, State};

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
