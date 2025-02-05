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
         delete_event/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {}).

-define(TABLE, partial_solutions).

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

%%%===================================================================
%%% gen_server Callbacks
%%%===================================================================

init([]) ->
    %% Start Mnesia and create the table if not exists.
    mnesia:create_schema([node()]),
    ok = mnesia:start(),
    %% Create the table with disc_copies on the local node.
    TableDef = [{attributes, record_info(fields, event_record)},
                {disc_copies, [node()]},
                {record_name, event_record}],
    mnesia:create_table(?TABLE, TableDef),
    {ok, #state{}}.

handle_call({get_solution, EventId}, _From, State) ->
    F = fun() ->
                case mnesia:read(?TABLE, EventId, read) of
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
                mnesia:write(?TABLE, Record, write)
        end,
    _ = mnesia:transaction(F),
    {reply, ok, State};

handle_call({delete_event, EventId}, _From, State) ->
    F = fun() ->
                mnesia:delete(?TABLE, EventId, write)
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
