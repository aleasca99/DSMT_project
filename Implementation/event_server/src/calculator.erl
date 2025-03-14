%%--------------------------------------------------------------------
%% @doc
%% Utility module for computing intersections of time intervals.
%%
%% Provides:
%%  - intersect/2: Computes the intersection between two lists of intervals.
%%  - final_intersection/1: Computes the intersection among a list of partial solutions.
%%
%% Each interval is represented as a tuple {Start, End} (Unix timestamps).
%% If no intersection is possible, the atom no_solution is returned.
%% If the event has expired, the atom expired is returned.
%% If only undefined intervals are present, the atom undefined is returned.
%% If the input is invalid, the atom invalid_input is returned. 
%%--------------------------------------------------------------------
-module(calculator).

-export([intersect/2, final_intersection/1]).

%%--------------------------------------------------------------------
%% @doc
%% intersect(Intervals1, Intervals2) -> NewSolution | no_solution | expired | invalid_input
%%
%% Intervals1 and Intervals2 are lists of intervals.
%% The function computes all intersections between intervals in Intervals1
%% and those in Intervals2.
%%
%% Examples:
%%   intersect([{10, 20}, {25, 35}], [{15, 30}]) returns [{15,20}, {25,30}]
%%   intersect([{10, 20}], [{21, 30}]) returns no_solution
%%--------------------------------------------------------------------
intersect(expired, _) -> expired;
intersect(_, expired) -> expired;
intersect(no_solution, _) -> no_solution;
intersect(_, no_solution) -> no_solution;
intersect(Intervals1, Intervals2) when not is_list(Intervals1); not is_list(Intervals2) ->
    invalid_input;
intersect(Intervals1, Intervals2) ->
    %% For each pair of intervals, compute their intersection (if any)
    NewIntervals = [ I ||
        I1 <- Intervals1,
        I2 <- Intervals2,
        I <- [intersect_two(I1, I2)],
        I =/= undefined
    ],
    case NewIntervals of
        [] -> no_solution;
        _  -> NewIntervals
    end.
    
%%--------------------------------------------------------------------
%% @private
%% intersect_two({S1, E1}, {S2, E2}) -> Intersection | undefined
%%
%% Computes the intersection of two intervals.
%% Returns {Max, Min} if Max < Min, otherwise returns undefined.
%%--------------------------------------------------------------------
intersect_two({S1, E1}, {S2, E2}) ->
    %% Compute the maximum of the start times and the minimum of the end times.
    MaxStart = max(S1, S2),
    MinEnd   = min(E1, E2),
    if
        MaxStart < MinEnd -> {MaxStart, MinEnd};
        true -> undefined
    end.

%%--------------------------------------------------------------------
%% @doc
%% final_intersection(PartialSolutions) -> FinalSolution | no_solution | undefined | expired | invalid_input
%%
%% Given a list of partial solutions (each a list of intervals),
%% compute their intersection. If the intersection at any stage results in no solution,
%% returns the atom no_solution. If the event has expired, returns the atom expired.
%% If only undefined intervals are present, returns the atom undefined.
%% If the input is invalid, returns the atom invalid_input.
%%
%% Example:
%%   final_intersection([[{10, 20}, {25, 35}], [{15, 30}]])
%%     -> returns the intersection [{15,20}, {25,30}]
%%--------------------------------------------------------------------
final_intersection([]) ->
    [];
final_intersection([Solution]) ->
    Solution;
final_intersection([undefined | Rest]) -> 
    final_intersection(Rest);
final_intersection([Solution, undefined | Rest]) -> 
    final_intersection([Solution | Rest]);
final_intersection([Solution1, Solution2 | Rest]) ->
    case intersect(Solution1, Solution2) of
        no_solution -> 
            no_solution;
        TempSolution -> 
            final_intersection([TempSolution | Rest])
    end.
