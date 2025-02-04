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
%%--------------------------------------------------------------------
-module(calculator).

-export([intersect/2, final_intersection/1]).

%%--------------------------------------------------------------------
%% @doc
%% intersect(ExistingSolution, NewConstraints) -> NewSolution | no_solution
%%
%% ExistingSolution and NewConstraints are lists of intervals.
%% The function computes all intersections between intervals in ExistingSolution
%% and those in NewConstraints.
%%
%% Examples:
%%   intersect([{10, 20}, {25, 35}], [{15, 30}]) returns [{15,20}, {25,30}]
%%   intersect([{10, 20}], [{21, 30}]) returns no_solution
%%--------------------------------------------------------------------
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
    %% MaxStart = if S1 >= S2 -> S1; true -> S2 end,
    %% MinEnd   = if E1 =< E2 -> E1; true -> E2 end,
    if
        MaxStart < MinEnd -> {MaxStart, MinEnd};
        true -> undefined
    end.

%%--------------------------------------------------------------------
%% @doc
%% final_intersection(PartialSolutions) -> FinalSolution | no_solution
%%
%% Given a list of partial solutions (each a list of intervals),
%% compute their intersection. If the intersection at any stage results in no solution,
%% returns the atom no_solution.
%%
%% Example:
%%   final_intersection([[{10, 20}, {25, 35}], [{15, 30}]])
%%     -> returns the intersection [{15,20}, {25,30}]
%%--------------------------------------------------------------------
final_intersection([]) ->
    [];
final_intersection([Solution]) ->
    Solution;
final_intersection([Solution1, Solution2 | Rest]) ->
    TempSolution = intersect(Solution1, Solution2),
    case TempSolution of
        no_solution ->
            no_solution;
        _ ->
            final_intersection([TempSolution | Rest])
    end.
