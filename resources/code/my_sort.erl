-module(my_sort).
-export([sort/1]).

-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").

sort([]) -> [];
sort([P|Xs]) ->
    sort([X || X <- Xs, X < P]) ++ [P] ++ sort([X || X <- Xs, P < X]).

%%%---------------------------------------------------------------------
%%% PropEr tests
%%%---------------------------------------------------------------------

prop_ordered() ->
    ?FORALL(L, list(integer()), ordered(sort(L))).

ordered([]) -> true;
ordered([_]) -> true;
ordered([A,B|T]) -> A =< B andalso ordered([B|T]).

prop_same_length() ->
    ?FORALL(L, list(integer()), length(L) =:= length(sort(L))).

prop_same_length_conditional_check() ->
    ?FORALL(L, list(integer()),
            ?IMPLIES(no_duplicates(L), length(L) =:= length(sort(L)))).

%% better implementations of no_duplicates/1 exist ...
no_duplicates([]) -> true;
no_duplicates([A|T]) ->
    not lists:member(A, T) andalso no_duplicates(T).

prop_same_length_no_dupls() ->
    ?FORALL(L, list_no_dupls(integer()), length(L) =:= length(sort(L))).

list_no_dupls(T) ->
    ?LET(L, list(T), remove_duplicates(L)).

%% better versions of remove_duplicates/1 exist ...
remove_duplicates([]) -> [];
remove_duplicates([A|T]) ->
    case lists:member(A, T) of
	true -> remove_duplicates(T);
	false -> [A|remove_duplicates(T)]
    end.

prop_equiv_usort() ->
    ?FORALL(L, list(), sort(L) =:= lists:usort(L)).

%%%---------------------------------------------------------------------
%%% EUnit tests
%%%---------------------------------------------------------------------

sort_test_() ->
    [test_zero(), test_two(), test_four()].
 
test_zero() ->
    ?_assertEqual([], sort([])). % notice underscore 
test_two() ->
    [?_assertEqual([17,42], sort([X,Y])) || {X,Y} <- [{17,42}, {42,17}]].
test_four() ->
    [?_assertEqual([1,2,3,4], sort([3,1,4,2]))].
