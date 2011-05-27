-module(movie_statem).
-behaviour(proper_statem).

-include_lib("proper/include/proper.hrl").

-export([test/0, sample/0]).
-export([initial_state/0, command/1, precondition/2, postcondition/3,
	 next_state/3]).

-type movie()    :: atom().
-type password() :: pos_integer().
 
-record(state, {users  :: [password()],
		rented :: [{password(),movie()}]}).

-define(SERVER, movie_server).
-define(NAMES, [bob, alice, john, mary, ben]).
-define(AVAILABLE_MOVIES, ?SERVER:available_movies()).
-define(MOVIE_TITLES,
	proplists:get_keys(?AVAILABLE_MOVIES) ++ [titanic, inception]).


%%--------------------------------------------------------------------
%%% Statem callbacks
%%--------------------------------------------------------------------

test() ->
    proper:quickcheck(?MODULE:prop_movies()).

sample() ->
    proper_gen:pick(commands(?MODULE)).

prop_movies() ->
    ?FORALL(Cmds, commands(?MODULE),
	?TRAPEXIT(
	   begin
	       ?SERVER:start_link(),
	       {H,S,Res} = run_commands(?MODULE, Cmds),
	       ?SERVER:stop(),
	       ?WHENFAIL(
		  io:format("History: ~w\nState: ~w\nRes: ~w\n",
			    [H,S,Res]),
		  aggregate(command_names(Cmds), Res =:= ok))
	   end)).

%% prop_p() ->
%%     ?FORALL(Cmds, parallel_commands(?MODULE),
%% 	?TRAPEXIT(
%% 	   begin
%% 	       start_link(),
%% 	       {S,P,Res} = run_parallel_commands(?MODULE, Cmds),
%% 	       stop(),
%% 	       ?WHENFAIL(
%% 		  io:format("Sequential: ~w\nParallel: ~w\nRes: ~w\n",
%% 			    [S,P,Res]),
%% 		  Res =:= ok)
%% 	   end)).

initial_state() ->
    #state{users  = [],
	   rented = []}.

command(S) ->
    frequency([{1, {call,?SERVER,create_account,[name()]}},
	       {1, {call,?SERVER,ask_for_popcorn,[]}}] ++
	      [{1, {call,?SERVER,delete_account,[password(S)]}}
	       || S#state.users =/= []] ++
	      [{5, {call,?SERVER,rent_dvd,[password(S), movie()]}}
	       || S#state.users =/= []] ++
	      [{5, ?LET({Pass,Movie}, elements(S#state.rented),
			{call,?SERVER,return_dvd,[Pass, Movie]})}
	       || S#state.rented =/= []]).

name() ->
    elements(?NAMES).

movie() ->
    elements(?MOVIE_TITLES).

password(#state{users = Passwords}) ->
    elements(Passwords).

precondition(S, {call,_,return_dvd,[Pass,Movie]}) ->
    lists:member({Pass,Movie}, S#state.rented);
precondition(S, {call,_,rent_dvd,[Pass,_Movie]}) ->
    not lists:member({Pass,_Movie}, S#state.rented) andalso
	lists:member(Pass, S#state.users);
precondition(S, {call,_,delete_account,[Pass]}) ->
    lists:member(Pass, S#state.users);
precondition(_, _) ->
    true.

next_state(S, V, {call,_,create_account,[_Name]}) ->
    S#state{users = [V|S#state.users]};
next_state(S, _V, {call,_,delete_account,[Pass]}) ->
    case proplists:is_defined(Pass, S#state.rented) of
    	false ->
    	    S#state{users = lists:delete(Pass, S#state.users)};
    	true ->
    	    S
    end;
next_state(S, _V, {call,_,rent_dvd,[Pass,Movie]}) ->
    case is_available(Movie, S) of
	true  ->
	    S#state{rented = [{Pass,Movie}|S#state.rented]};
	false ->
	    S
    end;
next_state(S, _V, {call,_,return_dvd,[Pass,Movie]}) ->
    S#state{rented = lists:delete({Pass,Movie}, S#state.rented)};
next_state(S, _V, {call,_,ask_for_popcorn,[]}) ->
    S.

postcondition(S, {call,_,create_account,[_Name]}, Res) ->
    not lists:member(Res, S#state.users);
postcondition(S, {call,_,delete_account,[Pass]}, Res) ->
    case proplists:is_defined(Pass, S#state.rented) of
    	false ->
    	    Res =:= account_deleted;
    	true ->
    	    Res =:= return_movies_first
    end;
postcondition(S, {call,_,rent_dvd,[_Pass,Movie]}, Res) ->
    case is_available(Movie, S) of
	true ->
	    lists:member(Movie, Res);
	false ->
	    not lists:member(Movie, Res)
    end;
postcondition(_S, {call,_,return_dvd,[_Pass,Movie]}, Res) ->
    not lists:member(Movie, Res);
postcondition(_S, {call,_,ask_for_popcorn,[]}, Res) ->
    Res =:= bon_appetit.

is_available(Movie, #state{rented = Rented}) ->
    Av = proplists:get_value(Movie, ?AVAILABLE_MOVIES, -1),
    R = count(Movie, Rented),
    if R < Av  -> true;
       R >= Av -> false
    end.

count(Movie, PropList) ->
    lists:foldl(fun({_,X}, Acc) -> matches(Movie, X) + Acc end, 0, PropList).

matches(X, X) -> 1;
matches(_, _) -> 0.
