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
    proper:quickcheck(?MODULE:prop_server_works_fine()).

sample() ->
    proper_gen:pick(commands(?MODULE)).

prop_server_works_fine() ->
    ?FORALL(Cmds, commands(?MODULE),
	    ?TRAPEXIT(
	       begin
		   ?SERVER:start_link(),
		   {History,State,Result} = run_commands(?MODULE, Cmds),
		   ?SERVER:stop(),
		   ?WHENFAIL(io:format("History: ~w~nState: ~w\nResult: ~w~n",
		   		       [History,State,Result]),
		   	     aggregate(command_names(Cmds), Result =:= ok))
	       end)).

initial_state() ->
    #state{users  = [],
	   rented = []}.

command(S) ->
    Users  = (S#state.users =/= []),
    Rented = (S#state.rented =/= []),
    frequency([{1, {call,?SERVER,create_account,[name()]}},
	       {1, {call,?SERVER,ask_for_popcorn,[]}}] ++
	      [{1, {call,?SERVER,delete_account,[password(S)]}} || Users] ++
	      [{5, {call,?SERVER,rent_dvd,[password(S), movie()]}} || Users] ++
	      [{5, ?LET({Password,Movie}, elements(S#state.rented),
	      		{call,?SERVER,return_dvd,[Password, Movie]})} || Rented]).

name() ->
    elements(?NAMES).

movie() ->
    elements(?MOVIE_TITLES).

password(#state{users = Passwords}) ->
    elements(Passwords).

precondition(S, {call,_,return_dvd,[Password,Movie]}) ->
    lists:member({Password,Movie}, S#state.rented);
precondition(S, {call,_,rent_dvd,[Password,_Movie]}) ->
    not lists:member({Password,_Movie}, S#state.rented) andalso
	lists:member(Password, S#state.users);
precondition(S, {call,_,delete_account,[Password]}) ->
    lists:member(Password, S#state.users);
precondition(_, _) ->
    true.

next_state(S, V, {call,_,create_account,[_Name]}) ->
    S#state{users = [V|S#state.users]};
next_state(S, _V, {call,_,delete_account,[Password]}) ->
    case proplists:is_defined(Password, S#state.rented) of
    	false ->
    	    S#state{users = lists:delete(Password, S#state.users)};
    	true ->
    	    S
    end;
next_state(S, _V, {call,_,rent_dvd,[Password,Movie]}) ->
    case is_available(Movie, S) of
	true  ->
	    S#state{rented = [{Password,Movie}|S#state.rented]};
	false ->
	    S
    end;
next_state(S, _V, {call,_,return_dvd,[Password,Movie]}) ->
    S#state{rented = lists:delete({Password,Movie}, S#state.rented)};
next_state(S, _V, {call,_,ask_for_popcorn,[]}) ->
    S.

postcondition(S, {call,_,create_account,[_Name]}, Result) ->
    not lists:member(Result, S#state.users);
postcondition(S, {call,_,delete_account,[Password]}, Result) ->
    case proplists:is_defined(Password, S#state.rented) of
    	false ->
    	    Result =:= account_deleted;
    	true ->
    	    Result =:= return_movies_first
    end;
postcondition(S, {call,_,rent_dvd,[_Password,Movie]}, Result) ->
    case is_available(Movie, S) of
	true ->
	    lists:member(Movie, Result);
	false ->
	    not lists:member(Movie, Result)
    end;
postcondition(_S, {call,_,return_dvd,[_Password,Movie]}, Result) ->
    not lists:member(Movie, Result);
postcondition(_S, {call,_,ask_for_popcorn,[]}, Result) ->
    Result =:= bon_appetit.

is_available(Movie, #state{rented = Rented}) ->
    Av = proplists:get_value(Movie, ?AVAILABLE_MOVIES, -1),
    count(Movie, Rented) < Av.

count(Movie, PropList) ->
    lists:foldl(fun({_,X}, Acc) -> matches(Movie, X) + Acc end, 0, PropList).

matches(X, X) -> 1;
matches(_, _) -> 0.
