-module(movie_server).

-behaviour(gen_server).

%% API
-export([start_link/0, stop/0]).
-export([create_account/1, delete_account/1, rent_dvd/2, return_dvd/2,
	 ask_for_popcorn/0, available_movies/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-type name()     :: atom().
-type movie()    :: atom().
-type password() :: pos_integer().
 
-record(state, {users     :: ets:tid(),
                movies    :: ets:tid(),
	        next_pass :: password()}). 

-define(MOVIES, [{mary_poppins,3}, {finding_nemo,2}, {despicable_me,3},
		 {toy_story,5}, {the_lion_king,2}, {peter_pan,1}]).


%%%-------------------------------------------------------------------
%%% API
%%%-------------------------------------------------------------------

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() -> 
    gen_server:call(?MODULE, stop).

-spec create_account(name()) -> password().
create_account(Name) ->
    gen_server:call(?MODULE, {new_account, Name}).

-spec delete_account(password()) ->
	 'not_a_client' | 'account_deleted' | 'return_movies_first'.
delete_account(Password) ->
    gen_server:call(?MODULE, {delete_account, Password}).

-spec rent_dvd(password(), movie()) -> [movie()] | 'not_a_client'.
rent_dvd(Password, Movie) ->
    gen_server:call(?MODULE, {rent, Password, Movie}).

-spec return_dvd(password(), movie()) -> [movie()] | 'not_a_client'.
return_dvd(Password, Movie) ->
    gen_server:call(?MODULE, {return, Password, Movie}).

-spec ask_for_popcorn() -> 'bon_appetit'.
ask_for_popcorn() ->
    gen_server:call(?MODULE, popcorn).

available_movies() -> ?MOVIES.


%%%-------------------------------------------------------------------
%%% Gen_server callbacks
%%%-------------------------------------------------------------------

init([]) ->
    Tid = ets:new(movies, []),
    ets:insert(Tid, ?MOVIES),
    {ok, #state{users     = ets:new(users, []),
		movies    = Tid, 
		next_pass = 1}}.

handle_call({new_account,Name}, _From, S) ->
    #state{users = Tab, next_pass = Pass} = S, 
    ets:insert(Tab, {Pass,Name,[]}),
    {reply, Pass, S#state{next_pass = Pass + 1}};
handle_call({delete_account,Pass}, _From, S) ->
    #state{users = Tab} = S,
    Reply = case ets:lookup(Tab, Pass) of
		[]  ->
		    not_a_client;
		[{_,_,[]}] ->
		    ets:delete(Tab, Pass),
		    account_deleted;
		[{_,_,_}] ->
		    return_movies_first
	    end,
    {reply, Reply, S};
handle_call({rent,Pass,Movie}, _From, S) ->
    #state{users = Users, movies = Movies} = S,
    Reply =
	case ets:lookup(Users, Pass) of
	    []  ->
		not_a_client;
	    [{_,_,Rented}] ->
		case ets:lookup(Movies, Movie) of
		    [] ->
			Rented;
		    [{_,0}] ->
			Rented;
		    [{_,N}] ->
			NewRented = [Movie|Rented],
			ets:update_element(Users, Pass, {3,NewRented}),
			ets:update_element(Movies, Movie, {2,N-1}),
			NewRented
		end
	end,
    {reply, Reply, S};
handle_call({return,Pass,Movie}, _From, S) ->
    #state{users = Users, movies = Movies} = S,
    Reply =
	case ets:lookup(Users, Pass) of
	    []  ->
		not_a_client;
	    [{_,_,Rented}] ->
		%% NewRented = lists:delete(Movie, Rented),
		%% ets:update_element(Users, Pass, {3,NewRented}),
		%% N = ets:lookup_element(Movies, Movie, 2),
		%% ets:update_element(Movies, Movie, {2,N+1}),
		%% NewRented
		case ets:lookup(Movies, Movie) of
		    [] ->
			Rented;
		    [{_,N}] ->
			NewRented = lists:delete(Movie, Rented),
			ets:update_element(Users, Pass, {3,NewRented}),
			ets:update_element(Movies, Movie, {2,N+1}),
			NewRented
		end
	end,
    {reply, Reply, S};
handle_call(popcorn, _From, S) ->
    {reply, bon_appetit, S};
handle_call(stop, _From, S) ->
    {stop, normal, stopped, S}.

terminate(_Reason, #state{users = Tab1, movies = Tab2}) ->
    ets:delete(Tab1),
    ets:delete(Tab2),
    ok.

handle_cast(_Msg, S) ->
    {noreply, S}.

handle_info(_Info, S) ->
    {noreply, S}.

code_change(_OldVsn, S, _Extra) ->
    {ok, S}.
