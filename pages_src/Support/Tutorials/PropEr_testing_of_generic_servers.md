Summary: a PropEr statem tutorial

Testing purely functional code is usually not enough for industrial erlang
applications like telecom software or http servers. In this tutorial, we
describe how to use PropEr for automated random testing of stateful systems.

Undoubtedly, automated random testing of a stateful API would require some
magic, unless some additional information is provided. Since we're not
magicians but erlang programmers, we choose to describe the side-effects of
the system under test via an abstract state machine and then, let PropEr do
some magic for us.


The example
-----------

Let us first introduce our example: a movie server at a dvd-club, implemented
as an erlang generic server (gen_server behaviour). You can find the code of
the server [here](/code/movies/movie_server.erl). Nevertheless, for the most
part of this tutorial you only need to understand the server's API, which is
described below:

*   _Create a new account_
  
    Just say your name, and a new account will be created for you. The server
    will return a password for the new account. This password can be
    used for all future requests.
    
        :::erlang
        -spec create_account(name()) -> password().
        create_account(Name) ->
            gen_server:call(?MODULE, {new_account, Name}).


*   _Delete an account_
  
    You can delete an account by giving its password. If the password doesn't
    correspond to a registered user, the server will reply `not_a_client`.
    Beware that if you still have some rented movies at home, the server won't
    let you delete your account. Instead, it will remind you to return the
    movies you owe to the dvd-club.

        :::erlang
        -spec delete_account(password()) ->
               'not_a_client' | 'account_deleted' | 'return_movies_first'.
        delete_account(Pass) ->
            gen_server:call(?MODULE, {delete_account, Pass}).


*   _Rent a dvd_
  
     When you ask for a movie, the server will check if there is a copy
     available at the moment and return the list of movies that you have
     currently rented (it's a polite way to remind you that you have to return
     them). If the movie you asked for is available, then it will be added to
     the list. Finally, in case your password is invalid, the server will not
     give you any movie.

        :::erlang
        -spec rent_dvd(password(), movie()) -> [movie()] | 'not_a_client'.
        rent_dvd(Pass, Movie) ->
            gen_server:call(?MODULE, {rent, Pass, Movie}).


*   _Return a dvd_

    You can return a dvd by giving your passord, so that the movie will be
    deleted from your account. The server will reply with the list of movies
    you still have to return. Again, if the password is invalid, the server
    won't be co-operative and won't accept the movie you are trying to return.

        :::erlang
        -spec return_dvd(password(), movie()) -> [movie()] | 'not_a_client'.
        return_dvd(Pass, Movie) ->
            gen_server:call(?MODULE, {return, Pass, Movie}).


*   _Ask for pop-corn_
  
    Everybody can buy pop-corn at the dvd-club.
    
        :::erlang
        -spec ask_for_popcorn() -> 'bon_appetit'.
        ask_for_popcorn() ->
            gen_server:call(?MODULE, popcorn).


But how do we know the server actually behaves as described above? Here are
some things we might want to test:

* no duplicate passwords are given to users
* when asking to rent a movie, the server will let you have it, unless it is
  not available or you are not registered
* when renting or returning a movie, a list of all the movies you owe will be
  returned by the server
* you can ask for pop-corn and the server will always serve you some


The PropEr approach
-------------------

**The problem:** Our server has some internal state based on previous
operations (e.g. user accounts, movies available, next password to be
allocated) and this state is not directly accessible, at least not from the
API.

**The solution:** Even if not observable, the state is there! And this means we
can try to compute it, even without knowing the details of the implementation.
To this end, we build an abstract state machine that models the behaviour
of the server. The state of the abstract state machine, also referred as the
_model state_, corresponds to the implicit internal state of the SUT. Now, all
we have to check is whether these states ever become inconsistent. Such an
inconsistency can be detected from the results of API calls. When this occurs,
there is much probability that the error lies in our model and not the
implemetation. But then, it is quite easy to redefine our model and run the
test again. In this way, well hidden bugs can be revealed, because after
e.g. 3000 random tests, PropEr will eventually try sequences of calls that
would not have been tested by traditional unit testing methods.

In a nutshell, the concept of testing stateful operations with PropEr is the
following:

* Write the code for the system under test (SUT).
* Write a callback module describing the expected behaviour of the SUT. The
  expected behaviour is specified via an abstract state machine that models
  the operations in the SUT, treating it as a blackbox accessible only from
  the API.
* Run a property which tests whether the real observed results of each API
  call match the predicted model state.

The property that we will use to test the movie server is:

    :::erlang
    prop_movies() ->
        ?FORALL(Cmds, commands(?MODULE),
            ?TRAPEXIT(
                begin 
                    start_link(),
                    {H,S,Res} = run_commands(?MODULE, Cmds),
                    stop(),
                    ?WHENFAIL(
                        io:format("History: ~w\nState: ~w\nRes: ~w\n",
                                  [H,S,Res]),
                        aggregate(command_names(Cmds), Res =:= ok))
                end)).


The actions described in this property are:

* Generate a random list of symbolic commands, i.e. symbolic API calls. As we
  can see, `commands/1` generator takes as argument a module name.
  In this module we should describe everything PropEr needs to know about the
  SUT. Don't worry, usually it's not that much, only the information provided
  by the API.
* Execute the list of commands, collecting the results of execution. This is
  done by `run_commands/2`, a function that evaluates a symbolic command
  sequence according to an abstract state machine specification.
  The function takes as arguments a module name (defining a model of the SUT)
  and the symbolic command sequence to be evaluated and returns a triple, which
  contains information about command execution.
* Each test should be self-contained. For this reason, almost every property
  for testing stateful systems contains some set-up and/or clean-up code,
  necessary to put the system in a known state, so that the next test can be
  executed independently from previous ones. In our case, set-up means
  spawning and linking to the server, while clean-up means stopping it.
* Enclose the property in `?TRAPEXIT`, so that PropEr won't crash if a linked
  process (i.e. our server) dies abnormally.
* In case of failure, report debugging information using `?WHENFAIL`. This is
  very useful for identifying the cause of failure.
* In case of success, collect statistics about command execution. This is
  useful to ensure that each command was tested as often as expected.

In order to get an idea of what testcases look like, you can try:

     :::erl
     3> proper_gen:pick(proper_statem:commands(movie_server)).
     {ok,[{set,{var,1},{call,movie_server,create_account,[ben]}},
          {set,{var,2},{call,movie_server,rent_dvd,[{var,1},toy_story]}},
          {set,{var,3},{call,movie_server,rent_dvd,[{var,1},the_lion_king]}},
          {set,{var,4},{call,movie_server,rent_dvd,[{var,1},despicable_me]}},
          {set,{var,5},{call,movie_server,rent_dvd,[{var,1},mary_poppins]}},
          {set,{var,6},{call,movie_server,return_dvd,[{var,1},the_lion_king]}},
          {set,{var,7},{call,movie_server,create_account,[alice]}},
          {set,{var,8},{call,movie_server,ask_for_popcorn,[]}}]}


The story here is quite simple. Ben creates an account at the dvd-club and
then decides to rent some movies. How do we know it's Ben who wants to
rent the movies? That's because we bind the result of each symbolic call to a
symbolic variable. Therefore, `{var,1}` is Ben's password and, in this way,
it can be used in subsequent commands/requests. After watching the Lion King,
Ben returns it to the dvd club. Then, Alice creates an account, someone
comes in to buy pop-corn and life goes on... As we cannot test every possible
scenario, we let PropEr make some random selections and test these instead.


Defining the abstract state machine
-----------------------------------

Now that you have a rough idea of what will be tested, it's time to implement
the abstract state machine of the movie server.

### Command generation ###
Since our testcases are symbolic call sequences, we definitely need a symbolic
call generator, i.e. a function that will be called to produce the next call
to be included in the testcase. In the general case, the generator should take
into account the model state. In our example, we are not sure yet if this is
necessary. Let us make a first attempt. What about...?

    :::erlang
    command(_S) ->
        oneof([{call,?MODULE,create_account,[name()]},
               {call,?MODULE,delete_account,[password()]},
               {call,?MODULE,rent_dvd,[password(), movie()]},
               {call,?MODULE,return_dvd,[password(), movie()]},
               {call,?MODULE,ask_for_popcorn,[]}]).


where `name()` generates a random name for a new client at the dvd club,
`movie()` generates a random movie name and `password()` a random password.

Although it is fairly easy to generate random names, movies and passwords
for each new symbolic call, it is more appropriate to select them from a rather
small predefined set, so that a testcase is likely to refer to the same objects
several times. Then, we have a bigger chance of detecting errors. So, we
introduce the following macros:

    :::erlang
    %% The module implementing the movie_server
    -define(SERVER, movie_server).

    %% people visiting the dvd-club
    -define(NAMES, [bob, alice, john, mary, ben]).

    %% a property list of the available movies,
    %% each pair in the list consists of a movie name and the number of
    %% existing copies of this movie
    -define(AVAILABLE_MOVIES, ?SERVER:available_movies()).

    %% movies that clients will ask to rent in the testcases
    %% apart from the movies available, clients will also ask for titanic
    %% and inception, in order to test how the server responds to these
    %% requests
    -define(MOVIE_TITLES,
            proplists:get_keys(?AVAILABLE_MOVIES) ++ [titanic, inception]).


The generators for names and movies become:
        
    :::erlang
    name() -> elements(?NAMES).

    movie() -> elements(?MOVIE_TITLES).


But what about passwords? Password allocation is on the server side, and we
have little information about it, since we decided to test it as a blackbox.
We could make an exception and take a look at the algorithm for password
generation, but in this way we would be copying and not actually testing how
passwords are created.

We decide to take a different approach: since passwords are available to the
users as the result of creating a new account (i.e. `create_count/1` calls),
they should be treated and tested exactly as such. In order to achieve this, we
will use the state of the abstract state machine. This is defined as follows:

    :::erlang
    -record(state, {users  :: [password()],
                    rented :: [{password(),movie()}]}).


The first component is a list of passwords generated from `create_account/1`
calls in the current test, while the second is a list keeping track of rented
movies. Given this state, we can arrange for the password generator to choose
one of the passwords already created in the same testcase (or possibly define
the frequency that invalid passwords may appear in our tests, depending on what
we are interested in testing).

Being interested only in valid passwords, we define:

    :::erlang
    password(#model_state{users = Passwords}) ->
        elements(Passwords).


An issue with this generator is that it will raise an exception if no passwords
have been previously generated in the same testcase. An elegant way to
introduce conditional command generation is the use of a list comprehension,
but in a rather unusual way:

    :::erlang
    command(S) ->
        oneof([{call, ?SERVER, create_account, [name()]}	,
               {call, ?SERVER, ask_for_popcorn, []}] ++
              [{call, ?SERVER, delete_account, [password(S)]}
               || S#state.users =/= []] ++
              [{call, ?SERVER, rent_dvd, [password(S), movie()]}
               || S#state.users =/= []] ++
              [{call, ?SERVER, return_dvd, [password(S), movie()]}
               || S#state.users =/= []]).

   
So, that's it! Our generator is ready. In the next section we will talk in more
detail about the model state, which was so useful for password generation.


### Updating the model state ###

The model state is initialized via the callback function `initial_state/0`.

    :::erlang    
    initial_state() ->
        #model_state{users  = [],
                     rented = []}.


Since each command might change the state, we need a way to keep track of these
changes. This is the role of the `next_state/3` callback function. It takes as
arguments the current state of the model, the symbolic call that was performed
and the result of this call, since the new state might depend on this. In fact,
this is the case for `create_account/1`, since we need to add the result
(i.e. the newly created password) to the list of registered users.

    :::erlang
    next_state(S, V, {call,_,create_account,[_Name]}) ->
        S#state{users = [V|S#state.users]};


A point to always keep in mind is that the actual results of the API calls are
not known during command generation. They are bound to symbolic variables.
Because of this, any information has to be extracted in a symbolic way, i.e. by
performing a symbolic call. For example, suppose that the result of
`create_account/1` was a non-empty list of passwords and that we wanted to
use the first password of the list. An attempt to extract it using `hd/1` would
fail. Instead, `next_state/3` should return:

    :::erlang
    S#state{users = [{call,erlang,hd,[V]}|S#state.users]}
  
The state transitions for the other calls make the expected changes to the
server's state. Since the password generator that we defined produces only
valid passwords, we expect the server to respond to our requests and not ignore
us with a `not_a_client` answer.

When a client deletes an account, the account's password should be erased from
the list of users.

    :::erlang
    next_state(S, _V, {call,_,delete_account,[Pass]}) ->
        S#state{users = lists:delete(Pass, S#state.users)};


When a client asks to rent a movie, the server will check the availability. If
there is an available copy, the server should allocate it to the user who asked
for it and mark it as rented. If there are no copies of that movie available
at that moment, the state shouldn't change. The function `is_available/2`
checks the availability of a movie based on the current model state and on the
list of initially available movies `?AVAILABLE_MOVIES`. 

    :::erlang
    next_state(S, _V, {call,_,rent_dvd,[Pass,Movie]}) ->
        case is_available(Movie, S) of
            true  ->
                S#state{rented = [{Pass,Movie}|S#state.rented]};
            false ->
                S
        end;


In a similar way, when a user returns a movie, the server should delete it from
the user's account and mark it as available again.
        
    :::erlang       
    next_state(S, _V, {call, _, return_dvd, [Pass,Movie]}) ->
        S#state{rented = lists:delete({Pass,Movie}, S#state.rented)};


Finally, buying pop-corn does not change the state.

    :::erlang
    next_state(S, _V, {call,_,ask_for_popcorn,[]}) -> S.


Note that `V` (i.e. the result of the call) can be either symbolic or dynamic.
Thus, while it is perfectly allowable to include it in the next state and/or
perform symbolic calls on it, actions like pattern matching have to be avoided.


### Specifying pre- and post- conditions ###


Preconditions are the PropEr way to impose constraints on valid command
sequences, since they are always checked before a command is actually included
in the testcase. The callback function `precondition/2` takes as arguments the
current state and the symbolic call to be performed and returns a boolean. For
the moment, we do not impose any restriction on the way commands are chosen.

    :::erlang
    precondition(_, _) -> true.


On the other hand, postconditions are checked during execution time to ensure
that the system actually behaves as expected. At this point, the state
contains dynamic values, i.e. real values, not symbolic variables and calls.
The callback function `postcondition/3` takes as arguments the state **prior**
to command execution, the symbolic call that has been performed and its actual
result. It returns a boolean.

When creating an account, a _new_ password is always returned.

    :::erlang
    postcondition(S, {call,_,create_account,[_Name]}, Res) ->
        not lists:member(Res, S#state.users);


Since our testcases include only valid passwords, deleting an account
always succeeds. 

    :::erlang
    postcondition(_S, {call,_,delete_account,[_Pass]}, Res) ->
       Res =:= account_deleted;


When someone asks for a movie, then if it's available it's added to her list,
otherwise not.
         
    :::erlang
    postcondition(S, {call,_,rent_dvd,[_Pass,Movie]}, Res) ->
        case is_available(Movie, S) of
            true ->
                lists:member(Movie, Res);
            false ->
                not lists:member(Movie, Res)
        end;


When someone returns a dvd, then it's no longer in her list.

    :::erlang
    postcondition(_S, {call,_,return_dvd,[_Pass,Movie]}, Res) ->
        not lists:member(Movie, Res);


Every time someone buys popcorn, the server wishes bon appetit.

    :::erlang
    postcondition(_S, {call,_,ask_for_popcorn,[]}, Res) ->
        Res =:= bon_appetit.


PropEr in action
----------------

Having specified the abstract state machine, it's high time to test the
property:

    :::erl
    5> proper:quickcheck(movie_server:prop_movies()).
    ....................!
    Failed: After 21 test(s).
    A linked process died with reason
    {badarg,[{ets,lookup_element,[491536,inception,2]},
             <...3 more lines of stacktrace...]}.

    <...ERROR REPORT produced...>

    [{set,{var,1},{call,movie_server,create_account,[john]}},
     {set,{var,2},{call,movie_server,create_account,[mary]}},
     {set,{var,3},{call,movie_server,create_account,[alice]}},
     {set,{var,4},{call,movie_server,ask_for_popcorn,[]}},
     {set,{var,5},{call,movie_server,rent_dvd,[{var,1},despicable_me]}},
     {set,{var,6},{call,movie_server,return_dvd,[{var,3},finding_nemo]}},
     {set,{var,7},{call,movie_server,rent_dvd,[{var,2},despicable_me]}},
     {set,{var,8},{call,movie_server,return_dvd,[{var,1},inception]}},
     {set,{var,9},{call,movie_server,ask_for_popcorn,[]}},
     {set,{var,10},{call,movie_server,create_account,[ben]}}]

    Shrinking .....
    <...ERROR REPORTS produced...>
    (5 time(s))
    [{set,{var,1},{call,movie_server,create_account,[john]}},
     {set,{var,8},{call,movie_server,return_dvd,[{var,1},inception]}}]
    false


As we can see, creating an account and then returning the movie "Inception" is
enough to make the server to crash. Luckily PropEr didn't crash, since we have
enclosed the property in a `?TRAPEXIT` macro. But why did the server crash in
the first place? This happened because "Inception" was never available at the
dvd-club. In real life we can be certain that nobody will ever return a movie
that she didn't rent in advance. However, in a production system there
is always the possibility that an unlikely sequence of events might actually
happen. For this reason, it's not a good idea to let our server crash and we
shall fix the code later. For the moment, we are interested in discovering more
bugs. So, we add a precondition that doesn't let this known bug appear.

    :::erlang
    precondition(S, {call,_,return_dvd,[Pass,Movie]}) ->
        lists:member({Pass,Movie}, S#state.rented);

And run the property once more:

    :::erl
    8> proper:quickcheck(movie_statem:prop_movies()).
    .............................!
    Failed: After 30 test(s).
    [{set,{var,1},{call,movie_server,create_account,[bob]}},
     {set,{var,2},{call,movie_server,delete_account,[{var,1}]}},
     {set,{var,3},{call,movie_server,create_account,[bob]}},
     {set,{var,4},{call,movie_server,delete_account,[{var,3}]}},
     {set,{var,5},{call,movie_server,ask_for_popcorn,[]}},
     {set,{var,6},{call,movie_server,create_account,[mary]}},
     {set,{var,7},{call,movie_server,rent_dvd,[{var,6},the_lion_king]}},
     {set,{var,8},{call,movie_server,create_account,[mary]}},
     {set,{var,9},{call,movie_server,delete_account,[{var,6}]}}]
     History: [{{state,[],[]},1},{{state,[1],[]},account_deleted},
               {{state,[],[]},2},{{state,[2],[]},account_deleted},
               {{state,[],[]},bon_appetit},{{state,[],[]},3},
               {{state,[3],[]},[the_lion_king]},
               {{state,[3],[{3,the_lion_king}]},4},
               {{state,[4,3],[{3,the_lion_king}]},return_movies_first}]
     State: {state,[4],[{3,the_lion_king}]}
     Res: {postcondition,false}

     Shrinking ..(2 time(s))
     [{set,{var,6},{call,movie_server,create_account,[mary]}},
      {set,{var,7},{call,movie_server,rent_dvd,[{var,6},the_lion_king]}},
      {set,{var,9},{call,movie_server,delete_account,[{var,6}]}}]
     History: [{{state,[],[]},1},{{state,[1],[]},[the_lion_king]},
               {{state,[1],[{1,the_lion_king}]},return_movies_first}]
     State: {state,[],[{1,the_lion_king}]}
     Res: {postcondition,false}
     false


The scenario that leaded to a false postcondition is that someone tried to
delete an account while still owing movies to the dvd-club. The server replied
`return_movies_first`, but our model did not account for this. There are two
options to deal with this inconsistency:

1. Add a precondition that prevents users from deleting an account
   before returning all dvds.
2. Change our postcondition to account for these annoying clients.

Since this kind of users might exist in practice, we decide to change the
postcondition. Keep in mind that when changing postconditions, we might also
need to change the `next_state/3` callback.

    :::erlang
    next_state(S, _V, {call, _, delete_account, [Pass]}) ->
        case proplists:is_defined(Pass, S#state.rented) of
    	    false ->
    	        S#state{users = lists:delete(Pass, S#state.users)};
    	    true ->
    	        S
        end;

    postcondition(S, {call, _, delete_account, [Pass]}, Res) ->
        case proplists:is_defined(Pass, S#state.rented) of
    	    false ->
    	        Res =:= account_deleted;
    	    true ->
    	        Res =:= return_movies_first
        end;

Running the property:

        :::erl
    12> proper:quickcheck(movie_statem:prop_movies()).
    ............................................................................
    .........!
    Failed: After 86 test(s).
    <...testcase of 34 commands...>
    History: <...long history of command execution...>
    State: {state,[1],[{1,peter_pan},{1,despicable_me},{1,finding_nemo}]}
    Res: {postcondition,false}

    Shrinking .........(9 time(s))
    [{set,{var,1},{call,movie_server,create_account,[alice]}},
     {set,{var,15},{call,movie_server,delete_account,[{var,1}]}},
     {set,{var,16},{call,movie_server,rent_dvd,[{var,1},peter_pan]}}]
    History: [{{state,[],[]},1},{{state,[1],[]},account_deleted},
              {{state,[],[]},not_a_client}]
    State: {state,[],[{1,peter_pan}]}
    Res: {postcondition,{exception,error,badarg,
                         [{lists,member,[peter_pan,not_a_client]},
                          {movie_statem,postcondition,3},
                          <...6 more lines of stacktrace...>]}}
    false


The counterexample produced contains a `rent_dvd/2` request by a user that had
already deleted his account. This shouldn't normally happen since our password
generator returns only valid passwords (i.e. elements of the model state).
Shrinking doesn't seem to work very well in this case, but this is not a PropEr
bug. Something is missing from our specification. But what is that?

When generating commands we can ensure valid passwords are used because the
password generator chooses only elements of the model state. But while
shrinking we usually attempt to perform a call with the system being in a state
different from the state it was when initially running the test. The most
straightforward way to impose constraints on shrunk command sequences is to
specify these constraints via the `precondition/2` callback, because the
shrinking mechanism tries to discard commands that do not contribute to
failure while ensuring that all preconditions still hold. Thus, we add the
following preconditions:

    :::erlang
    precondition(S, {call, _, return_dvd, [Pass,Movie]}) ->
        lists:member({Pass,Movie}, S#state.rented);
    precondition(S, {call, _, rent_dvd, [Pass,_Movie]}) ->
        lists:member(Pass, S#state.users);
    precondition(S, {call, _, delete_account, [Pass]}) ->
        lists:member(Pass, S#state.users);
    precondition(_, _) ->
        true.

And test the property again:

    :::erl
    15> proper:quickcheck(movie_statem:prop_movies()).
    ...................!
    Failed: After 20 test(s).
    [{set,{var,1},{call,movie_server,ask_for_popcorn,[]}},
     {set,{var,2},{call,movie_server,create_account,[mary]}},
     {set,{var,3},{call,movie_server,rent_dvd,[{var,2},peter_pan]}},
     {set,{var,4},{call,movie_server,rent_dvd,[{var,2},peter_pan]}},
     {set,{var,5},{call,movie_server,create_account,[mary]}},
     {set,{var,6},{call,movie_server,return_dvd,[{var,2},peter_pan]}},
     {set,{var,7},{call,movie_server,create_account,[alice]}},
     {set,{var,8},{call,movie_server,rent_dvd,[{var,7},inception]}},
     {set,{var,9},{call,movie_server,rent_dvd,[{var,7},titanic]}},
     {set,{var,10},{call,movie_server,delete_account,[{var,2}]}}]
    History: [{{state,[],[]},bon_appetit},{{state,[],[]},1},
              {{state,[1],[]},[peter_pan]},
              {{state,[1],[{1,peter_pan}]},[peter_pan]}]
    State: {state,[1],[{1,peter_pan}]}
    Res: {postcondition,false}

    Shrinking ...(3 time(s))
    [{set,{var,2},{call,movie_server,create_account,[mary]}},
     {set,{var,3},{call,movie_server,rent_dvd,[{var,2},peter_pan]}},
     {set,{var,4},{call,movie_server,rent_dvd,[{var,2},peter_pan]}}]
    History: [{{state,[],[]},1},{{state,[1],[]},[peter_pan]},
              {{state,[1],[{1,peter_pan}]},[peter_pan]}]
    State: {state,[1],[{1,peter_pan}]}
    Res: {postcondition,false}
    false


We have just detected another subtle error in our model. It is invoked in the
following case:

* Mary creates an account.
* Mary rents a copy of the movie Peter Pan.
* Mary wants to rent another copy of the same movie, but there are no
  more copies available.

In this case the postcondition for `rent_dvd/2` is false. Again, it is up to us
to redefine our model and we decide to add a precondition that prevents the
aforementioned scenario.

    :::erlang
    precondition(S, {call,_,rent_dvd,[Pass,_Movie]}) ->
        not lists:member({Pass,_Movie}, S#state.rented) andalso
            lists:member(Pass, S#state.users);

Eventually:

    :::erl
    18>  proper:quickcheck(movie_statem:prop_movies()).
    ...........................................................................
    ..........................
    OK: Passed 100 test(s).

    30% {movie_server,ask_for_popcorn,0}
    30% {movie_server,create_account,1}
    19% {movie_server,delete_account,1}
    18% {movie_server,rent_dvd,2}
     1% {movie_server,return_dvd,2}
    true


Now that tests seem to pass, we will temporarily remove the first precondition
that we introduced and correct our code to prevent the server from crashing.
The code handling `return_dvd` requests is the following:

    :::erlang
    handle_call({return,Pass,Movie}, _From, S) ->
        #state{users = Users, movies = Movies} = S,
        Reply = case ets:lookup(Users, Pass) of
                    []  ->
                        not_a_client;
                    [{_,_,Rented}] ->
                        NewRented = lists:delete(Movie, Rented),
                        ets:update_element(Users, Pass, {3,NewRented}),
                        N = ets:lookup_element(Movies, Movie, 2),
                        ets:update_element(Movies, Movie, {2,N+1}),
                        NewRented
                end,
        {reply, Reply, S};


The command `ets:lookup_element(Movies, Movie, 2)` makes the server crash with
the following counterexample:

    :::erl
    [{set,{var,1},{call,movie_server,create_account,[john]}},
     {set,{var,8},{call,movie_server,return_dvd,[{var,1},inception]}}]
    false


Let us rewrite:

    :::erlang
    handle_call({return,Pass,Movie}, _From, S) ->
        #state{users = Users, movies = Movies} = S,
        Reply =
            case ets:lookup(Users, Pass) of
                []  ->
                    not_a_client;
                [{_,_,Rented}] ->
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

And check that the counterexample now passes the test:

    :::erl 
    41> proper:check(movie_server:prop_movies(), proper:counterexample()).
    OK: The input passed the test.
    true


Now we are confident we fixed this bug. But while inspecting the reported
testcase distribution, we can easily notice that `return_dvd/2` calls are
rarely tested. This happens because of the precondition that allows to return
only movies you have previously rented. To remedy the situation, we will modify
the command generator so that `return_dvd/2` calls can be more frequently
selected. 

    :::erlang
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

The resulting distribution is:

    :::erl
    42> proper:quickcheck(movie_server:prop_movies(), 3000).
    <...3000 dots....>
    OK: Passed 3000 test(s).

    36% {movie_server,rent_dvd,2}
    19% {movie_server,return_dvd,2}
    18% {movie_server,ask_for_popcorn,0}
    17% {movie_server,create_account,1}
     8% {movie_server,delete_account,1}
    true

<!-- kate: replace-tabs-save on; replace-tabs on; tab-width 8; -->
