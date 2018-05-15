---
layout: page
title: A PropEr statem tutorial
sitemap:
    priority: 1.0
---

Testing purely functional code is usually not enough for industrial Erlang
applications like telecom software or http servers. In this tutorial, we will
describe how to use PropEr for automated random testing of stateful systems.

Undoubtedly, automated random testing of a stateful API would require some
magic, unless some additional information is provided. Since we are not
magicians but Erlang developers, we choose to describe the side-effects of
the system under test via an abstract state machine and then, let PropEr do
its magic for us.


## A server example

Let us first introduce our example: a movie server at a dvd-club, implemented
as an Erlang generic server (using the `gen_server` behaviour). The complete
code of the server can be found [here](/code/movies/movie_server.erl).
Nevertheless, for the most part of this tutorial we only need to understand
the server's API. We describe it below:

*   _Create a new account_

    We just say our name (e.g., 'Bond', 'James Bond'), and a new account is
    created for us. The server will return us a password for the new account.
    This password can be used for all future requests.

      {% highlight erlang %}
      -spec create_account(name()) -> password().
      create_account(Name) ->
          gen_server:call(?MODULE, {new_account, Name}).
      {% endhighlight %}


*   _Delete an account_

    We can delete an account by giving its password. If the password doesn't
    correspond to a registered user, the server will reply `not_a_client`.
    Beware that if we still have some rented movies at home, the server won't
    let us delete our account. Instead, it will remind us to return the
    movies we owe to the dvd-club.

      {% highlight erlang %}
      -spec delete_account(password()) ->
              'not_a_client' | 'account_deleted' | 'return_movies_first'.
      delete_account(Password) ->
          gen_server:call(?MODULE, {delete_account, Password}).
      {% endhighlight %}

*   _Rent a dvd_

     When we ask for a movie, the server will check if there is a copy
     available at the moment and return the list of movies that we have
     currently rented. (It's a polite way of reminding us that we should
     have returned them.) If the movie we asked for is available, then it
     will be added to the list. Finally, in case the password we supplied
     is invalid, the server will not give us any movies.

      {% highlight erlang %}
      -spec rent_dvd(password(), movie()) -> [movie()] | 'not_a_client'.
      rent_dvd(Password, Movie) ->
          gen_server:call(?MODULE, {rent, Password, Movie}).
      {% endhighlight %}

*   _Return a dvd_

    We can return a dvd by giving our password, so that the movie will be
    deleted from our account. The server will reply with the list of movies
    we still have to return. Again, if the password is invalid, the server
    will not accept the movie we are trying to return.

      {% highlight erlang %}
      -spec return_dvd(password(), movie()) -> [movie()] | 'not_a_client'.
      return_dvd(Password, Movie) ->
          gen_server:call(?MODULE, {return, Password, Movie}).
      {% endhighlight %}

*   _And some pop-corn, please_

    Everybody can buy pop-corn at the dvd-club.

      {% highlight erlang %}
      -spec ask_for_popcorn() -> 'bon_appetit'.
      ask_for_popcorn() ->
          gen_server:call(?MODULE, popcorn).
      {% endhighlight %}

But how do we know the server actually behaves as described above? Here are
some things we might want to test:

* no duplicate passwords are given to users
* when asking to rent a movie, the server will let us have it, unless it is
  not available or we are not registered
* when renting or returning a movie, a list of all the movies we owe is
  returned by the server
* and, most importantly, when we ask for pop-corn the server will always
  give us some!


## The PropEr approach to testing the server

**The problem:** Our server has some internal state based on previous
operations (e.g. user accounts which have been created, the set of available
movies, next password to be allocated, etc.) and this state is not directly
accessible, at least not from the API.

**The solution:** Even if not observable, the state is there! And this means we
can try to compute it, even without knowing the details of the implementation.
To this end, we build an abstract state machine that models the behaviour
of the server. The state of the abstract state machine, also referred as the
_model state_, corresponds to the implicit internal state of the system under
test. Now, all we have to check is whether these states ever become
inconsistent. Such an inconsistency can be detected from the results of API
calls. When this occurs, there is much probability that the error lies in our
model and not in the implementation. But then, it is quite easy to redefine our
model and run the test again. In this way, well hidden bugs can be revealed,
because after e.g. 3000 random tests, PropEr will eventually try sequences of
calls that would not have been tested by traditional unit testing methods.

In a nutshell, the concept of testing stateful operations with PropEr is the
following:

* Write the code for the system under test (SUT).
* Write a callback module describing the expected behaviour of the SUT. The
  expected behaviour is specified via an abstract state machine that models
  the operations in the SUT, treating it as a blackbox accessible only from
  the API.
* Write a property that tests whether the real observed results of each API
  call match the predicted model state.
* Run this property in PropEr.

### Testcases for stateful systems

Testcases generated for testing a stateful system are lists of _symbolic_ API
calls to that system. At this point, one may wonder why we choose to complicate
things, when we could simply perform calls to the system under test. As it
turns out, symbolic representation makes things much easier. Here are some
reasons to prefer it, listed in increasing order of importance:

* Generated testcases are easier to read and understand.
* Failing testcases are easier to shrink.
* The generation phase is side-effect free and this results in
  repeatable testcases, which is essential for correct shrinking.

Symbolic calls are not executed during testcase generation. Therefore, the
actual result of a call is not known at generation time and cannot be used in
subsequent operations. To remedy this situation, symbolic variables are used.
A _command_ is a symbolic term, used to bind a symbolic variable to the result
of a symbolic call. For example, if as we will see below, **`?SERVER`** denotes
the name of the server module, the command

{% highlight erlang %}
{set, {var,1}, {call,?SERVER,create_account,[james_bond]}}
{% endhighlight %}

binds Mr Bond's password to the symbolic variable `{var,1}`.

### Properties for stateful systems

Each test consists of two phases:

* As a first step, PropEr generates random symbolic command sequences
  deriving information from the callback module implementing the abstract state
  machine. This is the role of `commands/1` generator, which takes as argument
  the name of the callback module and returns a random command sequence.

* As a second step, command sequences are executed so as to check that the
  system behaves as expected. This is the role of `run_commands/2`, a function
  that evaluates a symbolic command sequence according to an abstract state
  machine specification. It takes as arguments the name of the callback module,
  where the state machine is specified, and the command sequence to be
  evaluated and returns a triple that contains information about the execution
  of the command.

These two phases are encapsulated in the following property, which can be used
for testing a generic server with PropEr:

{% highlight erlang %}
prop_server_works_fine() ->
    ?FORALL(Cmds, commands(?MODULE),
            begin
                ?SERVER:start_link(),
                {_,_,Result} = run_commands(?MODULE, Cmds),
                ?SERVER:stop(),
                Result =:= ok
            end).
{% endhighlight %}

As we can see, even in this simple property, it is very important to keep
each test self-contained. For this reason, almost every property for testing
stateful systems contains some set-up and/or clean-up code. Such code is
necessary to put the system in a known state, so that the next test can be
executed independently from previous ones. In this property, set-up means
spawning and linking to the server, while clean-up means stopping it.


## Defining the abstract state machine

### Command generation ###

The first callback function to consider is the command generator.
This function will be repeatedly called to produce the next symbolic call to
be included in the testcase. PropEr automatically binds the result of generated
symbolic calls to symbolic variables. In other words, symbolic calls are
automatically translated to commands.
In the general case, the command generator should take into account the model
state. In our example, we are not sure yet if this is necessary. Let us make a
first attempt. What about...?

{% highlight erlang %}
command(_S) ->
    oneof([{call,?MODULE,create_account,[name()]},
           {call,?MODULE,delete_account,[password()]},
           {call,?MODULE,rent_dvd,[password(), movie()]},
           {call,?MODULE,return_dvd,[password(), movie()]},
           {call,?MODULE,ask_for_popcorn,[]}]).
{% endhighlight %}

where `name()` generates a random name for a new client at the dvd club,
`movie()` generates a random movie name and `password()` a random password.

Although it is fairly easy to use the type definitions of `name()`, `movie()`,
and `password()` to generate random names, movies and passwords for each new
symbolic call, it is more appropriate to select them from a rather small
predefined set, so that a testcase is likely to refer to the same objects
several times. Then, we will have a bigger chance of detecting errors.
So, rather than using the type definitions, we introduce the following macros:

{% highlight erlang %}
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
{% endhighlight %}

Given these macros, the generators for names and movies are written using the
`elements/1` function of PropEr:

{% highlight erlang %}
name() -> elements(?NAMES).

movie() -> elements(?MOVIE_TITLES).
{% endhighlight %}

But what about passwords? Password allocation is on the server side and we
have little information about it, since we decided to test it as a blackbox.
We could make an exception and take a look at the algorithm for password
generation, but in this way we would be copying and not actually testing how
passwords are created.

We can take a different approach: since passwords are available to the
users as the result of creating a new account (i.e. `create_acount/1` calls),
they should be treated and tested exactly as such. In order to achieve this, we
will use the state of the abstract state machine. This is defined as follows:

{% highlight erlang %}
-record(state, {users  :: [password()],
                rented :: [{password(),movie()}]}).
{% endhighlight %}

The first component is a list of passwords generated from `create_account/1`
calls in the current test, while the second is a list keeping track of rented
movies. Given this state, we can arrange for the password generator to choose
one of the passwords already created in the same testcase. (Alternatively, we
can possibly define the frequency that invalid passwords may appear in our
tests. The choice depends on what we are interested in testing.)

Being interested only in valid passwords, we define:

{% highlight erlang %}
password(#state{users = Passwords}) ->
            elements(Passwords).
{% endhighlight %}

An issue with this generator is that it will raise an exception if no passwords
have been previously generated in the same testcase. An elegant way to
introduce conditional command generation is the use of a list comprehension,
but in a rather unusual way:

{% highlight erlang %}
command(S) ->
    Users = (S#state.users =/= []),
    oneof([{call, ?SERVER, create_account, [name()]},
           {call, ?SERVER, ask_for_popcorn, []}] ++
          [{call, ?SERVER, delete_account, [password(S)]} || Users] ++
          [{call, ?SERVER, rent_dvd, [password(S), movie()]} || Users] ++
          [{call, ?SERVER, return_dvd, [password(S), movie()]} || Users]).
{% endhighlight %}

So, that's it! Our command generator is ready. In the next section we will talk
in more detail about the model state, which was so useful for password
generation.


### Updating the model state

The model state is initialized via the callback function `initial_state/0`.

{% highlight erlang %}
initial_state() ->
    #state{users  = [],
           rented = []}.
{% endhighlight %}

Since each command might change the state, we need a way to keep track of these
changes. This is the role of the `next_state/3` callback function. It takes as
arguments the current state of the model, the symbolic call that was performed
and the result of this call, since the new state might depend on this. In fact,
this is the case for `create_account/1`, since we need to add the result
(i.e. the newly created password) to the list of registered users.

{% highlight erlang %}
next_state(S, V, {call,_,create_account,[_Name]}) ->
    S#state{users = [V|S#state.users]};
{% endhighlight %}

A point to always keep in mind is that the actual results of the API calls are
not known during command generation. They are bound to symbolic variables.
Because of this, pattern matching on the result `V` of the call should be
avoided. Moreover, any information has to be extracted in a symbolic way, i.e.
by performing a symbolic call. For example, suppose that the result of
`create_account/1` was a non-empty list of passwords and that we wanted to
use the first password of the list. An attempt to extract it using `hd/1` would
raise an exception. Instead, `next_state/3` should return:

{% highlight erlang %}
S#state{users = [{call,erlang,hd,[V]}|S#state.users]}
{% endhighlight %}

The state transitions for the other calls are less tricky to define. Since the
password generator that we use produces only valid passwords, we expect the
server to respond to all requests and not ignore them with a `not_a_client`
reply.

When a client deletes an account, the corresponding password should be erased
from the list of users.

{% highlight erlang %}
next_state(S, _V, {call,_,delete_account,[Password]}) ->
    S#state{users = lists:delete(Password, S#state.users)};
{% endhighlight %}

When a client asks to rent a movie, the server will check its availability. If
there is an available copy, the server should allocate it to the user who asked
for it and mark it as rented. If there are no copies of that movie available
at that moment, the state shouldn't change. The function `is_available/2`
checks the availability of a movie based on the current model state and on the
list of initially available movies `?AVAILABLE_MOVIES`.

{% highlight erlang %}
next_state(S, _V, {call,_,rent_dvd,[Password,Movie]}) ->
    case is_available(Movie, S) of
        true  ->
            S#state{rented = [{Password,Movie}|S#state.rented]};
        false ->
            S
    end;
{% endhighlight %}

In a similar way, when a user returns a movie, the server should delete it from
the user's account and mark it as available again.

{% highlight erlang %}
next_state(S, _V, {call,_,return_dvd,[Password,Movie]}) ->
    S#state{rented = lists:delete({Password,Movie}, S#state.rented)};
{% endhighlight %}

Finally, buying pop-corn does not change the state.

{% highlight erlang %}
next_state(S, _V, {call,_,ask_for_popcorn,[]}) -> S.
{% endhighlight %}

### Specifying pre- and post- conditions

Preconditions are the PropEr way to impose constraints on valid command
sequences, since they are always checked before a command is actually included
in the testcase. The callback function `precondition/2` takes as arguments the
current state and the symbolic call to be performed and returns a boolean. For
the moment, we do not impose any restriction on the way commands are chosen.

{% highlight erlang %}
precondition(_, _) -> true.
{% endhighlight %}

On the other hand, postconditions are checked during execution time to ensure
that the system actually behaves as expected. At this point, the state
contains dynamic values, i.e. real values, not symbolic variables and calls.
The callback function `postcondition/3` takes as arguments the state **prior**
to command execution, the symbolic call that has been performed and its actual
result. It returns a boolean.

When creating an account, a _new_ password is always returned.

{% highlight erlang %}
postcondition(S, {call,_,create_account,[_Name]}, Result) ->
    not lists:member(Result, S#state.users);
{% endhighlight %}

Since our testcases include only valid passwords, deleting an account
always succeeds.

{% highlight erlang %}
postcondition(_S, {call,_,delete_account,[_Password]}, Result) ->
    Result =:= account_deleted;
{% endhighlight %}

When someone asks for a movie, then if it's available it's added to her list,
otherwise not.

{% highlight erlang %}
postcondition(S, {call,_,rent_dvd,[_Password,Movie]}, Result) ->
    case is_available(Movie, S) of
        true ->
            lists:member(Movie, Result);
        false ->
            not lists:member(Movie, Result)
    end;
{% endhighlight %}

When someone returns a dvd, then it's no longer in her list.

{% highlight erlang %}
postcondition(_S, {call,_,return_dvd,[_Password,Movie]}, Result) ->
    not lists:member(Movie, Result);
{% endhighlight %}

Every time someone buys popcorn, the server wishes them bon appetit.

{% highlight erlang %}
postcondition(_S, {call,_,ask_for_popcorn,[]}, Result) ->
    Result =:= bon_appetit.
{% endhighlight %}

# PropEr in action

Having specified the abstract state machine, it's high time to test the
property:

{% highlight plaintext %}
2> proper:quickcheck(movie_statem:prop_server_works_fine()).
....
=ERROR REPORT==== 29-May-2011::22:28:16 ===
** Generic server movie_server terminating
** Last message in was {return,1,inception}
** When Server state == {state,53265,49168,2}
** Reason for termination ==
** {badarg,[{ets,lookup_element,[49168,inception,2]},
            {movie_server,handle_call,3},
            {gen_server,handle_msg,5},
            {proc_lib,init_p_do_apply,3}]}
** exception exit: badarg
     in function  ets:lookup_element/3
        called as ets:lookup_element(49168,inception,2)
     in call from movie_server:handle_call/3
     in call from gen_server:handle_msg/5
     in call from proc_lib:init_p_do_apply/3
{% endhighlight %}

Oh dear! This is not quite what we expected... The movie server crashed.
That the server crashes is expected at this point, since we are looking for
bugs in its code. What is not acceptable is that PropEr also crashes without
providing any kind of useful information about the cause of the failure.
This is improper behaviour!

In cases like this, the `?TRAPEXIT` macro comes to the rescue.
Enclosing a property in `?TRAPEXIT` prevents PropEr from crashing when
a linked process dies abnormally. Now that we know this trick, we can
revise the property:

{% highlight erlang %}
prop_server_works_fine() ->
    ?FORALL(Cmds, commands(?MODULE),
            ?TRAPEXIT(
                begin
                    ?SERVER:start_link(),
                    {_,_,Result} = run_commands(?MODULE, Cmds),
                    ?SERVER:stop(),
                    Result =:= ok
                end)).
{% endhighlight %}

Let's try it again:

{% highlight plaintext %}
4> proper:quickcheck(movie_statem:prop_server_works_fine()).
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
{% endhighlight %}

As we can see, creating an account and then returning the movie `inception` is
enough to make the server crash. Luckily PropEr didn't crash this time, since
we enclosed the property in a `?TRAPEXIT` macro. But why did the server crash
in the first place? This happened because the `inception` movie was never
available at the dvd-club. In real life, we can be certain that nobody will
ever return a movie that they didn't rent in advance. However, in a production
system there is always the possibility that an unlikely sequence of events
might actually happen. For this reason, it's not a good idea to let our server
crash and we shall fix the code later. For the moment, we are interested in
discovering more bugs. So, we add a precondition that doesn't let this
situation happen.

{% highlight erlang %}
precondition(S, {call,_,return_dvd,[Password,Movie]}) ->
    lists:member({Password,Movie}, S#state.rented);
{% endhighlight %}

And run the property once more:

{% highlight plaintext %}
6> proper:quickcheck(movie_statem:prop_server_works_fine()).
..............!
Failed: After 15 test(s).
[{set,{var,1},{call,movie_server,create_account,[mary]}},
 {set,{var,2},{call,movie_server,ask_for_popcorn,[]}},
 {set,{var,3},{call,movie_server,rent_dvd,[{var,1},despicable_me]}},
 {set,{var,4},{call,movie_server,rent_dvd,[{var,1},the_lion_king]}},
 {set,{var,5},{call,movie_server,rent_dvd,[{var,1},peter_pan]}},
 {set,{var,6},{call,movie_server,rent_dvd,[{var,1},titanic]}},
 {set,{var,7},{call,movie_server,ask_for_popcorn,[]}},
 {set,{var,8},{call,movie_server,delete_account,[{var,1}]}}]

Shrinking ....(4 time(s))
[{set,{var,1},{call,movie_server,create_account,[mary]}},
 {set,{var,5},{call,movie_server,rent_dvd,[{var,1},peter_pan]}},
 {set,{var,8},{call,movie_server,delete_account,[{var,1}]}}]
{% endhighlight %}

The property fails again and this time the minimal counterexample produced
contains three commands. Although we might suspect the cause of failure,
we would like to be more certain about it. Thus, we decide to add debugging
information to our property, using the `?WHENFAIL` macro. The second
argument of `?WHENFAIL` should be a boolean clause. In case it evaluates to
false, the `Action` specified as the first argument will be executed.

{% highlight erlang %}
prop_server_works_fine() ->
    ?FORALL(Cmds, commands(?MODULE),
            ?TRAPEXIT(
                begin
                    ?SERVER:start_link(),
                    {History,State,Result} = run_commands(?MODULE, Cmds),
                    ?SERVER:stop(),
                    ?WHENFAIL(io:format("History: ~w~nState: ~w\nResult: ~w~n",
                                        [History,State,Result]),
                              Result =:= ok)
                end)).
{% endhighlight %}

* _History_ contains the command execution history. For each command that was
  executed without raising an exception there is a tuple specifying the state
  prior to command execution and the actual result of the command.

* _State_ contains the state of the abstract state machine at the moment when
  execution stopped.

* Finally, _Result_ specifies the outcome of command execution. When it is
  the atom `ok`, it means that all commands were successfully run and all
  postconditions were true.

Running the test for the new property, we get more explicit information
about command execution and the cause of failure.

{% highlight plaintext %}
{% raw %}
8> proper:quickcheck(movie_statem:prop_server_works_fine()).
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
 Result: {postcondition,false}

Shrinking ..(2 time(s))
[{set,{var,6},{call,movie_server,create_account,[mary]}},
 {set,{var,7},{call,movie_server,rent_dvd,[{var,6},the_lion_king]}},
 {set,{var,9},{call,movie_server,delete_account,[{var,6}]}}]
History: [{{state,[],[]},1},{{state,[1],[]},[the_lion_king]},
          {{state,[1],[{1,the_lion_king}]},return_movies_first}]
State: {state,[],[{1,the_lion_king}]}
Result: {postcondition,false}
false
{% endraw %}
{% endhighlight %}

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

{% highlight erlang %}
next_state(S, _V, {call, _, delete_account, [Password]}) ->
    case proplists:is_defined(Password, S#state.rented) of
      false ->
          S#state{users = lists:delete(Password, S#state.users)};
      true ->
          S
    end;

postcondition(S, {call, _, delete_account, [Password]}, Result) ->
    case proplists:is_defined(Password, S#state.rented) of
      false ->
          Result =:= account_deleted;
      true ->
          Result =:= return_movies_first
    end;
{% endhighlight %}

And again we try:

{% highlight plaintext %}
{% raw %}
12> proper:quickcheck(movie_statem:prop_server_works_fine()).
............................................................................
.........!
Failed: After 86 test(s).
<...testcase of 34 commands...>
History: <...long history of command execution...>
State: {state,[1],[{1,peter_pan},{1,despicable_me},{1,finding_nemo}]}
Result: {postcondition,false}

Shrinking .........(9 time(s))
[{set,{var,1},{call,movie_server,create_account,[alice]}},
 {set,{var,15},{call,movie_server,delete_account,[{var,1}]}},
 {set,{var,16},{call,movie_server,rent_dvd,[{var,1},peter_pan]}}]
History: [{{state,[],[]},1},{{state,[1],[]},account_deleted},
          {{state,[],[]},not_a_client}]
State: {state,[],[{1,peter_pan}]}
Result: {postcondition,{exception,error,badarg,
                     [{lists,member,[peter_pan,not_a_client]},
                      {movie_statem,postcondition,3},
                      <...6 more lines of stacktrace...>]}}
false
{% endraw %}
{% endhighlight %}

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

{% highlight erlang %}
precondition(S, {call,_,return_dvd,[Password,Movie]}) ->
    lists:member({Password,Movie}, S#state.rented);
precondition(S, {call,_,rent_dvd,[Password,_Movie]}) ->
    lists:member(Password, S#state.users);
precondition(S, {call,_,delete_account,[Password]}) ->
    lists:member(Password, S#state.users);
precondition(_, _) ->
    true.
{% endhighlight%}

And test the property once more:

{% highlight plaintext %}
{% raw %}
15> proper:quickcheck(movie_statem:prop_server_works_fine()).
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
Result: {postcondition,false}

Shrinking ...(3 time(s))
[{set,{var,2},{call,movie_server,create_account,[mary]}},
 {set,{var,3},{call,movie_server,rent_dvd,[{var,2},peter_pan]}},
 {set,{var,4},{call,movie_server,rent_dvd,[{var,2},peter_pan]}}]
History: [{{state,[],[]},1},{{state,[1],[]},[peter_pan]},
          {{state,[1],[{1,peter_pan}]},[peter_pan]}]
State: {state,[1],[{1,peter_pan}]}
Result: {postcondition,false}
false
{% endraw %}
{% endhighlight%}

We have just detected another subtle error in our model. It is invoked in the
following case:

* Mary creates an account.
* Mary rents a copy of the movie Peter Pan.
* Mary wants to rent another copy of the same movie, but there are no
  more copies available.

In this case the postcondition for `rent_dvd/2` is false. Again, it is up to us
to redefine our model and we decide to add a precondition that prevents the
aforementioned scenario.

{% highlight erlang %}
precondition(S, {call,_,rent_dvd,[Password,Movie]}) ->
    not lists:member({Password,Movie}, S#state.rented) andalso
        lists:member(Password, S#state.users);
{% endhighlight%}

Eventually:

{% highlight plaintext %}
18>  proper:quickcheck(movie_statem:prop_server_works_fine()).
...........................................................................
..........................
OK: Passed 100 test(s).
true
{% endhighlight%}

Now that tests seem to pass, we will temporarily remove the first precondition
that we introduced and correct our code to prevent the server from crashing.
The code handling `return_dvd` requests is the following:

{% highlight erlang %}
handle_call({return,Password,Movie}, _From, S) ->
    #state{users = Users, movies = Movies} = S,
    Reply = case ets:lookup(Users, Password) of
                []  ->
                    not_a_client;
                [{_,_,Rented}] ->
                    NewRented = lists:delete(Movie, Rented),
                    ets:update_element(Users, Password, {3,NewRented}),
                    N = ets:lookup_element(Movies, Movie, 2),
                    ets:update_element(Movies, Movie, {2,N+1}),
                    NewRented
            end,
    {reply, Reply, S};
{% endhighlight%}

The command `ets:lookup_element(Movies, Movie, 2)` raises an exception if no
object with the key `Movie` is stored in the table. When the test failed with
the server craching, PropEr had produced the following counterexample:

{% highlight erlang %}
[{set,{var,1},{call,movie_server,create_account,[john]}},
 {set,{var,8},{call,movie_server,return_dvd,[{var,1},inception]}}]
false
{% endhighlight%}

Let us rewrite:

{% highlight erlang %}
handle_call({return,Password,Movie}, _From, S) ->
    #state{users = Users, movies = Movies} = S,
    Reply =
        case ets:lookup(Users, Password) of
            []  ->
                not_a_client;
            [{_,_,Rented}] ->
                case ets:lookup(Movies, Movie) of
                    [] ->
                        Rented;
                    [{_,N}] ->
                        NewRented = lists:delete(Movie, Rented),
                        ets:update_element(Users, Password, {3,NewRented}),
                        ets:update_element(Movies, Movie, {2,N+1}),
                        NewRented
                end
        end,
    {reply, Reply, S};
{% endhighlight%}

and check that the counterexample now passes the test:

{% highlight plaintext %}
41> proper:check(movie_statem:prop_server_works_fine(), proper:counterexample()).
OK: The input passed the test.
true
{% endhighlight%}

Can we be confident that we fixed all bugs? Well, let's take a moment to
think about how often each operation was tested. Or even better, let's
ask PropEr to do it for us. This is possible by using the `aggregate/2` function
to collect statistics about how often each command was executed.

{% highlight erlang %}
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
{% endhighlight%}

If we run the test now:

{% highlight plaintext %}
41> proper:quickcheck(movie_statem:prop_server_works_fine(), 3000).
<...3000 dots....>
OK: Passed 3000 test(s).

30% {movie_server,ask_for_popcorn,0}
30% {movie_server,create_account,1}
19% {movie_server,delete_account,1}
18% {movie_server,rent_dvd,2}
 1% {movie_server,return_dvd,2}
true
{% endhighlight%}

We can easily notice that `return_dvd/2` calls are rarely tested. This happens
because of the precondition that allows us to only return movies we have
previously rented. To remedy the situation, we will modify the command
generator so that `return_dvd/2` calls can be selected more frequently.

{% highlight erlang %}
command(S) ->
    Users = (S#state.users =/= []),
    Rented = (S#state.rented =/= []),
    frequency([{1, {call,?SERVER,create_account,[name()]}},
               {1, {call,?SERVER,ask_for_popcorn,[]}}] ++
              [{1, {call,?SERVER,delete_account,[password(S)]}} || Users] ++
              [{5, {call,?SERVER,rent_dvd,[password(S), movie()]}} || Users] ++
              [{5, ?LET({Password,Movie}, elements(S#state.rented),
                        {call,?SERVER,return_dvd,[Password, Movie]})} || Rented]).
{% endhighlight%}

The resulting distribution is:

{% highlight plaintext %}
42> proper:quickcheck(movie_statem:prop_server_works_fine(), 3000).
<...3000 dots....>
OK: Passed 3000 test(s).

36% {movie_server,rent_dvd,2}
19% {movie_server,return_dvd,2}
18% {movie_server,ask_for_popcorn,0}
17% {movie_server,create_account,1}
 8% {movie_server,delete_account,1}
true
{% endhighlight %}

You can get the complete final code of this tutorial by clicking on
the following links:
  [movie_server](/code/movies/movie_server.erl)
and
  [movie_statem](/code/movies/movie_statem.erl).
