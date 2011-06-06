Summary: yet another PropEr statem tutorial
Author: Eirini Arvaniti &ndash; some additions and fixes by Kostis Sagonas
kate: replace-tabs-save on; replace-tabs on; tab-width 8;

In this tutorial, we will use PropEr to test a group of interacting processes.
The system under test consists of one master and multiple slave processes. The
main concept is that the master plays ping-pong (i.e. exchanges ping and pong
messages) with all slave processes, which do not interact with each other. For
the rest of this tutorial, we will refer to the slave processes as _the
ping-pong players_.

The ping-pong master
--------------------

The ping-pong master is implemented as an Erlang `gen_server`. The internal
state of the server is a dictionary containing the scores (i.e. the number
of ping-pong message exchanges with the master) of the ping-pong players.
External clients can make the following requests:

*   start and link to the ping-pong master

        :::erlang
        start_link() ->
            gen_server:start_link({local, ?MASTER}, ?MASTER, [], []).

        init([]) ->
            {ok, dict:new()}.


*   stop the ping-pong master

        :::erlang
        stop() ->
            gen_server:cast(?MASTER, stop).

        handle_cast(stop, Dict) ->
            {stop, normal, Dict}.

        terminate(_Reason, Dict) ->
            Players = dict:fetch_keys(Dict),
            lists:foreach(fun (Name) -> exit(whereis(Name), kill) end, Players).


*   add a new ping-pong player to interact with the master

        :::erlang
        add_player(Name) ->
            gen_server:call(?MODULE, {add_player, Name}).

        handle_call({add_player, Name}, _From, Dict) ->
            case whereis(Name) of
                undefined ->
                    Pid = spawn(fun () -> ping_pong_player(Name) end),
                    true = register(Name, Pid);
                    {reply, ok, dict:store(Name, 0, Dict)};
                Pid when is_pid(Pid) ->
                    {reply, ok, Dict}
            end;


*   remove a ping-pong player

        :::erlang
        remove_player(Name) ->
            gen_server:call(?MODULE, {remove_player, Name}).

        handle_call({remove_player, Name}, _From, Dict) ->
            Pid = whereis(Name),
            exit(Pid, kill),
            {reply, {removed, Name}, dict:erase(Name, Dict)};


*   send a ping message to the server

        :::erlang
        ping(FromName) ->
            gen_server:call(?MODULE, {ping, FromName}).

        handle_call({ping, FromName}, _From, Dict) ->
            {reply, pong, dict:update_counter(FromName, 1, Dict)};


*   get the score of a given player

        :::erlang
        get_score(Name) ->
            gen_server:call(?MODULE, {get_score, Name}).

        handle_call({get_score, Name}, _From, Dict) ->
            Score = dict:fetch(Name, Dict),
            {reply, Score, Dict}.


In order to test the stand-alone behaviour of the ping-pong master we can
define an abstract state machine, as described in
[this tutorial](PropEr_testing_of_generic_servers.html) about testing generic
servers with PropEr. The state machine specification for the ping-pong master
can be found [here](/code/ping_pong/master_statem.erl).


The ping-pong players
---------------------

A ping-pong player is a process spawned and registered as `Name` that executes
the following loop:

    :::erlang
    ping_pong_player(Name) ->
        receive
            ping_pong ->
                ping(Name);
            {tennis, From} ->
                From ! maybe_later;
            {football, From} ->
                From ! no_way
        end,
        ping_pong_player(Name).


When a player is asked by an external client to play ping-pong, she will send a
`ping` message to the ping-pong master. On the other hand, if asked to play
tennis or football, the player replies with a message expressing her dislike for
any sport other than ping-pong. The API for interacting with a ping-pong player
is the following:

    :::erlang
    play_ping_pong(Player) ->
        Player ! ping_pong,
        ok.

    play_tennis(Player) ->
        Player ! {tennis, self()},
        receive
            Reply -> Reply
        end.

    play_football(Player) ->
        Player ! {football, self()},
        receive
            Reply -> Reply
        end.


It's ping-pong time!
--------------------

It's now time to test that the system behaves as expected when the ping-pong
players interact with the master. To this end, we will specify an abstract
state machine modeling the master's internal state, just as we would do to
test the stand-alone behaviour of the master. We choose to base our
state machine specification on the master process because this is the main
component of the system under test. But now, instead of making `ping/1` calls
directly to the master, we will instruct the ping-pong players to do so by
performing the asynchronous `play_ping_pong/1` call. Moreover, we will include
synchronous `play_tennis/1` calls to the ping-pong players, to test that such
calls do not influence the players' interaction with the master. In our case,
this is quite obvious. But when testing, for example, the interaction of
processes in a big supervision tree, we cannot be sure about the possible
side-effects of each operation.

On the other hand, it is important to keep the complexity of our model at a
reasonable level. Otherwise, it's quite probable to make errors in the
state machine specification. For each different feature we would like to test,
defining a simple state machine that concentrates on the operations related to
that feature will usually reveal any inconsistencies between the model and the
actual system behaviour. These inconsistencies will be reflected in the results
of the selected API calls.

Below we give the abstract state machine that will be used to test the ping-pong
system. As usual, it specifies:

*   The _initial state_ of the model:

        :::erlang
        -type name() :: atom().

        -record(state, {players = []         :: [name()],
                        scores  = dict:new() :: dict()}).

        initial_state() -> #state{}.


*   The _API calls_ that will be tested:

        :::erlang
        command(#state{players = []}) ->
            {call,?MASTER,add_player,[name()]};
        command(S) ->
            oneof([{call,?MASTER,add_player,[name()]},
                   {call,?MASTER,remove_player,[name(S)]},
                   {call,?MASTER,get_score,[name(S)]},
                   {call,?PLAYER,play_ping_pong,[name(S)]},
                   {call,?PLAYER,play_tennis,[name(S)]}]).

        name() -> elements(?NAMES).

        name(S) -> elements(S#state.players).


*   _State updates_:

        :::erlang
        next_state(S, _V, {call,_,add_player,[Name]}) ->
            case lists:member(Name, S#state.players) of
                false ->
                    S#state{players = [Name|S#state.players],
                            scores  = dict:store(Name, 0, S#state.scores)};
                true ->
                    S
            end;
        next_state(S, _V, {call,_,remove_player,[Name]}) ->
            S#state{players = lists:delete(Name, S#state.players),
                    scores  = dict:erase(Name, S#state.scores)};
        next_state(S = #state{scores = Sc}, _V, {call,_,play_ping_pong,[Name]}) ->
            S#state{scores = dict:update_counter(Name, 1, Sc)};
        next_state(S, _, _) ->
            S.


*   _Preconditions_ that should always be respected (even while shrinking):

        :::erlang
        precondition(S, {call,_,remove_player,[Name]}) ->
            lists:member(Name, S#state.players);
        precondition(S, {call,_,get_score,[Name]}) ->
            lists:member(Name, S#state.players);
        precondition(S, {call,_,play_ping_pong,[Name]}) ->
            lists:member(Name, S#state.players);
        precondition(S, {call,_,play_tennis,[Name]}) ->
            lists:member(Name, S#state.players);
        precondition(_, _) ->
            true.


*   And finally, _postconditions_ about the results of the calls:

        :::erlang
        postcondition(_S, {call,_,add_player,[_Name]}, Result) ->
            Result =:= ok;
        postcondition(_S, {call,_,remove_player,[Name]}, Result) ->
            Result =:= {removed, Name};
        postcondition(S, {call,_,get_score,[Name]}, Result) ->
            Result =:= dict:fetch(Name, S#state.scores);
        postcondition(_S, {call,_,play_ping_pong,[_Name]}, Result) ->
            Result =:= ok;
        postcondition(_S, {call,_,play_tennis,[_Name]}, Result) ->
            Result =:= maybe_later.


Having successfully tested the stand-alone behaviour of the master, we expect
this property to pass the tests:

    :::erlang
    prop_ping_pong_works() ->
        ?FORALL(Cmds, commands(?MODULE),
                ?TRAPEXIT(
                    begin
                        ?MASTER:start_link(),
                        {History,State,Result} = run_commands(?MODULE, Cmds),
                        ?MASTER:stop(),
                        ?WHENFAIL(io:format("History: ~w\nState: ~w\nResult: ~w\n",
                                            [History, State, Result]),
                                  aggregate(command_names(Cmds), Result =:= ok))
                    end)).

But...

    :::erl
    5> proper:quickcheck(ping_pong_statem:prop_ping_pong_works()).
    ...............
    =ERROR REPORT==== 30-May-2011::01:50:11 ===
    ** Generic server ping_pong terminating
    ** Last message in was {'$gen_cast',stop}
    ** When Server state ==  <...internal representation of the state...>
    ** Reason for termination ==
    ** {badarg,[{erlang,exit,[undefined,kill]},
                {ping_pong,'-terminate/2-lc$^0/1-0-',1},
                {ping_pong,terminate,2},
                {gen_server,terminate,6},
                {proc_lib,init_p_do_apply,3}]}
    ..!
    Failed: After 18 test(s).
    [{set,{var,1},{call,ping_pong,add_player,[mary]}},
     {set,{var,2},{call,ping_pong,play_ping_pong,[mary]}},
     {set,{var,3},{call,ping_pong,play_ping_pong,[mary]}},
     {set,{var,4},{call,ping_pong,get_score,[mary]}},
     {set,{var,5},{call,ping_pong,add_player,[mary]}},
     {set,{var,6},{call,ping_pong,play_tennis,[mary]}},
     {set,{var,7},{call,ping_pong,play_tennis,[mary]}},
     {set,{var,8},{call,ping_pong,play_tennis,[mary]}},
     {set,{var,9},{call,ping_pong,remove_player,[mary]}}]
    History: [{{state,[],{dict,0,16,16,8,80,48,{[],[],[],[],[],[],[],[],[],[],[],[],[],[],
               [],[]},{{[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[]}}}},ok},
              {{state,[mary],{dict,1,16,16,8,80,48,{[],[],[],[],[],[],[],[],[],[],[],[],[],
               [],[],[]},{{[],[],[],[],[],[],[],[],[],[[mary|0]],[],[],[],[],[],[]}}}},ok},
              {{state,[mary],{dict,1,16,16,8,80,48,{[],[],[],[],[],[],[],[],[],[],[],[],[],
               [],[],[]},{{[],[],[],[],[],[],[],[],[],[[mary|1]],[],[],[],[],[],[]}}}},ok},
              {{state,[mary],{dict,1,16,16,8,80,48,{[],[],[],[],[],[],[],[],[],[],[],[],[],
               [],[],[]},{{[],[],[],[],[],[],[],[],[],[[mary|2]],[],[],[],[],[],[]}}}},0}]
    State: {state,[mary],{dict,1,16,16,8,80,48,{[],[],[],[],[],[],[],[],[],[],[],[],[],[],
            [],[]},{{[],[],[],[],[],[],[],[],[],[[mary|2]],[],[],[],[],[],[]}}}}
    Result: {postcondition,false}

    Shrinking ...(3 time(s))
    [{set,{var,1},{call,ping_pong,add_player,[mary]}},
     {set,{var,3},{call,ping_pong,play_ping_pong,[mary]}},
     {set,{var,4},{call,ping_pong,get_score,[mary]}}]
    History: [{{state,[],{dict,0,16,16,8,80,48,{[],[],[],[],[],[],[],[],[],[],[],[],[],[],
               [],[]},{{[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[]}}}},ok},
              {{state,[mary],{dict,1,16,16,8,80,48,{[],[],[],[],[],[],[],[],[],[],[],[],[],
               [],[],[]},{{[],[],[],[],[],[],[],[],[],[[mary|0]],[],[],[],[],[],[]}}}},ok},
              {{state,[mary],{dict,1,16,16,8,80,48,{[],[],[],[],[],[],[],[],[],[],[],[],[],
               [],[],[]},{{[],[],[],[],[],[],[],[],[],[[mary|1]],[],[],[],[],[],[]}}}},0}]
    State: {state,[mary],{dict,1,16,16,8,80,48,{[],[],[],[],[],[],[],[],[],[],[],[],[],[],
            [],[]},{{[],[],[],[],[],[],[],[],[],[[mary|1]],[],[],[],[],[],[]}}}}
    Result: {postcondition,false}
    false

...the property fails, along with error reports on the server crashing!

What is more, the `History` and `State` fields contain dictionaries which are
printed out based on their internal representation. We decide to deal with this
issue by including some pretty-printing functions in the property, so as to
output more informative debugging information.

    :::erlang
    pretty_history(History) ->
        [{pretty_state(State),Result} || {State,Result} <- History].

    pretty_state(#state{scores = Scores} = S) ->
        S#state{scores = dict:to_list(Scores)}.

    prop_ping_pong_works() ->
        ?FORALL(Cmds, commands(?MODULE),
                ?TRAPEXIT(
                    begin
                        ?MASTER:start_link(),
                        {History,State,Result} = run_commands(?MODULE, Cmds),
                        ?MASTER:stop(),
                        ?WHENFAIL(io:format("History: ~w\nState: ~w\nRes: ~w\n",
                                           [pretty_history(History), pretty_state(State), Result]),
                                  aggregate(command_names(Cmds), Result =:= ok))
                    end)).

And run the test once more:

    :::erl
    7> proper:quickcheck(ping_pong_statem:prop_ping_pong_works()).
    .............
    =ERROR REPORT==== 30-May-2011::02:09:56 ===
    ** Generic server ping_pong terminating
    ** Last message in was {'$gen_cast',stop}
    ** When Server state == <...internal representation of the state...>
    ** Reason for termination ==
    ** {badarg,[{erlang,exit,[undefined,kill]},
                {ping_pong,'-terminate/2-lc$^0/1-0-',1},
                {ping_pong,terminate,2},
                {gen_server,terminate,6},
                {proc_lib,init_p_do_apply,3}]}
    .
    =ERROR REPORT==== 30-May-2011::02:09:56 ===
    <...similar error report...>
    ..........!
    Failed: After 25 test(s).
    [{set,{var,1},{call,ping_pong,add_player,[alice]}},
     {set,{var,2},{call,ping_pong,play_ping_pong,[alice]}},
     {set,{var,3},{call,ping_pong,play_tennis,[alice]}},
     {set,{var,4},{call,ping_pong,play_tennis,[alice]}},
     {set,{var,5},{call,ping_pong,remove_player,[alice]}},
     {set,{var,6},{call,ping_pong,add_player,[mary]}},
     {set,{var,7},{call,ping_pong,play_ping_pong,[mary]}},
     {set,{var,8},{call,ping_pong,get_score,[mary]}},
     {set,{var,9},{call,ping_pong,add_player,[john]}},
     {set,{var,10},{call,ping_pong,play_tennis,[john]}},
     {set,{var,11},{call,ping_pong,add_player,[alice]}},
     {set,{var,12},{call,ping_pong,add_player,[bob]}},
     {set,{var,13},{call,ping_pong,play_tennis,[john]}}]
    History: [{{state,[],[]},ok},{{state,[alice],[{alice,0}]},ok},
              {{state,[alice],[{alice,1}]},maybe_later},
              {{state,[alice],[{alice,1}]},maybe_later},
              {{state,[alice],[{alice,1}]},{removed,alice}},
              {{state,[],[]},ok},{{state,[mary],[{mary,0}]},ok},
              {{state,[mary],[{mary,1}]},0}]
    State: {state,[mary],[{mary,1}]}
    Result: {postcondition,false}

    Shrinking ........(8 time(s))
    [{set,{var,6},{call,ping_pong,add_player,[mary]}},
     {set,{var,7},{call,ping_pong,play_ping_pong,[mary]}},
     {set,{var,8},{call,ping_pong,get_score,[mary]}}]
    History: [{{state,[],[]},ok},{{state,[mary],[{mary,0}]},ok},
              {{state,[mary],[{mary,1}]},0}]
    State: {state,[mary],[{mary,1}]}
    Result: {postcondition,false}
    false

Of course the property still fails and new error reports are produced.
This happens because the asynchronous `play_ping_pong/1` operation introduces
non-determinism in the order in which messages are received by the server. Here
we can see yet another benefit of property based testing: it helps to increase
our understanding about process interaction in the system under test.

Fixing the postcondition of `get_score/1` so as to achieve deterministic
results is quite simple in this case:

    :::erlang
    postcondition(S, {call,_,get_score,[Name]}, Result) ->
        Result =< proplists:get_value(Name, S#state.scores);

The error reports, however, are triggered by a not-so-evident bug in the code.
They are occassionaly produced when stopping the server, because of an attempt
to get and subsequently kill the pid associated with a name that is actually
not present in the process registry. Let us re-examine the code that's
executed when stopping the server:

    :::erlang
    terminate(_Reason, Dict) ->
        Players = dict:fetch_keys(Dict),
        lists:foreach(fun (Name) -> exit(whereis(Name), kill) end, Players).

The exception raised suggests that there exist some names which are stored in
the server's internal dictionary, but are not associated with any (process) pid.
But where do these names come from? To get the answer we have to take a look at
how `ping` messages are handled by the server:

    :::erlang
    handle_call({ping, FromName}, _From, Dict) ->
        {reply, pong, dict:update_counter(FromName, 1, Dict)};

This suggests that incoming `ping` messages associated with names not present
in the server's dictionary are actually inserted in the dictionary. When we
perform an asynchronous `play_ping_pong/1` request to a player, there is a
chance that this player might be removed before her `ping` message is received
by the master. In this case, when the master eventually receives the `ping`
message, the name of the removed player will be added to the dictionary,
despite not being associated with any process. Having spotted the bug, we
can easily fix it:

    :::erlang
    handle_call({ping, FromName}, _From, Dict) ->
        case dict:is_key(FromName, Dict) of
    	    true ->
                {reply, pong, dict:update_counter(FromName, 1, Dict)};
    	    false ->
    	        {reply, {removed, FromName}, Dict}
        end;

And now the property successfully passes the tests:

    :::erl
    11> proper:quickcheck(ping_pong_statem:prop_ping_pong_works()).
    <...3000 dots...>
    OK: Passed 3000 test(s).

    34% {ping_pong,add_player,1}
    16% {ping_pong,remove_player,1}
    16% {ping_pong,play_ping_pong,1}
    16% {ping_pong,play_tennis,1}
    16% {ping_pong,get_score,1}
    true
