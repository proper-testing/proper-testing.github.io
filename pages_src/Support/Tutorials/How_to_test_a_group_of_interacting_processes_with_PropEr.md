Summary: More advanced use of PropEr statem

In this tutorial, we will use PropEr to test the interaction between a group
of processes. The system under test consists of one master and multiple slave
processes. The master plays ping-pong with all slave processes, which do not
interact with each other. Fot the rest of this tutorial, we will refer to the
slave processes as ping-pong players, because (as you will soon realize) they
are unwilling to play any other sport.

The ping-pong master
--------------------

The ping-pong master is implemented as a gen_server. The following
requests are possible:

* add a new ping-pong player

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    add_player(Name) ->
        gen_server:call(?MODULE, {add_player,Name}).

    handle_call({add_player,Name}, _From, Dict) ->
        case whereis(Name) of
	    undefined ->
	        Pid = spawn(?MODULE, ping_pong_player, [Name]),
	        register(Name, Pid);
	    Pid when is_pid(Pid) ->
	        ok
        end,
        {reply, ok, dict:store(Name, 0, Dict)};
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

* remove a ping-pong player

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    remove_player(Name) ->
        gen_server:call(?MODULE, {remove_player,Name}).

    handle_call({remove_player,Name}, _From, Dict) ->
        Pid = whereis(Name),
        exit(Pid, kill),
        {reply, {removed,Name}, dict:erase(Name, Dict)};
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

* send a ping message to the server

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ping(FromName) ->
        gen_server:call(?MODULE, {ping, FromName}).

    handle_call({ping,FromName}, _From, Dict) ->
        {reply, pong, dict:update_counter(FromName, 1, Dict)};
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

* get the score (i.e. number of ping-pong message exchanges with the
  master) for a given player

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    get_score(Name) ->
        gen_server:call(?MODULE, {get_score,Name}).

    handle_call({get_score,Name}, _From, Dict) ->
        Score = dict:fetch(Name, Dict),
        {reply, Score, Dict}.
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

In order to test the ping-pong master we can define an abstract state
machine as described in the Testing_generic_servers_with_PropEr tutorial.
The state machine can be found here. After successfully testing the
master, it's time to move on to the ping-pong players.


The ping-pong players
---------------------

A ping-pong player is a process spawned and registered as `Name` that executes
the folloowing function:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ping_pong_player(Name) ->
        receive
            {ping_pong,From} ->
                From ! ping(Name);
            {tennis,From} ->
                From ! maybe_later;
            {football,From} ->
                From ! no_way
        end,
        ping_pong_player(Name).
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

In other words, when the player is asked to play ping-pong, it sends a `ping`
message to the ping-pong master and sends back the answer it gets (i.e. `pong`).
On the other hand, if asked to play tennis or football, the player sends
back a message expressing his dislike for any other sport. The API to
interact with a ping-pong player is the following:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    play_ping_pong(Player) ->
        Player ! {ping_pong, self()},
        receive
            Reply -> Reply
        end.

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
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The ping-pong player has a rather simple interface, but still we would like
to test its stand-alone behaviour, before moving on to test the interaction with
the ping-pong master. The following abstract state machine specifies the
behaviour model. 

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    initial_state() -> [].
    
    command([]) ->
    	{call,?MODULE,spawn_reg,[name()]};
    command(Players) ->
        oneof([{call,?MODULE,spawn_reg,[name()]},
	       {call,?PLAYER,play_ping_pong,[elements(Players)]},
	       {call,?PLAYER,play_tennis,[elements(Players)]},
	       {call,?PLAYER,play_football,[elements(Players)]}]).

    name() ->
        elements(?NAMES).

    precondition(Players, {call,_,play_ping_pong,[P]}) ->
        lists:member(P, Players);
    precondition(Players, {call,_,play_tennis,[P]}) ->
        lists:member(P, Players);
    precondition(Players, {call,_,play_football,[P]}) ->
        lists:member(P, Players);
    precondition(_, _) ->
        true.

    next_state(Players, _V, {call,_,spawn_reg,[Name]}) ->
        case lists:member(Name, Players) of
            true -> Players;
            false -> [Name|Players]
        end;
    next_state(Players, _V, {call,_,_,_}) ->
        Players.	    

    postcondition(_, {call,_,play_tennis,_}, Res) ->
        Res =:= maybe_later;
    postcondition(_, {call,_,play_football,_}, Res) ->
        Res =:= no_way;
    postcondition(_, {call,_,play_ping_pong,_}, Res) ->
        Res =:= pong;
    postcondition(_, {call,_,spawn_reg,_}, Res) ->
        Res =:= true.
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The function `spawn_reg/1` is locally defined and used to spawn
and register new ping-pong player processes that can be used in
each testcase. Since we don't want these players to interact
with the server for the moment, we define a local mock player:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    spawn_reg(Name) ->
        case whereis(Name) of
	    undefined ->
	        Pid = spawn(?MODULE, ping_pong_player, [Name]),
	        register(Name, Pid);
	    Pid when is_pid(Pid) ->
	        true
        end.

    ping_pong_player(Name) ->
        receive
            {ping_pong,From} ->
                From ! ping(Name);
            {tennis,From} ->
                From ! maybe_later;
            {football,From} ->
                From ! no_way
        end,
        ping_pong_player(Name).
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

After each test is run, clean up is necessary to kill the ping-pong player
processes. The property is:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    prop_players() ->
        ?FORALL(
           Cmds, commands(?MODULE),
           begin
               {H,S,Res} = run_commands(?MODULE, Cmds),
               cleanup(),
               ?WHENFAIL(
                   io:format("History: ~w\nState: ~w\nRes: ~w\n",
                             [H,S,Res]),
                   aggregate(command_names(Cmds), Res =:= ok))
           end).

    cleanup() ->
        [catch unregister(Name) || Name <- ?NAMES].

    4> proper:quickcheck(player_statem:prop_players()).
    ...........................................................................
    .........................
    OK: Passed 100 test(s).

    32% {player_statem,spawn_reg,1}
    24% {ping_pong,play_football,1}
    22% {ping_pong,play_ping_pong,1}
    21% {ping_pong,play_tennis,1}
    true
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

It's ping pong time
-------------------

Finally, we have come to the main point of this tutorial. That is: how do we
test the whole system? Well, the answer it is embarrasingly simple. We take a
step back, and look at the whole system as a blackbox. Which operations
should we test? Probably the combination of the previously defined abstract
state machines. Only we leave out opeartions that depend on interaction.
In our case, we no longer need to send `ping` messages to the server. We
will instruct the ping-pong players to do so. Having a well defined abstract
state machine will eventually reveal inconsistencies between modeled and
actual system behaviour and this will be reflected in the results of the
external API calls and will most probably lead to a false or exception raising
postcondition. In our case, we will test the following operations which
include calls to both ?MASTER and ?PLAYER processes.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    -record(state, {players = [] :: [name()],
		    scores  = [] :: [{name(),score()}]}).

    initial_state() ->  #state{}.

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

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 
State updates:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    next_state(S, _V, {call,_,add_player,[Name]}) ->
        S#state{players = [Name|S#state.players],
	        scores = [{Name,0}|S#state.scores]};
    next_state(S, _V, {call,_,remove_player,[Name]}) ->
        S#state{players = lists:delete(Name, S#state.players),
	        scores = proplists:delete(Name, S#state.scores)};
    next_state(S = #state{scores = Sc}, _V, {call,_,play_ping_pong,[Name]}) ->
        Score = proplists:get_value(Name, Sc),
        S#state{scores = [{Name,Score+1}|proplists:delete(Name, Sc)]};
    next_state(S = #state{scores = Sc}, _V, {call,_,cast_ping_pong,[Name]}) ->
        Score = proplists:get_value(Name, Sc),
        S#state{scores = [{Name,Score+1}|proplists:delete(Name, Sc)]};
    next_state(S, _, _) ->    
        S.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Preconditions to ensure valid shrinking:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    precondition(S, {call,_,add_player,[Name]}) ->
        not lists:member(Name, S#state.players);
    precondition(S, {call,_,remove_player,[Name]}) ->
        lists:member(Name, S#state.players);
    precondition(S, {call,_,get_score,[Name]}) ->
        lists:member(Name, S#state.players);
    precondition(S, {call,_,play_ping_pong,[Name]}) ->
        lists:member(Name, S#state.players);
    precondition(S, {call,_,cast_ping_pong,[Name]}) ->
        lists:member(Name, S#state.players);
    precondition(S, {call,_,play_tennis,[Name]}) ->
        lists:member(Name, S#state.players);
    precondition(_, _) ->
        true.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Finally, postconditions:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    postcondition(_S, {call,_,add_player,[_Name]}, Res) ->
        Res =:= ok;
    postcondition(_S, {call,_,remove_player,[Name]}, Res) ->
        Res =:= {removed,Name};
    postcondition(S, {call,_,get_score,[Name]}, Res) ->
        Res =:= proplists:get_value(Name, S#state.scores);
    postcondition(_S, {call,_,play_ping_pong,[_Name]}, Res) ->
        Res =:= pong;
    postcondition(_S, {call,_,play_tennis,[_Name]}, Res) ->
        Res =:= maybe_later.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

And the property:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    prop_ping_pong_works() ->
        ?FORALL(
           Cmds, commands(?MODULE),
           ?TRAPEXIT(
	       begin
	           ?MASTER:start_link(),
	           {H,S,Res} = run_commands(?MODULE, Cmds),
	           ?MASTER:stop(),
	           ?WHENFAIL(
		       io:format("History: ~w\nState: ~w\nRes: ~w\n",
			         [H,S,Res]),
		       aggregate(command_names(Cmds), Res =:= ok))
	       end)).

    7> proper:quickcheck(ping_pong_statem:prop_ping_pong_works()).
    ...........................................................................
    .........................
    OK: Passed 100 test(s).

    29% {ping_pong,add_player,1}
    18% {ping_pong,get_score,1}
    17% {ping_pong,play_ping_pong,1}
    17% {ping_pong,remove_player,1}
    16% {ping_pong,play_tennis,1}
    true

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Assume now we add a `cast_ping_pong`. Behaves like `play_ping_pong` but doesn't
receive the answer. We add the correct postcondition and run:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    postcondition(_S, {call,_,cast_ping_pong,[_Name]}, Res) ->
        Res =:= ping_pong;

    9> proper:quickcheck(ping_pong_statem:prop_ping_pong_works()).
    ...................!
    Failed: After 20 test(s).
    [{set,{var,1},{call,ping_pong,add_player,[ben]}},
     {set,{var,2},{call,ping_pong,cast_ping_pong,[ben]}},
     {set,{var,3},{call,ping_pong,cast_ping_pong,[ben]}},
     {set,{var,4},{call,ping_pong,get_score,[ben]}},
     {set,{var,5},{call,ping_pong,get_score,[ben]}},
     {set,{var,6},{call,ping_pong,play_tennis,[ben]}},
     {set,{var,7},{call,ping_pong,play_tennis,[ben]}}]
    History: [{{state,[],[]},ok},{{state,[ben],[{ben,0}]},ping_pong},
              {{state,[ben],[{ben,1}]},ping_pong},{{state,[ben],[{ben,2}]},0}]
    State: {state,[ben],[{ben,2}]}
    Res: {postcondition,false}

    Shrinking ..(2 time(s))
    [{set,{var,1},{call,ping_pong,add_player,[ben]}},
     {set,{var,3},{call,ping_pong,cast_ping_pong,[ben]}},
     {set,{var,4},{call,ping_pong,get_score,[ben]}}]
    History: [{{state,[],[]},ok},{{state,[ben],[{ben,0}]},ping_pong},
              {{state,[ben],[{ben,1}]},0}]
    State: {state,[ben],[{ben,1}]}
    Res: {postcondition,false}
    false

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

We add the postcondition of `get_score` to achieve deterministic results.
We can see how property based testing, apart from finding bugs, increases our
understanding of the system under test and process interaction within.
Also, in a fairly complicated system it is usually enough to make a simple
model and concentrate on operations you actually want to test.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    postcondition(S, {call,_,get_score,[Name]}, Res) ->
        Res =< proplists:get_value(Name, S#state.scores);

    11> proper:quickcheck(ping_pong_statem:prop_ping_pong_works()).
    ...........................................................................
    .........................
    OK: Passed 100 test(s).

    25% {ping_pong,add_player,1}
    15% {ping_pong,play_tennis,1}
    15% {ping_pong,cast_ping_pong,1}
    14% {ping_pong,play_ping_pong,1}
    14% {ping_pong,get_score,1}
    13% {ping_pong,remove_player,1}
    true

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

And some parallel testing:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    spawn_reg(Name) ->
        case whereis(Name) of
	    undefined ->
	        Pid = spawn(?MODULE, ping_pong_player, [Name]),
	        erlang:yield(),
	        catch register(Name, Pid);
	    Pid when is_pid(Pid) ->
	        true
        end.

    prop_parallel() ->
        ?FORALL(
           Cmds, parallel_commands(?MODULE),
           begin
	       {SeqHist,ParHist,Res} = run_parallel_commands(?MODULE, Cmds),
	       cleanup(),
	       ?WHENFAIL(io:format("Seq:~w~nPar:~w~nRes:~w~n",
			           [SeqHist,ParHist,Res]),
		         Res =:= ok)
           end).

    36> proper:quickcheck(player_statem:prop_parallel()).
    .....!
    Failed: After 6 test(s).
    {[],[[{set,{var,1},{call,player_statem,spawn_reg,[alice]}},
          {set,{var,2},{call,ping_pong,play_ping_pong,[alice]}},
          {set,{var,3},{call,player_statem,spawn_reg,[bob]}},
          {set,{var,4},{call,player_statem,spawn_reg,[ben]}},
          {set,{var,6},{call,ping_pong,play_football,[ben]}}],
         [{set,{var,5},{call,player_statem,spawn_reg,[alice]}},
          {set,{var,7},{call,ping_pong,play_ping_pong,[alice]}},
          {set,{var,8},{call,player_statem,spawn_reg,[mary]}},
          {set,{var,9},{call,player_statem,spawn_reg,[bob]}},
          {set,{var,10},{call,player_statem,spawn_reg,[ben]}},
          {set,{var,11},{call,player_statem,spawn_reg,[mary]}}]]}
    Seq:[]
    Par:<...long parallel execution history...>
    Res:no_possible_interleaving

    Shrinking ......(6 time(s))
    {[],[[{set,{var,1},{call,player_statem,spawn_reg,[alice]}}],
         [{set,{var,5},{call,player_statem,spawn_reg,[alice]}}]]}
    Seq:[]
    Par:[[{{set,{var,1},{call,player_statem,spawn_reg,[alice]}},true}],
         [{{set,{var,5},{call,player_statem,spawn_reg,[alice]}},
           {'EXIT',{badarg,[{erlang,register,[alice,<0.4728.0>]},
                            {player_statem,spawn_reg,1},
                            {proper_statem,execute,4},
                            {proper_statem,'-spawn_jobs/2-fun-0-',3}]}}}]]
    Res:no_possible_interleaving
    false

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~