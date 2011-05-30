Summary: a PropEr fsm tutorial

In this tutorial we will use PropEr to test a finite state machine
specification. The testcases produced using the PropEr fsm library
(`proper_fsm`) are of exactly the same form as the testcases produced
using the PropEr statem library (`proper_statem`). Before reading this
tutorial, please make sure you have understood the basic concepts of
testing stateful systems with PropEr, as described
[here](PropEr_testing_of_generic_servers.html) and
[here](PropEr_testing_of_process_interaction.html).
Here we will concentrate on the differences between a PropEr statem and a
PropEr fsm callback module. 

PropEr fsm provides a convenient way to test a system that can be easily
described via a finite state machine. That is, a finite set of states and
transitions between them. A typical description of a system as a finite
state machine is a UML state diagram. PropEr fsm is designed to bring the
specification very close to a state diagram. Thus, a PropEr fsm callback module
can be derived from a state diagram in a straightforward way.

The example
-----------

Consider, for example, the following state diagram, describing the life of a
strange creature that feeds on cheese, grapes and lettuce but never eats the
same kind of food two days in a row. ![](/images/creature.png)

Let us briefly explain. Each day in the life of this creature is devoted to a
certain kind of food. When it gets hungry it consumes a portion of the daily
food which is kept in the food storage. To make life less boring and, more
importantly, to bring food to the storage, the creature goes shopping from time
to time. Every night, it decides what to eat the next day, but this cannot
be the same kind of food that it had that day.

One day, instead of going shopping, the creature decided to describe his life so
that it could be published one day. Erlang being its favourite programming
language, it implemented his habbits as a callback module of the `gen_fsm`
behaviour.

The food storage is kept in a record. Each of the record fields contains the
amount of available portions of each kind of food.

    :::erlang
    -record(storage, {cheese  = 1 :: non_neg_integer(),
                      lettuce = 1 :: non_neg_integer(),
                      grapes  = 1 :: non_neg_integer()}).

    start(Day) ->
        gen_fsm:start({local, creature}, ?MODULE, Day, []).

    init(Day) ->
        {ok, Day, #storage{}}.

Assume that the first day is a `cheese_day`. When the creature feels hungry, it
gets a portion of cheese to eat.

    :::erlang
    hungry() ->
        gen_fsm:send_event(creature, eat).

    cheese_day(eat, Caller, #storage{cheese = Cheese} = S) ->
        gen_fsm:reply(Caller, {cheese_left, Cheese}),
        {next_state, cheese_day, S#storage{cheese = Cheese - 1}}.

During the day it might decide to go shopping and buy some food to put in the
food storage.

    :::erlang
    buy(Food, Quantity) ->
        gen_fsm:send_event(creature, {store, Food, Quantity}).

    cheese_day({store, Food}, S) ->
        case Food of
            cheese ->
                {next_state, cheese_day, S#storage{cheese = S#storage.cheese + 1}};
            lettuce ->
                {next_state, cheese_day, S#storage{lettuce = S#storage.lettuce + 1}};
            grapes ->
                {next_state, cheese_day, S#storage{grapes = S#storage.grapes + 1}}
        end;

Finally, just before going to sleep, our creature decides what it will eat the
next day.

    :::erlang

    new_day(Food) ->
        gen_fsm:send_event(creature, {new_day, Food}).

    cheese_day({new_day, lettuce}, S) ->
        {next_state, lettuce_day, S};
    cheese_day({new_day, grapes}, S) ->
        {next_state, grapes_day, S}.

The same things happen on a `grapes_day` or on a `lettuce_day`:

    :::erlang
    lettuce_day(eat, Caller, S = #storage{lettuce = Lettuce}) ->
        gen_fsm:reply(Caller, {lettuce_left, Lettuce}),
        {next_state, lettuce_day, S#storage{lettuce = Lettuce - 1}}.

    lettuce_day({store, Food}, S) ->
        case Food of
            cheese ->
                {next_state, lettuce_day, S#storage{cheese = S#storage.cheese + 1}};
            lettuce ->
                {next_state, lettuce_day, S#storage{lettuce = S#storage.lettuce + 1}};  
            grapes ->
                {next_state, lettuce_day, S#storage{grapes = S#storage.grapes + 1}}
        end;
    lettuce_day({new_day, cheese}, S) -> 
        {next_state, cheese_day, S};
    lettuce_day({new_day, grapes}, S) ->
        {next_state, grapes_day, S}.

    grapes_day(eat, Caller, S = #storage{grapes = Grapes}) ->
        gen_fsm:reply(Caller, {grapes_left, Grapes}),
        {next_state, grapes_day, S#storage{grapes = Grapes - 1}}.

    grapes_day({store, Food}, S) ->
        case Food of
            cheese ->
                {next_state, grapes_day, S#storage{cheese = S#storage.cheese - 1}};
            lettuce ->
                {next_state, grapes_day, S#storage{lettuce = S#storage.lettuce - 1}};
            grapes ->
                {next_state, grapes_day, S#storage{grapes = S#storage.grapes - 1}}
        end;
    grapes_day({new_day, cheese}, S) ->
        {next_state, cheese_day, S};
    grapes_day({new_day, lettuce}, S) ->
        {next_state, lettuce_day, S}.

We will now describe how to use PropEr to test the finite state machine
specification. It happens that OTP provides the gen_fsm behaviour for
describing finite state machines. However, even when the system under test is
not specified via this behaviour, the information that PropEr needs can be
derived easily from the state diagram of the system.

The first difference from statem is state name - state data separation.
The command/1 generator is replaced by...

In our example, each day is specified via
a callback function, named after the state it represents. This callbacks
simply return a list of possible transitions from that state. The
transition is a pair consisting of the target state of the transition and
the symbolic call that triggers the transition. The parameter S describes
the internal state of the fsm, i.e. the amount of each food stored.

    :::erlang
    cheese_day(S) ->
        store_transition() ++ eat_transition(S#state.cheese) ++
            [{grapes_day, {call,?MODULE,new_day,[grapes]}},
             {lettuce_day, {call,?MODULE,new_day,[lettuce]}}].

    lettuce_day(S) ->
        store_transition() ++ eat_transition(S#state.lettuce) ++
            [{grapes_day, {call,?MODULE,new_day,[grapes]}},
             {cheese_day, {call,?MODULE,new_day,[cheese]}}].

    grapes_day(S) ->
        store_transition() ++ eat_transition(S#state.grapes) ++
            [{lettuce_day, {call,?MODULE,new_day,[lettuce]}},
             {cheese_day, {call,?MODULE,new_day,[cheese]}}].

A store transition is triggered every time the creature buys
some food, whereas the eat transition every time it is hungry.
Both of these transitions do not change the current state of
the fsm and this specified by having `history` as the target
state. What is more, we only want to make eat transition when
there is actually food to eat.

    :::erlang
    store_transition() ->
        [{history, {call,?MODULE,buy,[food()]}}].

    food() ->
        oneof([cheese, lettuce, grapes]).

    eat_transition(Food_left) ->
        [{history, {call,?MODULE,hungry,[]}} || Food_left > 0].

The state is initialized as following:

    :::erlang
    initial_state() -> cheese_day.

    initial_state_data() -> #state{}.

The state of the fsm is updated directly when choosing a transition.
For the state data, there is a separate callback:

    :::erlang
    next_state_data(_, _, S, _, {call,_,store,[Food]}) ->
        case Food of
            cheese ->
                S#state{cheese = S#state.cheese + 1};
            lettuce ->
                S#state{lettuce = S#state.lettuce + 1};    
            grapes ->
                S#state{grapes = S#state.grapes + 1}
        end;
    next_state_data(Today, _, S, _, {call,_,eat,[]}) ->
        case Today of
            cheese_day->
                S#state{cheese = S#state.cheese - 1};
            lettuce_day ->
                S#state{lettuce = S#state.lettuce - 1};    
            grapes_day ->
                S#state{grapes = S#state.grapes - 1}
        end; 
    next_state_data(_, _, S, _, {call,_,_,_}) ->
        S.

The preconditions:

    :::erlang
    precondition(Today, _, S, {call,_,eat,[]}) ->
        case Today of
            cheese_day->
                S#storage.cheese > 0;
            lettuce_day ->
                S#storage.lettuce > 0;    
            grapes_day ->
                S#storage.grapes > 0
        end;
    precondition(Day, Day, _, {call,_,new_day,_}) ->
        false;
    precondition(_, grapes_day, _, {call,_,new_day,[grapes]}) ->
        true;
    precondition(_, cheese_day, _, {call,_,new_day,[cheese]}) ->
        true;
    precondition(_, lettuce_day, _, {call,_,new_day,[lettuce]}) ->
        true;
    precondition(_, _, _, {call,_,new_day,_}) ->
        false;
    precondition(_, _, _, {call,_,_,_}) ->
        true.

Postconditions:

    :::erlang
    postcondition(_,_,_,_,Res) ->
        Res =:= ok.

Property:

    :::erlang
    prop_food() ->
        ?FORALL(Cmds, proper_fsm:commands(?MODULE),
                begin
                    start(),
                    {H,S,Res} = proper_fsm:run_commands(?MODULE, Cmds),
                    stop(),
                    ?WHENFAIL(
                        io:format("H: ~w\nS: ~w\nR: ~w\n", [H,S,Res]),
                        aggregate(zip(proper_fsm:state_names(H),
                                      command_names(Cmds)),
                                  Res =:= ok))
                end).

Now, if we would like to test certain operations more frequently
we can assign weights to transitions:

    :::erlang
    weight(_, _, {call,_,new_day,_}) -> 1;
    weight(_, _, {call,_,eat,_}) -> 5;
    weight(_, _, {call,_,store,_}) -> 10.

<!-- kate: replace-tabs-save on; replace-tabs on; tab-width 8; -->
