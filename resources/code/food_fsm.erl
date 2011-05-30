-module(food_fsm).
-behaviour(gen_fsm).

-include_lib("proper/include/proper.hrl").

-export([init/1, terminate/3, handle_sync_event/4]).
-export([start/0, stop/0, hungry/0, buy/1, new_day/1]).
-export([cheese_day/2, lettuce_day/2, grapes_day/2]).
-export([cheese_day/3, lettuce_day/3, grapes_day/3]).

-export([initial_state/0, initial_state_data/0, precondition/4,
	 postcondition/5, next_state_data/5, weight/3]).
-export([cheese_day/1, lettuce_day/1, grapes_day/1]).
-export([test/0, test/1]).

-record(storage, {cheese  = 5 :: non_neg_integer(),
		  lettuce = 5 :: non_neg_integer(),
		  grapes  = 5 :: non_neg_integer()}).

test() ->
    test(100).

test(Tests) ->
    proper:quickcheck(?MODULE:prop_food(),[{numtests,Tests}]).

init([]) ->
    {ok, cheese_day, #storage{}}.

cheese_day(eat, Caller, S = #storage{cheese = Cheese}) ->
    gen_fsm:reply(Caller, {cheese_left, Cheese}),
    {next_state, cheese_day, S#storage{cheese = Cheese-1}}.
cheese_day({store,Food}, S) ->
    case Food of
	cheese ->
	    {next_state, cheese_day, S#storage{cheese = S#storage.cheese+1}};
	lettuce ->
	    {next_state, cheese_day, S#storage{lettuce = S#storage.lettuce+1}};
	grapes ->
	    {next_state, cheese_day, S#storage{grapes = S#storage.grapes+1}}
    end;    
cheese_day({new_day,lettuce}, S) -> 
    {next_state, lettuce_day, S};
cheese_day({new_day,grapes}, S) ->
    {next_state, grapes_day, S}.

lettuce_day(eat, Caller, S = #storage{lettuce = Lettuce}) ->
    gen_fsm:reply(Caller, {lettuce_left, Lettuce}),
    {next_state, lettuce_day, S#storage{lettuce = Lettuce-1}}.
lettuce_day({store,Food}, S) ->
    case Food of
	cheese ->
	    {next_state, lettuce_day, S#storage{cheese = S#storage.cheese+1}};
	lettuce ->
	    {next_state, lettuce_day, S#storage{lettuce=S#storage.lettuce+1}};  
	grapes ->
	    {next_state, lettuce_day, S#storage{grapes = S#storage.grapes+1}}
    end;
lettuce_day({new_day,cheese}, S) -> 
    {next_state, cheese_day, S};
lettuce_day({new_day,grapes}, S) ->
    {next_state, grapes_day, S}.

grapes_day(eat, Caller, S = #storage{grapes = Grapes}) ->
    gen_fsm:reply(Caller, {grapes_left, Grapes}),
    {next_state, grapes_day, S#storage{grapes = Grapes-1}}.
grapes_day({store,Food}, S) ->
    case Food of
	cheese ->
	    {next_state, grapes_day, S#storage{cheese = S#storage.cheese+1}};
	lettuce ->
	    {next_state, grapes_day, S#storage{lettuce = S#storage.lettuce+2}};
	grapes ->
	    {next_state, grapes_day, S#storage{grapes = S#storage.grapes+1}}
    end;
grapes_day({new_day,cheese}, S) -> 
    {next_state, cheese_day, S};
grapes_day({new_day,lettuce}, S) ->
    {next_state, lettuce_day, S}.

hungry() ->
    gen_fsm:sync_send_event(hobbit, eat).

buy(Food) ->
    gen_fsm:send_event(hobbit, {store,Food}).

new_day(Food) ->
    gen_fsm:send_event(hobbit, {new_day,Food}).

start() ->
    gen_fsm:start({local,hobbit}, ?MODULE, [], []).

stop() ->
    gen_fsm:sync_send_all_state_event(hobbit, stop).

handle_sync_event(stop, _, _, _) ->
    {stop,normal,ok,[]}.

terminate(_, _, _) ->
    ok.

%% Proper Fsm 

cheese_day(S) ->
     store_transition() ++ eat_transition(S#storage.cheese) ++
         [{grapes_day, {call,?MODULE,new_day,[grapes]}},
          {lettuce_day, {call,?MODULE,new_day,[lettuce]}}].

lettuce_day(S) ->
    store_transition() ++ eat_transition(S#storage.lettuce) ++
        [{grapes_day, {call,?MODULE,new_day,[grapes]}},
         {cheese_day, {call,?MODULE,new_day,[cheese]}}].

grapes_day(S) ->
    store_transition() ++ eat_transition(S#storage.lettuce) ++
        [{lettuce_day, {call,?MODULE,new_day,[lettuce]}},
         {cheese_day, {call,?MODULE,new_day,[cheese]}}].

food() ->
    oneof([cheese, lettuce, grapes]).

store_transition() ->
    [{history, {call,?MODULE,buy,[food()]}}].

eat_transition(Food_left) ->
    [{history, {call,?MODULE,hungry,[]}} || Food_left > 0].

initial_state() -> cheese_day.

initial_state_data() -> #storage{}.

next_state_data(_, _, S, _, {call,_,buy,[Food]}) ->
    case Food of
	cheese ->
	    S#storage{cheese = S#storage.cheese + 1};
	lettuce ->
	    S#storage{lettuce = S#storage.lettuce + 1};    
	grapes ->
	    S#storage{grapes = S#storage.grapes + 1}
    end;
next_state_data(Today, _, S, _, {call,_,hungry,[]}) ->
    case Today of
	cheese_day->
	    S#storage{cheese = S#storage.cheese - 1};
	lettuce_day ->
	    S#storage{lettuce = S#storage.lettuce - 1};    
	grapes_day ->
	    S#storage{grapes = S#storage.grapes - 1}
    end; 
next_state_data(_, _, S, _, {call,_,_,_}) ->
    S.

precondition(Today, _, S, {call,_,hungry,[]}) ->
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

postcondition(cheese_day, _, #storage{cheese = Cheese}, {call,_,hungry,[]}, Res) ->
    Res =:= {cheese_left, Cheese};
postcondition(lettuce_day, _, #storage{lettuce = Lettuce}, {call,_,hungry,[]}, Res) ->
    Res =:= {lettuce_left, Lettuce};
postcondition(grapes_day, _, #storage{grapes = Grapes}, {call,_,hungry,[]}, Res) ->
    Res =:= {grapes_left, Grapes};
postcondition(_,_,_,_,Res) ->
    Res =:= ok.

weight(_, _, {call,_,new_day,_}) -> 1;
weight(_, _, {call,_,hungry,_}) -> 2;
weight(_, _, {call,_,buy,_}) -> 3.

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
