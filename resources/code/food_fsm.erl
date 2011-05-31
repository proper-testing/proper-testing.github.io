-module(food_fsm).

-behaviour(gen_fsm).

-include_lib("proper/include/proper.hrl").

-export([start/1, stop/0, hungry/0, buy/2, new_day/1]).

-export([init/1, handle_sync_event/4, terminate/3, code_change/4]).
-export([cheese_day/2, lettuce_day/2, grapes_day/2]).
-export([cheese_day/3, lettuce_day/3, grapes_day/3]).

-export([initial_state/0, initial_state_data/0, precondition/4,
	 postcondition/5, next_state_data/5, weight/3]).
-export([cheese_day/1, lettuce_day/1, grapes_day/1]).

-type day() :: 'cheese_day' | 'lettuce_day' | 'grapes_day'.
-type food() :: 'cheese' | 'lettuce' | 'grapes'.
-type quantity() :: non_neg_integer().

-record(storage, {cheese  = 3 :: quantity(),
		  lettuce = 3 :: quantity(),
		  grapes  = 3 :: quantity()}).


%%%===========================================================================
%%% API
%%%===========================================================================

start(Day) ->
    gen_fsm:start({local, creature}, ?MODULE, [Day], []).

stop() ->
    gen_fsm:sync_send_all_state_event(creature, stop).

hungry() ->
    gen_fsm:sync_send_event(creature, eat).

buy(Food, Quantity) ->
    gen_fsm:send_event(creature, {store, Food, Quantity}).

new_day(Food) ->
    gen_fsm:send_event(creature, {new_day, Food}).

handle_sync_event(stop, _, _, _) ->
    {stop, normal, ok, []}.


%%%===========================================================================
%%% gen_fsm callbacks
%%%===========================================================================

init([Day]) ->
    {ok, Day, #storage{}}.

cheese_day(eat, Caller, #storage{cheese = Cheese} = S) ->
    gen_fsm:reply(Caller, {cheese_left, Cheese}),
    case Cheese > 0 of
	true ->
	    {next_state, cheese_day, S#storage{cheese = Cheese - 1}};
	false ->
	    {next_state, cheese_day, S}
    end.

cheese_day({store, Food, Quantity}, S) ->
    case Food of
	cheese ->
	    {next_state, cheese_day,
	     S#storage{cheese = S#storage.cheese + Quantity}};
	lettuce ->
	    {next_state, cheese_day,
	     S#storage{lettuce = S#storage.lettuce + Quantity}};
	grapes ->
	    {next_state, cheese_day,
	     S#storage{grapes = S#storage.grapes + Quantity}}
    end;
cheese_day({new_day, lettuce}, S) ->
    {next_state, lettuce_day, S};
cheese_day({new_day, grapes}, S) ->
    {next_state, grapes_day, S}.

lettuce_day(eat, Caller, #storage{lettuce = Lettuce} = S) ->
    gen_fsm:reply(Caller, {lettuce_left, Lettuce}),
    case Lettuce > 0 of
	true ->
	    {next_state, lettuce_day, S#storage{lettuce = Lettuce - 1}};
	false ->
	    {next_state, lettuce_day, S}
    end.

lettuce_day({store, Food, Quantity}, S) ->
    case Food of
	cheese ->
	    {next_state, lettuce_day,
	     S#storage{cheese = S#storage.cheese + Quantity}};
	lettuce ->
	    {next_state, lettuce_day,
	     S#storage{lettuce=S#storage.lettuce + Quantity}};
	grapes ->
	    {next_state, lettuce_day,
	     S#storage{grapes = S#storage.grapes + Quantity}}
    end;
lettuce_day({new_day, cheese}, S) ->
    {next_state, cheese_day, S};
lettuce_day({new_day, grapes}, S) ->
    {next_state, grapes_day, S}.

grapes_day(eat, Caller, #storage{grapes = Grapes} = S) ->
    gen_fsm:reply(Caller, {grapes_left, Grapes}),
    case Grapes > 0 of
	true ->
	    {next_state, grapes_day, S#storage{grapes = Grapes - 1}};
	false ->
	    {next_state, grapes_day, S}
    end.

grapes_day({store, Food, Quantity}, S) ->
    case Food of
	cheese ->
	    {next_state, grapes_day,
	     S#storage{cheese = S#storage.cheese + Quantity}};
	lettuce ->
	    {next_state, grapes_day,
	     S#storage{lettuce = S#storage.lettuce + Quantity}};
	grapes ->
	    {next_state, grapes_day,
	     S#storage{grapes = S#storage.grapes + Quantity}}
    end;
grapes_day({new_day, cheese}, S) ->
    {next_state, cheese_day, S};
grapes_day({new_day, lettuce}, S) ->
    {next_state, lettuce_day, S}.

terminate(_, _, _) ->
    ok.

code_change(_OldVsn, StateName, StateData, _Extra) ->
    {ok, StateName, StateData}.


%%%===========================================================================
%%% PropEr Fsm specification
%%%===========================================================================

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

quantity() ->
    ?SUCHTHATMAYBE(I, pos_integer(), I < 5).

store_transition() ->
    [{history, {call,?MODULE,buy,[food(), quantity()]}}].

eat_transition(_Food_left) ->
    %% [{history, {call,?MODULE,hungry,[]}} || Food_left > 0].
    [{history, {call,?MODULE,hungry,[]}}].

initial_state() -> cheese_day.

initial_state_data() -> #storage{}.

next_state_data(_, _, S, _, {call,_,buy,[Food, Quantity]}) ->
    case Food of
	cheese ->
	    S#storage{cheese = S#storage.cheese + Quantity};
	lettuce ->
	    S#storage{lettuce = S#storage.lettuce + Quantity};    
	grapes ->
	    S#storage{grapes = S#storage.grapes + Quantity}
    end;
next_state_data(Today, _, S, _, {call,_,hungry,[]}) ->
    case Today of
	cheese_day ->
	    S#storage{cheese = S#storage.cheese - 1};
	lettuce_day ->
	    S#storage{lettuce = S#storage.lettuce - 1};    
	grapes_day ->
	    S#storage{grapes = S#storage.grapes - 1}
    end; 
next_state_data(_, _, S, _, {call,_,_,_}) ->
    S.

%% precondition(Today, _, S, {call,_,hungry,[]}) ->
%%     case Today of
%% 	cheese_day->
%% 	    S#storage.cheese > 0;
%% 	lettuce_day ->
%% 	    S#storage.lettuce > 0;
%% 	grapes_day ->
%% 	    S#storage.grapes > 0
%%     end;
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

postcondition(cheese_day, _, S, {call,_,hungry,[]}, Result) ->
    Cheese = S#storage.cheese,
    Cheese > 0 andalso Result =:= {cheese_left, Cheese};
postcondition(lettuce_day, _, S, {call,_,hungry,[]}, Result) ->
    Lettuce = S#storage.lettuce,
    Lettuce > 0 andalso Result =:= {lettuce_left, Lettuce};
postcondition(grapes_day, _, S, {call,_,hungry,[]}, Result) ->
    Grapes = S#storage.grapes,
    Grapes > 0 andalso Result =:= {grapes_left, Grapes};
postcondition(_, _, _, _, Result) ->
    Result =:= ok.

weight(_, _, {call,_,new_day,_}) -> 1;
weight(_, _, {call,_,hungry,_}) -> 3;
weight(_, _, {call,_,buy,_}) -> 3.

prop_doesnt_run_out_of_supplies() ->
    ?FORALL(Cmds, proper_fsm:commands(?MODULE),
	    begin
		start(cheese_day),
		{History, State, Result} =
		    proper_fsm:run_commands(?MODULE, Cmds),
		stop(),
		?WHENFAIL(
		   io:format("History: ~w\nState: ~w\nResult: ~w\n",
			     [History, State, Result]),
		   aggregate(zip(proper_fsm:state_names(History),
				 command_names(Cmds)), Result =:= ok))
	    end).
