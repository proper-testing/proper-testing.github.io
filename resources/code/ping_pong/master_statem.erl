-module(master_statem).
-behaviour(proper_statem).

-include_lib("proper/include/proper.hrl").

-export([initial_state/0, command/1, precondition/2, next_state/3,
	 postcondition/3]).

-type name() :: atom().
-type score() :: non_neg_integer().

-record(state, {players = [] :: [name()],
		scores  = [] :: [{name(),score()}]}).

-define(MASTER, ping_pong).
-define(NAMES, [bob, alice, john, mary, ben]).


%%% Property

prop_master() ->
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

initial_state() -> #state{}.

command(#state{players = []}) ->
    {call,?MASTER,add_player,[name()]};
command(S) ->
    oneof([{call,?MASTER,add_player,[name()]},
	   {call,?MASTER,remove_player,[name(S)]},
	   {call,?MASTER,get_score,[name(S)]},
	   {call,?MASTER,ping,[name(S)]}]).

name() ->
    elements(?NAMES).

name(S) ->
    elements(S#state.players).

precondition(S, {call,_,add_player,[Name]}) ->
    not lists:member(Name, S#state.players);
precondition(S, {call,_,remove_player,[Name]}) ->
    lists:member(Name, S#state.players);
precondition(S, {call,_,get_score,[Name]}) ->
    lists:member(Name, S#state.players);
precondition(S, {call,_,ping,[Name]}) ->
    lists:member(Name, S#state.players).

next_state(S, _V, {call,_,add_player,[Name]}) ->
    S#state{players = [Name|lists:delete(Name, S#state.players)],
	    scores = [{Name,0}|S#state.scores]};
next_state(S, _V, {call,_,remove_player,[Name]}) ->
    S#state{players = lists:delete(Name, S#state.players),
	    scores = proplists:delete(Name, S#state.scores)};
next_state(S = #state{scores = Scores}, _V, {call,_,ping,[Name]}) ->
    Score = proplists:get_value(Name, Scores),
    S#state{scores = [{Name,Score+1}|proplists:delete(Name, Scores)]};
next_state(S, _, _) ->    
    S.

postcondition(_S, {call,_,add_player,[_Name]}, Res) ->
    Res =:= ok;
postcondition(_S, {call,_,remove_player,[Name]}, Res) ->
    Res =:= {removed,Name};
postcondition(S, {call,_,get_score,[Name]}, Res) ->
    Res =:= proplists:get_value(Name, S#state.scores);
postcondition(_S, {call,_,ping,[_Name]}, Res) ->
    Res =:= pong.
