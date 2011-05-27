-module(player_statem).
-behaviour(proper_statem).

-include_lib("proper/include/proper.hrl").

-export([initial_state/0, command/1, precondition/2, next_state/3,
	 postcondition/3]).
-export([spawn_reg/1, ping_pong_player/1]).

-define(PLAYER, ping_pong).
-define(NAMES, [bob, alice, john, mary, ben]).


%%% Property

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

cleanup() ->
    [catch unregister(Name) || Name <- ?NAMES].


%%% Statem Callbacks

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

spawn_reg(Name) ->
    case whereis(Name) of
	undefined ->
	    Pid = spawn(?MODULE, ping_pong_player, [Name]),
	    %% erlang:yield(),
	    catch register(Name, Pid);
	Pid when is_pid(Pid) ->
	    true
    end.

ping_pong_player(Name) ->
    receive
	{ping_pong,From} ->
	    From ! pong; 
	{tennis,From} ->
	    From ! maybe_later;
	{football,From} ->
	    From ! no_way
    end,
    ping_pong_player(Name).
