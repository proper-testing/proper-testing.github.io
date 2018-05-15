-module(ping_pong).

-behaviour(gen_server).

%% API
-export([start_link/0, stop/0, add_player/1, remove_player/1,
	 get_score/1, ping/1]).
-export([play_ping_pong/1, play_tennis/1, play_football/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(MASTER, ?MODULE).
-define(PLAYER, ?MODULE).


%%%-------------------------------------------------------------------
%%% Master's API
%%%-------------------------------------------------------------------

start_link() ->
    gen_server:start_link({local, ?MASTER}, ?MASTER, [], []).

stop() -> 
    gen_server:cast(?MASTER, stop).

add_player(Name) ->
    gen_server:call(?MASTER, {add_player, Name}).

remove_player(Name) ->
    gen_server:call(?MASTER, {remove_player, Name}).

get_score(Name) ->
    gen_server:call(?MASTER, {get_score, Name}).

ping(FromName) ->
    gen_server:call(?MASTER, {ping, FromName}).


%%%-------------------------------------------------------------------
%%% Player's API
%%%-------------------------------------------------------------------

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


%%%-------------------------------------------------------------------
%%% Player's internal loop
%%%-------------------------------------------------------------------

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


%%%-------------------------------------------------------------------
%%% Gen_server callbacks
%%%-------------------------------------------------------------------

init([]) ->
    {ok, dict:new()}.

handle_call({add_player, Name}, _From, Dict) ->
    case whereis(Name) of
	undefined ->
	    Pid = spawn(fun () -> ping_pong_player(Name) end),
	    true = register(Name, Pid),
	    {reply, ok, dict:store(Name, 0, Dict)};
	Pid when is_pid(Pid) ->
	    {reply, ok, Dict}
    end;
handle_call({remove_player, Name}, _From, Dict) ->
    Pid = whereis(Name),
    exit(Pid, kill),
    {reply, {removed, Name}, dict:erase(Name, Dict)};
handle_call({ping, FromName}, _From, Dict) ->
    case dict:is_key(FromName, Dict) of
	true ->
	    {reply, pong, dict:update_counter(FromName, 1, Dict)};
	false ->
	    {reply, {removed, FromName}, Dict}
    end;
handle_call({get_score, Name}, _From, Dict) ->
    Score = dict:fetch(Name, Dict),
    {reply, Score, Dict}.

handle_cast(stop, Dict) ->
    {stop, normal, Dict}.

handle_info(_Info, S) ->
    {noreply, S}.

terminate(_Reason, Dict) ->
    Players = dict:fetch_keys(Dict),
    lists:foreach(fun (Name) -> exit(whereis(Name), kill) end, Players).

code_change(_OldVsn, S, _Extra) ->
    {ok, S}.
