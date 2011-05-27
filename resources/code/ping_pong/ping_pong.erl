-module(ping_pong).
-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).
-export([start_link/0, stop/0, add_player/1, remove_player/1,
	 get_score/1, ping/1]).
-export([play_ping_pong/1, play_tennis/1, play_football/1, cast_ping_pong/1]).
-export([ping_pong_player/1]).


%% Master's API

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() -> 
    gen_server:cast(?MODULE, stop).

add_player(Name) ->
    gen_server:call(?MODULE, {add_player,Name}).

remove_player(Name) ->
    gen_server:call(?MODULE, {remove_player,Name}).

get_score(Name) ->
    gen_server:call(?MODULE, {get_score,Name}).

ping(FromName) ->
    gen_server:call(?MODULE, {ping, FromName}).


%% Player's API

cast_ping_pong(Player) ->
    Player ! ping_pong.

play_ping_pong(Player) ->
    Player ! {ping_pong, self()},
    receive
        Reply -> Reply
    %% after
    %% 	1000 -> {removed,Player}
    end.

play_tennis(Player) ->
    Player ! {tennis, self()},
    receive
        Reply -> Reply
    %% after
    %% 	1000 -> {removed,Player}
    end.

play_football(Player) ->
    Player ! {football, self()},
    receive
	Reply -> Reply
    end.


%%--------------------------------------------------------------------
%%% Player's loop
%%--------------------------------------------------------------------

ping_pong_player(Name) ->
    receive
	{ping_pong,From} ->
	    From ! ping(Name); 
	{tennis,From} ->
	    %% ping(Name),
	    From ! maybe_later;
	{football,From} ->
	    From ! no_way;
	ping_pong ->
	    ping(Name)
    end,
    ping_pong_player(Name).


%%--------------------------------------------------------------------
%%% Gen_server callbacks
%%--------------------------------------------------------------------

init([]) ->
    {ok, dict:new()}.

handle_call({add_player,Name}, _From, Dict) ->
    case whereis(Name) of
	undefined ->
	    Pid = spawn(?MODULE, ping_pong_player, [Name]),
	    register(Name, Pid);
	Pid when is_pid(Pid) ->
	    ok
    end,
    {reply, ok, dict:store(Name, 0, Dict)};
handle_call({remove_player,Name}, _From, Dict) ->
    Pid = whereis(Name),
    exit(Pid, kill),
    {reply, {removed,Name}, dict:erase(Name, Dict)};
handle_call({ping,FromName}, _From, Dict) ->
    %% case dict:is_key(FromName, Dict) of
    %% 	true ->
	    {reply, pong, dict:update_counter(FromName, 1, Dict)};
    	%% false ->
    %% 	    {reply, invalid_player, Dict}
    %% end;
handle_call({get_score,Name}, _From, Dict) ->
    Score = dict:fetch(Name, Dict),
    {reply, Score, Dict}.

handle_cast(stop, Dict) ->
    {stop, normal, Dict}.

terminate(_Reason, Dict) ->
    Players = dict:fetch_keys(Dict),
    [exit(whereis(Name), kill) || Name <- Players,
				  whereis(Name) =/= undefined],
    ok.

handle_info(_Info, S) ->
    {noreply, S}.
code_change(_OldVsn, S, _Extra) ->
    {ok, S}.
