-module(querl).
-compile(export_all).
-behaviour(gen_server).

%% gen_server behaviour
-export([start/0,code_change/3,
         handle_call/3,init/1,
         handle_cast/2,handle_info/2,
         terminate/2,add/1]).

start() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init(Args) ->
    io:format("~p~n", [Args]),
    {ok, []}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% We don't have any specific needs for these yet but we need to over-
%% ride them for the gen_server behaviour.
handle_cast(_Request, State) ->
    {noreply, State}.
handle_info(_Message, Library) ->
    {noreply, Library}.
terminate(_Reason, _Library) -> ok.

%% Locks a resource and assigns it to the client
handle_call({add, What}, _From, State) ->
    {reply, ok, lists:append(State, [What])};
handle_call({remove}, _From, State) ->
    [First|Rest] = case length(State) of
                       0 ->
                           [empty];
                       _Else ->
                           State
                   end,
    {reply, First, Rest}.

add(What) ->
    gen_server:call(?MODULE, {add, What}).
remove() ->
    gen_server:call(?MODULE, {remove}).
