-module(querl).
-compile(export_all).
-behaviour(gen_server).

%% gen_server behaviour
-export([start/0,code_change/3,
         handle_call/3,init/1,
         handle_cast/2,handle_info/2,
         terminate/2]).

start() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init(Args) ->
    io:format("~p~n", [Args]),
    {ok, dict:new()}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% We don't have any specific needs for these yet but we need to over-
%% ride them for the gen_server behaviour.
handle_cast(_Request, State) ->
    {noreply, State}.
handle_info(_Message, Library) ->
    {noreply, Library}.
terminate(_Reason, _Library) -> ok.

%% Handle job stack
handle_call({add, What, Queue}, _From, State) ->
    Return = case dict:is_key(Queue, State) of
                 true ->
                     dict:fetch(Queue, State) ! {push, self(), What},
                     ok;
                 false ->
                     {error, "Invalid Queue"}
             end,
    {reply, Return, State};
handle_call({new, Queue}, _From, State) ->
    case dict:is_key(Queue, State) of
        true ->
            {reply, {error, "Queue Exists!"}, State};
        false ->
            {reply, ok, dict:store(Queue, diskqueue:newqueue(Queue), State)}
    end;
handle_call({remove, Queue}, _From, State) ->
    Return = case dict:is_key(Queue, State) of
                 true ->
                     dict:fetch(Queue, State) ! {pop, self()},
                     receive
                         {pop, Value} ->
                             Value
                     after 10000 ->
                             {error, "Timed out"}
                     end;
                 false ->
                     {error, "Invalid Queue"}
             end,
    {reply, Return, State}.

newqueue(Queue) ->
    gen_server:call(?MODULE, {new, Queue}).
add(What, Queue) ->
    gen_server:call(?MODULE, {add, What, Queue}).
remove(Queue) ->
    gen_server:call(?MODULE, {remove, Queue}).

rander(0) ->
    ok;
rander(N) ->
    FunList = [
               [fun newqueue/1, "tt"],
               [fun add/2, "sup", "tt"],
               [fun remove/1, "tt"]
              ],
    [Head|Rest] = lists:nth(random:uniform(2)+1, FunList),
    io:format("~p~n", [erlang:apply(Head, Rest)]),
    rander(N-1).

main() ->
    start(),
    newqueue("tt"),
    rander(100000).
