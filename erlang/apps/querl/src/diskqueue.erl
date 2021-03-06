-module(diskqueue).

%% API
-export([newqueue/1]).

%% Callbacks
-export([monitor/2, timerout/2, queue/2]).

%% newqueue takes a string name and returns a Pid of a process which
%% will be handling the queueing and dequeing of items passed in.
newqueue(Name) ->
	Pid1 = spawn(?MODULE, queue, [dict:from_list([{"file", Name}]), []]),
	Pid2 = spawn(?MODULE, timerout, [Pid1, [10000, 3000]]),
	spawn(?MODULE, monitor, [Pid1, Pid2]),
	Pid1.

%% monitor watches both the timer for the queue and the queue itself
%% for failures, any failures in either one will force the other pid
%% to die.
monitor(Pid1, Pid2) ->
	process_flag(trap_exit, true),
	link(Pid1),
	link(Pid2),
	receive
		{_Value, Pid1, Reason} ->
			lager:info("~p~n", [Reason]),
			Pid2 ! quit;
		{_Value, Pid2, Reason} ->
			lager:info("~p~n", [Reason]),
			Pid1 ! quit
	end.

%% queue is the underlying server loop for the queue
%%
%% We support several operations:
%%    - Quit
%%        Quit will end the server loop.
%%    - push
%%        Push takes two arguments, Who and Element. We take the Pid
%%        of the caller and the Element they want to push and store
%%        the element in our queue.
%%   -  pop
%%        Pop takes the Pid of the caller and dumps out all of the
%%        stored values to the caller.
%%   -  save
%%        Save forces all the in-memory items to-disk so they can be
%%        persisted in the case of failure.
%%   - reload
%%        Reload will start using the new version of the code on-disk.
%%   - timetoquit
%%        powers down the queue saving any state to disk.
queue(Settings, State) ->
	Filename = dict:fetch("file", Settings),
	receive
		quit ->
			ok;
		{push, Who, Element} ->
			Who ! ok,
			queue(Settings, lists:append(State, [Element]));
		{pop, Who} ->
			Who ! {pop, State ++ fileio:get_stored(Filename)},
			queue(Settings, []);
		{save, Who} ->
			Who ! ok,
			lager:info("Saving queue: ~p~n", [Filename]),
			case fileio:save(Filename, State) of
				ok ->
					queue(Settings, []);
				{error, Reason} ->
					lager:info("Pid: ~p, ~p~n", [Filename, Reason]),
					queue(Settings, State)
			end;
		reload ->
			lager:info("Reloading queue: ~p~n", [Filename]),
			?MODULE:queue(Settings, State);
		timetoquit ->
			lager:info("Quitting: ~p~n", [self()]),
			fileio:save(Filename, State),
			exit(caught_quit)
	end.

%% Timerout posts to the queue's mailbox when it's time to persist
%% items to disk.
timerout(Pid, [CheckTime, ErrorTime]) ->
	receive
		quit ->
			ok;
		reload ->
			?MODULE:timerout(Pid, [CheckTime, ErrorTime])
	after CheckTime ->
			Pid ! {save, self()},
			receive
				ok ->
					timerout(Pid, [CheckTime, ErrorTime])
			after ErrorTime ->
					logerror(Pid, "Timed out when saving to disk"),
					timerout(Pid, [CheckTime, ErrorTime])
			end
	end.

%% Helper to log messages.
logerror(Pid, Message) ->
	lager:info("Pid: ~p ~p", [Pid, Message]).
