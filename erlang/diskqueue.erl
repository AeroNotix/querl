-module(diskqueue).
-compile(export_all).

newqueue(Name) ->
    Pid1 = spawn(?MODULE, queue, [dict:from_list([{"file", Name}]), []]),
    Pid2 = spawn(?MODULE, timerout, [Pid1, [10000, 3000]]),
    spawn(?MODULE, monitor, [Pid1, Pid2]),
    Pid1.

monitor(Pid1, Pid2) ->
    process_flag(trap_exit, true),
    link(Pid1),
    link(Pid2),
    receive
        {Value, Pid1, Reason} ->
            io:format("~p~n", [Reason]),
            Pid2 ! quit;
        {Value, Pid2, Reason} ->
            io:format("~p~n", [Reason]),
            Pid1 ! quit
    end.
    
queue(Settings, State) ->
    receive
        quit ->
            ok;
        {push, Who, Element} ->
            Who ! ok,
            queue(Settings, lists:append(State, [Element]));
        {pop, Who} ->
            Who ! {pop, State ++ fileio:get_stored(dict:fetch("file", Settings))},
            queue(Settings, []);
        {save, Who} ->
            Who ! ok,
            fileio:save(dict:fetch("file", Settings), State),
            queue(Settings, [])    
    end.

timerout(Pid, [CheckTime, ErrorTime]) ->
    receive
        quit ->
            ok
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

logerror(Pid, Message) ->
    io:format("Pid: ~p ~p", [Pid, Message]).
