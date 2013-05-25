-module(fileio).
-export([save/2, get_stored/1]).

dir() ->
	{ok, Dir} = application:get_env(dir),
	Dir.

save(File, Jobs) ->
    case file:open(dir() ++ File, [write, append]) of
        {ok, Fd} ->
            save_to_disk(Fd, Jobs);
        {error, Reason} ->
            {error, Reason}
    end.

get_stored(File) ->
    Data = case file:open(dir() ++ File, [read]) of
               {ok, Fd} ->
                   get_from_disk(Fd);
               {error, _Reason} ->
                   []                   
           end,
    file:delete(dir() ++ File),
    Data.

get_from_disk(Fd) ->
    get_from_disk(Fd, []).
get_from_disk(Fd, []) ->
    case file:read_line(Fd) of
        {ok, Data} ->
            get_from_disk(Fd, [Data]);
        eof ->
            [];
        {error, Reason} ->
            file:close(Fd),
            {error, Reason}
    end;
get_from_disk(Fd, Data) ->
    case file:read_line(Fd) of
        {ok, NewData} ->
            get_from_disk(Fd, lists:append(Data, [NewData]));
        eof ->
            Data;
        {error, Reason} ->
            file:close(Fd),
            {error, Reason}
    end.

save_to_disk(Fd, []) ->
    file:close(Fd),
    ok;
save_to_disk(Fd, [H|T]) ->
    case file:write(Fd, H) of
        ok ->
            case file:write(Fd, "\n") of 
                ok ->
                    save_to_disk(Fd, T);
                {error, Reason} ->
                    {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end.
