-module(pop_handler).

-export([handle/2]).
-export([terminate/3]).
-import(genreq, [do404/1, do400/1, do201/1]).

handle(Req, State) ->
    case cowboy_req:method(Req) of
        {<<"GET">>, _} ->
            {ok, Req2} = handleGET(Req);
        {<<"POST">>, _} ->
            {ok, Req2} = do404(Req);
        Else ->
            io:format("~p~n", [Else]),
            {ok, Req2} = do404(Req)
    end,
    {ok, Req2, State}.

handleGET(Req) ->
    cowboy_req:reply(200, <<"Sup\n">>, Req).

terminate(_Reason, _Req, _State) ->
    ok.
