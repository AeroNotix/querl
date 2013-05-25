-module(pop_handler).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

%% Import the generic http handlers
-import(genreq, [do404/1, do400/1, do201/1, do200/1]).
%% Record tools
-import(genreq, [request_to_record_raw/1]).

-include("job.hrl").

init(_Transport, Req, []) ->
    {ok, Req, undefined}.

handle(Req, State) ->
    case cowboy_req:method(Req) of
        {<<"POST">>, _} ->
            {ok, Req2} = handlePOST(Req);
        _Else ->
            {ok, Req2} = do404(Req)
    end,
    {ok, Req2, State}.

handlePOST(Req) ->
    Body = cowboy_req:body(Req),
    case Body of
        {ok, Data, Req2} ->
            lager:info(Data),
            do200(Req2);
        {error, Reason} ->
            io:format("~p~n", [Reason]),
            do404(Req)
    end.

pop(Job, Req) ->
    io:format("~p~n", [Job]),
    do200(Req).

terminate(_Reason, _Req, _State) ->
    ok.
