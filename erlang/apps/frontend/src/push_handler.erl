-module(push_handler).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).
-import(genreq, [do404/1, do400/1, do201/1, request_to_record/1]).

-include("job.hrl").

init(_Transport, Req, []) ->
    {ok, Req, undefined}.

%% The entry point into our server. Currently can push but cannot pop.
handle(Req, State) ->
    case cowboy_req:method(Req) of
        {<<"GET">>, _} ->
            {ok, Req2} = do404(Req);
        {<<"POST">>, _} ->
            {ok, Req2} = handlePOST(Req);
        Else ->
            io:format("~p~n", [Else]),
            {ok, Req2} = do404(Req)
    end,
    {ok, Req2, State}.

handlePOST(Req) ->
    Body = cowboy_req:body(Req),
    case Body of
        {ok, Data, Req2} ->
            Decoded = jsx:decode(Data),
            case request_to_record(Decoded) of
                {error, Reason} ->
                    lager:info("Bad Request: ~p / ~p~n", [Decoded, Reason]),
                    do400(Req2);
                JobRecord ->
                    push(JobRecord, Req2)
            end;
        {error, Reason} ->
            io:format("~p~n", [Reason]),
            do404(Req)
    end.

terminate(_Reason, _Req, _State) ->
    ok.

%% Push will call the underlying querl module to put the job into the
%% queue.
push(JobRecord, Req) ->
    case querl:add(JobRecord, JobRecord#job.queuename) of
        ok ->
            do201(Req);
        Else ->
            print([Else, JobRecord]),
            do400(Req)
    end.

print(What) ->
    io:format("~p~n", [What]).
