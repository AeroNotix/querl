-module(toppage_handler).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

-include("job.hrl").

init(_Transport, Req, []) ->
    {ok, Req, undefined}.

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

baseresp(Number, Message, Req) ->
    cowboy_req:reply(Number, [], Message, Req).

do404(Req) ->
    baseresp(404, <<"Page Not Found\n">>, Req).
do400(Req) ->
    baseresp(400, <<"Bad Request\n">>, Req).
do201(Req) ->
    baseresp(201, <<>>, Req).

handlePOST(Req) ->
    Body = cowboy_req:body(Req),
    case Body of
        {ok, Data, Req2} ->
            Decoded = jsx:decode(Data),
            case request_to_record(Decoded) of
                {error, Reason} ->
                    io:format("Bad Request: ~p / ~p~n", [Decoded, Reason]),
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

push(JobRecord, Req) ->
    case querl:add(JobRecord, JobRecord#job.queuename) of
        ok ->
            do201(Req);
        Else ->
            print([Else, JobRecord]),
            do400(Req)
    end.

request_to_record(Raw) ->
    Queue = proplists:get_value(<<"queue">>, Raw),
    ID = proplists:get_value(<<"id">>, Raw),
    EntryDate = proplists:get_value(<<"entrydate">>, Raw),
    case any([Queue, ID, EntryDate], undefined) of
        false ->
            {error, missing_fields};
        true ->
            #job{
          id=ID,
          queuename=binary_to_list(Queue),
          entrydate=EntryDate
         }
    end.

print(What) ->
    io:format("~p~n", [What]).

any([], _What) ->
    true;
any([H|StuffList], What) ->
    case H of
        What ->
            false;
        _ ->
            any(StuffList, What)
    end.
