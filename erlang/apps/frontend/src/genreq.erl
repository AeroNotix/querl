-module(genreq).

-include("job.hrl").

%% Web API
-export([do404/1, do400/1, do201/1]).

%% Record API
-export([request_to_record/1]).

%% Some basic helper methods for certain response types.
baseresp(Number, Message, Req) ->
    cowboy_req:reply(Number, [], Message, Req).
do404(Req) ->
    baseresp(404, <<"Page Not Found\n">>, Req).
do400(Req) ->
    baseresp(400, <<"Bad Request\n">>, Req).
do201(Req) ->
    baseresp(201, <<>>, Req).

%% Request to record will take a proplist from the request and return
%% possibly a job record or an error depending on whether the request
%% was valid.
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

any([], _What) ->
    true;
any([H|StuffList], What) ->
    case H of
        What ->
            false;
        _ ->
            any(StuffList, What)
    end.
