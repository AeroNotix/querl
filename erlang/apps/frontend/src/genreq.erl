-module(genreq).

-include("job.hrl").

%% Web API
-export([do404/1, do400/1, do201/1]).

%% Record API
-export([request_to_record/1, request_to_record_raw/1]).

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
    Rec = request_to_record(Raw),
    Queue = Rec#job.queuename,
    ID = Rec#job.id,
    EntryDate = Rec#job.entrydate,
    case any([Queue, ID, EntryDate], undefined) of
        false ->
            {error, missing_fields};
        true ->
            Rec
    end.

%% Request to record raw will take a raw job request and return it, if
%% any of the fields are undefined it's up to the caller to care about
%% that.
request_to_record_raw(Raw) ->
    #job{id=proplists:get_value(<<"id">>, Raw),
         queuename=binary_to_list(proplists:get_value(<<"queue">>, Raw)),
         entrydate=proplists:get_value(<<"entrydate">>, Raw)}.

%% Any checks if the iterable contains any value of What. If so, we
%% return false since we're looking to have no instances of the What
%% value.
any([], _What) ->
    true;
any([H|StuffList], What) ->
    case H of
        What ->
            false;
        _ ->
            any(StuffList, What)
    end.
