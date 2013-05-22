-module(genreq).

-export([do404/1, do400/1, do201/1]).

%% Some basic helper methods for certain response types.
baseresp(Number, Message, Req) ->
    cowboy_req:reply(Number, [], Message, Req).
do404(Req) ->
    baseresp(404, <<"Page Not Found\n">>, Req).
do400(Req) ->
    baseresp(400, <<"Bad Request\n">>, Req).
do201(Req) ->
    baseresp(201, <<>>, Req).
