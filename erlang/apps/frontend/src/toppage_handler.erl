-module(toppage_handler).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

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
            case extract_element("queue", Decoded) of
                {error, Reason} ->
                    io:format("~p~n", [Reason]),
                    cowboy_req:reply(404, [], <<"Page Not Found\n">>, Req2);
                QueueName ->
                    push(QueueName, Decoded, Req2)
            end;
        {error, Reason} ->
            io:format("~p~n", [Reason]),
            cowboy_req:reply(404, [], <<"Page Not Found\n">>, Req)
    end.

terminate(_Reason, _Req, _State) ->
    ok.

push(QueueName, JSON, Req) ->
    cowboy_req:reply(200, [], QueueName, Req).

extract_element(_Element, []) ->
    {error, not_found};
extract_element(Element, [H|JSON]) ->
    case is_list(Element) of
        true ->
            extract_element(list_to_binary(Element), [H|JSON]);
        false ->
            {E1, Data} = H,
            if
                Element =:= E1 ->
                    Data;
                true ->
                    extract_element(Element, JSON)
            end
    end.
