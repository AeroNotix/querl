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
			pop(request_to_record_raw(jsx:decode(Data)), Req);
        {error, Reason} ->
            io:format("~p~n", [Reason]),
            do404(Req)
    end.

pop(Job, Req) ->
	case Job#job.queuename of
		undefined ->
			do404(Req);
		QueueName ->
			case querl:remove(QueueName) of
				{error, Reason} ->
					JSON = [{<<"error">>, atom_to_binary(Reason, utf8)}],
					cowboy_req:reply(200, [], jsx:encode(JSON), Req);
				Data ->
					cowboy_req:reply(200, [], job_list(Data), Req)
			end
	end.

ll2bin(LL) ->
	ll2bin(LL, []).
ll2bin([], Rest) ->
	Rest;
ll2bin([H|T], Rest) ->
	ll2bin(T, [Rest|list_to_binary(H)]).

job_list(Jobs) ->
	jsx:encode([{<<"jobs">>, ll2bin(Jobs)}]).

terminate(_Reason, _Req, _State) ->
    ok.
