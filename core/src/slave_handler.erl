-module(slave_handler).
-behaviour(cowboy_http_handler).
-export([init/3,
         handle/2,
         terminate/2]).

-include("user.hrl").

init({_Any, http}, Req, []) ->
    {ok, Req, undefined}.

handle(Req, State) ->
    H = [{<<"Content-Type">>, <<"application/json">>},
         {<<"Cache-Control">>, <<"max-age=0, private">>}
        ],
    case user:check_token_from_req(Req) of
        {false, _,_, Req2} -> %% IF NOT LOGIN
            {ok, Req3} = cowboy_http_req:reply(403, [], [], Req2),
            {ok, Req3, State};
        {true, UId, _Token, Req2} -> %% IF LOGIN
            {Method, Req3} = cowboy_http_req:method(Req2),
            {Url0, Req4} = cowboy_http_req:path(Req3),
            Url = utils:parse_url(Url0),
            case do(Method, Url, UId, Req4) of
                {Req5, Code, none} ->
                    {ok, Req6} = cowboy_http_req:reply(Code, H, [], Req5);
                {Req5, Code, Response0} ->
                    Response = jsx:to_json(Response0),
                    {ok, Req6} = cowboy_http_req:reply(Code, H, Response, Req5)
            end,
            {ok, Req6, State}
    end.

terminate(_Req, _State) ->
    ok.

%% GET ALL UNREAD
do('GET', unread, UId, Req) ->
    case slave:get_unread(UId) of
        none -> {Req, 304, none};
        Res  -> {Req, 200, Res}
    end;

%% GET UNREAD FROM CHANNEL
do('GET', {unread, CId}, UId, Req) ->
    case slave:get_unread(UId, CId) of
        none -> {Req, 304, none};
        Res  -> {Req, 200, Res}
    end;

%% SET AS READ
do('GET', {read, CId, OId}, UId, Req) ->
    ok = slave:set_as_read(UId, CId, OId),
    {Req, 200, none};

do(_Method, _Type, _CheckUser, Req) ->
    io:fwrite("CHECK: ~p~n~n", [_CheckUser]),
    Url = [{<<"method">>, utils:convert(_Method, bin)},
           {<<"type">>, utils:convert(_Type, bin) }],
    Response = [{<<"error">>, <<"not_found">>},
                {<<"request">>, Url}],
    {Req, 404, Response}.

