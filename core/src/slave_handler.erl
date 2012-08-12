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

    {Method, Req2} = cowboy_http_req:method(Req),
    {Url, Req3} = cowboy_http_req:path(Req2),
    {IfLog, UId, Token, Req4} = user:check_token_from_req(Req3),
    CheckUser = {IfLog, UId, Token},
    {Req5, Code, Response0} = do(Method, Url, CheckUser, Req4),
    Response = case Response0 of
                   none -> [];
                   X -> jsx:to_json(X)
               end,
    {ok, Req6} = cowboy_http_req:reply(Code, H, Response, Req5),
    {ok, Req6, State}.

terminate(_Req, _State) ->
    ok.

%% NOT LOGIN
do(_Method, _Type, {false, _,_}, Req) ->
    Response = [{<<"error">>, <<"forbidden">>}],
    {Req, 403, Response};

%% GET ALL UNREAD
do('GET', [<<"unread">>], {true, UId, _Token}, Req) ->
    case db_mnesia:get_match_object(#user_unread{key = {UId, '_'},
                                                 _ = '_'}) of
        {ok, []} ->
            {Req, 304, none};
        {ok, Records} ->
            Response = unread_to_term(Records),
            {Req, 200, Response}
    end;

%% SET AS READ
do('GET', [<<"read">>, CId, OId], {true, UId, _Token}, Req) ->

    case db_mnesia:get_match_object(#user_unread{key = {UId, '_'},
                                                 cid = CId}) of
        {ok, []} ->
            {Req, 304, none};
        {ok, Records} ->
            Response = unread_to_term(Records),
            {Req, 200, Response}
    end;

do(_Method, _Type, _CheckUser, Req) ->
    io:fwrite("CHECK: ~p~n~n", [_CheckUser]),
    Url = [{<<"method">>, utils:convert(_Method, bin)},
           {<<"type">>, utils:convert(_Type, bin) }],
    Response = [{<<"error">>, <<"not_found">>},
                {<<"request">>, Url}],
    {Req, 404, Response}.

unread_to_term(Records) ->
    [ {utils:convert(CId, bin), OId} || #user_unread{cid=CId, oid=OId} <- Records ].
