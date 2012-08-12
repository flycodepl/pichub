-module(user_handler).
-behaviour(cowboy_http_handler).
-export([init/3,
         handle/2,
         terminate/2]).

init({_Any, http}, Req, []) ->
    {ok, Req, undefined}.

handle(Req, State) ->
    H = [{<<"Content-Type">>, <<"application/json">>},
         {<<"Cache-Control">>, <<"max-age=0, private">>}
        ],

    {Method, Req2} = cowboy_http_req:method(Req),
    {Url, Req3} = cowboy_http_req:path(Req2),
    {Req4, Code, Response, MoreHeaders} = do(Url, Req3),
    Headers = lists:flatten([MoreHeaders, H]),
    {ok, Req5} = cowboy_http_req:reply(Code, Headers,
                                       jsx:to_json(Response), Req4),
    {ok, Req5, State}.

terminate(_Req, _State) ->
    ok.


do([<<"user">>, <<"login">>, Username, Password], Req) ->
    {IP, Req2} = cowboy_http_req:peer_addr(Req),
     case user:login(Username, Password, IP) of
         {ok, UId, Token} ->
             StringUId = io_lib:fwrite("uid=~b; path=/", [UId]),
             StringToken = io_lib:fwrite("token=~s; path=/", [Token]),
             %% Set cookie 'uid' and 'token'
             Cookies = [{<<"Set-Cookie">>, StringUId},
                        {<<"Set-Cookie">>, StringToken}],
             {Req2, 200, <<"ok">>, Cookies};
         {error, not_match} ->
             {Req2, 404, <<"error">>, []}
     end;

do([<<"user">>, <<"logout">>], Req) ->
     case user:check_token_from_req(Req) of
         {true, UId, _Token, Req2} ->
             ok = user:logout(UId),
             {Req2, 200, <<"ok">>, []};
         {false, _, _, Req2} ->
             {Req2, 404, <<"not login">>, []}
     end;

do([<<"user">>, <<"test">>], Req) ->
     case user:check_token_from_req(Req) of
         {true, _, _, Req2} ->
             {Req2, 200, <<"ok">>, []};
         {false, Req2} ->
             {Req2, 404, <<"not login">>, []}
     end.
