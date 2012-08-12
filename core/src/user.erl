%%% @author convict <convict@magda>
%%% @copyright (C) 2012, convict
%%% @doc
%%% Operation at users
%%% @end
%%% Created : 14 Mar 2012 by convict <convict@magda>

-module(user).

-include("config.hrl").
-include("user.hrl").
-include("obj.hrl").

-export([login/3,
         logout/1,
         register/2,
         check_token/3,
         check_token_from_req/1]).

check_token_from_req(Req) ->
    {UId0, Req2} = cowboy_http_req:cookie(<<"uid">>, Req, undefined),
    {Token, Req3} = cowboy_http_req:cookie(<<"token">>, Req2, undefined),
    UId = utils:convert(UId0, int),
    case {UId, Token} of
        {U, T} when (U =:= undefined) or (T =:= undefined) ->
            {false, undefined, undefined, Req3};
        {U, T} ->
            {IP, Req4} = cowboy_http_req:peer_addr(Req3),
            Check = check_token(U, T, IP),
            {Check, UId, Token, Req4}
    end.
    
check_token(UId, Token, IP) ->
    case ?DB:get_record(user, UId) of
        {error, not_found} ->
            false;
        {ok, User} ->
            case {User#user.token, User#user.ip} of
                {Token, IP}  ->
                    true;
                _ ->
                    false
            end
    end.

login(Username, Password, IP) ->
    {ok, R} = ?DB:transaction(fun() -> do_login(Username, Password, IP) end),
    R.
do_login(Username, Password, IP) ->
    case ?DB:get_user(Username) of
        {error, not_found} ->
            {error, not_match};
        {ok, [User]} ->
            case User#user.password of
                Password ->
                    Token = utils:get_token(),
                    UpdateUser = User#user{ip = IP,
                                           token = Token},
                    ?DB:add_record(UpdateUser),
                    {ok, User#user.uid, Token};
                Other when is_binary(Other) ->
                    {error, not_match}
            end
    end.

logout(UId) ->
    {ok, R} = ?DB:transaction(fun() -> do_logout(UId) end),
    R.
do_logout(UId) ->
    case ?DB:get_record(user, UId) of
        {error, not_found} ->
            {error, not_found_user};
        {ok, User} ->
            UpdateUser = User#user{ip = undefined,
                                   token = undefined},
            ?DB:add_record(UpdateUser),
            ok
    end.

register(Username, Password) ->
    case ?DB:user_exist(Username) of
        false ->
            UId = ?DB:get_next(user),
            R = #user{uid = UId,
                      username = Username,
                      password = Password},
            ?DB:add_record(R),
            {ok, UId};
        true ->
            {error, user_exists}
    end.
