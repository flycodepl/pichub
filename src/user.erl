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

-export([register_user/2]).




register_user(Username, Password) ->
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
