
%%% @author convict <convict@magda>
%%% @copyright (C) 2012, convict
%%% @doc
%%%
%%% @end
%%% Created : 14 Mar 2012 by convict <convict@magda>

-module(db_mnesia).

-export([init/0,
         start/0]).

-export([create_table/1,
         add_record/1,
         add_records/1,
         get_record/2,
         get_match_object/1,
         delete_record/2,
         delete_object/1,
         delete_objects/1,
         transaction/1]).

-export([get_next/1,
         get_next/2,
         user_exist/1,
         get_user/1]).

-include("config.hrl").
-include("user.hrl").
-include("obj.hrl").

start() ->
    ?DEBUG("Starting mnesia", []),
    mnesia:start().


init() ->
    ?DEBUG("Init mnesia", []),    
    mnesia:create_schema([node()]),
    start(),

    init_other(),
    init_user(),
    test_data().
    %% init_obj(?OBJ).

init_user() ->
    create_table(user),
    create_table(user_info), 
    create_table(user_unread, ordered_set),
    add_record(#id_seq{thing = user}).
    
init_obj(ObjList) ->
    create_table(obj_config),
    [ Module:first_init() || Module <- ObjList ].

init_other() ->
    create_table(id_seq).

test_data() ->
    user:register(<<"convict">>, <<"qwerty">>).

create_table(Name) ->
    create_table(Name, set).
create_table(Name, Type) ->
    ?DEBUG("Create '~p' table", [Name]),
    ok =  case mnesia:create_table(Name, [{disc_copies, [node()]},
                                          {type, Type},
                                          {attributes,
                                           utils:record_field(Name)}]) of
              {atomic, ok} -> ok;
              {aborted, {already_exists, _}} -> ok
          end.

add_record(R) ->
    add_records([R]).

add_records(RL) ->
    case mnesia:transaction(fun() ->
                                    [ mnesia:write(R) || R <- RL ]
                            end) of
        {atomic, _} ->
            ok;
        {aborted, Error} ->
            {error, Error}
    end.

get_record(From, Id) ->
    case mnesia:transaction(fun() -> mnesia:read(From, Id) end) of
        {atomic, []} ->
            {error, not_found};
        {atomic, [R]} ->
            {ok, R}
    end.

get_match_object(Object) ->
    case mnesia:transaction(fun() -> mnesia:match_object(Object) end) of
        {atomic, R} ->
            {ok, R};
        {aborted, Reason} ->
            {error, Reason}
    end.

get_next(Thing) ->
    get_next(Thing, 1).
get_next(Thing, Increment) ->
    Fun = fun() ->
                  mnesia:dirty_update_counter(id_seq, Thing, Increment)
          end,
    {atomic, I} = mnesia:sync_transaction(Fun),
    I.

delete_record(From, Id) ->
    case mnesia:transaction(fun() -> mnesia:delete(From, Id, write) end) of
        {error,  _}  -> {error, not_found};
        {atomic, ok} -> ok
    end.

delete_object(Object) ->
    case mnesia:transaction(fun() -> mnesia:delete_object(Object) end) of
        {atomic,  ok}     -> ok;
        {aborted, Reason} -> {error, Reason}
    end.

delete_objects(Objects) ->
    case mnesia:transaction(fun() -> [delete_object(O) || O <- Objects] end) of
        {atomic,  _}      -> ok;
        {aborted, Reason} -> {error, Reason}
    end.


transaction(Fun) ->
    case mnesia:transaction(Fun) of
        {atomic, R} ->
            {ok, R};
        {aborted, Reason} ->
            {error, Reason}
    end.


%% OTHER
user_exist(Username) ->
    MatchHead = #user{username='$1', _ = '_'},
    Guard = {'==', '$1', Username},
    Result = '$_',
    case mnesia:transaction(
           fun() ->
                   mnesia:select(user, [{MatchHead, [Guard], [Result]}])
           end) of
        {atomic, [_User]} -> true;
        {atomic, []} -> false
    end.

get_user(Username) ->
    Obj = #user{username = Username, _='_'},
    get_match_object(Obj).
