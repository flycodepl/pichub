
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
         get_record/2]).

-export([get_next/1,
         get_next/2,
         user_exist/1]).

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
    init_obj(?OBJ).

init_user() ->
    create_table(user),
    create_table(user_info), 
    create_table(user_unread, bag),
    add_record(#id_seq{thing = user}).
    
init_obj(ObjList) ->
    create_table(obj_config),
    [ Module:first_init() || Module <- ObjList ].

init_other() ->
    create_table(id_seq).


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

get_next(Thing) ->
    get_next(Thing, 1).
get_next(Thing, Increment) ->
    {atomic, I} = mnesia:sync_transaction(
                    fun() ->
                            mnesia:dirty_update_counter(id_seq, Thing, Increment)
                    end),
    I.


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


            
    
