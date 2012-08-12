-module(slave).

-export([get_unread/1,
         get_unread/2,
         
         set_as_read/3]).

-include("user.hrl").

get_unread(UId) ->
    get_unread(UId, '_').

get_unread(UId, CId) ->
    MatchObj = #user_unread{key = {UId, '_'},
                            cid = CId,
                            _   = '_'},
    case db_mnesia:get_match_object(MatchObj) of
        {ok, []} ->
            none;
        {ok, Records} ->
            Response = unread_to_term(Records),
            Response
    end.

set_as_read(UId, CId, OId) ->
    {ok, R} = db_mnesia:transaction(fun() -> do_set_as_read(UId, CId, OId) end),
    R.
do_set_as_read(UId, CId, OId) ->
    MatchObj = #user_unread{key = {UId, '_'},
                            cid = CId,
                            oid = OId},
    case db_mnesia:get_match_object(MatchObj) of
        {ok, []} -> ok;
        {ok, [R]} -> db_mnesia:delete_object(R)
    end.
                        
%% Internal
unread_to_term(Records) ->
    [ {utils:convert(CId, bin), OId} || #user_unread{cid=CId, oid=OId} <- Records ].
