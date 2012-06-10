%%% @author convict <convict@magda>
%%% @copyright (C) 2012, convict
%%% @doc
%%%
%%% @end
%%% Created : 14 Mar 2012 by convict <convict@magda>

-module(utils).

-include("config.hrl").
-include("user.hrl").
-include("obj.hrl").

-compile(export_all).

record_field(user) ->
    record_info(fields, user);
record_field(user_info) ->
    record_info(fields, user_info);
record_field(user_unread) ->
    record_info(fields, user_unread);
record_field(obj_config) ->
    record_info(fields, obj_config);
record_field(id_seq) ->
    record_info(fields, id_seq);





%% OBJECT
record_field(obj_kwejk) ->
    record_info(fields, obj_kwejk).


printable_date() ->
    printable_date(calendar:now_to_local_time(now())).
printable_date(undefined) ->
    [];
printable_date({{Y,Mo,D},{H, M, S}}) ->
    Str = io_lib:fwrite("~b-~s-~b::~b:~b:~b", [D,month(Mo),Y,H,M,S]),
    iolist_to_binary(Str).

time() ->
    calendar:now_to_local_time(now()).


month(1) -> "Jan";
month(2) -> "Feb";
month(3) -> "Mar";
month(4) -> "Apr";
month(5) -> "May";
month(6) -> "Jun";
month(7) -> "Jul";
month(8) -> "Aug";
month(9) -> "Sep";
month(10) -> "Oct";
month(11) -> "Nov";
month(12) -> "Dec".


get_data_from_site(Site) ->
    case ibrowse:send_req(Site, [], get) of
        {ok, "200", Header, Data} ->
            {ok, Header, Data};
        {ok, OtherCode, Header} ->
            ?DEBUG("Get site (~s) failed - unknown response code: ~s~nHeader: ~p",
                   [Site, OtherCode, Header]),
            {error, unknown_response};
        {error, Desc} ->
            ?DEBUG("Get site (~s) failed:~n Error msg: ~p", [Site, Desc]),
            {error, Desc}                
    end.

get_priv_path(App) ->
    AppFile = App++".app",
    FilePath = code:where_is_file(AppFile),
    FilePath2 = filename:dirname(filename:absname(FilePath)),
    filename:join([FilePath2, "..", "priv"]).
