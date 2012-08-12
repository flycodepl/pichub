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

-spec get_token() -> string().
get_token() ->
    <<A:(16*8), _/binary>> = crypto:rand_bytes(16),
    list_to_binary(lists:sublist(lists:flatten(io_lib:format("~25.36.0b", [A])), 1, 32)).

to_int(X) when is_binary(X) -> to_int(binary_to_list(X));
to_int(X) when is_list(X) -> list_to_integer(X);
to_int(X) when is_integer(X) -> X;
to_int(X) when X==undefined -> undefined;
to_int(X) when is_atom(X) -> to_int(atom_to_list(X)).

random_string() ->
    sockjs_util:rand_string(9).

fmt(S, A) ->
    convert(io_lib:format(S, A), bin).

b2a(B) when is_binary(B) ->
    binary_to_atom(B, latin1).

a2b(A) when is_atom(A) ->
    atom_to_binary(A, latin1).

i2b(I) when is_integer(I) ->
    list_to_binary(integer_to_list(I)).
b2i(I) when is_binary(I) ->
    list_to_integer(binary_to_list(I)).

convert(Val, int)
  when is_list(Val) ->
    list_to_integer(Val);

convert(Val, int)
  when is_binary(Val) ->
    list_to_integer(binary_to_list(Val));

convert(Val, atom)
  when is_list(Val) ->
    list_to_atom(Val);

convert(Val, atom)
  when is_binary(Val) ->
    b2a(Val);

convert(Val, str)
  when is_integer(Val) ->
    integer_to_list(Val);

convert(Val, str)
  when is_float(Val) ->
    float_to_list(Val);

convert(Val, str)
  when is_atom(Val) ->
    atom_to_list(Val);

convert(Val, str)
  when is_binary(Val) ->
    binary_to_list(Val);

convert(Val, bin)
  when is_list(Val) ->
    iolist_to_binary(Val);

convert(Val, bin)
  when is_atom(Val) ->
    a2b(Val);

convert(Val, bin)
  when is_integer(Val) ->
    i2b(Val);

convert(Val, bin)
  when is_binary(Val) ->
    Val;

convert(undefined, bool) ->
    false;

convert(false, bool) ->
    false;

convert("", bool) ->
    false;

convert("0", bool) ->
    false;

convert("false", bool) ->
    false;

convert(<<"">>, bool) ->
    false;

convert(<<"0">>, bool) ->
    false;

convert(<<"false">>, bool) ->
    false;

convert(_, bool) ->
    true;

convert(Val, _) ->
    Val.

fltr(L) ->
    [ {K, V} || {K, V} <- L, V /= undefined, V /= "", V /= <<"">> ].

optconvert(undefined, _V) ->
    undefined;
optconvert(V, T) ->
    utils:convert(V, T).

%% set_cookies(Lists, Req) ->
%%     ListCookies = [ cowboy_cookies:cookie(K,V,O) || {K,V,O} <- Lists ],
%%     Cookies = concat_cookies(ListCookies, []),
%%     cowboy_http_req:set_resp_header(<<"Set-Cookie">>, Cookies, Req).

%% %% concat_cookies([], Acc) ->
%% %%     A = lists:reverse(Acc),
%% %%     io:fwrite("A: ~p~n", [A]),
%% %%     Rev = string:join( A, ", "),
%% %%     list_to_binary(Rev);
%% %% concat_cookies([{_,V}|R], Acc) ->
%% %%     Acc2 = [binary_to_list(V)] ++ Acc,
%% %%     concat_cookies(R, Acc2).

%% concat_cookies([], AccValue, AccOpts) ->
%%     USortValue = lists:usort(AccValue),
%%     USortOpts = lists:usort(AccOpts),
%%     USortList = USortValue ++ USortOpts,
%%     Join = string:join(USortList, "; "),
%%     list_to_binary(Join);

%% concat_cookies([{_, V} | R], AccValue, AccOpts) ->
%%     [D | O] = string:tokens(binary_to_list(V), "; "),
%%     AccValue2 = [D] ++ AccValue,
%%     AccOpts2 = O ++ AccOpts,
%%     concat_cookies(R, AccValue2, AccOpts2).

parse_url([<<"unread">>]) ->
    unread;
parse_url([<<"unread">>, CId]) ->
    {unread, utils:convert(CId, int)};
parse_url([<<"read">>, CId, OId]) ->
    {read, utils:convert(CId, int), utils:convert(OId, int)}.
