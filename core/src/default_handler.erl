-module(default_handler).
-behaviour(cowboy_http_handler).
-export([init/3,
         handle/2,
         terminate/2]).

-include("config.hrl").

init({_Any, http}, Req, []) ->
    {ok, Req, undefined}.

handle(Req, State) ->
    {Path0, _} = cowboy_http_req:path(Req),
    Path = case Path0 of
               [] -> ["index.html"];
               _ -> Path0
           end,
    FilePath = filename:join(Path),
    WwwPath = list_to_binary(utils:get_priv_path("pichub") ++ "/www/"),
    File = filename:join([WwwPath, FilePath]),
    ?DEBUG("Request to file: ~p", [File]),
    %% FIXME: THERE IS A SECURITY HOLE HERE!
    case file:read_file(File) of
        {ok, Data} ->
            ContentType = get_content_type(File),
            {ok, Req1} = cowboy_http_req:reply(200, [{<<"Content-Type">>, ContentType},
                                                     {<<"Cache-Control">>, <<"max-age=0, private">>}],
                                               Data, Req),
            {ok, Req1, State};
        {error, Reason} ->
            ?DEBUG("Cant read file ~p. Reason: ~p", [File, Reason]),
            {ok, Req1} = cowboy_http_req:reply(404, [{<<"Content-Type">>, <<"text/html">>}],
                                               <<"404: File non found.">>, Req),
            {ok, Req1, State}
    end.

terminate(_Req, _State) ->
    ok.

get_content_type(Path) ->
    case filename:extension(Path) of
        <<".js">> -> <<"text/javascript">>;
        <<".html">> -> <<"text/html">>;
        _ -> <<"text/html">>
    end.
