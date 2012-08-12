%%% @author convict <convict@laura.lan>
%%% @copyright (C) 2012, convict
%%% @doc
%%% cowboy acceptors
%%% @end
%%% Created :  3 Jun 2012 by convict <convict@laura.lan>
-module(pichub_web).
-behaviour(application).
-author("convict <convict@laura>").

-include("config.hrl").

-export([start/0,
         start/2,
         stop/1]).

start() ->
    ?DEBUG("START COWBOY", []),
    application:start(cowboy),
    application:start(crypto),
    application:start(pichub).


stop(_Pid) ->
    application:stop(cowboy),
    application:stop(crypto),
    application:stop(pichub).

start(_T, _A) ->
    Dispatch = [
                {'_', [
                       {[<<"user">>, '...'], user_handler, []},
                       {[<<"unread">>], slave_handler, []},
                       {[<<"read">>, '_', '_'], slave_handler, []},

                       {'_', default_handler, []}
                      ]}
               ],
    cowboy:start_listener(spock_http_listener, 100,
                          cowboy_tcp_transport, [{port, ?HTTP_PORT}],
                          cowboy_http_protocol, [{dispatch, Dispatch}]
                         ),
    pichub_sup:start_link().
