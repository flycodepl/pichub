%% @author convict <convict@laura.lan>

-module(pichub_app).
-author('convict <convict@laura.lan>').

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    pichub_sup:start_link().

stop(_State) ->
    ok.
