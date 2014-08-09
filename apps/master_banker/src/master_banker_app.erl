-module(master_banker_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    application:start(mnesia),
    master_banker_sup:start_link().

stop(_State) ->
    application:stop(mnesia),
    ok.
