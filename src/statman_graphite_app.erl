-module(statman_graphite_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    ok = application:ensure_started(asn1),
    ok = application:ensure_started(crypto),
    ok = application:ensure_started(public_key),
    ok = application:ensure_started(ssl),
    ok = application:ensure_started(lhttpc),
    ok = application:ensure_started(statman),

    statman_graphite_sup:start_link().

stop(_State) ->
    ok.
