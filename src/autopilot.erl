-module(autopilot).
-export([start/0]).

start() ->
    application:start(explane),
    application:start(autopilot).
