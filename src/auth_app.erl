-module(auth_app).
-behaviour(application).
-export([start/2, stop/1]).

start(_Type, StartArgs) -> auth_sup:start_link(StartArgs).
stop(_State) -> ok.
