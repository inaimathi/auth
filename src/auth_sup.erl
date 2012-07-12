-module(auth_sup).
-behavior(supervisor).

-export([start/0, start_link/1, init/1]).

start() ->
    spawn(fun() -> supervisor:start_link({local, ?MODULE}, ?MODULE, _Arg = []) end).

start_link(Args) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, Args).


init([]) ->
    {ok, {{one_for_one, 3, 10},
	  [
	   {users, {users, start, []}, permanent, 5000, worker, [users]},
	   {groups, {groups, start, []}, permanent, 5000, worker, [groups]},
	   {rsa_auth, {rsa_auth, start, ["priv/server_auth.key", "password"]}, permanent, 5000, worker, [rsa_auth]},
	   {m2crypto, {m2crypto, start, []}, permanent, 5000, worker, [m2crypto]}
	  ]}}.
